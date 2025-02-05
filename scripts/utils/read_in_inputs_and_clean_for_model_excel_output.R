read_in_inputs_and_clean_for_model_excel_output = function(onedrive_wd){
  # Table of Region, species and waterbody names. Note: might be nicer to make this
  # in excel and read it in.

  d = readxl::read_excel("inputs_for_prioritization_model.xlsx")
  
  named_wbs = sf::read_sf("W:/CMadsen/shared_data_sets/summarized_bc_waterbodies.shp")
  
  d = d |> 
    dplyr::select(Region:Established_in_Waterbody)
  
  # If any rows have been added or multiple species, split those now
  d = d |> 
    tidyr::separate_longer_delim(cols = Species, delim = ", ")
  
  d = d |>
    dplyr::mutate(Species = dplyr::case_when(
      Species == 'Oriental weatherfish' ~ 'Oriental weather loach',
      Species == 'Fathead minnow' ~ 'Rosy red fathead minnow',
      Species == 'Mosquitofish' ~ 'Western mosquitofish',
      Species %in% c('Pumpkinseed sunfish','Pumpkinseed Sunfish') ~ 'Pumpkinseed',
      Species == 'Common freshwater jellyfish' ~ 'Freshwater jellyfish',
      Species == 'Bluegill' ~ 'Bluegill sunfish',
      Species == 'Yellow pickerel' ~ 'Walleye',
      Species %in% c("Asiatic clam","Golden clam","Good luck clam") ~ 'Asian clam',
      Species %in% c("Carp","European Carp","Common Carp") ~ "Common carp",
      Species == "Rosy red minnow" ~ "Rosy red fathead minnow",
      T ~ Species
    ))
  
  # =========================================
 
  # If user has input a Watershed, snag all the waterbodies for it.
  d_watershed = d |> 
    dplyr::filter(!is.na(Watershed)) |> 
    dplyr::group_by(Watershed) |> 
    dplyr::group_split()
  
  for(watershed_row in 1:length(d_watershed)){
    if(nrow(d_watershed[[watershed_row]]) > 0){
      
      
      the_watershed = unique(d_watershed[[watershed_row,]]$Watershed)
      
      wbs_overlap_sara_cdc = sf::read_sf(paste0(onedrive_wd,"waterbodies_overlapping_with_SARA_and_CDC_occs.gpkg"))
      
      if(str_detect(unique(d_watershed[[watershed_row]]$Watershed), "Columbia River")){
        major_watershed = sf::read_sf("W:/CMadsen/shared_data_sets/Columbia_River_Big_Watershed.shp") |> 
          dplyr::summarise()
      } 
      if(str_detect(unique(d_watershed[[watershed_row]]$Watershed), "Fraser River")){
        major_watershed = sf::read_sf("W:/CMadsen/shared_data_sets/Fraser_River_Big_Watershed.shp") |> 
          dplyr::summarise()
      } 
      # Filter for just those waterbodies overlapping with CDC or SARA in the chosen Watershed.
      # Currently only works for 'Columbia River Watershed' or 'Fraser River Watershed'
      wbs_overlap_sara_cdc = wbs_overlap_sara_cdc |> 
        sf::st_filter(major_watershed)
      
      # Find Natural Resource Regions for each waterbody.
      regions = bcmaps::nr_regions() |> 
        dplyr::mutate(Region = str_remove(REGION_NAME, " Natural.*")) |> 
        dplyr::select(Region)
      
      wbs_overlap_sara_cdc = wbs_overlap_sara_cdc |> 
        sf::st_join(regions)
      
      d_watershed_rows_for_join = wbs_overlap_sara_cdc |> 
        sf::st_drop_geometry() |> 
        dplyr::select(Region,
                      Waterbody = GNIS_NAME_) |> 
        dplyr::distinct() |> 
        crossing(Species = unique(d_watershed[[watershed_row]]$Species)) |> 
        dplyr::mutate(Established_in_Waterbody = NA,
                      Watershed = the_watershed)
      
      # If we are ONLY running the CNF Fraser and Columbia River Watershed rows,
      # we need to ensure that the 'Region' column is character, not logical.
      if(is.logical(d$Region)) d$Region = as.character(d$Region)
      
      d = d |> 
        dplyr::filter(!is.na(Region)) |>
        dplyr::bind_rows(d_watershed_rows_for_join)
    }
  }
  # # TEMPORARY REMOVAL OF CREEKS!! *
  # d = d |> 
  #   dplyr::filter(Waterbody != 'Bittner Creek')
  
  # Find Waterbodies (based on name and region!)
  unique_wbs = d |> 
    dplyr::select(Waterbody, Region) |> 
    dplyr::distinct()
  
  # If the waterbody is any of the following: Okanagan Lake, Okanagan River,
  # Skaha Lake, Vaseux Lake, Vaseux Creek, Osoyoos Lake, treat those as one monolithic
  # unit.
  TO_string = c("Okanagan Lake","Okanagan River","Skaha Lake",
                "Vaseux Lake","Vaseux Creek","Osoyoos Lake")
  
  # Add region to named waterbodies shapefile.
  named_wbs = named_wbs |> 
    sf::st_join(regions)
  
  # Filter for just those waterbodies named in d.
  named_wbs_unique = named_wbs |> 
    # We just want the largest geometry for any given name-region pairing!
    dplyr::group_by(GNIS_NAME_, Region) |> 
    dplyr::arrange(dplyr::desc(AREA_HA)) |> 
    dplyr::slice(1) |> 
    dplyr::ungroup() |> 
    # dplyr::filter(paste0(GNIS_NAME_,Region) %in% unique(paste0(d$Waterbody,d$Region))) |> 
    dplyr::select(Waterbody = GNIS_NAME_, Region, FWA_WATERSHED_CODE = FWA_WAT)
  
  # Left join the waterbody geometry onto the d table based on Region and Waterbody name.
  d = d |> 
    dplyr::left_join(named_wbs_unique)
  
  d = sf::st_set_geometry(d, 'geometry')
  
  # Who still needs geometries?? Probably any streams.
  d = d |> 
    dplyr::mutate(row_numb = row_number())
  
  d_still_needs_geom = d |> 
    dplyr::filter(sf::st_is_empty(geometry)) |> 
    dplyr::select(-FWA_WATERSHED_CODE)
  
  if(nrow(d_still_needs_geom) > 0){
    stream_geometries = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
      filter(GNIS_NAME %in% d_still_needs_geom$Waterbody) |> 
      collect() |> 
      sf::st_zm() |> 
      sf::st_transform(4326) |>
      dplyr::group_by(GNIS_NAME, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |> 
      dplyr::summarise() |> 
      dplyr::ungroup() |> 
      sf::st_join(regs |> dplyr::select(Region = REGION_NAME) |> dplyr::mutate(Region = str_remove(Region, " Natural.*"))) |> 
      sf::st_buffer(dist = 10) |> 
      sf::st_transform(3005) |> 
      dplyr::select(Region, Waterbody = GNIS_NAME, FWA_WATERSHED_CODE)
    
    d_still_needs_geom = d_still_needs_geom |> 
      sf::st_drop_geometry() |> 
      dplyr::left_join(stream_geometries)
    
    
    d = d |> 
      dplyr::filter(!sf::st_is_empty(geometry)) |> 
      dplyr::bind_rows(d_still_needs_geom) |> 
      dplyr::arrange(row_numb) |> 
      dplyr::select(-row_numb)
  }
  # Add waterbody area and depth if available.
  d$Waterbody_Area_sq_km = as.numeric(sf::st_area(d$geometry))/1000000
  
  # Transform d's geometry to WGS 84.
  d = d |> 
    sf::st_transform(4326)
  
  return(
    list(
      d,
      unique_wbs,
      wbs_overlap_sara_cdc
    )
  )
}
