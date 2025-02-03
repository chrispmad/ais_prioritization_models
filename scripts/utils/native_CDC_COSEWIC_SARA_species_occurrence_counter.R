native_CDC_COSEWIC_SARA_species_occurrence_counter = function(d,unique_wbs,wbs_overlap_sara_cdc,previous_results,onedrive_wd,redo = F){

    TO_string = c("Okanagan Lake","Okanagan River","Skaha Lake",
                "Vaseux Lake","Vaseux Creek","Osoyoos Lake")
  
  # Go up one level in the file structure to find the SAR scraper R project folder.
  # This is where the 2 cleaned tables (cleaned by John with Python!) reside.
  federal_risk_registry_tbl = tidyr::as_tibble(read.csv("../SAR_scraper/output/risk_status_merged.csv"))
  
  cosewic_risk_status_sp = federal_risk_registry_tbl |> 
    dplyr::filter(COSEWIC.status %in% c("Endangered","Special Concern","Threatened")) |>
    dplyr::filter(!COSEWIC.status %in% c("","No Status")) |> 
    dplyr::filter(Taxonomic.group %in% c("Fishes (freshwater)","Molluscs")) |> 
    dplyr::select(COSEWIC.common.name) |> 
    dplyr::filter(!COSEWIC.common.name %in% c("White Sturgeon")) |> 
    dplyr::distinct() |> 
    dplyr::pull(COSEWIC.common.name)
  
  sara_sp = read.csv("data/sara_species_to_look_for.csv")
  
  d$sara_in_wb = 0
  d$sara_in_wb_names = NA
  d$sara_downstream = 0
  d$sara_downstream_names = NA
  d$cdc_listed_in_wb = 0
  d$cdc_listed_in_wb_names = NA
  d$cdc_listed_downstream = 0
  d$cdc_listed_downstream_names = NA
  d$COSEWIC_in_wb = 0
  d$COSEWIC_in_wb_names = NA
  d$COSEWIC_downstream = 0
  d$COSEWIC_downstream_names = NA
  d$native_species_in_wb = 0
  d$native_species_in_wb_names = NA
  d$other_ais_in_wb = 0
  d$other_ais_in_wb_names = NA
  
  for(i in 1:nrow(unique_wbs)){
  
    redo = F
    
    the_wb = unique_wbs[i,]
    browser()
    # Has this row been 'done' already in this session? If so, skip!
    if("sara_in_wb" %in% names(previous_results)){
    the_wb_prev = previous_results |> 
      dplyr::filter(Region == the_wb$Region & Waterbody == the_wb$Waterbody) |> 
      dplyr::filter(!is.na(sara_in_wb)) |> 
      dplyr::select(-Species) |> 
      dplyr::arrange(dplyr::desc(query_date)) |> 
      dplyr::slice(1)
    } else {
      
    }
    if(nrow(the_wb_prev) > 0){
      date_cutoff = lubridate::ymd(the_wb_prev$query_date) + lubridate::dmonths(1)
    } else {
      date_cutoff = lubridate::ymd(Sys.Date()) - lubridate::mday(1)
    }
    
    if(nrow(the_wb_prev) > 0 & Sys.Date() < date_cutoff & redo == F){
      #print("This waterbody already done! You're lucky!")
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$sara_in_wb = the_wb_prev$sara_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$sara_in_wb_names = the_wb_prev$sara_in_wb_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$sara_downstream = the_wb_prev$sara_downstream
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$sara_downstream_names = the_wb_prev$sara_downstream_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$cdc_listed_in_wb = the_wb_prev$cdc_listed_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$cdc_listed_in_wb_names = the_wb_prev$cdc_listed_in_wb_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$cdc_listed_downstream = the_wb_prev$cdc_listed_downstream
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$cdc_listed_downstream_names = the_wb_prev$cdc_listed_downstream_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$COSEWIC_in_wb = the_wb_prev$COSEWIC_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$COSEWIC_in_wb_names = the_wb_prev$COSEWIC_in_wb_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$COSEWIC_downstream = the_wb_prev$COSEWIC_downstream
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$COSEWIC_downstream_names = the_wb_prev$COSEWIC_downstream_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$native_species_in_wb = the_wb_prev$native_species_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$native_species_in_wb_names = the_wb_prev$native_species_in_wb_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$other_ais_in_wb = the_wb_prev$other_ais_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$other_ais_in_wb_names = the_wb_prev$other_ais_in_wb_names 
    } else {
      
      print(paste0("Working on waterbody ",i,", which is ",the_wb$Waterbody))
      
      geom_and_fwa_for_wb = d |> 
        dplyr::filter(Waterbody == the_wb$Waterbody, Region == the_wb$Region) |> 
        dplyr::select(Waterbody,Region,FWA_WATERSHED_CODE,geometry) |> 
        dplyr::distinct()
      
      if(sf::st_is_empty(geom_and_fwa_for_wb)){
        print(paste0("Warning: ", the_wb$Waterbody, " in ", the_wb$Region," has empty geometry - you should fix this!"))
        next
      }
      
      if(nrow(geom_and_fwa_for_wb) > 1){
        warning(paste0("More than one waterbody found for: ",the_wb$Waterbody, " ",the_wb$Region),
                ". The highest order waterbody will be selected.")
        geom_and_fwa_for_wb<-geom_and_fwa_for_wb |> 
          rowwise() |> 
          mutate(trailing_zeroes = count_trailing_zeroes(FWA_WATERSHED_CODE)) |> 
          ungroup() |> 
          arrange(desc(trailing_zeroes)) %>%  # Sort descending by trailing zeroes
          slice(1)                            # Select the first row (highest order)
      }
      
      the_wb = the_wb |> dplyr::left_join(geom_and_fwa_for_wb, by = join_by(Waterbody, Region))
      the_wb = sf::st_set_geometry(the_wb, "geometry")
      
      # Find downstream waterbody
      # If the chosen waterbody is one of the Thompson string, don't look downstream.
      
      if(the_wb$Waterbody %in% TO_string){
        ds_wb = the_wb |> dplyr::select(geometry)
        ds_wb = ds_wb[0,]
      } else {
        # Waterbodies downstream, up to 5 kilometers away.
        buffer_5km = sf::st_buffer(sf::st_as_sfc(sf::st_bbox(the_wb)),5000)
        wb_fwa_code = the_wb$FWA_WATERSHED_CODE
        ds_wb_fwa_code = stringr::str_replace(wb_fwa_code,"[0-9]{6}(?=\\-000000)","000000")
        ds_river = bcdata::bcdc_query_geodata('freshwater-atlas-rivers') |> filter(FWA_WATERSHED_CODE == ds_wb_fwa_code) |> collect() |> sf::st_transform(4326)
        if(nrow(ds_river) > 0){
          # Find the longest / largest river. Keep that name, in cases where there's multiple.
          largest_river = ds_river |> 
            dplyr::filter(!is.na(GNIS_NAME_1)) |> 
            dplyr::arrange(dplyr::desc(AREA_HA)) |> 
            dplyr::slice(1) |> 
            dplyr::pull(GNIS_NAME_1)
          
          ds_river = ds_river |> 
            dplyr::filter(!is.na(GNIS_NAME_1)) |> 
            dplyr::rename(Waterbody = GNIS_NAME_1) |> 
            dplyr::filter(Waterbody == largest_river) |> 
            dplyr::group_by(Waterbody,FWA_WATERSHED_CODE) |> 
            dplyr::summarise()
        }
        
        ds_lakes = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |> 
          filter(FWA_WATERSHED_CODE == ds_wb_fwa_code) |> 
          collect() |> sf::st_transform(4326)
        
        # Check for streams too, if necessary.
        if(nrow(ds_lakes) == 0 & nrow(ds_river) == 0){
          ds_streams = bcdata::bcdc_query_geodata('freshwater-atlas-stream-network') |> 
            filter(FWA_WATERSHED_CODE == ds_wb_fwa_code) |> 
            collect() |> sf::st_zm() |> 
            sf::st_transform(4326)
          
          if(nrow(ds_streams) > 0){
            ds_streams = ds_streams |> 
              dplyr::rename(Waterbody = GNIS_NAME) |> 
              dplyr::group_by(FWA_WATERSHED_CODE, Waterbody) |> 
              dplyr::summarise(.groups = "drop")
            
            ds_streams = ds_streams |> 
              dplyr::arrange(Waterbody) |> 
              dplyr::slice(1)
          }
        }
        
        #modified, removed summary and added a select
        if(nrow(ds_lakes) > 0){
          ds_lakes = ds_lakes |> 
            dplyr::filter(!is.na(GNIS_NAME_1)) |> 
            dplyr::rename(Waterbody = GNIS_NAME_1) |> 
            # dplyr::group_by(Waterbody,FWA_WATERSHED_CODE) |> 
            dplyr::select(Waterbody, FWA_WATERSHED_CODE, AREA_HA, geometry)
        }
        # Some big edge cases: 
        
        # 1. Is the downstream the Columbia River?
        # Since it dips out of BC, then back in, things get very complicated, and 
        # lakes that seem implicated perhaps should not be; in this case, just take
        # the Columbia River, trim it to 5 km, and be done with it!
        
        if("Columbia River" %in% ds_river$Waterbody){
          ds_lakes = ds_lakes[0,]
        }
        
        # 2. is the Thompson-Okanagan string of connected lakes
        # present in the downstream names? If so, those get priority and we 
        # can set the geometry to be the merged lakes.
        
        if("Okanagan Lake" %in% unique(ds_lakes$Waterbody)){
          # Downstream waterbody is the Thompson-Okanagan string!! Assign that to wb_ds
          wbs_rivers = bcdc_query_geodata('freshwater-atlas-rivers') |> 
            filter(GNIS_NAME_1 %in% TO_string) |> 
            collect() |> 
            dplyr::summarise()
          
          wbs_lakes = bcdc_query_geodata('freshwater-atlas-lakes') |> 
            filter(GNIS_NAME_1 %in% TO_string) |> 
            collect() |> 
            dplyr::summarise()
          
          wbs = dplyr::bind_rows(wbs_rivers,wbs_lakes) |> 
            dplyr::summarise() |> 
            dplyr::mutate(Waterbody = 'Okanagan_Lake_System') |> 
            dplyr::mutate(FWA_WATERSHED_CODE = "300-432687-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000") |> 
            sf::st_transform(4326)
          
          ds_lake = wbs
        } else {
          # If there are multiple lakes with the same name and FWA code, merge them
          if(nrow(ds_lakes) > 1 & length(unique(ds_lakes$Waterbody)==1) & length(unique(ds_lakes$FWA_WATERSHED_CODE)==1)){
            ds_lakes = ds_lakes |> 
              dplyr::group_by(Waterbody,FWA_WATERSHED_CODE) |> 
              dplyr::mutate(AREA_HA = sum(AREA_HA,na.rm=T)) |> 
              dplyr::group_by(Waterbody,FWA_WATERSHED_CODE,AREA_HA) |> 
              dplyr::summarise(.groups = 'drop')
          }
          # If there are multiple lakes, take the largest one.
          if(nrow(ds_lakes) > 1){
            chosen_lake = ds_lakes |> 
              dplyr::ungroup() |>                        
              dplyr::arrange(dplyr::desc(AREA_HA)) |>    
              dplyr::slice(1) |>                         
              dplyr::pull(Waterbody)                    
            chosen_lake = chosen_lake[1]
            
            ds_lake = ds_lakes |> 
              dplyr::filter(Waterbody == chosen_lake)
          } else {
            ds_lake = ds_lakes
          }
          # ds_lake = ds_lakes
          # rm(ds_lakes)
        }
        
        if(nrow(ds_lake) > 0){
          if(ds_lake$Waterbody != the_wb$Waterbody){
            ds_wb = ds_lake
          }
          # breaking here - what should be done? There are 5 waterbodies 
          # for Green Lake that are downstream lakes 
        } else {
          if(nrow(ds_river) > 0){
            ds_wb = ds_river |> 
              sf::st_intersection(buffer_5km)
          } else {
            # No river or lake found! We're using a stream!
            ds_wb = ds_streams |> 
              sf::st_intersection(buffer_5km)
          }
        }
      }
      
      # Join on SARA and CDC that overlapped with the target waterbody.
      
      the_wb = the_wb |> 
        dplyr::left_join(
          wbs_overlap_sara_cdc |> 
            sf::st_drop_geometry() |> 
            dplyr::select(Waterbody = GNIS_NAME_,
                          Region,
                          FWA_WATERSHED_CODE = FWA_WAT,
                          starts_with("sara_"),
                          starts_with("cdc")),
          join_by(Waterbody, Region, FWA_WATERSHED_CODE)
        ) |> 
        dplyr::distinct()
      
      # Do the same for the downstream waterbody.
      if(nrow(ds_wb) > 0){
        if("Region" %in% names(ds_wb)){
          ds_wb = ds_wb |> 
            dplyr::left_join(
              wbs_overlap_sara_cdc |> 
                sf::st_drop_geometry() |> 
                dplyr::select(Waterbody = GNIS_NAME_,
                              Region,
                              FWA_WATERSHED_CODE = FWA_WAT,
                              starts_with("sara_"),
                              starts_with("cdc")),
              by = join_by(Waterbody, Region, FWA_WATERSHED_CODE)
            )
        } else {
          ds_wb = ds_wb |> 
            dplyr::left_join(
              wbs_overlap_sara_cdc |> 
                sf::st_drop_geometry() |> 
                dplyr::select(Waterbody = GNIS_NAME_,
                              FWA_WATERSHED_CODE = FWA_WAT,
                              starts_with("sara_"),
                              starts_with("cdc")),
              by = join_by(Waterbody, FWA_WATERSHED_CODE)
            )
        }
      }
      # sara_polys_in_wb = sara |> sf::st_filter(the_wb)
      # sara_polys_in_ds_wb = sara |> sf::st_filter(ds_wb)
      # cdc_polys_in_wb = cdc_f |> sf::st_filter(the_wb)
      # cdc_polys_in_ds_wb = cdc_f |> sf::st_filter(ds_wb)
      
      # Look up all species present in the waterbody, to find COSEWIC species
      # and native species. This looks only for taxa labelled "fishes","Actinopterygii", and "Mollusca",
      # but you can add more taxa if desired.
      species_in_wb = tryCatch(
        expr = bcinvadeR::find_all_species_in_waterbody(wb = sf::st_transform(the_wb,3005)),
        error = function(e) return(data.frame(DataSource = "",Date = "", Species = "", Location = "", geometry = 0)[0,])
      )
      # Same for downstream waterbody, unless there is none, as in the case of the Thompson-Okanagan string of lakes, which
      # we are treating as a single, connected unit.
      if(nrow(ds_wb) == 0){
        species_in_ds_wb = c()
      } else {
        species_in_ds_wb = tryCatch(
          expr = bcinvadeR::find_all_species_in_waterbody(wb = sf::st_transform(ds_wb,3005)),
          error = function(e) return(data.frame(DataSource = "",Date = "", Species = "", Location = "", geometry = 0)[0,])
        )
      }
      
      # Find all species present.
      unique_species_in_wb = unique(species_in_wb$Species)
      unique_species_in_ds_wb = unique(species_in_ds_wb$Species)
      
      # Clean up this vector a bit: delete anything in parentheses. Any other steps?
      unique_species_in_wb = stringr::str_squish(stringr::str_remove_all(unique_species_in_wb, " (\\(.*\\)|\\-.*)"))
      unique_species_in_ds_wb = stringr::str_squish(stringr::str_remove_all(unique_species_in_ds_wb, " (\\(.*\\)|\\-.*)"))
      
      # Drop empty elements.
      unique_species_in_wb = unique_species_in_wb[unique_species_in_wb != ""]
      unique_species_in_ds_wb = unique_species_in_ds_wb[unique_species_in_ds_wb != ""]
      
      # Find list of native species, aquatic invasive species, COSEWIC, SARA listed species.
      native_sp_in_wb = unique(unique_species_in_wb[!unique_species_in_wb %in% str_to_title(pr_sp$name)])
      ais_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% str_to_title(pr_sp$name)])
      cosewic_sp_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% cosewic_risk_status_sp])
      sara_sp_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% sara_sp])
      
      # Remove slugs from native species; they're kind of not a fish?
      native_sp_in_wb = native_sp_in_wb[!stringr::str_detect(native_sp_in_wb, "Slug$")]
      
      # Remove things from cosewic that are present in sara.
      cosewic_sp_in_wb = cosewic_sp_in_wb[!cosewic_sp_in_wb %in% sara_sp_in_wb]
      
      # Same for downstream.
      # native_sp_in_ds_wb = unique(unique_species_in_ds_wb[!unique_species_in_ds_wb %in% str_to_title(pr_sp$name)]
      # ais_in_ds_wb = unique_species_in_ds_wb[unique_species_in_ds_wb %in% str_to_title(pr_sp$name)]
      cosewic_sp_in_ds_wb = unique(unique_species_in_ds_wb[unique_species_in_ds_wb %in% cosewic_risk_status_sp])
      sara_sp_in_ds_wb = unique(unique_species_in_ds_wb[unique_species_in_ds_wb %in% sara_sp])
      
      cosewic_sp_in_ds_wb = cosewic_sp_in_ds_wb[!cosewic_sp_in_ds_wb %in% sara_sp_in_ds_wb]
      
      # Now find all rows in our dataframe that have this waterbody as their target.
      # For each one, count up the above species groupings.
      for(y in 1:nrow(d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,])){
        # Ensure this gets applied to the right rows in d.
        
        the_species = d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$Species
        
        # Add on AIS and Native species
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$other_ais_in_wb = length(unique(ais_in_wb[ais_in_wb != d[i,]$Species]))
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$other_ais_in_wb_names = ifelse(length(ais_in_wb[ais_in_wb != d[i,]$Species]) > 0, paste0(ais_in_wb[ais_in_wb != the_species], collapse = ", "), NA)
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$native_species_in_wb = length(unique(native_sp_in_wb))
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$native_species_in_wb_names = ifelse(length(native_sp_in_wb) > 0, paste0(native_sp_in_wb, collapse = ", "), NA)
        
        # Combine sara spatial overlaps with point data that is of a SARA-listed species name.
        sara_combo = unique(na.omit(c(unique(sara_sp_in_wb), unique(the_wb$sara_common_name))))
        sara_combo_ds = unique(na.omit(c(unique(sara_sp_in_ds_wb),unique(ds_wb$sara_common_name))))
        
        # CDC names. Make sure SARA and COSEWIC names aren't present here.
        # Drop anything in parentheses for cdc polys.
        # cdc_polys_in_wb = cdc_polys_in_wb |> dplyr::mutate(ENG_NAME = stringr::str_remove_all(ENG_NAME," \\(.*\\)"))
        # cdc_polys_in_ds_wb = cdc_polys_in_ds_wb |> dplyr::mutate(ENG_NAME = stringr::str_remove_all(ENG_NAME," \\(.*\\)"))
        
        # cdc_in_wb = unique(cdc_polys_in_wb$ENG_NAME)
        cdc_in_wb = unique(na.omit(the_wb$cdc_common_name))
        cdc_in_wb = cdc_in_wb[!cdc_in_wb %in% sara_sp]
        cdc_in_wb = cdc_in_wb[!cdc_in_wb %in% cosewic_risk_status_sp]
        cdc_in_ds_wb = unique(na.omit(ds_wb$cdc_common_name))
        cdc_in_ds_wb = cdc_in_ds_wb[!cdc_in_ds_wb %in% sara_sp]
        cdc_in_ds_wb = cdc_in_ds_wb[!cdc_in_ds_wb %in% cosewic_risk_status_sp]
        
        if(y == 2) browser()
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_in_wb = length(unique(sara_combo))
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_in_wb_names = ifelse(length(sara_combo) > 0, paste0(unique(na.omit(sara_combo)), collapse = ", "), "none")
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_downstream = length(unique(sara_combo_ds))
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_downstream_names = ifelse(length(sara_combo_ds) > 0, paste0(unique(na.omit(sara_combo_ds)), collapse = ", "), "none")
        
        # COSEWIC species
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_in_wb = length(cosewic_sp_in_wb)
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_in_wb_names = ifelse(length(cosewic_sp_in_wb) > 0, paste0(unique(na.omit(cosewic_sp_in_wb)), collapse = ", "), "none")
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_downstream = length(cosewic_sp_in_ds_wb)
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_downstream_names = ifelse(length(cosewic_sp_in_ds_wb) > 0, paste0(unique(na.omit(cosewic_sp_in_ds_wb)), collapse = ", "), NA)
        
        # CDC species
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_in_wb = length(cdc_in_wb)
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_in_wb_names = ifelse(length(cdc_in_wb) > 0, paste0(unique(na.omit(cdc_in_wb)), collapse = ", "), "none")
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_downstream = length(cdc_in_ds_wb)
        d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_downstream_names = ifelse(length(cdc_in_ds_wb) > 0, paste0(unique(na.omit(cdc_in_ds_wb)), collapse = ", "), "none")
      
        
        # Now do the same thing to update the previous_results file!
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$other_ais_in_wb = length(unique(ais_in_wb[ais_in_wb != d[i,]$Species]))
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$other_ais_in_wb_names = ifelse(length(ais_in_wb[ais_in_wb != d[i,]$Species]) > 0, paste0(ais_in_wb[ais_in_wb != the_species], collapse = ", "), NA)
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$native_species_in_wb = length(unique(native_sp_in_wb))
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$native_species_in_wb_names = ifelse(length(native_sp_in_wb) > 0, paste0(native_sp_in_wb, collapse = ", "), NA)
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$sara_in_wb = length(unique(sara_combo))
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$sara_in_wb_names = ifelse(length(sara_combo) > 0, paste0(unique(na.omit(sara_combo)), collapse = ", "), "none")
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$sara_downstream = length(unique(sara_combo_ds))
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$sara_downstream_names = ifelse(length(sara_combo_ds) > 0, paste0(unique(na.omit(sara_combo_ds)), collapse = ", "), "none")
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$COSEWIC_in_wb = length(cosewic_sp_in_wb)
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$COSEWIC_in_wb_names = ifelse(length(cosewic_sp_in_wb) > 0, paste0(unique(na.omit(cosewic_sp_in_wb)), collapse = ", "), "none")
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$COSEWIC_downstream = length(cosewic_sp_in_ds_wb)
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$COSEWIC_downstream_names = ifelse(length(cosewic_sp_in_ds_wb) > 0, paste0(unique(na.omit(cosewic_sp_in_ds_wb)), collapse = ", "), NA)
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$cdc_listed_in_wb = length(cdc_in_wb)
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$cdc_listed_in_wb_names = ifelse(length(cdc_in_wb) > 0, paste0(unique(na.omit(cdc_in_wb)), collapse = ", "), "none")
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$cdc_listed_downstream = length(cdc_in_ds_wb)
        previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$cdc_listed_downstream_names = ifelse(length(cdc_in_ds_wb) > 0, paste0(unique(na.omit(cdc_in_ds_wb)), collapse = ", "), "none")
      }
      
      # Save this new row to our query results!
      # browser()
      
      saveRDS(previous_results |> 
                dplyr::group_by(Region,Waterbody) |> 
                dplyr::arrange(dplyr::desc(query_date)) |> 
                dplyr::slice(1) |> 
                dplyr::ungroup(), 
              file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
      print("saved new waterbody row to the previous results RDS file")
      
    }
  }
  return(d)
}

