add_first_nations_territories_and_wildlife_habitat_areas_overlaps = function(d,unique_wbs,onedrive_wd){
  # First Nations territories within 10 kilometers
  d$first_nations_cons_area_overlapped = NA
  
  for(i in 1:nrow(d)){
    # for(i in 1:34){
    print(i)
    the_wb = unique_wbs[i,]
    geom_for_wb = d |> 
      dplyr::filter(Waterbody == the_wb$Waterbody, Region == the_wb$Region) |> 
      dplyr::select(Waterbody,Region,FWA_WATERSHED_CODE,geometry) |> 
      dplyr::distinct()
    previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
    if(!'first_nations_cons_area_overlapped' %in% names(previous_results)) {
      previous_results$first_nations_cons_area_overlapped = NA
    }
    this_prev <- previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][1,]
    redo_row_test = is.na(this_prev$first_nations_cons_area_overlapped) | this_prev$query_date + lubridate::dmonths(1) < Sys.Date()
    
    # Do we have a result for this variable already in 'this_prev' file? If so, load in now!
    if(!redo_row_test){
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$first_nations_cons_area_overlapped = this_prev$first_nations_cons_area_overlapped
      print("We already had a value for this value of i!")
    } else {
      # Otherwise, calculate it!
      if(!exists("fnpip")){
        fnpip = sf::read_sf("W:/CMadsen/shared_data_sets/first_nations_PIP_consultation_areas.shp") |> 
          sf::st_transform(4326)
        fnpip = st_make_valid(fnpip)
      }
      if(nrow(geom_for_wb) > 0){
        if(!sf::st_is_empty(geom_for_wb$geometry)){
          fnpip_con_areas = fnpip |> sf::st_filter(sf::st_buffer(geom_for_wb$geometry,10000))
          d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$first_nations_cons_area_overlapped = paste0(unique(fnpip_con_areas$CONTACT_NA),collapse = ', ')
          previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,]$first_nations_cons_area_overlapped = paste0(unique(fnpip_con_areas$CONTACT_NA),collapse = ', ')
          saveRDS(previous_results |> 
                    dplyr::group_by(Region,Waterbody,Species) |> 
                    dplyr::arrange(dplyr::desc(query_date)) |> 
                    dplyr::slice(1) |> 
                    dplyr::ungroup(), 
                  file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
        } else {
          print(paste0("Warning: ",the_wb$Waterbody," in ",the_wb$Region," has no geometry! You should probably fix this."))
        }
      }
    }
  }
  
  d$wildlife_habitat_areas = NA
  d$wildlife_habitat_areas_hectares = NA
  # this_prev$wildlife_habitat_areas = NA
  # this_prev$wildlife_habitat_areas_hectares = 0
  # Wildlife Habitat Areas (GAR)
  for(i in 1:nrow(d)){
    # for(i in 1:34){
    print(i)
    the_wb = unique_wbs[i,]
    geom_for_wb = d |> 
      dplyr::filter(Waterbody == the_wb$Waterbody, Region == the_wb$Region) |> 
      dplyr::select(Waterbody,Region,FWA_WATERSHED_CODE,geometry) |> 
      dplyr::distinct()
    previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
    if(!'wildlife_habitat_areas' %in% names(previous_results)) {
      previous_results$wildlife_habitat_areas = NA
      previous_results$wildlife_habitat_areas_hectares = NA
    }
    this_prev <- previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][1,]
    redo_row_test = is.na(this_prev$wildlife_habitat_areas) | this_prev$query_date + lubridate::dmonths(1) < Sys.Date()
    
    # Do we have a result for this variable already in 'this_prev' file? If so, load in now!
    if(!redo_row_test){
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas = this_prev$wildlife_habitat_areas
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas_hectares = this_prev$wildlife_habitat_areas_hectares
      print("We already had a value for this value of i!")
    } else {
      if(nrow(geom_for_wb) > 0){
        if(!sf::st_is_empty(geom_for_wb$geometry)){
          
          wb_in_albers = sf::st_buffer(sf::st_transform(geom_for_wb, 3005),1000)
          
          wha_touching_wb = bcdata::bcdc_query_geodata("wildlife-habitat-areas-approved") |> 
            dplyr::filter(bcdata:::INTERSECTS(wb_in_albers$geometry)) |> 
            bcdata::collect()
          
          if(nrow(wha_touching_wb) > 0){
            d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas = nrow(wha_touching_wb)
            d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas_hectares = sum(wha_touching_wb$HECTARES, na.rm=T)
            this_prev[this_prev$Region == the_wb$Region & this_prev$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas = nrow(wha_touching_wb)
            this_prev[this_prev$Region == the_wb$Region & this_prev$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas_hectares = sum(wha_touching_wb$HECTARES, na.rm=T)
          }else{
            d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas = 0
            d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas_hectares = 0
            this_prev[this_prev$Region == the_wb$Region & this_prev$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas = 0
            this_prev[this_prev$Region == the_wb$Region & this_prev$Waterbody == the_wb$Waterbody,]$wildlife_habitat_areas_hectares = 0
          }
          saveRDS(previous_results |> 
                    dplyr::group_by(Region,Waterbody,Species) |> 
                    dplyr::arrange(dplyr::desc(query_date)) |> 
                    dplyr::slice(1) |> 
                    dplyr::ungroup(), 
                  file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
        }
        else {
          print(paste0("Warning: ",the_wb$Waterbody," in ",the_wb$Region," has no geometry! You should probably fix this."))
        }
      }
    }
  }
  return(d)
}
