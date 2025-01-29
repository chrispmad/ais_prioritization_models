add_first_nations_territories_and_wildlife_habitat_areas_overlaps = function(d,onedrive_wd,prev_res){
  # First Nations territories within 10 kilometers
  fnpip = sf::read_sf("W:/CMadsen/shared_data_sets/first_nations_PIP_consultation_areas.shp") |> 
    sf::st_transform(4326)
  
  fnpip = st_make_valid(fnpip)
  
  d$first_nations_cons_area_overlapped = NA
  
  for(i in 1:nrow(d)){
    
    print(i)
    the_wb = d[i,]
    this_previous_result = prev_res[prev_res$Waterbody == the_wb$Waterbody & prev_res$Region == the_wb$Region,][1,]
    
    # Do we have a result for this variable already in 'prev_res' file? If so, load in now!
    if(nrow(this_previous_result[!is.na(this_previous_result$first_nations_cons_area_overlapped),]) > 0){
      d[i,]$first_nations_cons_area_overlapped = this_previous_result$first_nations_cons_area_overlapped
      print("We already had a value for this value of i!")
    } else {
      # Otherwise, calculate it!
      if(!sf::st_is_empty(the_wb$geometry)){
        fnpip_con_areas = fnpip |> sf::st_filter(sf::st_buffer(d[i,]$geometry,10000))
        d[i,]$first_nations_cons_area_overlapped = paste0(unique(fnpip_con_areas$CONTACT_NA),collapse = ', ')
        prev_res[prev_res$Waterbody == the_wb$Waterbody & prev_res$Region == the_wb$Region,]$first_nations_cons_area_overlapped = paste0(unique(fnpip_con_areas$CONTACT_NA),collapse = ', ')
        saveRDS(prev_res |> dplyr::distinct(), file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
      } else {
        print(paste0("Warning: ",the_wb$Waterbody," in ",the_wb$Region," has no geometry! You should probably fix this."))
      }
    }
  }
  
  d$wildlife_habitat_areas = NA
  d$wildlife_habitat_areas_hectares = NA
  # prev_res$wildlife_habitat_areas = NA
  # prev_res$wildlife_habitat_areas_hectares = 0
  # Wildlife Habitat Areas (GAR)
  for(i in 1:nrow(d)){
    
    print(i)
    
    the_wb = d[i,]
    this_previous_result = prev_res[prev_res$Waterbody == the_wb$Waterbody & prev_res$Region == the_wb$Region,][1,]
    
    # if(i == 10) browser()
    
    # Do we have a result for this variable already in 'prev_res' file? If so, load in now!
    if(nrow(this_previous_result[!is.na(this_previous_result$wildlife_habitat_areas),]) > 0){
      print("We already had a value for this value of i!")
      d[i,]$wildlife_habitat_areas = this_previous_result$wildlife_habitat_areas
      d[i,]$wildlife_habitat_areas_hectares = this_previous_result$wildlife_habitat_areas_hectares
    } else {
      if(!sf::st_is_empty(the_wb$geometry)){
        
        wb_in_albers = sf::st_buffer(sf::st_transform(the_wb, 3005),1000)
        
        wha_touching_wb = bcdata::bcdc_query_geodata("wildlife-habitat-areas-approved") |> 
          dplyr::filter(bcdata:::INTERSECTS(wb_in_albers$geometry)) |> 
          bcdata::collect()
        
        if(nrow(wha_touching_wb) > 0){
          d[i,]$wildlife_habitat_areas = nrow(wha_touching_wb)
          d[i,]$wildlife_habitat_areas_hectares = sum(wha_touching_wb$HECTARES, na.rm=T)
        }else{
          d[i,]$wildlife_habitat_areas = 0
          d[i,]$wildlife_habitat_areas_hectares = 0
        }
        prev_res[prev_res$Waterbody == the_wb$Waterbody & prev_res$Region == the_wb$Region,]$wildlife_habitat_areas = nrow(wha_touching_wb)
        prev_res[prev_res$Waterbody == the_wb$Waterbody & prev_res$Region == the_wb$Region,]$wildlife_habitat_areas_hectares = sum(wha_touching_wb$HECTARES, na.rm=T)
        saveRDS(prev_res |> dplyr::distinct(), file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
        print("We've updated prev_res again!")
      }
      else {
        print(paste0("Warning: ",the_wb$Waterbody," in ",the_wb$Region," has no geometry! You should probably fix this."))
      }
    }
  }
  return(d)
}
