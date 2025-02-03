add_SAR_overlap_wbs_in_10_km = function(d,previous_results){
  
  d$SAR_overlap_wbs_in_10_km = NA
  previous_results$SAR_overlap_wbs_in_10_km = NA
  
  for(i in 1:nrow(d)){
    print(i)
    the_wb = d[i,] |> sf::st_transform(3005) |> sf::st_buffer(dist = 10000)
    this_prev <- previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][1,]
    
    if(is.na(this_prev$SAR_overlap_wbs_in_10_km)){
      
      the_fwa_code = the_wb$FWA_WATERSHED_CODE
      neighbour_lakes = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |> 
        filter(INTERSECTS(the_wb)) |> 
        collect() |> 
        filter(FWA_WATERSHED_CODE != the_fwa_code) |> 
        sf::st_filter(sar_overlap_wbs)
      d[i,]$SAR_overlap_wbs_in_10_km = nrow(neighbour_lakes)
      
      prev_result_row = previous_results |> 
        dplyr::filter(Region == the_wb$Region, Waterbody == the_wb$Waterbody) |> 
        dplyr::arrange(dplyr::desc(query_date)) |> 
        dplyr::slice(1)
      
      previous_results[previous_results$Region == the_wb$Region & previous_results$Waterbody == the_wb$Waterbody,]$SAR_overlap_wbs_in_10_km = nrow(neighbour_lakes)
      saveRDS(previous_results, paste0(onedrive_wd,"AIS_previous_query_results.rds"))
    }else{
      d[i,]$SAR_overlap_wbs_in_10_km <- this_prev$SAR_overlap_wbs_in_10_km
    }
  }
  return(d)
}
