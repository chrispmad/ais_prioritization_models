add_SAR_overlap_wbs_in_10_km = function(d,sar_overlap_wbs,previous_results){
  
  d$SAR_overlap_wbs_in_10_km = NA
  if(!'SAR_overlap_wbs_in_10_km' %in% names(previous_results)) previous_results$SAR_overlap_wbs_in_10_km = NA
  
  for(i in 1:nrow(d)){
    print(i)
    the_wb_buffered = d[i,] |> sf::st_transform(3005) |> sf::st_buffer(dist = 10000)
    this_prev <- previous_results[previous_results$Waterbody == the_wb_buffered$Waterbody & previous_results$Region == the_wb_buffered$Region,][1,]
    
    redo_row_test = is.na(this_prev$SAR_overlap_wbs_in_10_km) | this_prev$query_date + lubridate::dmonths(1) < Sys.Date()
    
    if(redo_row_test){
      the_fwa_code = the_wb_buffered$FWA_WATERSHED_CODE
      neighbour_lakes = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |> 
        filter(INTERSECTS(the_wb_buffered)) |> 
        collect() |> 
        filter(FWA_WATERSHED_CODE != the_fwa_code) |> 
        sf::st_filter(sar_overlap_wbs)
      d[i,]$SAR_overlap_wbs_in_10_km = nrow(neighbour_lakes)
      # Update row(s) in previous_results file.
      previous_results[previous_results$Region == the_wb_buffered$Region & previous_results$Waterbody == the_wb_buffered$Waterbody,]$SAR_overlap_wbs_in_10_km = nrow(neighbour_lakes)
      saveRDS(previous_results, paste0(onedrive_wd,"AIS_previous_query_results.rds"))
    }else{
      d[i,]$SAR_overlap_wbs_in_10_km <- this_prev$SAR_overlap_wbs_in_10_km
    }
  }
  return(d)
}
