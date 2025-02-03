add_occurrence_record_fields = function(d, lan_root){
  
  print("Reading in AIS spatial layer from LAN")
  
  occ_species = sf::read_sf(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/AIS_occurrences_all_sources.gpkg")) |> 
    dplyr::filter(Species %in% unique(d$Species))
  
  # Ensure date is always in this format: YYYY-MM-DD and not in 'excel' format.
  occ_species = occ_species |> 
    dplyr::mutate(Date2 = case_when(
      str_detect(Date,"^[0-9]{5}") ~ openxlsx::convertToDateTime(Date),
      str_detect(Date,"^[0-9]{4}$") ~ lubridate::ymd(paste0(Date,"01-01")),
      T ~ lubridate::ymd(Date)
    )
    )
  
  d$records_in_wb = 0
  d$present_or_absent = 'ABSENT'
  
  print("Records by species...")
  for(i in 1:nrow(d)){
    # print(i)
    recs_by_sp = occ_species[occ_species$Species == d[i,]$Species,]
    recs_by_wb = recs_by_sp |> sf::st_filter(st_buffer(d[i,]$geometry, 20))
    d[i,]$records_in_wb = nrow(recs_by_wb)
    d[i,]$present_or_absent = ifelse(nrow(recs_by_wb) > 0, "PRESENT", "ABSENT")
  }
  
  # 2. New to Waterbody - e.g. are occurrence records for waterbody from the last year?
  # Also, year of oldest record.
  d$new_to_waterbody = FALSE
  d$oldest_record = NA
  
  # Some species don't have an oldest record - why?
  print("Finding oldest record etc and new to waterbody fields")
  for(i in 1:nrow(d)){
    # print(i)
    recs_by_sp = occ_species[occ_species$Species == d[i,]$Species,]
    recs_by_wb = recs_by_sp |> sf::st_filter(d[i,]$geometry)
    
    if(nrow(recs_by_wb) > 0){
      earliest_record_date = min(lubridate::ymd(recs_by_wb$Date),na.rm=T)
      
      d[i,]$oldest_record = lubridate::year(earliest_record_date)
      
      # Is this earliest date within the last six months?
      six_months_ago = lubridate::ymd(Sys.Date()) - lubridate::dmonths(6)
      
      is_new_to_waterbody = earliest_record_date >= six_months_ago
      
      d[i,]$new_to_waterbody = is_new_to_waterbody
    } else {
      d[i,]$new_to_waterbody = NA
    }
  }
  return(d)
}
