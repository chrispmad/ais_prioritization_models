native_CDC_COSEWIC_SARA_species_occurrence_counter = function(d,unique_wbs,wbs_overlap_sara_cdc,onedrive_wd,redo = F){

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
  
  sara_sp = read.csv("data/sara_species_to_look_for.csv") |> 
    dplyr::pull(species)
  
  d$sara_in_wb = NA
  d$sara_in_wb_names = NA
  d$sara_downstream = NA
  d$sara_downstream_names = NA
  d$cdc_listed_in_wb = NA
  d$cdc_listed_in_wb_names = NA
  d$cdc_listed_downstream = NA
  d$cdc_listed_downstream_names = NA
  d$COSEWIC_in_wb = NA
  d$COSEWIC_in_wb_names = NA
  d$COSEWIC_downstream = NA
  d$COSEWIC_downstream_names = NA
  d$native_species_in_wb = NA
  d$native_species_in_wb_names = NA
  d$other_ais_in_wb = NA
  d$other_ais_in_wb_names = NA
  
  if(!'sara_in_wb' %in% names(previous_results)) {
    previous_results$sara_in_wb = NA
    previous_results$sara_in_wb_names = NA
    previous_results$sara_downstream = NA
    previous_results$sara_downstream_names = NA
    previous_results$cdc_listed_in_wb = NA
    previous_results$cdc_listed_in_wb_names = NA
    previous_results$cdc_listed_downstream = NA
    previous_results$cdc_listed_downstream_names = NA
    previous_results$COSEWIC_in_wb = NA
    previous_results$COSEWIC_in_wb_names = NA
    previous_results$COSEWIC_downstream = NA
    previous_results$COSEWIC_downstream_names = NA
    previous_results$native_species_in_wb = NA
    previous_results$native_species_in_wb_names = NA
    previous_results$other_ais_in_wb = NA
    previous_results$other_ais_in_wb_names = NA
  }
  
  
  for(i in 1:nrow(unique_wbs)){
  # for(i in 1:19){

    print(i)
    # This is a bit wasteful, but it's important to have the absolute most up-to-date version of this file..
    previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
    
    the_wb = unique_wbs[i,]
    
    # if(the_wb$Waterbody == "Bloom Creek") browser()
    # Has this row been 'done' already in this session? If so, skip!
 
    this_prev <- previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][1,]
    
    redo_row_test = is.na(this_prev$sara_in_wb) | this_prev$query_date + lubridate::dmonths(1) < Sys.Date()
    
    if(this_prev$cdc_listed_in_wb_names == 'NA') redo_row_test = T
    
    if(!redo_row_test){
      #print("This waterbody already done! You're lucky!")
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$sara_in_wb = this_prev$sara_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$sara_in_wb_names = this_prev$sara_in_wb_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$sara_downstream = this_prev$sara_downstream
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$sara_downstream_names = this_prev$sara_downstream_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$cdc_listed_in_wb = this_prev$cdc_listed_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$cdc_listed_in_wb_names = this_prev$cdc_listed_in_wb_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$cdc_listed_downstream = this_prev$cdc_listed_downstream
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$cdc_listed_downstream_names = this_prev$cdc_listed_downstream_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$COSEWIC_in_wb = this_prev$COSEWIC_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$COSEWIC_in_wb_names = this_prev$COSEWIC_in_wb_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$COSEWIC_downstream = this_prev$COSEWIC_downstream
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$COSEWIC_downstream_names = this_prev$COSEWIC_downstream_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$native_species_in_wb = this_prev$native_species_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$native_species_in_wb_names = this_prev$native_species_in_wb_names
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$other_ais_in_wb = this_prev$other_ais_in_wb
      d[d$Region == the_wb$Region & d$Waterbody == the_wb$Waterbody,]$other_ais_in_wb_names = this_prev$other_ais_in_wb_names 
    } else {
      # Find downstream waterbody.
      fdw_results = find_downstream_waterbody(i, the_wb, d, TO_string)
      the_wb = fdw_results[[1]]
      ds_wb = fdw_results[[2]]

      # Attach SAR / native species etc. columns to waterbody.
      d = join_sara_cdc_to_wb(i, d, the_wb, ds_wb, wbs_overlap_sara_cdc, sara_sp, cosewic_risk_status_sp, previous_results)
    }
  }
  return(d)
}

