# Use this 'for' loop to run the entire script.
for(m in 1){
  
  library(openxlsx)
  library(tidyverse)
  library(bcdata)
  library(sf)
  library(DBI)
  library(bcinvadeR)
  library(terra)
  library(geodata)
  library(predicts)
  library(ggpubr)
  library(dismo)
  library(rJava)
  library(patchwork)
  library(ecospat)
  # If you don't have this package installed, grab it from Chris' github page:
  #devtools::install_github('chrispmad/fwa.connect')
  library(fwa.connect)
  # library(ENMeval)
  
  # =========================================
  
  # Get / set file paths
  proj_wd = getwd()
  onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
  lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"
  output_folder = paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/")
  maxent_output_folder = paste0(output_folder,"MaxEnt_predictions/")
  
  # Read in utility scripts for functions
  source('scripts/utils/gather_AIS_data.R')
  source('scripts/utils/native_CDC_COSEWIC_SARA_species_occurrence_counter.R')
  source('scripts/utils/read_in_inputs_and_clean_for_model_excel_output.R')
  source('scripts/utils/add_occurrence_record_fields.R')
  source("scripts/utils/native_CDC_COSEWIC_SARA_species_occurrence_counter.R")
  source("scripts/utils/add_first_nations_territories_and_wildlife_habitat_areas_overlaps.R")
  source("scripts/utils/add_introduction_risk.R")
  source("scripts/utils/add_maxent_prediction_vars.R")
  source("scripts/utils/summarise_columns_and_produce_excel_output_file.R")
  
  # list of invasive species on our watch list.
  pr_sp = gather_ais_data(data = 'species list', lan_root = lan_root, onedrive_wd = onedrive_wd)
  
  # File that tracks previously performed queries / runs of these script, to save time.
  previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  # Ensure we're only dealing with the most recent date.
  previous_results = previous_results |> 
    dplyr::group_by(Region,Species,Watershed,Waterbody) |> 
    dplyr::arrange(Watershed,Waterbody,dplyr::desc(query_date)) |> 
    dplyr::slice(1) |> 
    dplyr::ungroup()
  saveRDS(previous_results,paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  # Functions
  
  count_trailing_zeroes <- function(code) {
    parts <- unlist(strsplit(code, "-"))
    count <- sum(parts == "000000")
    return(count)
  }
  
  # Make BC shapefile.
  bc = bcmaps::bc_bound() |> 
    sf::st_transform(4326) |> 
    terra::vect()
  
  # Grab regions of BC. These will be useful for finding waterbodies.
  regs = bcmaps::nr_regions() |> sf::st_transform(4326)
  
  # =========================================
  # Read in data and clean it up a bit
  data_files = read_in_inputs_and_clean_for_model_excel_output(onedrive_wd)
  d = data_files[[1]]
  unique_wbs = data_files[[2]]
  wbs_overlap_sara_cdc = data_files[[3]]
  
  # =========================================
  
  # Bring in / calculate variables
  
  # 1. Calculate number of inflows and outflows using fwa.connect package!!
  tbl_of_fwa_junctions = fwa.connect::stream_conn_tbl()
  # The following function takes about 1-2 minutes.
  d = d |> 
    rowwise() |> 
    dplyr::mutate(number_outflows = nrow(tbl_of_fwa_junctions[tbl_of_fwa_junctions$upstream_fwa_code == FWA_WATERSHED_CODE,]),
                  number_inflows = nrow(tbl_of_fwa_junctions[tbl_of_fwa_junctions$downstream_fwa_code == FWA_WATERSHED_CODE,])) |> 
    dplyr::ungroup()
  
  # Additional test - look for waterbodies within x KM that are connected
  # to focal waterbody and that have SAR overlaps
  sar_overlap_wbs = sf::read_sf(paste0(onedrive_wd,"waterbodies_overlapping_with_SARA_and_CDC_occs.gpkg"))
  d$SAR_overlap_wbs_in_10_km = NA
  
  previous_results = readRDS(paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  
  for(i in 1:nrow(d)){
    
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
    
    # if(is.na(prev_result_row$SAR_overlap_wbs_in_10_km)){
    #   previous_results[previous_results$Region == the_wb$Region & previous_results$Waterbody == the_wb$Waterbody,]$SAR_overlap_wbs_in_10_km = nrow(neighbour_lakes)
    #   saveRDS(previous_results, paste0(onedrive_wd,"AIS_previous_query_results.rds"))
    # }
  }
  
  # 2. Number of records in waterbody
  d = add_occurrence_record_fields(d,lan_root)
  
  # 3. Distinct DFO SARA species, COSEWIC and CDC species in waterbody.
  
  previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  d = native_CDC_COSEWIC_SARA_species_occurrence_counter(d,unique_wbs,wbs_overlap_sara_cdc,previous_results,onedrive_wd)
  
  # 4. MaxEnt Predictions
  previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  d = add_maxent_prediction_vars(d,onedrive_wd,maxent_output_folder)
  
  # 5. Overlaps with First Nations Territories and also Wildlife Habitat Areas
  previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  d = add_first_nations_territories_and_wildlife_habitat_areas_overlaps(d,onedrive_wd,previous_results)
  
  previous_results = readRDS(file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  d = add_introduction_risk(d, lan_root)
  # =========================================
  # Make bins for variables
  summarise_columns_and_produce_excel_output_file(d,output_folder,maxent_output_folder)
}
