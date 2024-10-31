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
source("ZuurFuncs.R")
library(ecospat)
library(ENMeval)

# =========================================

source("scripts/utils/prep_predictor_data_f.R")
source("scripts/utils/run_maxent_f.R")

#set locations

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

# Make BC shapefile.
bc = bcmaps::bc_bound() |> 
  sf::st_transform(4326) |> 
  terra::vect()

# Grab regions of BC. These will be useful for finding waterbodies.
regs = bcmaps::nr_regions() |> sf::st_transform(4326)

predictor_data = prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = paste0(onedrive_wd),
                                     ext_vect = bc)

# =========================================

# Table of Region, species and waterbody names. Note: might be nicer to make this
# in excel and read it in.
d = tibble(
  Region = c("Thompson-Okanagan","Thompson-Okanagan","Thompson-Okanagan","South Coast"),
  species = c("Smallmouth Bass","Smallmouth Bass","Asian Clam","Goldfish"),
  Waterbody = c("Okanagan Lake","Kalamalka Lake","Shuswap Lake","Fraser River")
)

# =========================================

# Find Waterbodies (based on name and region!)
unique_wbs = d |> 
  dplyr::select(Waterbody, Region) |> 
  dplyr::distinct()

wbs_list = purrr::map2(unique_wbs$Waterbody, unique_wbs$Region, ~ {
  
  the_reg = regs[str_detect(regs$ORG_UNIT_NAME,.y),] |> sf::st_transform(3005)
  
  potential_lakes = bcdc_query_geodata('freshwater-atlas-lakes') |> 
    filter(INTERSECTS(the_reg)) |>
    filter(GNIS_NAME_1 == .x) |> 
    collect()
  
  potential_rivers = bcdc_query_geodata('freshwater-atlas-rivers') |> 
    filter(INTERSECTS(the_reg$geometry)) |>
    filter(GNIS_NAME_1 == .x) |> 
    collect()
  
  if(nrow(potential_lakes) > 0 & nrow(potential_rivers) > 0){
    wbs = dplyr::bind_rows(
      potential_lakes |> dplyr::select(wb_name = GNIS_NAME_1, geometry),
      potential_rivers |> dplyr::select(wb_name = GNIS_NAME_1, geometry)
    )
  }
  if(nrow(potential_lakes) > 0 & nrow(potential_rivers) == 0){
    wbs = potential_lakes |> dplyr::select(wb_name = GNIS_NAME_1, geometry)
  }
  if(nrow(potential_lakes) == 0 & nrow(potential_rivers) > 0){
    wbs = potential_rivers |> dplyr::select(wb_name = GNIS_NAME_1, geometry)
  }
  
  if(nrow(potential_lakes) == 0 & nrow(potential_rivers) == 0){
    return(NULL)
  } else {
    return(
      wbs |> 
        dplyr::group_by(wb_name) |> 
        dplyr::summarise()
    )
  }
  
}, .progress = TRUE)

wbs = wbs_list |> 
  dplyr::bind_rows() |> 
  sf::st_transform(4326)

# Add these geometries onto 'd'
d = d |> 
  dplyr::left_join(wbs |> dplyr::rename(Waterbody = wb_name))

d = sf::st_set_geometry(d, 'geometry')

# =========================================

# Bring in / calculate variables

# 1. New to Waterbody

# 2. Number of occurrence in waterbody

occ_species = unique(d$species) |> 
  purrr::map( ~ {bcinvadeR::grab_aq_occ_data(.x)}) |> 
  dplyr::bind_rows()

d$occurrences_in_wb = 0

for(i in 1:nrow(d)){
  recs_by_sp = occ_species[occ_species$Species == d[i,]$species,]
  recs_by_wb = recs_by_sp |> sf::st_filter(d[1,]$geometry)
  d[i,]$occurrences_in_wb = nrow(recs_by_wb)
}

# 3. Distinct DFO SARA species in waterbody
sara = sf::read_sf(paste0(onedrive_wd,"CNF/DFO_SARA_occ_data_QGIS_simplified.gpkg")) |> 
  sf::st_transform(4326) |> 
  sf::st_make_valid()

d$sara_in_wb = 0

for(i in 1:nrow(d)){
  sara_by_wb = sara |> sf::st_filter(d[1,]$geometry)
  d[i,]$sara_in_wb = length(unique(sara_by_wb$Scientific_Name))
}

# 4. MaxEnt predicted suitability of waterbodies
maxent_results_l = unique(d$species) |> 
  purrr::map( ~ {

    sp_occ = occ_species[occ_species$Species == .x,]
    
    maxent_results = run_maxent(species = sp_occ, 
               predictor_data = predictor_data,
               onedrive_path = onedrive_wd,
               number_pseudoabsences = 10000
    )
    
    maxent_results
  })

names(maxent_results_l) = unique(d$species)

# Snag the predictions_r object from each element of the list.
d$wb_maxent_suitability = 0

for(i in 1:nrow(d)){
  
  the_species = d[i,]$species
  
  the_pred_r = maxent_results_l[[the_species]]$predictions_r
  
  # Pull out average values for the waterbody.
  mean_pred_val = terra::extract(the_pred_r, terra::vect(d[i,]$geometry), 'mean', na.rm = T)
  
  d[i,]$wb_maxent_suitability = round(mean_pred_val[1,2],3)
}

# 5. Risk of Introduction

# 6. Risk of Introduction - Uncertainty

# =========================================

# Make bins for variables and sum bin levels

d_sum = d |> 
  sf::st_drop_geometry() |> 
  rowwise() |> 
  dplyr::mutate(occurrences_in_wb_b = dplyr::case_when(
    occurrences_in_wb / nrow(occ_species[occ_species$Species == species,]) <= 0.33 ~ 1,
    occurrences_in_wb / nrow(occ_species[occ_species$Species == species,]) <= 0.66 ~ 2,
    occurrences_in_wb / nrow(occ_species[occ_species$Species == species,]) > 0.66 ~ 3,
    T ~ 0
  )) |> 
  dplyr::mutate(sara_in_wb_b = dplyr::case_when(
    sara_in_wb <= 1 ~ 1,
    sara_in_wb <= 3 ~ 2,
    sara_in_wb > 3 ~ 3,
    T ~ 0
  )) |> 
  dplyr::mutate(m_suit_b = dplyr::case_when(
    wb_maxent_suitability <= 0.33 ~ 1,
    wb_maxent_suitability <= 0.66 ~ 2,
    wb_maxent_suitability > 0.66 ~ 3,
    T ~ 0
  )) |> 
  ungroup() |> 
  tidyr::pivot_longer(cols = dplyr::ends_with("_b")) |> 
  dplyr::group_by(Region, species, Waterbody) |> 
  dplyr::mutate(summed_bins = sum(value)) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider()

# Put the "summed_bins" column at the very right-side of the output table.
d_sum = d_sum |> 
  dplyr::select(names(d_sum)[names(d_sum) != 'summed_bins'], "summed_bins")

# =========================================

# Create excel workbook
my_wb = createWorkbook()

# Add sheet(s)
openxlsx::addWorksheet(my_wb, "model")

# Add data to worksheet.
openxlsx::writeData(my_wb, "model", d_sum)

red_text = openxlsx::createStyle(fontColour = 'red', fontSize = 14, borderColour = '#bdf0e6')

openxlsx::addStyle(my_wb, "model", style = red_text, rows = (2:(1+nrow(d_sum))), cols = which(names(d_sum)=="summed_bins"))

openxlsx::setColWidths(my_wb, "model", cols = 1:ncol(d_sum), widths = "auto")

openxlsx::saveWorkbook(my_wb, file = "output/example_ais_prioritization_results.xlsx",
                       overwrite = T)
