library(tidyverse)
library(DBI)
library(bcinvadeR)
library(terra)
library(sf)
library(geodata)
library(predicts)
library(ggpubr)
library(dismo)
library(rJava)
source("ZuurFuncs.R")
library(ecospat)
library(ENMeval)

#set locations

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

# Get functions from other scripts.

source("scripts/utils/prep_predictor_data_f.R")
source("scripts/utils/run_maxent_f.R")

predictor_data = prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = paste0(onedrive_wd))

if(!file.exists("data/goldfish_example_data.rds")){
  goldfish = bcinvadeR::grab_aq_occ_data('goldfish')
  
  
  # Ensure unique coordinates by adding 
  goldfish_jitter = goldfish |> 
    dplyr::filter(duplicated(paste0(geometry))) |> 
    sf::st_jitter(factor = 0.0001)
  
  not_jittered_goldfish = goldfish |> 
    dplyr::filter(!duplicated(paste0(geometry)))
  
  goldfish_j = dplyr::bind_rows(goldfish_jitter,
                                not_jittered_goldfish)
  # library(leaflet)
  # leaflet() |>
  #   addTiles() |> 
  #   addCircleMarkers(data = goldfish, color = 'black', fillColor = 'transparent') |> 
  #   addCircleMarkers(data = goldfish_jitter, color = 'red', fillColor = 'red')
  
  
  saveRDS(goldfish_j, "data/goldfish_example_data.rds")
} else {
  goldfish = readRDS("data/goldfish_example_data.rds")
}

pumpkinseed = bcinvadeR::grab_aq_occ_data('pumpkinseed')
smb = bcinvadeR::grab_aq_occ_data('smallmouth bass')

# freshwaterjelly = bcinvadeR::grab_aq_occ_data('freshwater jellyfish')
# freshwaterjelly2 = bcinvadeR::grab_aq_occ_data('common freshwater jellyfish')
# freshwaterjelly = dplyr::bind_rows(freshwaterjelly, freshwaterjelly2)

source("scripts/utils/run_maxent_f.R")

goldfish_results = run_maxent(species = goldfish, 
                              predictor_data = predictor_data,
                              onedrive_path = onedrive_wd,
                              number_pseudoabsences = 10000
                              )

pumkinseed_results = run_maxent(species = pumpkinseed, 
                              predictor_data = predictor_data,
                              onedrive_path = onedrive_wd,
                              number_pseudoabsences = 10000
)

smb_results = run_maxent(species = smb, 
                              predictor_data = predictor_data,
                              onedrive_path = onedrive_wd,
                              number_pseudoabsences = 10000
)


goldfish_results$maxent_results

goldfish_results$key_metrics

goldfish_results$model_fit

terra::plot(goldfish_results$predictions_r)

goldfish_results$predictions_plot

terra::plot(goldfish_results$habitat_predictions)
