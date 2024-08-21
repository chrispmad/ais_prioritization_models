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
#set locations

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

# Get functions from other scripts.

source("scripts/utils/prep_predictor_data_f.R")
source("scripts/utils/run_maxent_f.R")

predictor_data = prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = onedrive_wd)

goldfish = bcinvadeR::grab_aq_occ_data('goldfish')

goldfish_results = run_maxent(species = goldfish, 
                              predictor_data = predictor_data,
                              onedrive_path = onedrive_wd)

goldfish_results$key_metrics

goldfish_results$model_fit

goldfish_results$predictions_plot

goldfish_results$evaluation_output
