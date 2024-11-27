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
library(ecospat)
library(ENMeval)
library(readxl)
library(ENMTools)
library(data.table)

#set locations

print("Beginning rerunning of MaxEnt models")

lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

bc = bcmaps::bc_bound() |> dplyr::summarise() |> sf::st_transform(4326) |> terra::vect()

# Get functions from other scripts.

source("scripts/utils/prep_predictor_data_f.R")
source("scripts/utils/run_maxent_f.R")
source("scripts/utils/gather_AIS_data.R")

file.copy(
    from = paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Master Incidence Report Records.xlsx"),
    to = 'data/Master Incidence Report Records.xlsx',
    overwrite = T
  )

predictor_data = prep_predictor_data(proj_path = proj_wd,
                             onedrive_path = paste0(onedrive_wd),
                             ext_vect = bc)

predictor_var_matrix = read_excel("inputs_for_prioritization_model.xlsx",
                                  sheet = "species_predvars")

output_folder = paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/")

# Save plots of the predictor variables.
names(predictor_data)[names(predictor_data) != 'asian_clam_temperature_limits'] |>
  lapply(\(x) {
    
    var_name = stringr::str_remove_all(x, "(_)?\\(.*\\)")
    
    if(!file.exists(paste0(output_folder,"predictor_variable_plots/",var_name,".jpg"))){

      the_plot = ggplot2::ggplot() + 
        tidyterra::geom_spatraster(data = predictor_data[[x]]) + 
        ggplot2::labs(fill = var_name) + 
        ggplot2::scale_fill_viridis_c(option = "D", na.value = NA)+
        ggplot2::theme_minimal()
      
      ggplot2::ggsave(filename = paste0(output_folder,"predictor_variable_plots/",var_name,".jpg"),
                      plot = the_plot,
                      dpi = 300, width = 8, height = 8)
    }
  })

pr_sp = gather_ais_data(data = 'species list', lan_root = lan_root, onedrive_wd = onedrive_wd)

occ_dat_res_b = gather_ais_data(data = 'occurrences', redo = T, lan_root = lan_root, onedrive_wd = onedrive_wd)

occ_dat_res_f = occ_dat_res_b |> 
  dplyr::filter(Species %in% pr_sp[pr_sp$status != "Prevent",]$name)

unique_sp = unique(occ_dat_res_f$Species)

for(i in 1:length(unique_sp)){
  
  print(i)

  the_sp = unique_sp[i]
  the_sp_snake = snakecase::to_snake_case(the_sp)
  the_sp_occ = occ_dat_res_f |> dplyr::filter(Species == the_sp)
  
  if(nrow(the_sp_occ) == 0){
    print("No occurrence records for this species in BC, according to our sources. Skipping MaxEnt run...")
  } else {
    # Do we have a MaxEnt results folder for this species yet? If not, create it.
    if(!dir.exists(paste0(output_folder,the_sp_snake)) | !file.exists(paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/",the_sp_snake,"/MaxEnt_model_run_metadata.csv"))){
      past_expiration_date = TRUE
      new_occurrences = TRUE
    } else {
      # Look to see if this species needs maxent rerun - either because it has new occurrences
      # or because its last run was more than 2 months prior.
      maxent_metadata = read.csv(file = paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/",the_sp_snake,"/MaxEnt_model_run_metadata.csv"))
      
      # Two tests of whether we should rerun maxent or not:
      # 1. Have 3 months elapsed since we last ran this species?
      past_expiration_date = lubridate::ymd(Sys.Date()) > lubridate::ymd(maxent_metadata$run_date) + lubridate::days(90)
      # 2. Have any additional occurrences been added within BC?
      new_occurrences = nrow(the_sp_occ) > maxent_metadata$number_occurrences
    }
    if(past_expiration_date | new_occurrences){
      
      print(paste0("Rerunning maxent for ",the_sp))
      
      vars_for_this_species = predictor_var_matrix |> 
        dplyr::filter(name == the_sp) |> 
        dplyr::rename(species = name) |> 
        tidyr::pivot_longer(-species) |> 
        dplyr::filter(value) |> 
        dplyr::select(name) |> 
        dplyr::distinct() |> 
        dplyr::pull(name)
      
      
      # Run maxent for this species
      tryCatch(
        expr = run_maxent(
          species = the_sp_occ, 
          predictor_data = predictor_data[[vars_for_this_species]],
          onedrive_path = onedrive_wd,
          number_pseudoabsences = 10000,
          output_folder = output_folder,
          feature_classes = c("LQ"),
          regularisation_levels = c(0.33,0.66,1)
        ),
        error = function(e) NULL
      )
    } else {
      print(paste0("No need to rerun maxent for ",the_sp,", as the species occurrences have not changed and 3 months have not yet elapsed since the model was last run."))
    }
  }
}
