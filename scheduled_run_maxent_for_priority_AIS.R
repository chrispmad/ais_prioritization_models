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

# list of invasive species on our watch list.
pr_sp = readxl::read_excel(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/AIS_priority_species.xlsx"),
                           skip = 20)
names(pr_sp) <- c("group","status","name","genus","species")
# Just for our species of interest.
pr_sp = pr_sp |>
  dplyr::filter(group == 'Fish' | name %in% c("Whirling disease") | stringr::str_detect(name, '(mussel|crayfish|mystery snail|mudsnail|clam|jellyfish|shrimp|waterflea)')) |> 
  # Split out grouped species names into separate rows.
  dplyr::mutate(name = stringr::str_squish(name)) |>
  dplyr::filter(name != 'Bullhead') |>
  dplyr::arrange(name) |> 
  # Add a couple alternate ways of spelling species common names.
  dplyr::bind_rows(
    tidyr::tibble(
      group = c('Fish','Fish','Fish','Fish','Other invertebrates','Fish',
                'Other invertebrates','Other invertebrates','Other invertebrates',
                'Fish','Fish'),
      status = c('Provincial EDRR','Provincial EDRR','Management','Management','Management','Management',
                 'Provincial Containment','Provincial Containment','Provincial Containment','Management',
                 'Prevent'),
      name = c('Oriental weatherfish','Fathead minnow','Pumpkinseed','Carp','Common Freshwater Jellyfish','Bluegill',
               'Asiatic clam','Golden clam','Good luck clam','Yellow pickerel',
               'Mosquitofish'),
      genus = c('Misgurnus','Pimephales','Lepomis','Cyprinus','Craspedacusta','Lepomis',
                'Corbicula','Corbicula','Corbicula','Sander','Gambusia'),
      species = c('anguillicaudatus','promelas','gibbosus','carpio','sowerbyi','macrochirus',
                  'fluminea','fluminea','fluminea','vitreus','affinis')
    )
  )

# Ensure species' common names are Sentence case.
pr_sp$name = stringr::str_to_sentence(pr_sp$name)

# Do record search for all species of interest! This takes a minute.
occ_dat_search_results = pr_sp$name |>
  lapply(\(x) {
    tryCatch(bcinvadeR::grab_aq_occ_data(x, 
                                         excel_path = paste0(getwd(),"/data/Master Incidence Report Records.xlsx")),
                       error=function(e)return(NULL))
    })

occ_dat_res_b = dplyr::bind_rows(occ_dat_search_results)

occ_dat_res_b = dplyr::mutate(occ_dat_res_b, Species = stringr::str_to_sentence(Species))

# Just include records that had coordinates within BC's bounding box.
occ_dat_res_b = occ_dat_res_b |>
  sf::st_transform(3005) |>
  sf::st_filter(sf::st_as_sfc(sf::st_bbox(dplyr::summarise(bcmaps::bc_bound())))) |>
  sf::st_transform(4326)

# For species with multiple common names, homogenize the names to fit whatever
# is present in 'priority_species_table.xlsx' file.
occ_dat_res_b = occ_dat_res_b |>
  dplyr::mutate(Species = dplyr::case_when(
    Species == 'Oriental weatherfish' ~ 'Oriental weather loach',
    Species == 'Fathead minnow' ~ 'Rosy red fathead minnow',
    Species == 'Mosquitofish' ~ 'Western mosquitofish',
    Species == 'Pumpkinseed' ~ 'Pumpkinseed sunfish',
    Species == 'Common freshwater jellyfish' ~ 'Freshwater jellyfish',
    Species == 'Bluegill' ~ 'Bluegill sunfish',
    Species == 'Yellow pickerel' ~ 'Walleye',
    Species %in% c("Asiatic clam","Golden clam","Good luck clam") ~ 'Asian clam',
    Species %in% c("Carp","European Carp","Common Carp") ~ "Common carp",
    T ~ Species
  ))

# In case we've picked up some Asian Carp or other species that
# we might not actually want because they're not (yet?) in BC, drop those.
occ_dat_res_b = occ_dat_res_b |>
  dplyr::filter(!Species %in% c("Asian Carp","Grass Carp","Silver Carp","Black Carp",
                                "Bighead Carp"))

# pr_sp |> 
#   dplyr::select(name) |> 
#   dplyr::mutate(vars = paste0(names(predictor_data),collapse = ', ')) |> 
#   tidyr::separate_longer_delim(cols = vars, delim = ', ') |> 
#   dplyr::mutate(include = TRUE) |> 
#   pivot_wider(names_from = vars, values_from = include) |> 
#   openxlsx::write.xlsx("data/maxent_predictor_variables_by_species_input.xlsx")

# For each species, get occurrence data and 
# run MaxEnt and save results in a folder for that species.
# Filter away occurrence records for species that are in the "Prevent" category,
# for now? Might change this if we are going to use NA-scale MaxEnt run to predict
# suitable habitat within BC.
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
          feature_classes = c("L","LQ"),
          regularisation_levels = c(1:5)
        ),
        error = function(e) NULL
      )
    } else {
      print(paste0("No need to rerun maxent for ",the_sp,", as the species occurrences have not changed and 3 months have not yet elapsed since the model was last run."))
    }
  }
}
