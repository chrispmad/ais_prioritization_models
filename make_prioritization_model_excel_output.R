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

# Get / set file paths
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

# Find which drive letter you (or I!) are using to link to the 2 SCIENCE - Invasives folder.
sysdrivereport <- system("wmic logicaldisk get caption", intern = TRUE)
drive_letters = substr(sysdrivereport[-c(1, length(sysdrivereport))], 1, 1)
drive_letter_to_use = NA
for(drive_letter in drive_letters[!drive_letters %in% c("C","H")]){
  while(is.na(drive_letter_to_use)){
    subdirs = list.dirs(path = paste0(drive_letter,":/"), recursive = F)
    # Sometimes people have this drive set to one level above 'General' instead of at 'General', as I do.
    if("General" %in% subdirs){
      subdirs = list.dirs(path = paste0(drive_letter,":/General/"), recursive = F)
      if(length(subdirs[stringr::str_detect(subdirs, "2 SCIENCE - Invasives")]) > 0){
        drive_letter_to_use <- paste0(drive_letter,":/General/")
        print("Found your drive letter!")
      }
    }
    else {
      if(length(subdirs[stringr::str_detect(subdirs, "2 SCIENCE - Invasives")]) > 0){
        drive_letter_to_use <- paste0(drive_letter,":/")
        print("Found your drive letter!")
      }
    }
  }
}

# list of invasive species on our watch list.
pr_sp = readxl::read_excel(paste0(drive_letter_to_use,"2 SCIENCE - Invasives/SPECIES/AIS_priority_species.xlsx"),
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
d = readxl::read_excel("inputs_for_prioritization_model.xlsx")

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

all_wb = d |> dplyr::summarise()

# =========================================

# Bring in / calculate variables

# 1. Number of records in waterbody

occ_species = unique(d$Species) |> 
  purrr::map( ~ {bcinvadeR::grab_aq_occ_data(.x)}) |> 
  dplyr::bind_rows()

# Ensure date is always in this format: YYYY-MM-DD and not in 'excel' format.
occ_species = occ_species |> 
  dplyr::mutate(Date2 = case_when(
    str_detect(Date,"^[0-9]{5}") ~ openxlsx::convertToDateTime(Date),
    str_detect(Date,"^[0-9]{4}$") ~ lubridate::ymd(paste0(Date,"01-01")),
    T ~ lubridate::ymd(Date)
  )
)

d$records_in_wb = 0

for(i in 1:nrow(d)){
  recs_by_sp = occ_species[occ_species$Species == d[i,]$Species,]
  recs_by_wb = recs_by_sp |> sf::st_filter(st_buffer(d[i,]$geometry, 20))
  d[i,]$records_in_wb = nrow(recs_by_wb)
}

# See if other AIS are in the waterbody - if so, count up number of unique species!
d$other_ais_in_wb = 0
d$other_ais_in_wb_names = NA

for(i in 1:nrow(d)){
  species_in_wb = bcinvadeR::find_all_species_in_waterbody(wb = d[1,])
  # Just keep invasive species from our list.
  ais_in_wb = species_in_wb |> dplyr::filter(Species %in% str_to_title(pr_sp$name))
  d[i,]$other_ais_in_wb = length(unique(ais_in_wb$Species))
  d[i,]$other_ais_in_wb_names = paste0(unique(ais_in_wb$Species), collapse = ', ')
}

# 2. New to Waterbody - e.g. are occurrence records for waterbody from the last year?
# Also, year of oldest record.
d$new_to_waterbody = FALSE
d$oldest_record = NA

for(i in 1:nrow(d)){
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

# 3. Distinct DFO SARA species in waterbody
sara = sf::read_sf(paste0(onedrive_wd,"CNF/DFO_SARA_occ_data_QGIS_simplified.gpkg")) |> 
  sf::st_transform(4326) |> 
  sf::st_make_valid()

d$sara_in_wb = 0
d$sara_in_wb_names = NA

for(i in 1:nrow(d)){
  sara_by_wb = sara |> sf::st_filter(d[i,]$geometry)
  d[i,]$sara_in_wb = length(unique(sara_by_wb$Scientific_Name))
  d[i,]$sara_in_wb_names = paste0(unique(sara_by_wb$Common_Name_EN), collapse = ', ')
}

# 3.a CDC Red- and Blue-listed species.

cdc = bcdc_query_geodata('species-and-ecosystems-at-risk-publicly-available-occurrences-cdc') |> 
  filter(INTERSECTS(local(st_transform(all_wb,3005)))) |> 
  collect() |> 
  st_transform(4326)

cdc_f = cdc |> 
  dplyr::filter(!is.na(TAX_CLASS)) |> 
  dplyr::filter(TAX_CLASS %in% c("amphibians","ray-finned fishes","bivalves","gastropods"))

d$cdc_listed_in_wb = 0
d$cdc_listed_in_wb_names = NA

for(i in 1:nrow(d)){
  cdc_by_wb = cdc_f |> sf::st_filter(d[i,]$geometry)
  d[i,]$cdc_listed_in_wb = length(unique(cdc_by_wb$SCI_NAME))
  d[i,]$cdc_listed_in_wb_names = paste0(unique(cdc_by_wb$ENG_NAME),collapse=', ')
}

# 4. MaxEnt predicted suitability of waterbodies
maxent_results_l = unique(d$Species) |> 
  purrr::map( ~ {

    sp_occ = occ_species[occ_species$Species == .x,]
    
    maxent_results = run_maxent(species = sp_occ, 
               predictor_data = predictor_data,
               onedrive_path = onedrive_wd,
               number_pseudoabsences = 10000
    )
    
    maxent_results
  })

names(maxent_results_l) = unique(d$Species)

# Snag the predictions_r object from each element of the list.
d$wb_maxent_suitability = 0

for(i in 1:nrow(d)){
  
  the_species = d[i,]$Species
  
  the_pred_r = maxent_results_l[[the_species]]$predictions_r
  
  # Pull out average values for the waterbody.
  mean_pred_val = terra::extract(the_pred_r, terra::vect(d[i,]$geometry), 'mean', na.rm = T)
  
  d[i,]$wb_maxent_suitability = round(mean_pred_val[1,2],3)
}

# First Nations territories within 10 kilometers
fnpip = sf::read_sf("W:/CMadsen/shared_data_sets/first_nations_PIP_consultation_areas.shp") |> 
  sf::st_transform(4326)

fnpip = st_make_valid(fnpip)

d$first_nations_cons_area_overlapped = NA

for(i in 1:nrow(d)){
  fnpip_con_areas = fnpip |> sf::st_filter(d[i,]$geometry)
  # d[i,]$first_nations_cons_area_overlapped = paste0(unique(fnpip_con_areas$CNSLTN_A_2),collapse = ', ')
  d[i,]$first_nations_cons_area_overlapped = paste0(unique(fnpip_con_areas$CONTACT_NA),collapse = ', ')
}

# =========================================

# Make bins for variables and sum bin levels

d_sum = d |> 
  sf::st_drop_geometry() |> 
  rowwise() |> 
  dplyr::mutate(records_in_wb_b = dplyr::case_when(
    records_in_wb / nrow(occ_species[occ_species$Species == Species,]) <= 0.33 ~ 1,
    records_in_wb / nrow(occ_species[occ_species$Species == Species,]) <= 0.66 ~ 2,
    records_in_wb / nrow(occ_species[occ_species$Species == Species,]) > 0.66 ~ 3,
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
  dplyr::group_by(Region, Species, Waterbody) |> 
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
openxlsx::setColWidths(my_wb, "model", cols = which(names(d_sum) == 'first_nations_cons_area_overlapped'), widths = 30)

# Add metadata.
openxlsx::addWorksheet(my_wb, "metadata")

metadata = tibble(
  variable = c("new_to_waterbody"),
  notes = c("Is the oldest occurrence record for this species in this waterbody within the last six months (this time range updates each time this R script is re-run)")
)

openxlsx::writeData(my_wb, "metadata", metadata)

openxlsx::saveWorkbook(my_wb, file = "output/example_ais_prioritization_results.xlsx",
                       overwrite = T)
