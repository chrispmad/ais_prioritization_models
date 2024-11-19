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
# library(ENMeval)

# =========================================

# Get / set file paths
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"

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

# Make BC shapefile.
bc = bcmaps::bc_bound() |> 
  sf::st_transform(4326) |> 
  terra::vect()

# Grab regions of BC. These will be useful for finding waterbodies.
regs = bcmaps::nr_regions() |> sf::st_transform(4326)

# predictor_data = prep_predictor_data(proj_path = proj_wd,
#                                      onedrive_path = paste0(onedrive_wd),
#                                      ext_vect = bc)

# =========================================

# Table of Region, species and waterbody names. Note: might be nicer to make this
# in excel and read it in.
d = readxl::read_excel("inputs_for_prioritization_model.xlsx")

# =========================================

# Find Waterbodies (based on name and region!)
unique_wbs = d |> 
  dplyr::select(Waterbody, Region) |> 
  dplyr::distinct()

# If the waterbody is any of the following: Okanagan Lake, Okanagan River,
# Skaha Lake, Vaseux Lake, Vaseux Creek, Osoyoos Lake, treat those as one monolithic
# unit.
TO_string = c("Okanagan Lake","Okanagan River","Skaha Lake",
              "Vaseux Lake","Vaseux Creek","Osoyoos Lake")

wbs_list = purrr::map2(unique_wbs$Waterbody, unique_wbs$Region, ~ {
  
  the_reg = regs[str_detect(regs$ORG_UNIT_NAME,.y),] |> sf::st_transform(3005)
  
  if(!.x %in% TO_string){
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
        potential_lakes |> dplyr::select(wb_name = GNIS_NAME_1, FWA_WATERSHED_CODE, geometry),
        potential_rivers |> dplyr::select(wb_name = GNIS_NAME_1, FWA_WATERSHED_CODE, geometry)
      )
    }
    if(nrow(potential_lakes) > 0 & nrow(potential_rivers) == 0){
      wbs = potential_lakes |> dplyr::select(wb_name = GNIS_NAME_1, FWA_WATERSHED_CODE, geometry)
    }
    if(nrow(potential_lakes) == 0 & nrow(potential_rivers) > 0){
      wbs = potential_rivers |> dplyr::select(wb_name = GNIS_NAME_1, FWA_WATERSHED_CODE, geometry)
    }
    
    if(nrow(potential_lakes) == 0 & nrow(potential_rivers) == 0){
      return(NULL)
    } else {
      return(
        wbs |> 
          dplyr::group_by(wb_name,FWA_WATERSHED_CODE) |> 
          dplyr::summarise()
      )
    }
  } else {
    # The name is one of the Thompson-Okanagan string. Let's just snag all those 
    # waterbodies.
    wbs_rivers = bcdc_query_geodata('freshwater-atlas-rivers') |> 
      filter(GNIS_NAME_1 %in% TO_string) |> 
      collect() |> 
      dplyr::summarise()
    
    wbs_lakes = bcdc_query_geodata('freshwater-atlas-lakes') |> 
      filter(GNIS_NAME_1 %in% TO_string) |> 
      collect() |> 
      dplyr::summarise()
    
    wbs = dplyr::bind_rows(wbs_rivers,wbs_lakes) |> 
      dplyr::summarise() |> 
      dplyr::mutate(wb_name = .x) |> 
      dplyr::mutate(FWA_WATERSHED_CODE = "300-432687-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000")
    
    wbs
  }
}, .progress = TRUE)

wbs = wbs_list |> 
  dplyr::bind_rows() |> 
  sf::st_transform(4326)

# Add these geometries onto 'd'
d = d |> 
  dplyr::left_join(wbs |> dplyr::rename(Waterbody = wb_name))

d = sf::st_set_geometry(d, 'geometry')

# Add waterbody area and depth if available.
d$Waterbody_Area_sq_km = as.numeric(sf::st_area(d$geometry))/1000000

all_wb = d |> dplyr::summarise()

# Calculate number of inflows and outflows using fwa.connect package!!
# If you don't have this package installed, grab it from Chris' github page:
#devtools::install_github('chrispmad/fwa.connect')
library(fwa.connect)

tbl_of_fwa_junctions = fwa.connect::stream_conn_tbl()

d = d |> 
  rowwise() |> 
  dplyr::mutate(number_outflows = nrow(tbl_of_fwa_junctions[tbl_of_fwa_junctions$upstream_fwa_code == FWA_WATERSHED_CODE,]),
                number_inflows = nrow(tbl_of_fwa_junctions[tbl_of_fwa_junctions$downstream_fwa_code == FWA_WATERSHED_CODE,])) |> 
  dplyr::ungroup()

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

# # See if other AIS are in the waterbody - if so, count up number of unique species!
# d$native_species_in_wb = 0
# d$native_species_in_wb_names = NA
# d$other_ais_in_wb = 0
# d$other_ais_in_wb_names = NA
# 
# for(the_wb in wbs_list){
#   
#   species_in_wb = bcinvadeR::find_all_species_in_waterbody(wb = the_wb)
#   
#   # Invasive species (identified on our priority AIS list).
#   ais_in_wb = species_in_wb |> dplyr::filter(Species %in% str_to_title(pr_sp$name))
#   other_ais_in_wb = unique(ais_in_wb$Species)
#   
#   # Native species (not on the AIS list)
#   native_sp_in_wb = species_in_wb |> dplyr::filter(!Species %in% str_to_title(pr_sp$name))
#   
#   d_for_waterbody = d[d$Waterbody == the_wb$wb_name,]
#   
#   for(i in 1:nrow(d_for_waterbody)){
#     other_ais_in_wb_2 = other_ais_in_wb[!stringr::str_detect(other_ais_in_wb, d_for_waterbody[i,]$Species)]
#     d[d$Waterbody == the_wb$wb_name,][i,]$other_ais_in_wb = length(other_ais_in_wb_2)
#     d[d$Waterbody == the_wb$wb_name,][i,]$other_ais_in_wb_names = paste0(other_ais_in_wb_2, collapse = ', ')
#     d[d$Waterbody == the_wb$wb_name,][i,]$native_species_in_wb = length(native_sp_in_wb)
#     d[d$Waterbody == the_wb$wb_name,][i,]$native_species_in_wb_names = paste0(native_sp_in_wb, collapse = ', ')
#   }
# }

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

# 3. Distinct DFO SARA species, COSEWIC and CDC species in waterbody.
sara = sf::read_sf(paste0(onedrive_wd,"CNF/DFO_SARA_occ_data_QGIS_simplified.gpkg")) |> 
  sf::st_transform(4326) |> 
  sf::st_make_valid()

# Go up one level in the file structure to find the SAR scraper R project folder.
# This is where the 2 cleaned tables (cleaned by John with Python!) reside.
federal_risk_registry_tbl = tidyr::as_tibble(read.csv("../SAR_scraper/output/risk_status_merged.csv"))
  
cosewic_risk_status_sp = federal_risk_registry_tbl |> 
  dplyr::filter(COSEWIC.status %in% c("Endangered","Special Concern","Threatened")) |>
  dplyr::filter(COSEWIC.status %in% c("","No Status")) |> 
  dplyr::filter(Taxonomic.group %in% c("Fishes (freshwater)","Molluscs")) |> 
  dplyr::select(COSEWIC.common.name) |> 
  dplyr::filter(!COSEWIC.common.name %in% c("White Sturgeon")) |> 
  dplyr::distinct() |> 
  dplyr::pull(COSEWIC.common.name)

sara_sp = federal_risk_registry_tbl |> 
  dplyr::filter(COSEWIC.status %in% c("Endangered","Special Concern","Threatened")) |> 
  dplyr::filter(Taxonomic.group %in% c("Fishes (freshwater)","Molluscs")) |> 
  dplyr::select(COSEWIC.common.name) |> 
  dplyr::distinct() |> 
  dplyr::pull(COSEWIC.common.name)

cdc = bcdc_query_geodata('species-and-ecosystems-at-risk-publicly-available-occurrences-cdc') |> 
  filter(INTERSECTS(local(st_transform(all_wb,3005)))) |> 
  collect() |> 
  st_transform(4326) |> 
  sf::st_filter(all_wb)

cdc_f = cdc |> 
  dplyr::filter(!is.na(TAX_CLASS)) |> 
  dplyr::filter(TAX_CLASS %in% c("amphibians","ray-finned fishes","bivalves","gastropods"))

d$sara_in_wb = 0
d$sara_in_wb_names = NA
d$sara_downstream = 0
d$sara_downstream_names = NA
d$cdc_listed_in_wb = 0
d$cdc_listed_in_wb_names = NA
d$cdc_listed_downstream = 0
d$cdc_listed_downstream_names = NA
d$COSEWIC_in_wb = 0
d$COSEWIC_in_wb_names = NA
d$COSEWIC_downstream = 0
d$COSEWIC_downstream_names = NA
d$native_species_in_wb = 0
d$native_species_in_wb_names = NA
d$other_ais_in_wb = 0
d$other_ais_in_wb_names = NA

# This loop cycles through the waterbodies, searching for spatial overlaps with
# polygons (in the case of SARA and CDC) as well as overlap with point occurrence 
# data from iNaturalist, the BC Data Catalogue, our invasive species tracking sheet,
# etc. It fills in all the variables we initialize above.
for(i in 1:nrow(unique_wbs)){
  
  the_wb = unique_wbs[i,]
  
  print(paste0("Working on waterbody ",i,", which is ",the_wb$Waterbody))
  
  geom_and_fwa_for_wb = d |> dplyr::filter(Waterbody == the_wb$Waterbody, Region == the_wb$Region) |> 
    dplyr::select(Waterbody,Region,FWA_WATERSHED_CODE,geometry)
  
  the_wb = the_wb |> dplyr::left_join(geom_and_fwa_for_wb, by = join_by(Waterbody, Region))
  
  the_wb = sf::st_set_geometry(the_wb, "geometry")
  
  # Find downstream waterbody
  # If the chosen waterbody is one of the Thompson string, don't look downstream.
  if(the_wb$Waterbody %in% TO_string){
    ds_wb = the_wb |> dplyr::select(geometry)
    ds_wb = ds_wb[0,]
  } else {
    # Waterbodies downstream, up to 5 kilometers away.
    buffer_5km = sf::st_buffer(sf::st_as_sfc(sf::st_bbox(the_wb)),5000)
    wb_fwa_code = the_wb$FWA_WATERSHED_CODE
    ds_river = bcdata::bcdc_query_geodata('freshwater-atlas-rivers') |> filter(FWA_WATERSHED_CODE == wb_fwa_code) |> collect() |> dplyr::summarise(Waterbody = GNIS_NAME_1) |> sf::st_transform(4326)
    ds_lake = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |> filter(FWA_WATERSHED_CODE == wb_fwa_code) |> collect() |> dplyr::summarise(Waterbody = GNIS_NAME_1) |> sf::st_transform(4326)
    if(nrow(ds_lake)>0 & ds_lake$Waterbody != the_wb$Waterbody){
      ds_wb = ds_lake
    } else {
      ds_wb = ds_river
    }
    ds_wb = sf::st_intersection(ds_wb, buffer_5km)
  }
  
  # Do initial spatial filtering with SARA and CDC spatial files.
  sara_polys_in_wb = sara |> sf::st_filter(the_wb)
  sara_polys_in_ds_wb = sara |> sf::st_filter(ds_wb)
  cdc_polys_in_wb = cdc_f |> sf::st_filter(the_wb)
  cdc_polys_in_ds_wb = cdc_f |> sf::st_filter(ds_wb)
  
  # Look up all species present in the waterbody, to find COSEWIC species
  # and native species. This looks only for taxa labelled "fishes","Actinopterygii", and "Mollusca",
  # but you can add more taxa if desired.
  species_in_wb = bcinvadeR::find_all_species_in_waterbody(wb = sf::st_transform(the_wb,3005))
  # Same for downstream waterbody, unless there is none, as in the case of the Thompson-Okanagan string of lakes, which
  # we are treating as a single, connected unit.
  if(nrow(ds_wb) == 0){
    species_in_ds_wb = c()
  } else {
    species_in_ds_wb = bcinvadeR::find_all_species_in_waterbody(wb = sf::st_transform(ds_wb,3005))
  }
  
  # Find all species present.
  unique_species_in_wb = unique(species_in_wb$Species)
  unique_species_in_ds_wb = unique(species_in_ds_wb$Species)
  
  # Clean up this vector a bit: delete anything in parentheses. Any other steps?
  unique_species_in_wb = stringr::str_squish(stringr::str_remove_all(unique_species_in_wb, " (\\(.*\\)|\\-.*)"))
  unique_species_in_ds_wb = stringr::str_squish(stringr::str_remove_all(unique_species_in_ds_wb, " (\\(.*\\)|\\-.*)"))
  
  # Drop empty elements.
  unique_species_in_wb = unique_species_in_wb[unique_species_in_wb != ""]
  unique_species_in_ds_wb = unique_species_in_ds_wb[unique_species_in_ds_wb != ""]
  
  # Find list of native species, aquatic invasive species, COSEWIC, SARA listed species.
  native_sp_in_wb = unique(unique_species_in_wb[!unique_species_in_wb %in% str_to_title(pr_sp$name)])
  ais_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% str_to_title(pr_sp$name)])
  cosewic_sp_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% cosewic_risk_status_sp])
  sara_sp_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% sara_sp])
  
  # Same for downstream.
  # native_sp_in_ds_wb = unique(unique_species_in_ds_wb[!unique_species_in_ds_wb %in% str_to_title(pr_sp$name)]
  # ais_in_ds_wb = unique_species_in_ds_wb[unique_species_in_ds_wb %in% str_to_title(pr_sp$name)]
  cosewic_sp_in_ds_wb = unique(unique_species_in_ds_wb[unique_species_in_ds_wb %in% cosewic_risk_status_sp])
  sara_sp_in_ds_wb = unique(unique_species_in_ds_wb[unique_species_in_ds_wb %in% sara_sp])
  
  # Now find all rows in our dataframe that have this waterbody as their target.
  # For each one, count up the above species groupings.
  for(y in 1:nrow(d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,])){
    # Ensure this gets applied to the right rows in d.
    # Add on AIS and Native species
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$other_ais_in_wb = length(unique(ais_in_wb))
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$other_ais_in_wb_names = ifelse(length(ais_in_wb) > 0, paste0(ais_in_wb, collapse = ", "), NA)
    
    # Combine sara spatial overlaps with point data that is of a SARA-listed species name.
    sara_combo = unique(c(unique(sara_sp_in_wb),sara_polys_in_wb$Common_Name_EN))
    sara_combo_ds = unique(c(unique(sara_sp_in_ds_wb),sara_polys_in_ds_wb$Common_Name_EN))
    
    # CDC names. Make sure SARA and COSEWIC names aren't present here.
    cdc_in_wb = unique(cdc_polys_in_wb$ENG_NAME)
    cdc_in_wb = cdc_in_wb[!cdc_in_wb %in% sara_sp]
    cdc_in_wb = cdc_in_wb[!cdc_in_wb %in% cosewic_risk_status_sp]
    cdc_in_ds_wb = unique(cdc_polys_in_ds_wb$ENG_NAME)
    cdc_in_ds_wb = cdc_in_ds_wb[!cdc_in_ds_wb %in% sara_sp]
    cdc_in_ds_wb = cdc_in_ds_wb[!cdc_in_ds_wb %in% cosewic_risk_status_sp]
    
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_in_wb = length(unique(sara_combo))
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_in_wb_names = ifelse(length(sara_combo) > 0, paste0(sara_combo, collapse = ", "), NA)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_downstream = length(unique(sara_combo_ds))
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_downstream_names = ifelse(length(sara_combo_ds) > 0, paste0(sara_combo_ds, collapse = ", "), NA)
    
    # COSEWIC species
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_in_wb = length(cosewic_sp_in_wb)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_in_wb_names = ifelse(length(cosewic_sp_in_wb) > 0, paste0(cosewic_sp_in_wb, collapse = ", "), NA)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_downstream = length(cosewic_sp_in_ds_wb)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_downstream_names = ifelse(length(cosewic_sp_in_ds_wb) > 0, paste0(cosewic_sp_in_ds_wb, collapse = ", "), NA)
    
    # CDC species
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_in_wb = length(cdc_in_wb)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_in_wb_names = ifelse(length(cdc_in_wb) > 0, paste0(cdc_in_wb, collapse = ", "), NA)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_downstream = length(cdc_in_ds_wb)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_downstream_names = ifelse(length(cdc_in_ds_wb) > 0, paste0(cdc_in_ds_wb, collapse = ", "), NA)
  }
}

# Snag the predictions_r object from each element of the list.
d$wb_maxent_suitability = 0
d$wb_maxent_training_AUC = 0
d$wb_maxent_suitability_fig = "need link"

for(i in 1:nrow(d)){
  
  the_species = d[i,]$Species
  the_species_snake = snakecase::to_snake_case(the_species)
  species_folder = paste0(output_folder,the_species_snake,"/")
  # Temporary fix for pumpkinseed sunfish... will have to figure this out.
  species_folder = stringr::str_replace(species_folder,"pumpkinseed\\/","pumpkinseed_sunfish\\/")
  
  # Read maxent results in for species
  the_pred_r = terra::rast(paste0(species_folder,"MaxEnt_prediction_raster.tif"))

  # Pull out average values for the waterbody.
  mean_pred_val = terra::extract(the_pred_r, terra::vect(d[i,]$geometry), 'mean', na.rm = T)
  # Is Median better??
  median_pred_val = terra::extract(the_pred_r, terra::vect(d[i,]$geometry), 'median', na.rm = T)
  
  d[i,]$wb_maxent_suitability = round(mean_pred_val[1,2],3)
  
  maxent_key_metrics = read.csv(paste0(species_folder,"MaxEnt_key_metrics.csv"))
  
  d[i,]$wb_maxent_training_AUC = maxent_key_metrics[maxent_key_metrics$metric == "training_auc",]$value
  
  the_wb = wbs[wbs$wb_name == d[i,]$Waterbody,]
  
  backdrop_path = paste0(species_folder,"MaxEnt_prediction_plot_no_occ.jpg")
  
  backdrop = ggplot() + 
    geom_image(aes(x=1,y=1,image = backdrop_path), size = 1) + 
    theme(axis.text = element_blank())
  
  the_pred_for_wb_r = terra::crop(the_pred_r, terra::vect(sf::st_buffer(d[i,]$geometry,5000)))
  
  inset = ggplot() + 
    tidyterra::geom_spatraster(data = the_pred_for_wb_r) + 
    geom_sf(data = d[i,], col = 'red', fill = 'transparent') + 
    ggthemes::theme_map() + 
    scale_fill_viridis_c() +
    labs(fill = 'Predicted\nSuitability') + 
    coord_sf(expand = F) +
    theme(legend.position = 'none',
          plot.background = element_rect(fill = 'transparent', color = 'black'))
  
  combo_plot = backdrop + 
    patchwork::inset_element(inset, 
                             left = 0.1,
                             bottom = 0.15,
                             right = 0.3,
                             top = 0.5)
  
  ggsave(filename = paste0(species_folder,"MaxEnt_prediction_plot_no_occ_w_inset.jpg"),
         plot = combo_plot,
         dpi = 300,
         width = 8, height = 8)
  
  d[i,]$wb_maxent_suitability_fig = paste0(
    "HYPERLINK(\"",
    paste0(species_folder,"MaxEnt_prediction_plot_no_occ_w_inset.jpg"),
    "\", \"",
    "LINK",
    "\")"
  )
}

# First Nations territories within 10 kilometers
fnpip = sf::read_sf("W:/CMadsen/shared_data_sets/first_nations_PIP_consultation_areas.shp") |> 
  sf::st_transform(4326)

fnpip = st_make_valid(fnpip)

d$first_nations_cons_area_overlapped = NA

for(i in 1:nrow(d)){
  fnpip_con_areas = fnpip |> sf::st_filter(sf::st_buffer(d[i,]$geometry,10000))
  # d[i,]$first_nations_cons_area_overlapped = paste0(unique(fnpip_con_areas$CNSLTN_A_2),collapse = ', ')
  d[i,]$first_nations_cons_area_overlapped = paste0(unique(fnpip_con_areas$CONTACT_NA),collapse = ', ')
}

# =========================================

# Make bins for variables and sum bin levels

d_sum = d |> 
  sf::st_drop_geometry() |> 
  rowwise() |> 
  # dplyr::mutate(records_in_wb_b = dplyr::case_when(
  #   records_in_wb / nrow(occ_species[occ_species$Species == Species,]) <= 0.33 ~ 1,
  #   records_in_wb / nrow(occ_species[occ_species$Species == Species,]) <= 0.66 ~ 2,
  #   records_in_wb / nrow(occ_species[occ_species$Species == Species,]) > 0.66 ~ 3,
  #   T ~ 0
  # )) |> 
  dplyr::mutate(other_ais_in_wb_b = dplyr::case_when(
    other_ais_in_wb <= 3 ~ 0,
    other_ais_in_wb <= 6 ~ -1,
    other_ais_in_wb > 6 ~ -2,
    T ~ 0
  )) |> 
  dplyr::mutate(sara_in_wb_b = dplyr::case_when(
    sara_in_wb == 0 ~ 0,
    sara_in_wb == 1 ~ 1,
    sara_in_wb == 2 ~ 2,
    sara_in_wb >= 3 ~ 3,
    T ~ 0
  )) |> 
  dplyr::mutate(maxent_suitability_b = dplyr::case_when(
    wb_maxent_suitability <= 0.33 ~ 1,
    wb_maxent_suitability <= 0.66 ~ 2,
    wb_maxent_suitability > 0.66 ~ 3,
    T ~ 0
  )) |> 
  dplyr::mutate(m_suit_uncertainty_b = dplyr::case_when(
    wb_maxent_training_AUC >= 0.9 ~ 1,
    wb_maxent_training_AUC >= 0.8 ~ 2,
    wb_maxent_training_AUC < 0.8 ~ 3,
    T ~ 0
  )) |> 
  dplyr::mutate(oldest_record_b = round(1/log(as.numeric(stringr::str_extract(Sys.Date(),'^[0-9]{4}')) - oldest_record + 2.71),3)) |> 
  ungroup() |> 
  tidyr::pivot_longer(cols = c(dplyr::ends_with("_b"),dplyr::contains("Current"))) |> 
  dplyr::group_by(Region, Species, Waterbody) |> 
  dplyr::mutate(summed_bins = round(sum(value,na.rm=T),0)) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider()

# Put the "summed_bins" column at the very right-side of the output table.
d_sum = d_sum |> 
  dplyr::select(names(d_sum)[names(d_sum) != 'summed_bins'], "summed_bins")

# Modify columns that will be hyperlinks
for(i in 1:nrow(d_sum)){
  
  the_species = d_sum[i,]$Species
  the_species_snake = snakecase::to_snake_case(the_species)
  species_folder = paste0(output_folder,the_species_snake,"/")
  # Temporary fix for pumpkinseed sunfish... will have to figure this out.
  species_folder = stringr::str_replace(species_folder,"pumpkinseed\\/","pumpkinseed_sunfish\\/")
  
  # Link Species column to MaxEnt folder.
  d_sum[i,]$Species = paste0(
        "HYPERLINK(\"",
        species_folder,
        "\", \"",
        d_sum[i,]$Species,
        "\")"
      )
}


# Make column names nicer.
d_sum = d_sum |> 
  dplyr::rename(
    `Total Records` = records_in_wb,
    `Other AIS in WB` = other_ais_in_wb,
    `Other AIS in WB names` = other_ais_in_wb_names,
    `New to Waterbody` = new_to_waterbody,
    `Oldest Record` = oldest_record,
    `Distinct SARA in WB` = sara_in_wb,
    `Distinct SARA in WB names` = sara_in_wb_names,
    `CDC-listed species in WB` = cdc_listed_in_wb,
    `CDC-listed species in WB names` = cdc_listed_in_wb_names,
    `MaxEnt Habitat Suitability` = wb_maxent_suitability,
    `MaxEnt Model Performance` = wb_maxent_training_AUC,
    `First Nations Consultation Areas` = first_nations_cons_area_overlapped
  )
# =========================================

# specify column as formula per openxlsx::writeFormula option #2
class(d_sum$Species) <- "formula"
class(d_sum$wb_maxent_suitability_fig) <- "formula"

# Create excel workbook
my_wb = createWorkbook()

# Add sheet(s)
openxlsx::addWorksheet(my_wb, "model")

# Add data to worksheet.
openxlsx::writeData(my_wb, "model", d_sum)

red_text = openxlsx::createStyle(fontColour = 'red', fontSize = 14, borderColour = '#bdf0e6')
blue_text = openxlsx::createStyle(fontColour = 'blue', fontSize = 12)

openxlsx::addStyle(my_wb, "model", style = red_text, rows = (2:(1+nrow(d_sum))), cols = which(names(d_sum)=="summed_bins"))
openxlsx::addStyle(my_wb, "model", style = blue_text, rows = (2:(1+nrow(d_sum))), cols = which(names(d_sum)=="Species"))
openxlsx::addStyle(my_wb, "model", style = blue_text, rows = (2:(1+nrow(d_sum))), cols = which(names(d_sum)=="wb_maxent_suitability_fig"))

openxlsx::setColWidths(my_wb, "model", cols = 1:ncol(d_sum), widths = "auto")
openxlsx::setColWidths(my_wb, "model", cols = which(names(d_sum) == "Species"), widths = 20)
openxlsx::setColWidths(my_wb, "model", cols = which(names(d_sum) == "First Nations Consultation Areas"), widths = 30)
openxlsx::setColWidths(my_wb, "model", cols = which(names(d_sum) == 'Other AIS in WB names'), widths = 40)

# Add metadata.
openxlsx::addWorksheet(my_wb, "metadata")

metadata = tibble(
  variable = c("New to Waterbody",
               "First Nations Consultation Areas",
               "Oldest Record Bin",
               "Other AIS in WB",
               "MaxEnt Habitat Suitability",
               "MaxEnt Model Performance"),
  notes = c("Is the oldest occurrence record for this species in this waterbody within the last six months (this time range updates each time this R script is re-run)",
            "The First Nations PIP Consultation Areas that are within 10 kilometers of the waterbody",
            "Records from this year receive a level of 1, while records from previous years receive an expoentially smaller priority number. 
            To calculate this, we take the reciprocal of the log of the current year minus the oldest record in the waterbody plus 2.71",
            "The number of other aquatic invasive species present in the waterbody, according to our occurrence records and iNaturalist. If this number is 0 to 3, it does not affect the priority ranking; 
            if it is 4 to 6, it reduces the priority by 1; if it is greater than 6, it reduces the priority by 2",
            "We use MaxEnt based on numerous predictor variables and known locations of species to predict the relative probability of finding the species in question at the chosen waterbody; the binned value
            is simply the range from 0 to 1 cut into thirds: if the predicted probability is from 0 to 0.33, the binned value is 1; from 0.34 to 0.66, 2; 0.67 to 1, 3",
            "The Area-Under-Curve of the MaxEnt model is used to evaluate how well a model can estimate known presence and background locations - 
            it is a score that describes model performance, where the maximum value is 1 and the minimum is 0. 
            To bin this value, I take anything over 0.9 as having the lowest uncertainty (1), anything between 0.8 and 0.9 as more uncertain (2), and anything lower than 0.8 to be much more uncertain (3)")
)

openxlsx::writeData(my_wb, "metadata", metadata)
openxlsx::setColWidths(my_wb,"metadata",cols = 2:ncol(metadata), widths = 120)

openxlsx::saveWorkbook(my_wb, file = "output/example_ais_prioritization_results.xlsx",
                       overwrite = T)
