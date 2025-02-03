# Do overlap of named waterbodies and SAR

# Libraries
library(tidyverse)
library(sf)
library(bcdata)

# Get / set file paths
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"

# 1. Named waterbodies
wbs = sf::read_sf("W:/CMadsen/shared_data_sets/summarized_bc_waterbodies.shp")

# Summarise waterbodies that share BLUE_LINE_KEY and GNIS_NAME?
wbs_s = wbs |> 
  dplyr::group_by(BLUE_LI, GNIS_NAME_,WATERSHED_, FWA_WAT) |> 
  dplyr::summarise()

# 2. Federally listed SARA species
sara = sf::read_sf(paste0(onedrive_wd,"CNF/DFO_SARA_occ_data_QGIS_simplified.gpkg")) |> 
  sf::st_transform(3005) |> 
  sf::st_make_valid()

sara_trim = sara |> 
  dplyr::select(sara_common_name = Common_Name_EN,
                sara_pop_name = Population_EN,
                sara_sci_name = Scientific_Name,
                sara_taxon = Taxon)

# Filter for just those species mentioned in the CNF agreement (page 31 of 41 in 2023-NF-PAC-001_BC MWLRS CA_FINAL.docx)
# SARA: Coastrange Sculpin - Cultus, Westslope Cutthroat Trout - Pacific, Bull Trout - South Coast British Columbia, Rocky Mountain Ridged Mussel
# COSEWIC: Sockeye Salmon - Cultus
sara_trim = sara_trim |> 
  dplyr::filter(sara_common_name == 'Cultus Pygmy Sculpin' & sara_pop_name == 'Cultus' | 
                  sara_common_name == 'Westslope Cutthroat Trout' & sara_pop_name == "Pacific" | 
                  sara_common_name == 'Bulltrout' & sara_pop_name == "Pacific"| 
                  sara_common_name == "Rocky Mountain Ridged Mussel")

sara_trim |> 
  sf::st_drop_geometry() |> 
  dplyr::count(sara_common_name, sara_pop_name, sort = T)
# Please note: there are no bulltrout south coast records, nor rocky mountain ridged mussel records in DFO SARA shapefile.

# Query for these species to add to the DFO SARA data
bulltrout_occs = grab_aq_occ_data("Bull Trout",excel_path = "../BC_Invasives/app/www/Master Incidence Report Records.xlsx",
                                  sources = c('SPI',"Old Aquatic","iNaturalist"))

south_coast = bcmaps::nr_regions()[stringr::str_detect(bcmaps::nr_regions()$REGION_NAME ,"South Coast"),]

bulltrout_occs_south_coast = sf::st_filter(bulltrout_occs, south_coast |> sf::st_transform(4326)) |> 
  sf::st_buffer(dist = 50) |> 
  dplyr::mutate(sara_common_name = "Bull Trout", sara_pop_name = "South Coast",
                sara_sci_name = "Salvelinus confluentus", sara_taxon = "Fishes") |> 
  sf::st_transform(3005)

sara_trim = sara_trim |> 
  dplyr::bind_rows(bulltrout_occs_south_coast |> dplyr::rename(geom = geometry))

# if("geom" %in% names(sara_trim)){
#   names(sara_trim)[which(names(sara_trim) == "geom")] = "geometry"
#   sf::st_geometry(sara_trim) = "geometry"
# }

# We've now added bulltrout in south coast to sara_trim object!

# 3. Provincially listed species-at-risk by the Conservation Data Center
cdc = bcdc_query_geodata('species-and-ecosystems-at-risk-publicly-available-occurrences-cdc') |> 
  filter(TAX_CLASS %in% c("Lampreys","bivalves","ray-finned fishes")) |> 
  collect()

cdc_trim = cdc |> 
  dplyr::select(cdc_common_name = ENG_NAME,
                cdc_sci_name = SCI_NAME,
                cdc_taxon = TAX_CLASS)

cdc_trim = cdc_trim |> 
  dplyr::filter(cdc_common_name == 'Cultus Pygmy Sculpin'| 
                  cdc_common_name == 'Westslope Cutthroat Trout'| 
                  cdc_common_name == 'Bulltrout'| 
                  cdc_common_name == "Rocky Mountain Ridged Mussel")

cdc_trim |> 
  sf::st_drop_geometry() |> 
  dplyr::count(cdc_common_name, cdc_taxon, sort = T)
# We do have rocky mountain ridged mussel in the CDC records! Nice.

wbs_overlapping_with_sara = wbs_s |> 
  sf::st_join(sara_trim)
  
wbs_overlapping_with_sara = wbs_overlapping_with_sara |> 
  dplyr::group_by(WATERSHED_,GNIS_NAME_,BLUE_LI,FWA_WAT) |> 
  dplyr::mutate(dplyr::across(names(sara_trim)[-5], \(x) paste0(unique(x), collapse = ", "))) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup()

wbs_overlapping_with_sara_and_cdc = wbs_overlapping_with_sara |> 
  sf::st_join(cdc_trim)

wbs_overlapping_with_sara_and_cdc = wbs_overlapping_with_sara_and_cdc |> 
  dplyr::group_by(WATERSHED_,GNIS_NAME_,BLUE_LI,FWA_WAT) |> 
  dplyr::mutate(dplyr::across(names(cdc_trim)[-4], \(x) paste0(unique(x), collapse = ", "))) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup() |> 
  dplyr::filter(cdc_common_name != "NA" | sara_common_name != "NA")

wbs_overlapping_with_sara_and_cdc |> 
  sf::st_drop_geometry() |> 
  dplyr::count(sara_common_name, cdc_common_name, sort = T)

sf::write_sf(wbs_overlapping_with_sara_and_cdc, paste0(onedrive_wd,"waterbodies_overlapping_with_SARA_and_CDC_occs.gpkg"))

sara_sp_to_use = tibble(species = unique(na.omit(wbs_overlapping_with_sara_and_cdc$sara_common_name)))

write.csv(sara_sp_to_use,"data/sara_species_to_look_for.csv", row.names = F)
