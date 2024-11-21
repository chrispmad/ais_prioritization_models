library(terra)
library(bcdata)
library(tidyverse)
library(raster)
library(sf)
library(exactextractr)

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"

bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))

ref = terra::rast(paste0(onedrive_wd, "reference_raster_wgs84.tif"))

# Rob Paynter has shared this DFO survey data with us.

if(!file.exists("data/question_10_from_2023_DFO_angler_survey.rds")){
  dfo_survey_filepaths = list.files(path = onedrive_wd,
                                    pattern = "set_DFO",
                                    full.names = T)
  
  dfo_surveys = lapply(dfo_survey_filepaths, openxlsx::read.xlsx)
  
  #Reading in thwe excel files takes a lot of RAM; saving as RDS files.
  survey_2023 = dfo_surveys[[1]]
  survey_2024 = dfo_surveys[[2]]
  rm(dfo_surveys)
  
  # These surveys have a ridiculous number of columns. Ugh.
  # We just care about question X ( ).
  # Q5 is which months were they active.
  # Q7 is how many days fishing during active months
  # Q10 is the number of days fished by waterbody / "area"
  
  days_per_wb = survey_2023 |> 
    # Grab the metadata columns and also the responses to question 10.
    dplyr::select(ID:NOTES,dplyr::contains("Q10A")) |> 
    tidyr::as_tibble() |> 
    dplyr::select(ID,DATE,dplyr::contains("Q10A")) |> 
    dplyr::mutate(across(everything(), as.character)) |> 
    tidyr::pivot_longer(cols = -c(ID,DATE)) |> 
    dplyr::filter(!is.na(value)) |> 
    dplyr::mutate(name = stringr::str_remove(name, "\\.[0-9]+$"))
  
  saveRDS(days_per_wb, "data/question_10_from_2023_DFO_angler_survey.rds")
  
  # Not sure exactly how to parse the 2024 survey: it has a different list of 
  # questions from the 2023 survey.
} else {
  days_per_wb = readRDS("data/question_10_from_2023_DFO_angler_survey.rds")
}

# Clean up this table.
days_per_wb_sum = days_per_wb |>
  # Drop the row describing fish species.
  dplyr::filter(!stringr::str_detect(name,"species$")) |> 
  dplyr::distinct() |> 
  # Remove the number from inside the name column.
  dplyr::mutate(name = stringr::str_remove(name,'(?<=_)[0-9]+_')) |> 
  dplyr::distinct() |> 
  # Fill out new columns for GMZ, Waterbody, and number of days fishing
  dplyr::group_by(ID,DATE) |> 
  dplyr::mutate(GMZ = ifelse(name == 'Q10A_gmz', value, NA)) |> 
  tidyr::fill(GMZ, .direction = 'down') |> 
  dplyr::mutate(Waterbody = ifelse(name == 'Q10A_lakes', value, NA)) |> 
  tidyr::fill(Waterbody, .direction = 'updown') |> 
  dplyr::mutate(days_fished = ifelse(name == 'Q10A_wmu', value, NA)) |> 
  tidyr::fill(days_fished, .direction = 'up') |> 
  # Remove the prefix number from WMU - I'm assuming this number refers to the GMZ column, 
  # not the lower range of fishing days.
  dplyr::mutate(days_fished = stringr::str_remove(days_fished,'[0-9]+-')) |> 
  dplyr::ungroup() |> 
  dplyr::select(-name, -value) |> 
  dplyr::distinct() |> 
  # Pull out excel-type dates from the WMU column
  dplyr::mutate(excel_date = openxlsx::convertToDate(days_fished)) |> 
  # Values like 3, 4, and 15 get accidentally converted to a date in 1900. Remove those.
  dplyr::mutate(excel_date = ifelse(excel_date < as.Date('1980-01-01'), NA, excel_date)) |> 
  # Find the number of rows of such excel-type dates, by ID and DATE columns;
  # assign that integer of rows to be the WMU for those rows.
  dplyr::group_by(ID,DATE,GMZ,Waterbody) |> 
  dplyr::mutate(number_days_fished_rows = as.character(n())) |> 
  dplyr::mutate(days_fished = ifelse(!is.na(excel_date), number_days_fished_rows, days_fished)) |> 
  dplyr::ungroup() |> 
  dplyr::select(-excel_date, -number_days_fished_rows) |> 
  dplyr::distinct() |> 
  # Split rows with multiple waterbodies listed together, separated by a comma, 
  # into new rows.
  tidyr::separate_longer_delim(cols = Waterbody, delim = ', ')

# Ok, now summarise these numbers to water body by name (and region?)
days_per_wb_sum = days_per_wb_sum |> 
  dplyr::count(GMZ,Waterbody, wt = as.numeric(days_fished), sort = T) |> 
  dplyr::mutate(GMZ = stringr::str_to_upper(GMZ))

length(unique(days_per_wb_sum$Waterbody))

the_hash = rlang::hash(days_per_wb_sum)

if(the_hash != readRDS("data/fished_lake_polygons_hashcode.rds")){
  # I have to figure out how to tie these to waterbody geometries... what is the GMZ field??
  fished_lakes = unique(days_per_wb_sum$Waterbody)
  
  fished_lakes_polys = bcdc_query_geodata('freshwater-atlas-lakes') |> 
    filter(GNIS_NAME_1 %in% fished_lakes) |> 
    collect()
  
  fished_lakes_polys = fished_lakes_polys |> 
    dplyr::group_by(WATERSHED_GROUP_ID,GNIS_NAME_1) |> 
    dplyr::summarise() |> 
    dplyr::ungroup()
  
  saveRDS(fished_lakes_polys,"data/fished_lakes_polygons.rds")
  saveRDS(the_hash,"data/fished_lake_polygons_hashcode.rds")
  
} else {
  fished_lakes_polys = readRDS("data/fished_lakes_polygons.rds")
}

# The following object can be joined to the days_per_wb_sum to inform
# exactly which polygon for a given name, and also spatially joined
# to our lake polygons to help us join our data summaries to the polygons.
manage_units = bcdc_query_geodata('wildlife-management-units') |> 
  collect()

manage_units_by_zone = manage_units |> 
  dplyr::rename(GMZ = GAME_MANAGEMENT_ZONE_ID) |> 
  dplyr::mutate(GMZ = stringr::str_to_upper(GMZ)) |> 
  dplyr::group_by(GMZ,GAME_MANAGEMENT_ZONE_NAME) |> 
  dplyr::summarise()

days_per_wb_w_zone = days_per_wb_sum |> 
  dplyr::left_join(
    manage_units_by_zone |> sf::st_drop_geometry()
  )

# Now add the zones to waterbody polygons
fished_lakes_polys_w_zone = fished_lakes_polys |> 
  sf::st_join(manage_units_by_zone)

# Now join the summaries to the fished_lakes_polygons.
fished_lakes_w_sum = fished_lakes_polys_w_zone |> 
  dplyr::rename(Waterbody = GNIS_NAME_1) |> 
  dplyr::left_join(days_per_wb_w_zone) |> 
  dplyr::rename(days_fished = n)

# Write out the lake polygons with the number of days fished - we'll use this
# in the ZQM Prioritization model!
sf::write_sf(fished_lakes_w_sum,
             "W:/CMadsen/shared_data_sets/freshwater_fisheries_society_angler_survey_2022_2023.gpkg")

# Time to rasterize this!
fished_lakes_w_sum_vect = fished_lakes_w_sum |> 
  sf::st_transform(crs = 4326) |> 
  terra::vect()

fished_lakes_rast = terra::rasterize(fished_lakes_w_sum_vect, ref, field = "days_fished", fun = max, na.rm=T)

terra::plot(fished_lakes_rast)

fished_lakes_rast[is.na(fished_lakes_rast)] <- 0

fished_lakes_rast = terra::mask(fished_lakes_rast, ref)

# Write out the raster to the AIS prioritization model folder, to be used in the model!
terra::writeRaster(fished_lakes_rast, 
                   paste0(onedrive_wd,"DFO_angling_survey_days_fished_raster.tif"),
                   overwrite = TRUE)
