library(terra)
library(bcdata)
library(tidyverse)
library(raster)
library(sf)
library(exactextractr)

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))

ref = terra::rast(paste0(onedrive_wd, "reference_raster_wgs84.tif"))

# Read in the destination waterbody shapefile with the number of boats.
# wbs = sf::read_sf(paste0(onedrive_wd,'Waterbodies_with_binned_and_original_values.gpkg'))
wbs = sf::read_sf(paste0(onedrive_wd,'Waterbodies_with_Inspection_Data_Summaries_all_years.gpkg'))

wbs = wbs |> dplyr::filter(!GNIS_NA %in% c("Pacific Ocean","Dry Storage")) |> sf::st_transform(4326) |> terra::vect()

wbs_r = terra::rasterize(wbs, ref, field = "TotalInspections", fun = max)

terra::plot(wbs_r)

# Set area inside BC to be 0; might need to recrop this.
wbs_r[is.na(wbs_r)] <- 0

wbs_r = terra::mask(wbs_r, ref)

terra::writeRaster(x = wbs_r, filename = paste0(onedrive_wd,"watercraft_visits_all_years.tif"), overwrite = TRUE)

