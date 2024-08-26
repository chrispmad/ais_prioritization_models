# Title: Download Continental-scale Datasets
#
# Date: 2024-08-08
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca) and John Phelan
# 
# Description: This script (re)downloads the following datasets:
# 1. Dissolved calcium and pH (from EMS project folder)
# 2. temperature and precipitation (predicted, from WorldClim)

library(tidyverse)
library(DBI)
library(bcinvadeR)
library(terra)
library(sf)
library(geodata)

#set locations
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")


# Get occurrence data for species.
d = grab_aq_occ_data('goldfish')
# Pull in climate variables
ph_NAM = terra::rast(paste0(onedrive_wd,"ph-KR-208784-median_10km_ZN.tif")) |> terra::project("EPSG:4326")
names(ph_NAM) <- "pH"
Calc_NAM = terra::rast(paste0(onedrive_wd,"calcium-KR-97648-median-10km-ZN.tif")) |> terra::project("EPSG:4326")
names(Calc_NAM) <- "calc"

# Read in variable names for the worldclim variables.
source(paste0(proj_wd,"/scripts/utils/worldclim_bioc_var_names.R"))
cmidata<-cmip6_world("ACCESS-CM2", ssp = "585", var = "bioc", res = 5, time = "2021-2040",path="CMI/")
names(cmidata)<-renames
pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
names(pred_bioc)<-renames

# Cut our rasters down to just BC.
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
NA_ph_c = mask(crop(ph_NAM, bc_vect), bc_vect)
NA_calc_c = mask(crop(Calc_NAM, bc_vect), bc_vect)
pred_bioc_c = mask(crop(pred_bioc, bc_vect), bc_vect)

# Pull in elevation raster with {elevatr}
elev = elevatr::get_elev_raster(locations = sf::st_as_sf(bc_vect), z = 4) |>
  terra::rast() |> 
  terra::crop(bc_vect) |> 
  terra::mask(bc_vect)
names(elev) = "elev"

# Combine our rasters.
NA_ph_res = terra::resample(NA_ph_c, pred_bioc_c)
NA_calc_res = terra::resample(NA_calc_c, pred_bioc_c)
elev_res = terra::resample(elev, pred_bioc_c)
all_rasters = c(pred_bioc_c, NA_ph_res, NA_calc_res, elev_res)
names(all_rasters)[c(1,12)] <- c("temp","precip")
random_sample = spatSample(x = pred_bioc_c,
                           size = 1000,
                           na.rm=T,
                           values = FALSE,
                           xy = TRUE) |> 
  as_tibble() |> 
  dplyr::mutate(present = 0)

# Combine random sample with count data
samp = dplyr::bind_rows(
  random_sample,
  d |> dplyr::mutate(x = st_coordinates(geometry)[,1],
                     y = st_coordinates(geometry)[,2]) |> 
    sf::st_drop_geometry() |> 
    dplyr::select(x,y,Location) |> 
    mutate(present = 1)
)

# Extract raster variable values at species occurrence point locations.
samp$temp = terra::extract(pred_bioc_c[[1]], samp[,c("x","y")], ID = FALSE)
samp$precip = terra::extract(pred_bioc_c$Annual_Precipitation, samp[,c("x","y")], ID = FALSE)
samp$calc = terra::extract(NA_calc_res$calc, samp[,c("x","y")], ID = FALSE)
samp$pH = terra::extract(NA_ph_res, samp[,c("x","y")], ID = FALSE)
samp$elev = terra::extract(elev, samp[,c("x","y")], ID = FALSE)
samp = as.data.frame(samp)

samp <- samp |>
  mutate(across(!Location, \(x) as.numeric(unlist(x))))

# Drop any rows that have one or more NA - those gum up our statistical models!

samp = samp[complete.cases(samp),]
