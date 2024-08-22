
library(tidyverse)
library(ENMeval)
library(raster)
library(dplyr)
library(DBI)
library(bcinvadeR)
library(terra)
library(sf)
library(geodata)
library(predicts)
library(ggpubr)
library(ENMeval)
source("ZuurFuncs.R")


proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/CNF/data/")

sppOI<-"goldfish"
# Get occurrence data for species.
d = suppressMessages(grab_aq_occ_data(sppOI))
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
ph_clipped = mask(crop(ph_NAM, bc_vect), bc_vect)
calc_clipped = mask(crop(Calc_NAM, bc_vect), bc_vect)
pred_bioc_clipped = mask(crop(pred_bioc, bc_vect), bc_vect)

# Pull in elevation raster with {elevatr}
elev = suppressMessages(elevatr::get_elev_raster(locations = sf::st_as_sf(bc_vect), z = 4)) |>
  terra::rast() |> 
  terra::crop(bc_vect) |> 
  terra::mask(bc_vect)
names(elev) = "elev"

NA_ph_res = terra::resample(ph_clipped, pred_bioc_clipped)
NA_calc_res = terra::resample(calc_clipped, pred_bioc_clipped)

elev_res = terra::resample(elev, pred_bioc_clipped)

all_rasters = c(pred_bioc_clipped, NA_ph_res, NA_calc_res, elev_res)

names(all_rasters)[c(1,12)] <- c("temp","precip")
temps<-pred_bioc_clipped[[1]]
precip<-pred_bioc_clipped[[2]]

stack_data<-c(NA_ph_res, NA_calc_res, elev_res, temps, precip)
names(stack_data)<-c("pH", "Calc", "elev", "temp", "precip")

plot(stack_data[[4]], main = "Temperature Raster")
points(d)
baseRast<-raster(stack_data[[4]])
baseRast<-setValues(-1)
bg<-randomPoints(baseRast, n = 5000) %>% 
  as.data.frame()
plot(baseRast)
points(bg, pch = 3, col = "black")

stack_data<-raster(stack_data)
block<-get.block(d, bg, orientation = "lat_lon")
table(block$occs.grp)

samp<- d %>% 
  plyr::mutate(x = st_coordinates(geometry)[,1],
               y = st_coordinates(geometry)[,2]) %>% 
  sf::st_drop_geometry() |> 
  dplyr::select(x,y) %>% 
  as.data.frame()

pa_all = dplyr::bind_rows(
  samp |> dplyr::mutate(type = 1),
  bg |> dplyr::mutate(type = 0)
)




modeltest<-ENMevaluate(occs = samp, envs = stack_data, bg = bg, algorithm = "maxent", partitions = "block")
