library(terra)
library(bcdata)
library(tidyverse)
library(raster)
library(sf)
# library(exactextractr)
library(tidyhydat)
library(data.table)

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))

ref = terra::rast(paste0(onedrive_wd, "reference_raster_wgs84.tif"))

# ==== Grab flow for WSC stations in BC ====

# 1. (re)download {tidyhydat} database (if ours is out of date)
tidyhydat::download_hydat()

# 2. Pull BC flow records.
bc_stations = tidyhydat::hy_stations(prov_terr_state_loc = 'BC')

bc_flows = tidyhydat::hy_daily_flows(station_number = bc_stations$STATION_NUMBER)
# The line above takes a whiiiiile ~ 10 minutes.

nrow(bc_flows)

# Find average flow per month per station, as a first step.

bc_flows = as.data.table(bc_flows)

bc_flows[,Month := lubridate::month(Date)]

bc_flows_by_m = bc_flows[!is.na(Value) & Parameter == 'Flow', .(mean_month_flow = mean(Value)), by = list(STATION_NUMBER,Month)]

bc_flows_by_m

station_sf = st_as_sf(bc_stations, coords = c("LONGITUDE","LATITUDE"), crs = 4326)

ggplot() + geom_sf(data = station_sf, aes(col = HYD_STATUS))
