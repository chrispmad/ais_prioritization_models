# ==========
# DFO-styled Ordinary Kriging Method for interpolation
library(tidyverse)
library(sf)
library(gstat)
library(raster)
library(automap)
library(terra)

path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/CNF/")

# Make raster grid
# Make BC polygon.
bc = bcmaps::bc_bound() |> 
  dplyr::summarise() |> 
  terra::vect()

#create interpolation grid encompassing Canada and USA
bbox <- sf::st_bbox(st_as_sf(bc))

grid10km <- expand.grid(
  X = seq(from = bbox["xmin"], to = bbox["xmax"], by = 10000),
  Y = seq(from = bbox["ymin"], to = bbox["ymax"], by = 10000)) %>%
  mutate(Z = 0)  %>% 
  raster::rasterFromXYZ(crs = 3005) 

terra::plot(grid10km)

# If we haven't done this before, cycle through subwatersheds and download streams
# of stream order 3+ and then rasterize it. Save rasters to a folder on the onedrive data drive.
ws = sf::read_sf(paste0(onedrive_path,'watershed_groups.gpkg'))

ws_rasters = list()

for(i in 1:nrow(ws)){
  
  print(i)
  
  the_ws = ws[i,]
  
  streams = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
    filter(INTERSECTS(the_ws)) |> 
    filter(STREAM_ORDER >= 6) |> 
    collect() |> 
    sf::st_zm()
  
  print("Downloaded streams...")
  
  streams_grid_r = rasterize(vect(streams), rast(grid10km))
  
  ws_rasters[[i]] <- streams_grid_r
}

rsrc <- sprc(ws_rasters)

m <- mosaic(rsrc)

terra::plot(m)

# Filter data.
dat = tempSF |> 
  dplyr::filter(!is.na(RESULT)) |>
  dplyr::filter(LOCATION_TYPE == "RIVER,STREAM OR CREEK") |> 
  dplyr::group_by(MONITORING_LOCATION) |> 
  dplyr::mutate(number_datapoints = n()) |> 
  dplyr::ungroup() |> 
  dplyr::filter(number_datapoints >= 3) |> 
  dplyr::mutate(COLLECTION_DATE = lubridate::ymd(COLLECTION_DATE)) |> 
  dplyr::filter(COLLECTION_DATE >= '2023-01-01', COLLECTION_DATE < '2024-01-01') |> 
  sf::st_transform(3005)

dat = dat |> 
  dplyr::group_by(MONITORING_LOCATION) |> 
  dplyr::summarise(RESULT = median(RESULT,na.rm=T))

# Try to merge (median) data values at 5km intervals or something.
bc_grid = st_make_grid(bc, cellsize = c(5000,5000)) |> 
  sf::st_as_sf() |> dplyr::mutate(cell_number = row_number())

# ggplot() + geom_sf(data = st_as_sf(bc)) + geom_sf(data = bc_grid)
# Try taking average of temperature at monitoring locations.
dat_by_grid = dat |> 
  st_join(bc_grid)

dat_for_grid_cells = dat_by_grid |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(cell_number) |> 
  dplyr::summarise(RESULT = median(RESULT, na.rm=T))

bc_grid = bc_grid |> 
  dplyr::left_join(dat_for_grid_cells)

gridified_data = bc_grid |> 
  dplyr::filter(!is.na(RESULT)) |> 
  sf::st_centroid()

ggplot() + geom_sf(data = st_as_sf(bc)) + 
  geom_sf(data = gridified_data)


# Read in a reference raster to interpolate.
# ref_rast = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")

# Crop reference raster to BC.  
# ref_rast_bc = terra::mask(terra::crop(ref_rast, bc_vect), bc_vect)

# terra::plot(ref_rast_bc$bio01)

# Project everything to BC Albers.
# bc_vect = terra::project(bc_vect, crs("EPSG:3005"))
# ref_rast_bc = terra::project(ref_rast_bc, crs("EPSG:3005"))

st_crs(dat)
# st_crs(river2024_data_rich)
st_crs(grid10km)

dat_v = vect(dat) |> terra::project(crs("ESRI:102008"))
grid10km_v = rast(grid10km) |> terra::project(crs("ESRI:102008"))

#fit kriging variogram - with fixed zero nugget
varKRca <- autofitVariogram(formula = RESULT ~ 1, 
                            input_data = as(gridified_data, "Spatial"),
                            # as(dat, "Spatial"),
                            verbose=TRUE,
                            fix.values = c(0,NA,NA))

# # JUST IN CASE sp is an issue... try a difference variance function?
# v = variogram(RESULT~1, dat_v |> sf::st_as_sf())
# plot(v, plot.numbers = TRUE)
# 
# v.m = fit.variogram(v, vgm(c("Sph","Gau","Exp","Ste"), psill = NA, range = NA, nugget = NA))
# plot(v, v.m, plot.numbers = FALSE)

#inspect variogram
plot(varKRca)

# ## d) Variogram fit
# v = variogram(RESULT~1, dat |> dplyr::filter(RESULT > 0) |> mutate(RESULT = log(RESULT)))
# plot(v, plot.numbers = TRUE)
# 
# v.m = fit.variogram(v, vgm("Gau", psill = 22, range = 5, nugget = 1))
# plot(v, v.m, plot.numbers = FALSE)

#interpolation model
KRcamod <- gstat(formula=RESULT~1,
                 locations=as(dat,"Spatial"),
                 model=varKRca$var_model)

#interpolation - using gstat::predict (more complex to parallelise, so is single-thread here for simplicity - but produces variance map)
KR_grid10km <- as(raster::raster(grid10km), "SpatialGrid")
KRca_interpolation <- predict(KRcamod, KR_grid10km, debug.level = -1)

#convert output to rasters and save 
KRca_interpolation_raster <- raster(KRca_interpolation) 
KRca_interpolation_variance_raster <- raster(KRca_interpolation, layer = "var1.var") 

# Trim to BC (yet again)
KRca_interpolation_r = rast(KRca_interpolation_raster) |> crop(bc) |> mask(bc)

# See how many NAs have been 'predicted'. If it's over some threshold, reduce nmax until it's below the threshold.

terra::plot(KRca_interpolation_r)
terra::plot(bc, add = T)
terra::plot(vect(dat), add = T)

# Bring in stream network.
library(bcdata)
bcmaps::
streams = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
  filter(STREAM_ORDER >= 3) |> 
  collect() |> 
  sf::st_zm() |> 
  dplyr::mutate(var = 1) |> 
  dplyr::select(var)

# Rasterize the streams.
streams_r = rasterize(vect(streams), rast(grid10km))
  
KRca_interpolation_r_streams = terra::mask(KRca_interpolation_r, streams_r)

terra::plot(KRca_interpolation_r_streams)
terra::plot(bc, add = T)
terra::plot(vect(dat), add = T)


writeRaster(KRca_interpolation_raster, "rasters/unmasked/calcium-KR-97648-median-10km-ZN.tif", overwrite=TRUE)
writeRaster(KRca_interpolation_variance_raster, "rasters/unmasked/calcium-KR-97648-median-10km-ZN.tif", overwrite=TRUE)