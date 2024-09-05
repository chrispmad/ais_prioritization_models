library(ggplot2)
library(terra)
library(stringr)

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

# Make a reference raster (a terra rast)

bc = bcmaps::bc_bound() |> 
  dplyr::summarise()

ggplot() + geom_sf(data = bc)

bc = vect(bc)

terra::plot(bc)

bc_r = rast(bc, nrows = 420, ncols = 900)

bc_r = terra::rasterize(bc, bc_r)

bc_r = terra::project(bc_r, "EPSG:4326")

terra::plot(bc_r)

terra::writeRaster(bc_r, paste0(onedrive_wd,"reference_raster_wgs84.tif"), overwrite = T)
