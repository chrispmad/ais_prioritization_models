# DFO-styled Ordinary Kriging Method for interpolation
library(tidyverse)
library(sf)
library(gstat)
library(raster)
library(automap)
library(terra)

path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/CNF/")

stream_order_threshold = 6

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
terra::plot(vect(ws), add = T)

terra::writeRaster(m, paste0(onedrive_path,"stream_density_",stream_order_threshold,"_plus_order_raster.tif"))