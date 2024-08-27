library(fwa.connect)
library(bcdata)
library(sf)
library(terra)
library(tidyverse)

path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/")

# Make raster grid
# Make BC polygon.
bc = bcmaps::bc_bound() |> 
  dplyr::summarise() |> 
  terra::vect()

#create interpolation grid encompassing Canada and USA
bbox <- sf::st_bbox(st_as_sf(bc))

# Bring in raster for reference (dimensions, extent, etc.)
pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
pred_bioc_clipped = mask(crop(pred_bioc, bc_vect), bc_vect)
ref = pred_bioc_clipped$bio01
terra::plot(ref)
rm(pred_bioc_clipped)

ws = sf::read_sf(paste0(onedrive_path,'CNF/watershed_groups.gpkg'))

ws = ws |> 
  dplyr::mutate(snake_name = snakecase::to_snake_case(WATERSHED_GROUP_NAME))

# Download the streams (order 3+) for each of the BC Natural Resource Regions.
# If already downloaded, this section does nothing.
for(i in 1:nrow(ws)){
  
  print(i)
  
  the_ws = ws[i,]
  
  if(!file.exists(paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.rds"))){
  streams = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
    filter(INTERSECTS(the_ws)) |> 
    filter(STREAM_ORDER >= 3) |> 
    collect() |> 
    sf::st_zm() |> 
    sf::st_filter(the_ws)
  
  print("Downloaded streams...")
  
  saveRDS(streams, paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.rds"))
  }
}

# Load in all the streams from OneDrive.
streams = list.files(path = paste0(onedrive_path,"fwa_streams/"),
           pattern = ".rds",
           full.names = T) |> 
  lapply(\(x) readRDS(x) |> 
           dplyr::select(FWA_WATERSHED_CODE,BLUE_LINE_KEY,STREAM_ORDER)) |> 
  dplyr::bind_rows()

# Try this with a much smaller number of streams!
ref_albers = terra::project(ref, "EPSG:3005")
terra::plot(ref_albers)
streams_r = terra::rasterize(streams, #|> 
                               #dplyr::filter(STREAM_ORDER >= 8), #|> 
                               # dplyr::group_by(FWA_WATERSHED_CODE,STREAM_ORDER) |> 
                               # dplyr::summarise() |> 
                               # dplyr::select(STREAM_ORDER), 
                             ref_albers, field = "STREAM_ORDER", fun = "max", na.rm=T)

terra::plot(streams_r)

# Write to OneDrive!
terra::writeRaster(streams_r,paste0(onedrive_path,"fwa_streams/stream_order_three_plus_raster.tif"), overwrite = TRUE)
terra::writeRaster(streams_r,paste0(onedrive_path,"CNF/stream_order_three_plus_raster.tif"), overwrite = TRUE)
