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
# pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
# pred_bioc_clipped = mask(crop(pred_bioc, bc_vect), bc_vect)
# ref = pred_bioc_clipped$bio01
# terra::plot(ref)
ref = terra::rast(paste0(onedrive_wd, "reference_raster_wgs84.tif"))

rm(pred_bioc_clipped)

ws = sf::read_sf(paste0(onedrive_path,'CNF/watershed_groups.gpkg'))

ws = ws |> 
  dplyr::mutate(snake_name = snakecase::to_snake_case(WATERSHED_GROUP_NAME))

ref_more_detailed = terra::disagg(ref, 3)
ref_more_detailed[] <- 1:terra::ncell(ref_more_detailed)
terra::plot(ref_more_detailed)
terra::plot(terra::project(bc, "EPSG:4326"), add = T)

# ref_more_detailed = terra::project(ref_more_detailed, "EPSG:3005")

terra::plot(ref)
terra::plot(ref_more_detailed)
# Download the streams (order 3+) for each of the BC Natural Resource Regions.
# If already downloaded, this section does nothing.

# Stupid data problems: two subwatersheds are called Salmon River.
the_salmons = which(ws$WATERSHED_GROUP_NAME == 'Salmon River')
ws[the_salmons[2],]$snake_name = "salmon_river_vi"

for(i in 1:nrow(ws)){
  
  print(i)
  
  the_ws = ws[i,]
  
  # Have the streams not yet been downloaded for this subwatershed? If not, download them.
  if(!file.exists(paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.rds"))){
    
    streams = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
      filter(INTERSECTS(the_ws)) |> 
      filter(STREAM_ORDER >= 3) |> 
      collect() |> 
      sf::st_zm() |> 
      sf::st_filter(the_ws)
    
    print("Downloaded streams...")
    
    # Buffer the streams by their stream order times five! This is a fairly good
    # approximation of the size of rivers' actual polygons :D
    
    saveRDS(streams, paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.rds"))
    
  }
  
  # Has the raster version of these streams' extents not been made? If not, run that below.
  if(!file.exists(paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.tif"))){
    
    streams = readRDS(paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.rds"))
    
    # buffer streams
    streams = sf::st_buffer(streams, dist = streams$STREAM_ORDER * 5)
    
    # Reproject streams to WGS 84.
    streams = sf::st_transform(streams, 4326)
    
    ggplot() + geom_sf(data = streams, aes(col = as.character(STREAM_ORDER)))
    
    library(exactextractr)
    streams_r <- exactextractr::rasterize_polygons(streams, 
                                                   ref_more_detailed)
    
    all_stream_orders = streams[streams_r[],]$STREAM_ORDER
    
    streams_r$stream_order = all_stream_orders
    
    terra::plot(streams_r$stream_order)
    
    streams_r$bio01 = NULL
    
    terra::writeRaster(streams_r, paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.tif"), overwrite = TRUE)
  }
}

rast_paths = list.files(path = paste0(onedrive_path,"fwa_streams/"),
                        pattern = 'plus.tif',
                        full.names = T)

# One of the our subwatersheds didn't run properly - which one?
# ws_names = snakecase::to_snake_case(ws$WATERSHED_GROUP_NAME)

# finished_raster_file_names = unlist(str_extract_all(rast_paths,"(?<=fwa_streams\\/).*(?=_streams)"))

# ggplot() + geom_sf(data = ws[ws$WATERSHED_GROUP_NAME == 'Salmon River',])

stream_r_m = rast_paths |> 
  lapply(\(x) terra::rast(x))

stream_r_stack = terra::rast(stream_r_m)

# Function to sum non-NA values across the raster stack
combine_non_na <- function(...) {
  rasters <- list(...)
  # Use sum with na.rm = TRUE to sum non-NA values
  return(max(unlist(rasters), na.rm = TRUE))
}

# Apply the function across all layers in the stack
combined_raster <- app(stream_r_stack, fun = combine_non_na)

the_min_value = min(terra::values(combined_raster))

combined_raster[combined_raster == the_min_value] <- NA

terra::plot(combined_raster)

terra::writeRaster(combined_raster, paste0(onedrive_path, "fwa_streams/stream_order_three_plus_2km_res.tif"), overwrite = TRUE)
