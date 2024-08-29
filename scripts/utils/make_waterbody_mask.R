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

ref_more_detailed = terra::disagg(ref, 3)
ref_more_detailed[] <- 1:terra::ncell(ref_more_detailed)
terra::plot(ref_more_detailed)
terra::plot(terra::project(bc, "EPSG:4326"), add = T)

# ref_more_detailed = terra::project(ref_more_detailed, "EPSG:3005")

terra::plot(ref)
terra::plot(ref_more_detailed)
# Download the streams (order 3+) for each of the BC Natural Resource Regions.
# If already downloaded, this section does nothing.
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

    streams_grid = sf::st_make_grid(streams, 
                                    cellsize = c(0.02777778,0.02777778)) |> 
      sf::st_as_sf()
    
    streams_grid = streams_grid |> 
      sf::st_join(streams |> dplyr::mutate(hello = "Hi!") |> dplyr::select(hello))
    
    ggplot() +
      geom_sf(data = the_ws) +
      geom_sf(data = streams, aes(col = as.character(STREAM_ORDER))) +
      geom_sf(data = streams_grid, aes(col = hello, fill = hello), alpha = 0.5) + 
      coord_sf(xlim = sf::st_bbox(the_ws)[c(1,3)],
               ylim = sf::st_bbox(the_ws)[c(2,4)])
    
    streams_r = terra::rasterize(streams, 
                                 ref_more_detailed#, 
                                 # fun = 'max', 
                                 # na.rm=T,
                                 # field = "STREAM_ORDER"
                                 )
    
    streams_r = terra::crop(streams_r, 
                            terra::ext(terra::vect(sf::st_transform(the_ws,4326))))
    
    terra::plot(streams_r)
    
    ggplot() +
      geom_sf(data = the_ws) +
      geom_sf(data = streams) +
      tidyterra::geom_spatraster(data = streams_r) +
      coord_sf(xlim = sf::st_bbox(the_ws)[c(1,3)],
               ylim = sf::st_bbox(the_ws)[c(2,4)])
    
    terra::writeRaster(streams_r, 'C:/Users/CMADSEN/Downloads/is_this_raster_empty.tif', overwrite = T)
    
    terra::plot(ref)
    terra::plot(terra::vect(streams), add = T)
    terra::plot(streams_r, add = T)
    #add = T)

    ref_alb = terra::project(ref,"EPSG:3005")
    
    terra::plot(ref_alb)
    terra::plot(streams_r)
    terra::plot(streams_r, color = 'black')
    terra::plot(terra::vect(streams), add = T)
    
    ggplot() + 
      geom_sf(data = the_ws) + 
      geom_sf(data = streams) + 
      tidyterra::geom_spatraster(data = streams_r) + 
      coord_sf(xlim = sf::st_bbox(streams)[c(1,3)],
               ylim = sf::st_bbox(streams)[c(2,4)])
    
    terra::writeRaster(streams_r, paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.tif"))
    
    # streams = readRDS(paste0(onedrive_path, "fwa_streams/",the_ws$snake_name,"_streams_order_3_plus.rds"))
    # 
    # unique_stream_codes = unique(streams$FWA_WATERSHED_CODE)
    # 
    # rivers = bcdc_query_geodata('freshwater-atlas-rivers') |> 
    #   # filter(FWA_WATERSHED_CODE %in% unique_stream_codes) |> 
    #   filter(GNIS_NAME_1 == 'Fraser River') |> 
    #   collect() |> 
    #   dplyr::group_by(FWA_WATERSHED_CODE) |> 
    #   dplyr::summarise() |> 
    #   dplyr::ungroup()
    # 
    # # str_s = streams |> 
    # str_s = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
    #   filter(GNIS_NAME == 'Fraser River') |> 
    #   collect() |> 
    #   # dplyr::filter(FWA_WATERSHED_CODE %in% unique(rivers$FWA_WATERSHED_CODE)) |> 
    #   dplyr::group_by(STREAM_ORDER,FWA_WATERSHED_CODE) |> 
    #   dplyr::summarise() |> 
    #   dplyr::ungroup() |> 
    #   sf::st_intersection(rivers)
    # 
    # rivers = rivers |> 
    #   dplyr::mutate(area = as.numeric(st_area(geometry)))
    # 
    # str_b = sf::st_buffer(str_s, str_s$STREAM_ORDER * 5) |> 
    #   dplyr::mutate(area = as.numeric(st_area(geometry))) |> 
    #   dplyr::group_by(FWA_WATERSHED_CODE) |> 
    #   dplyr::summarise(area = sum(area))
    # 
    # rivers$area / str_b$area
    # 
    # ggplot() + 
    #   geom_sf(data = rivers[1,], col = 'darkblue', fill = 'darkblue') + 
    #   geom_sf(data = str_b[1,], col = 'red', fill = 'red', alpha = 0.5)
    # 
  }
}

# Load in all the streams from OneDrive.
# streams = list.files(path = paste0(onedrive_path,"fwa_streams/"),
#            pattern = ".rds",
#            full.names = T) |>
#   lapply(\(x) readRDS(x) |>
#            dplyr::select(FWA_WATERSHED_CODE,BLUE_LINE_KEY,STREAM_ORDER)) |>
#   dplyr::bind_rows()
rast_paths = list.files(path = paste0(onedrive_path,"fwa_streams/"),
                        pattern = 'plus.tif',
                        full.names = T)

stream_r_m = terra::rast(rast_paths)
stream_r_m = Reduce(f = 'c', x = stream_r_m)

terra::plot(stream_r_m)

rast_1 = terra::rast(rast_paths[1])

terra::ext(rast_1) == terra::ext(rast_2)
stream_rasts_l = list.files(path = paste0(onedrive_path,"fwa_streams/"),
           pattern = '.tif',
           full.names = T) |> 
  lapply(\(x) {
    stream_rast = terra::rast(x)
    if(terra::ext(stream_rast) != terra::ext(rast_1)) stream_rast = NULL
    stream_rast
    })



# Buffer streams by 5 times their stream order, in meters; this approximates
# river polygon areas pretty well, and should be accurate enough for 
# our purposes :)

streams = st_buffer(streams, streams$STREAM_ORDER * 5)

# Increase resolution of template raster by a factor of 10.
ref_more_detailed = terra::disagg(ref, 10)
ref
ref_more_detailed
ref_albers = terra::project(ref_more_detailed, "EPSG:3005")

streams_r = terra::rasterize(streams, #|> 
                               #dplyr::filter(STREAM_ORDER >= 8), #|> 
                               # dplyr::group_by(FWA_WATERSHED_CODE,STREAM_ORDER) |> 
                               # dplyr::summarise() |> 
                               # dplyr::select(STREAM_ORDER), 
                             ref_albers, field = "STREAM_ORDER", fun = "max", na.rm=T)

terra::plot(streams_r)

# Write to OneDrive!
terra::writeRaster(streams_r,paste0(onedrive_path,"fwa_streams/stream_order_three_plus_raster_1km_res.tif"), overwrite = TRUE)
terra::writeRaster(streams_r,paste0(onedrive_path,"CNF/stream_order_three_plus_raster_1km_res.tif"), overwrite = TRUE)
