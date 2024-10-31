# DFO-styled Ordinary Kriging Method for interpolation
library(tidyverse)
library(sf)
library(gstat)
library(raster)
library(automap)
library(terra)
library(stars)

path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/CNF/")


stream_order_threshold = 6

bc = bcmaps::bc_bound() |> 
  dplyr::summarise() |> 
  terra::vect()
bbox <- sf::st_bbox(st_as_sf(bc))

ws = sf::read_sf(paste0(onedrive_path,'watershed_groups.gpkg'))

ws_rasters = list()

for(i in 1:nrow(ws)){
  
  print(i)
  
  the_ws = ws[i,]
  
  streams = bcdc_query_geodata('freshwater-atlas-stream-network') |> 
    filter(INTERSECTS(the_ws)) |> 
    filter(STREAM_ORDER >= 4) |> 
    collect() |> 
    sf::st_zm() 
  
  print("Downloaded streams...")

  
  ws_rasters[[i]] <- streams
}

singleSF<-rbindlist(ws_rasters)
singleSF<-st_as_sf(singleSF)


max_mags<- singleSF |> 
  group_by(GNIS_ID) |> 
  summarise(max_mag = max(STREAM_MAGNITUDE))
max(max_mags$max_mag)

ggplot()+
  geom_sf(data = singleSF, color = "blue")


st_write(obj=singleSF, dsn = paste0(onedrive_path,"stream_order_four.gpkg"))


v<-vect(singleSF)
#set the spatial resolution. This is 1000m, as the data is in BC Albers
r<-rast(v, resolution = 10000)
x<-rasterize(v,r, "STREAM_MAGNITUDE", fun = "max")
plot(x)
max(x$STREAM_MAGNITUDE)
writeRaster(x, filename = paste0(onedrive_path,"streamMagnitude_order_four_rast.tif"), overwrite = TRUE)
