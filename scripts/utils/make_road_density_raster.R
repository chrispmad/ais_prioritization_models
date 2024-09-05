library(terra)
library(bcdata)
library(tidyverse)

all_l = bcdc_list()
all_l[str_detect(all_l,"road-atlas")]

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

# Bring in raster for reference (dimensions, extent, etc.)
# pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
# pred_bioc_clipped = mask(crop(pred_bioc, bc_vect), bc_vect)

# terra::plot(pred_bioc_clipped$bio01)

# ref = pred_bioc_clipped$bio01
ref = terra::rast(paste0(onedrive_wd, "reference_raster_wgs84_500_by_1000m_res.tif"))

# Bring in roads over a certain size.
big_roads = bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') |> 
  # filter(NUMBER_OF_LANES > 4) |> 
  filter(!is.na(HIGHWAY_ROUTE_NUMBER)) |> 
  collect()

big_roads = sf::st_transform(big_roads, 4326)

# count number of roads in each raster cell.
road_dens_r = terra::rasterize(terra::vect(big_roads), ref)

terra::plot(road_dens_r)

# Next, let's measure distance for every cell to the nearest cell with a road present.
road_dens_r[road_dens_r == 0] <- NA # Convert 0 to NA

road_distance_r <- distance(road_dens_r)

# Clip back to BC.
road_distance_r = terra::mask(road_distance_r, bc_vect)

terra::plot(road_distance_r)
terra::plot(road_dens_r, add = T)

terra::writeRaster(x = road_distance_r, filename = paste0(onedrive_wd,"distance_to_road_raster.tif"))
