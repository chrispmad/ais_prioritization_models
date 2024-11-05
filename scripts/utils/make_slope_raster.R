library(terra)
library(bcdata)
library(tidyverse)

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

# Bring in raster for reference (dimensions, extent, etc.)
# pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))

ref = terra::rast(paste0(onedrive_wd, "reference_raster_wgs84.tif"))

# Read in elevation
elev = elevatr::get_elev_raster(locations = sf::st_as_sf(bc_vect), z = 9) |> 
  terra::rast()

elev_with_ocean = elev

# Remove ocean stuff.
elev[elev <= 0] = 0

terra::plot(elev)

# Calculate slope
the_slope = terra::terrain(elev)

the_slope[the_slope >= 70] <- 70

terra::plot(the_slope)

slope_resampled = terra::resample(the_slope, ref)

terra::plot(slope_resampled)

slope_resampled = slope_resampled |>
  terra::crop(bc_vect) |> 
  terra::mask(bc_vect)

terra::plot(slope_resampled)
# min_elev_in_bc = min(elev_resampled[], na.rm=T)

terra::writeRaster(x = slope_resampled, filename = paste0(onedrive_wd,"slope_BC.tif"))
