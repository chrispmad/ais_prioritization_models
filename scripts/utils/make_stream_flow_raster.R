library(terra)
library(bcdata)
library(tidyverse)

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

# Bring in raster for reference (dimensions, extent, etc.)
# pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
# pred_bioc_clipped = mask(crop(pred_bioc, bc_vect), bc_vect)

# terra::plot(pred_bioc_clipped$bio01)

# ref = pred_bioc_clipped$bio01
ref = terra::rast(paste0(onedrive_wd, "reference_raster_wgs84.tif"))

# Read in Ron's data (tied to water survey Canada stations).

ron_d = sf::read_sf(paste0(onedrive_wd,'Ptolemy_WSC_Drought_Prioritization_2024-09-04.gpkg'))

ggplot() + geom_sf(data = bcmaps::bc_bound()) + geom_sf(data = ron_d)

# Set up variables for kriging
ext_ref <- ext(ref)
res_ref <- res(ref)
x_seq <- seq(from = ext_ref[1], to = ext_ref[2], by = res_ref[1])
y_seq <- seq(from = ext_ref[3], to = ext_ref[4], by = res_ref[2])
grid10km <- expand.grid(x = x_seq, y = y_seq)
grid10km <- rast(grid10km, crs = crs(ref))
grid10km$nlyr<-1

st_crs(ron_d)
st_crs(grid10km)

varKRVar <- autofitVariogram(mad_l_s ~ 1, 
                             as(ron_d, "Spatial"),
                             verbose=TRUE,
                             fix.values = c(0,NA,NA))

KRvarmod <- gstat(formula=mad_l_s~1,
                  locations=as(ron_d,"Spatial"),
                  model=varKRVar$var_model
)

KRgrid10km <- as(raster::raster(grid10km), "SpatialGrid")
KRVar_interpolation <- predict(KRvarmod, KRgrid10km, debug.level = -1)


interp_r = terra::rast(KRVar_interpolation)
interp_r = terra::resample(interp_r, ref)
interp_r = terra::mask(interp_r, ref)
terra::plot(interp_r$var1.pred)

terra::writeRaster(interp_r$var1.pred, paste0(onedrive_wd,"../raster/stream_flow_All_krig.tif"), overwrite = T)

