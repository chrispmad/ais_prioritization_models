library(tidyverse)
library(sf)
library(gstat)
library(raster)
library(automap)
library(DBI)
library(RSQLite)
library(sf)
library(ggplot2)
library(dplyr)
library(bcmaps)

#prep file structure (if required)
if(!file.exists("rasters")){
  dir.create("rasters")
}

if(!file.exists("rasters/unmasked")){
  dir.create("rasters/unmasked")
}

#coordinate reference systems 
crswgs <- 4326 
crsalbers<-3005
# crs2 <- "ESRI:102008"

#import calcium data, log-transform median value, convert to spatial data and project to North America Albers Equal Area Conic
# calcium.sites <- read.csv("data/interpolation_data_calcium_97648.csv") %>%
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = crs1, agr = "constant")  %>%
#   st_transform(crs = crs2) 

conn<-dbConnect(RSQLite::SQLite(),"../EMS/output/EMS.sqlite")

dbListTables(conn) # list the table(s)

test1<-dbGetQuery(conn, "select * from results where parameter like '%Temperature%'")

#create interpolation grid encompassing Canada and USA
# bbox <- c(
#   "xmin" = -5000000,
#   "ymin" = -1650000,
#   "xmax" = 2990000,
#   "ymax" = 4520000)



grid10km <- expand.grid(
  X = seq(from = bbox["xmin"], to = bbox["xmax"], by = 10000),
  Y = seq(from = bbox["ymin"], to = bbox["ymax"], by = 10000)) %>%
  mutate(Z = 0)  %>% 
  raster::rasterFromXYZ(crs = crs2) 

#check projections 
st_crs(calcium.sites)
st_crs(grid10km)

#fit kriging variogram - with fixed zero nugget
varKRca <- autofitVariogram(MEDIAN ~ 1, 
                            as(calcium.sites, "Spatial"),
                            verbose=TRUE,
                            fix.values = c(0,NA,NA))

#inspect variogram
plot(varKRca)

#interpolation model
KRcamod <- gstat(formula=MEDIAN~1,
                 locations=as(calcium.sites,"Spatial"),
                 model=varKRca$var_model,
                 nmax=100,
                 nmin=15)

#interpolation - using gstat::predict (more complex to parallelise, so is single-thread here for simplicity - but produces variance map)
KRgrid10km <- as(grid10km, "SpatialGrid")
KRca_interpolation <- predict(KRcamod, KRgrid10km, debug.level = -1)

#convert output to rasters and save 
KRca_interpolation_raster <- raster(KRca_interpolation) 
KRca_interpolation_variance_raster <- raster(KRca_interpolation, layer = "var1.var") 

writeRaster(KRca_interpolation_raster, "rasters/unmasked/calcium-KR-97648-median-10km-ZN.tif", overwrite=TRUE)
writeRaster(KRca_interpolation_variance_raster, "rasters/unmasked/calcium-KR-97648-median-10km-ZN.tif", overwrite=TRUE)
