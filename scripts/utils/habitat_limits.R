library(terra)
library(stringr)

proj_wd <- getwd()
onedrive_wd <- paste0(str_extract(getwd(), "C:/Users/[A-Z]+/"), "OneDrive - Government of BC/data/raster/")

spp <- "asian_clam"
variable <- "temperature"

wint_rast <- rast("./rasters/test/raster_temp_Winter.tif")
# plot(wint_rast)

wint_hab <- wint_rast
# Classify values using a conditional statement
wint_hab[wint_rast < 0.5 | wint_rast >= 36] <- 0
wint_hab[wint_rast >= 0.5 & wint_rast < 36] <- 1

# Convert to factor
#wint_hab <- factor(wint_hab, levels = c(0, 1), labels = c("unsuitable", "suitable"))
plot(wint_hab)

sum_raster <- rast("./rasters/test/raster_temp_Summer.tif")
sum_hab <- sum_raster
sum_hab[sum_raster < 0.5] <- 0
sum_hab[sum_raster >= 0.5 & sum_raster < 36] <- 1
sum_hab[sum_raster >= 36] <- 0

# Convert to factor
#sum_hab <- factor(sum_hab, levels = c(0, 1), labels = c("unsuitable", "suitable"))
plot(sum_hab)

merged_habitat <- mosaic(wint_hab, sum_hab)

merged_habitat <- subst(merged_habitat, 0.5, 0)
plot(merged_habitat)
hab_df<-data.frame(id=0:1, habitat=c("not suitable","suitable"))
levels(merged_habitat)<-hab_df
# Convert to factor
is.factor(merged_habitat)
#merged_habitat <- factor(merged_habitat, levels = c(0, 1), labels = c("unsuitable", "suitable"))
plot(merged_habitat)

writeRaster(merged_habitat, paste0(onedrive_wd,"limits/", spp, "_", variable, "_limits.tif"), overwrite = TRUE)
