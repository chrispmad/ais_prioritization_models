library(terra)
library(ggplot2)

path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/CNF/")
new_path <- gsub("CNF/", "", onedrive_path)

rast1<-terra::rast(paste0(new_path,"raster/Chlorophyll_All_krig.tif"))
plot(rast1)
terra::plot(paste0(new_path,"raster/Chlorophyll_All_krig.tif"))

rast2<-terra::rast(paste0(new_path,"raster/Specific_Conductivity-Field_All_masked_krig.tif"))
plot(rast2)


