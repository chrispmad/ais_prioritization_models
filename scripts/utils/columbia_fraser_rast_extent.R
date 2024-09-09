library(bcmaps)
library(dplyr)
library(stringr)
library(ggplot2)
library(terra)
bcmaps::available_layers()

rd<-regional_districts()
rd$ADMIN_AREA_NAME

frasColum<- rd %>% 
  filter(str_detect(ADMIN_AREA_NAME, "\\Fraser") | 
           str_detect(ADMIN_AREA_NAME, "\\Columbia"))
nrR<-bcmaps::nr_regions()
frasColum<- nrR %>% 
  filter(str_detect(ORG_UNIT_NAME, "\\Fraser") | 
           str_detect(ORG_UNIT_NAME, "\\Columbia"))

bc<-bc_bound()
southcoast = suppressMessages(bcmaps::nr_regions())
frasColum<- southcoast %>% 
  filter(REGION_NAME == "South Coast Natural Resource Region" |
           REGION_NAME == "Thompson-Okanagan Natural Resource Region" |
           REGION_NAME == "Kootenay-Boundary Natural Resource Region" |
           REGION_NAME == "Cariboo Natural Resource Region" 
  )


# ggplot()+
#   geom_sf(data = bc)+
#   geom_sf(data = frasColum)

frasColumVect<-terra::vect(frasColum)
values(frasColumRast)<-1
plot(frasColumRast)  
ext(frasColumRast)

terra::writeVector(frasColumVect, "./vect/fras_colum.gpkg", overwrite = T)
