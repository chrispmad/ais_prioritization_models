library(tidyverse)
library(ENMeval)
library(raster)
library(dplyr)
library(DBI)
library(bcinvadeR)
library(terra)
library(sf)
library(geodata)
library(predicts)
library(ggpubr)
library(ENMeval)
library(spThin)
source("ZuurFuncs.R")
#https://cran.r-project.org/web/packages/spThin/vignettes/spThin_vignette.html

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

sppOI<-"goldfish"
# Get occurrence data for species (if we haven't done this yet!)
if(!exists("d")){
  d = suppressMessages(grab_aq_occ_data(sppOI))
}
head(d)

spp_df<- d %>% 
  plyr::mutate(x = st_coordinates(geometry)[,1],
               y = st_coordinates(geometry)[,2]) %>% 
  sf::st_drop_geometry() %>% 
  #dplyr::select(x,y,Date,Species, Location, Data) %>% 
  mutate(present = 1)

#table(spp_df)
#thin par is 10km. This matches the resolution of rasters I believe!
thinned_data_full<-spThin::thin(loc.data = spp_df,
                                lat.col = "y", long.col = "x",
                                spec.col = "Species",
                                thin.par = 10,
                                reps = 100,
                                locs.thinned.list.return = T,
                                write.files = F,
                                write.log.file = F)

thinned_data_full[1]
plotThin(thinned_data_full)
summaryThin(thinned_data_full)

ggplot()+
  geom_sf(data = d, col = "red")+
  geom_point(data = thinned_data_full[[1]], aes(x = Longitude, y = Latitude), col = "purple", shape = 2)

library(patchwork)

plot1 <- ggplot() +
  geom_sf(data = d, color = "red")
plot2 <- ggplot(thinned_data_full[[6]]) +
  geom_point(aes(x = Longitude, y = Latitude), color = "purple", shape = 2)

plot1 / plot2
