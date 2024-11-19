library(tidyverse)
library(ggplot2)
library(DBI)
library(bcinvadeR)
library(terra)
library(sf)
library(geodata)
library(predicts)
library(ggpubr)
library(dismo)
library(rJava)
#source("ZuurFuncs.R")
library(ecospat)
library(ENMeval)
library(bcmaps)
library(data.table)
library(here)
source(here("scripts/utils/prep_predictor_data_f.R"))

sppOI<-"asian clam"

#set locations
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
onedrive_path = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

bc<-bc_bound()
extentvect<- project(vect(bc),"EPSG:4326")
#extentvect<-terra::vect("./vect/fras_colum.gpkg")
extentvect<-project(extentvect, "EPSG:4326")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
bc_wgs<-sf::st_transform(bcmaps::bc_bound(),4326)


predictor_data = prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = paste0(onedrive_wd),
                                     extentvect)


predictor_data <- predictor_data[[c("population_density",
                                    "TotalInspections", "days_fished","dist_to_highways")]]

sppDF = bcinvadeR::grab_aq_occ_data(sppOI)

sppDF_jitter = sppDF |>
  dplyr::filter(duplicated(paste0(geometry))) |>
  sf::st_jitter(factor = 0.0001)

not_jittered_sppDF = sppDF |>
  dplyr::filter(!duplicated(paste0(geometry)))

sppDF_j = dplyr::bind_rows(sppDF_jitter,
                           not_jittered_sppDF)

dat = sppDF |> 
  dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                y = sf::st_coordinates(geometry)[,2])


for(raster_var in unique(names(predictor_data))){
  dat[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                      dat[,c("x","y")], ID = FALSE)[[raster_var]]
}


modelData <- dat |> 
  select(geometry, x, y, population_density, TotalInspections, days_fished, dist_to_highways)

modelData$presence<-1


###########
##########

# Create pseudo-absences for use in GLM

###############
##############



library(lme4)
modelData$location <- paste(modelData$x, modelData$y, sep = "_")

# Fit the logistic mixed model
risk_model <- glmer(
  presence ~ population_density + dist_to_highways + TotalInspections + days_fished +
    (1 | location),  # Random effect for spatial grouping by location
  data = modelData,
  family = binomial(link = "logit")
)

# r_stack<-raster::stack(predictor_data)
# 
# datDT<-setDT(dat)
# 
# samp<- dat %>% 
#   plyr::mutate(x = st_coordinates(geometry)[,1],
#                y = st_coordinates(geometry)[,2]) %>% 
#   sf::st_drop_geometry() |> 
#   dplyr::select(x,y) |> 
#   mutate(present = 1)
