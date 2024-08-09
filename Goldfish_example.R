# Title: Download Continental-scale Datasets
#
# Date: 2024-08-08
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca) and John Phelan
# 
# Description: This script (re)downloads the following datasets:
# 1. Dissolved calcium and pH (from EMS project folder)
# 2. temperature and precipitation (predicted, from WorldClim)

library(tidyverse)
library(DBI)
library(bcinvadeR)
library(terra)
library(sf)
library(geodata)

# Get occurrence data for species.

d = grab_aq_occ_data('goldfish')

# Establish connection with the EMS database to be able to query stuff.
# con = DBI::dbConnect(RSQLite::SQLite(), "../EMS/output/EMS.sqlite")

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/CNF/data/")

setwd(onedrive_wd)

# Pull in climate variables
NA_ph = terra::rast("ph-KR-208784-median_10km_ZN.tif") |> terra::project("EPSG:4326")
names(NA_ph) <- "pH"
# NA_ph_var = terra::rast("ph-KR-208784-median_10km_ZN_variance.tif") |> terra::project("EPSG:4326")

NA_calc = terra::rast("calcium-KR-97648-median-10km-ZN.tif") |> terra::project("EPSG:4326")
names(NA_calc) <- "calc"
# NA_calc_var = terra::rast("calcium-KR-97648-median-10km-ZN_variance.tif") |> terra::project("EPSG:4326")

# Worldclim Bioclimatic variables. 
# Download 2021-2040 future climate predictions.
#https://www.worldclim.org/data/bioclim.html

# Read in variable names for the worldclim variables.
source(paste0(proj_wd,"/scripts/utils/worldclim_bioc_var_names.R"))

#https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/
#https://search.r-project.org/CRAN/refmans/geodata/html/cmip6.html
# explalation of ssp codes https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/
cmidata<-cmip6_world("ACCESS-CM2", ssp = "585", var = "bioc", res = 5, time = "2021-2040",path="CMI/")
names(cmidata)<-renames

pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
names(pred_bioc)<-renames

# Cut our rasters down to just BC.
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
NA_ph_c = mask(crop(NA_ph, bc_vect), bc_vect)
NA_calc_c = mask(crop(NA_calc, bc_vect), bc_vect)
pred_bioc_c = mask(crop(pred_bioc, bc_vect), bc_vect)

# Pull in elevation raster with {elevatr}
elev = elevatr::get_elev_raster(locations = sf::st_as_sf(bc_vect), z = 4) |>
  terra::rast() |> 
  terra::crop(bc_vect) |> 
  terra::mask(bc_vect)

names(elev) = "elev"

# Take a glance at our data?

ggpubr::ggarrange(
  ggplot() + 
    tidyterra::geom_spatraster(data = NA_calc_c) +
    geom_sf(data = d) + 
    labs(title = "Calcium"),
  ggplot() + 
    tidyterra::geom_spatraster(data = NA_ph_c) +
    geom_sf(data = d) +
    labs(title = "pH"),
  ncol = 2
)

# Combine our rasters.

NA_ph_res = terra::resample(NA_ph_c, pred_bioc_c)
NA_calc_res = terra::resample(NA_calc_c, pred_bioc_c)

elev_res = terra::resample(elev, pred_bioc_c)

all_rasters = c(pred_bioc_c, NA_ph_res, NA_calc_res, elev_res)

names(all_rasters)[c(1,12)] <- c("temp","precip")

random_sample = spatSample(x = pred_bioc_c,
                           size = 1000,
                           na.rm=T,
                           values = FALSE,
                           xy = TRUE) |> 
  as_tibble() |> 
  dplyr::mutate(present = 0)

# Combine random sample with count data
samp = dplyr::bind_rows(
  random_sample,
  d |> dplyr::mutate(x = st_coordinates(geometry)[,1],
                     y = st_coordinates(geometry)[,2]) |> 
    sf::st_drop_geometry() |> 
    dplyr::select(x,y) |> 
    mutate(present = 1)
)


# Extract raster variable values at species occurrence point locations.
samp$temp = terra::extract(pred_bioc_c[[1]], samp[,c("x","y")], ID = FALSE)
samp$precip = terra::extract(pred_bioc_c$Annual_Precipitation, samp[,c("x","y")], ID = FALSE)
samp$calc = terra::extract(NA_calc_res$calc, samp[,c("x","y")], ID = FALSE)
samp$pH = terra::extract(NA_ph_res, samp[,c("x","y")], ID = FALSE)
samp$elev = terra::extract(elev, samp[,c("x","y")], ID = FALSE)
samp = as.data.frame(samp)

samp = samp |> 
  mutate(across(everything(), \(x) as.numeric(unlist(x))))

# Drop any rows that have one or more NA - those gum up our statistical models!

samp = samp[complete.cases(samp),]

# names(samp) = str_remove_all(names(samp), "^[a-zA-Z]+\\$")

library(predicts)

fold <- folds(x = samp,
              k = 5,
              by = samp$present)

table(fold)

testing = samp[fold == 1,]
training = samp[fold != 1,]

glm_model = glm(present ~ temp + precip + calc + pH + , data = training, family = binomial())

step(glm_model, direction = 'backward')

summary(glm_model)

plot(glm_model$residuals)
plot(glm_model$fitted.values)

glm_predict = predict(all_rasters, glm_model, type = "response")

# Evaluate how successfully our model can use the training data to predict the 
# testing data.
glm_eval = pa_evaluate(
  p = testing[testing$present == 1, ],
  a = testing[testing$present == 0, ],
  model = glm_model,
  type = "response")

terra::plot(glm_predict)

# Check out the minimum threshold for presence:
glm_threshold = glm_eval@thresholds$max_spec_sens

predicted_presence = glm_predict
predicted_presence[predicted_presence >= glm_threshold] <- 1
predicted_presence[predicted_presence < glm_threshold] <- 0

values(predicted_presence) <- as.character(values(predicted_presence))

# predicted_presence[predicted_presence$lyr1 == "0",] <- "absent"
# predicted_presence[predicted_presence$lyr1 == "1"] <- "present"

ggplot() + 
  geom_sf(data = sf::st_as_sf(bc_vect)) + 
  tidyterra::geom_spatraster(data = predicted_presence) + 
  geom_sf(data = d) + 
  scale_fill_manual(values = c("1" = "gold", "0" = "grey"), na.value = 'transparent') +
  labs(fill = 'Predicted Presence of Species') +
  ggthemes::theme_map()
