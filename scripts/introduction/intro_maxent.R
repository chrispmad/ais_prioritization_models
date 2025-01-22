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
library(ecospat)
library(ENMeval)
library(bcmaps)
library(data.table)
library(here)
library(viridis)
library(spatialEco)
library(nortest)
source(here("scripts/utils/prep_predictor_data_f.R"))
source(here("scripts/utils/zuurFuncs.R"))


#set locations
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
onedrive_path = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"

bc<-bc_bound()
extentvect<- project(vect(bc),"EPSG:4326")
extentvect<-project(extentvect, "EPSG:4326")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
bc_wgs<-sf::st_transform(bcmaps::bc_bound(),4326)

predictor_data = prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = paste0(onedrive_wd),
                                     extentvect)

predictor_data <- predictor_data[[c("population_density",
                                    "TotalInspections", "days_fished","dist_to_paved_roads")]]

sppDF<- sf::st_read(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/AIS_occurrences_all_sources.gpkg"))

# Split occurrences by group
source("scripts/utils/gather_AIS_data.R")
pr_sp = gather_ais_data(lan_root = lan_root, onedrive_wd = onedrive_wd, data = "species list",
                excel_path = paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Master Incidence Report Records.xlsx"))

spp_df_l = sppDF |> 
  dplyr::left_join(
    pr_sp |> dplyr::select(Species = name, group)
  ) |> 
  dplyr::group_by(group) |> 
  dplyr::group_split()

# Do we have a group for which no corresponding AIS group was found? Drop it, if so.
if(is.na(unique(spp_df_l[[length(spp_df_l)]]$group))){
  spp_df_l[[length(spp_df_l)]] = NULL
}

# not_jittered_sppDF = sppDF |>
  # dplyr::filter(!duplicated(paste0(geom)))

spp_df_l |> 
  purrr::iwalk( ~ {
    
    the_group = unique(.x$group)
    
    print(paste0("Working on group: ",the_group))
    
    dat = .x |> 
      dplyr::mutate(x = sf::st_coordinates(geom)[,1],
                    y = sf::st_coordinates(geom)[,2])
    
    for(raster_var in unique(names(predictor_data))){
      dat[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                          dat[,c("x","y")], ID = FALSE)[[raster_var]]
    }
    
    
    ##############
    watercourses = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif")) 
    watercourses<-terra::crop(watercourses, extentvect)
    watercourses<-terra::mask(watercourses, extentvect)
    
    # Generate the same number of absences as seen in what we are predicting - see Massin et al. (2012)
    pseudoabsences <- predicts::backgroundSample(watercourses, p = terra::vect(dat), n = 10000, extf = 0.9) |> 
      as.data.frame()
    
    pres_xy<- dat |> 
      dplyr::select(x,y) |> 
      sf::st_drop_geometry()
    
    pseudo<- pseudoabsences |> 
      dplyr::select(x,y)
    
    me = ENMevaluate(occs = pres_xy,
                     envs = predictor_data,
                     bg = pseudo,
                     algorithm = 'maxent.jar',
                     partitions = 'block',
                     tune.args = list(fc = c("L"),
                                      rm = c(1:5)))
    
    top_model = me@results |> 
      dplyr::mutate(auccbi = (cbi.train + auc.train) / 2) |> 
      dplyr::arrange(dplyr::desc(auccbi)) |> 
      dplyr::slice(1)
    
    top_model = me@predictions[[top_model$tune.args]]
    
    # terra::plot(top_model)
    
    # Write out the predicted raster to be used in the excel doc as
    # the 'risk of introduction' variable.
    terra::writeRaster(top_model, paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk/introduction_risk_",snakecase::to_snake_case(the_group),".tif"))
    
  })


