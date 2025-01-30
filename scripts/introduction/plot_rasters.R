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
library(patchwork)
source(here("scripts/utils/prep_predictor_data_f.R"))
source(here("scripts/utils/zuurFuncs.R"))


#set locations
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
onedrive_path = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"
folder_path<-"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk/"


bc<-bc_bound()
extentvect<- project(vect(bc),"EPSG:4326")
extentvect<-project(extentvect, "EPSG:4326")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
bc_wgs<-sf::st_transform(bcmaps::bc_bound(),4326)

predictor_data = prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = paste0(onedrive_wd),
                                     extentvect)

predictor_data <- predictor_data[[c("population_density","days_fished", "dist_to_paved_roads",
                                    "TotalInspections")]]

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



pop_plot = ggplot() +
  tidyterra::geom_spatraster(data = predictor_data[[1]]) +
  #geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  #scale_fill_viridis_c(name = "Population size", na.value = NA, option = "E") +
  scale_fill_gradient(low = "blue", high = "orange", na.value = NA, name = "Population size") + # Example using RColorBrewer
  labs(title = paste0("Population"),
       subtitle = paste0()
       ) +  # Add shape to the legend
theme_minimal()



fished_plot = ggplot() +
  geom_sf(data = bc, fill = NA, colour = "grey")+
  tidyterra::geom_spatraster(data =predictor_data[[2]])+
  #geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  scale_fill_gradient(low = "blue", high = "orange", na.value = NA, name = "Days (log10)",
                      trans = "log10") + # Example using RColorBrewer
  labs(title = paste0("Days fished"),
       subtitle = paste0()
  ) +  # Add shape to the legend
  theme_minimal()

dist_plot = ggplot() +
  tidyterra::geom_spatraster(data = predictor_data[[3]]) +
  #geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  scale_fill_gradient(low = "black  ", high = "orange", na.value = NA, name = "Distance (m)") + # Example using RColorBrewer
  labs(title = paste0("Distance to paved roads"),
       subtitle = paste0()
  ) +  # Add shape to the legend
  theme_minimal()

insp_plot = ggplot() +
  geom_sf(data = bc, fill = NA, colour = "grey")+
  tidyterra::geom_spatraster(data = predictor_data[[4]]) +
  #geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  scale_fill_gradient(low = "blue", high = "orange", na.value = NA, name = "No.",
                      trans = "log10") + # Example using RColorBrewer
  
  labs(title = paste0("Total inspections"),
       subtitle = paste0()
  ) +  # Add shape to the legend
  theme_minimal()


fishspp_plot <- ggplot() +
  geom_sf(data = bc, fill = NA, colour = "grey")+
  geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  scale_fill_viridis_c(name = "") +
  labs(title = paste0("Invasive fish species in BC"),
       subtitle = paste0()
  ) +  # Add shape to the legend
  theme_minimal()

invertspp_plot <- ggplot() +
  geom_sf(data = bc, fill = NA, colour = "grey")+
  geom_sf(data = spp_df_l[[2]], color = "brown", alpha = 0.8) +
  scale_fill_viridis_c(name = "") +
  labs(title = paste0("Invasive spp (not fish) species in BC"),
       subtitle = paste0()
  ) +  # Add shape to the legend
  theme_minimal()

final_plot<- (pop_plot | fished_plot | fishspp_plot ) / 
  (dist_plot | insp_plot| invertspp_plot)

ggplot2::ggsave(filename = paste0("./images/intro_predictors.png"),
                plot = final_plot,
                dpi = 300, width = 14, height = 8)


maxent_fish_reduced<-terra::rast(paste0(lan_root, folder_path,"introduction_risk_reduced_fish.tif"))

maxent_fish_reduced_plot = ggplot() +
  tidyterra::geom_spatraster(data = maxent_fish_reduced[[1]]) +
  #geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  scale_fill_viridis_c(name = "Probability of occurrence", na.value = NA, option = "C") +
  #scale_fill_gradient(low = "blue", high = "orange", na.value = NA, name = "Population size") + # Example using RColorBrewer
  labs(title = paste0("Maxent output: Introduction of fish spp (2 predictors)"),
       subtitle = paste0("Population density, days fished")
  ) +  # Add shape to the legend
  theme_minimal()

maxent_fish_full<-terra::rast(paste0(lan_root, folder_path,"introduction_risk_fish.tif"))

maxent_fish_plot = ggplot() +
  tidyterra::geom_spatraster(data = maxent_fish_full[[1]]) +
  #geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  scale_fill_viridis_c(name = "Probability of occurrence", na.value = NA, option = "C") +
  #scale_fill_gradient(low = "blue", high = "orange", na.value = NA, name = "Population size") + # Example using RColorBrewer
  labs(title = paste0("Maxent output: Introduction of fish spp (4 predictors)"),
       subtitle = paste0("Population density, days fished, total inspections, distance to paved roads")
  ) +  # Add shape to the legend
  theme_minimal()

maxent_invert_reduced<-terra::rast(paste0(lan_root, folder_path,"introduction_risk_other_invertebrates_reduced.tif"))

maxent_invert_reduced_plot = ggplot() +
  tidyterra::geom_spatraster(data = maxent_invert_reduced[[1]]) +
  #geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  scale_fill_viridis_c(name = "Probability of occurrence", na.value = NA, option = "C") +
  #scale_fill_gradient(low = "blue", high = "orange", na.value = NA, name = "Population size") + # Example using RColorBrewer
  labs(title = paste0("Maxent output: Introduction of inverts spp (2 predictors)"),
       subtitle = paste0("Population density, days fished")
  ) +  # Add shape to the legend
  theme_minimal()

maxent_inverts_full<-terra::rast(paste0(lan_root, folder_path,"introduction_risk_other_invertebrates.tif"))

maxent_invert_plot = ggplot() +
  tidyterra::geom_spatraster(data = maxent_inverts_full[[1]]) +
  #geom_sf(data = spp_df_l[[1]], color = "red", alpha = 0.8) +
  scale_fill_viridis_c(name = "Probability of occurrence", na.value = NA, option = "C") +
  #scale_fill_gradient(low = "blue", high = "orange", na.value = NA, name = "Population size") + # Example using RColorBrewer
  labs(title = paste0("Maxent output: Introduction of invert spp (4 predictors)"),
       subtitle = paste0("Population density, days fished, total inspections, distance to paved roads")
  ) +  # Add shape to the legend
  theme_minimal()

maxent_comp<-(maxent_fish_plot | maxent_fish_reduced_plot | fishspp_plot) /
  (maxent_invert_plot | maxent_invert_reduced_plot | invertspp_plot)

ggplot2::ggsave(filename = paste0("./images/intro_maxent_comparison.png"),
                plot = maxent_comp,
                dpi = 300, width = 15, height = 8)
