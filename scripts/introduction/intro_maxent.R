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

predictor_data <- predictor_data[[c("population_density","days_fished")]]

sppDF<- sf::st_read(paste0(lan_root,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/AIS_occurrences_all_sources.gpkg"))


northern_pike <- sppDF |> filter(Species == "Northern pike")

bc_watersheds <- st_read(paste0(onedrive_wd,"CNF/watershed_groups.gpkg")) 
bc_watersheds <- bc_watersheds |> st_transform(4326)



pike_watersheds<- bc_watersheds |> 
  filter(str_detect(WATERSHED_GROUP_NAME, c("Peace|Liard|Yukon|Alsek|Taku|Makenzie")))


# plot(st_geometry(bc_watersheds), border = "black", col = NA)  # Transparent fill, black border
# plot(st_geometry(pike_watersheds), col = "blue", add = TRUE)
# plot(st_geometry(northern_pike), col = "purple", add = TRUE)

p1<-ggplot()+
  geom_sf(data = bc_watersheds, color = "darkgrey", fill = NA)+
  geom_sf(data = pike_watersheds, colour = "black", aes(fill = as.factor(WATERSHED_GROUP_NAME)))+
  geom_sf(data = northern_pike |> st_transform(3005), color = "brown")+
  scale_fill_viridis_d(option = "D", name = "Watersheds")+
  theme_minimal()

# Map is in Harvey (2009) and https://waves-vagues.dfo-mpo.gc.ca/Library/337844.pdf and https://www.tru.ca/__shared/assets/Dan_Doutaz_thesis45335.pdf
# For Lower Peace River, Peace Arm and Upper Peace Arm - mark all points to the north with "native" tag (inclusive of watersheds named)
# For Laird and Upper Laird - If they have a "native" tag from previous, check if they are to the east (inclusive of watersheds named)
# and keep "native" tag if so, else remove native tag.
northern_pike_transformed <- st_transform(northern_pike, st_crs(pike_watersheds))

#get the bounding box of the watersheds, where pike are native
bbox <- st_bbox(pike_watersheds)

#get the points of pike where they are within the limits of the bounding box 
# ymin and xmin. This will generate the points that are north and east of the watersheds
points_north_east <- northern_pike_transformed %>%
  filter(st_coordinates(.)[,2] > bbox["ymin"] & st_coordinates(.)[,1] > bbox["xmin"])

    
plot_native_pike <- ggplot() +
  geom_sf(data = bc, fill = "lightgrey", color = "black", alpha = 0.5) +
  geom_sf(data = pike_watersheds, color = "purple", alpha = 0.8)+
  geom_sf(data = points_north_east, color = "orange", size = 2) +
  ggtitle("Northern Pike Points in Watersheds") +
  theme_minimal()
#plot_native_pike

# This works, so lets apply it to the AIS occurrences, only for northen pike

sppdf_backup<-sppDF

pike_watersheds<-st_transform(pike_watersheds, st_crs(sppDF))

sppDF_filtered <- sppDF %>%
  mutate(X = st_coordinates(.)[,1],  # Extract longitude
         Y = st_coordinates(.)[,2]) %>%  # Extract latitude
  filter(!(Species == "Northern pike" & Y > bbox["ymin"] & X > bbox["xmin"])) %>%
  dplyr::select(-X, -Y)  # Remove temporary columns

## test to see if this worked as intended
sppDF_northern_pike <- sppDF %>%
  mutate(X = st_coordinates(.)[,1],  # Extract longitude
         Y = st_coordinates(.)[,2]) %>%  # Extract latitude
  filter((Species == "Northern pike" & Y > bbox["ymin"] & X > bbox["xmin"])) %>%
  dplyr::select(-X, -Y)  # Remove temporary columns  

#plot(st_geometry(sppDF_northern_pike)) # Itworked!
## Pike done
sppDF <- sppDF_filtered


############# Yellow perch
#yellow_perch <-  sppDF |> filter(Species == "Yellow perch")


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
    
    top_model1 = me@results |> 
      dplyr::mutate(auccbi = (cbi.train + auc.train) / 2) |> 
      dplyr::arrange(dplyr::desc(auccbi)) |> 
      dplyr::slice(1)
    
    top_model = me@predictions[[top_model1$tune.args]]
    
    maxent_html = me@models[[top_model1$tune.args]]@html
    
    
    
    jpeg(paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk/response_curve_",snakecase::to_snake_case(the_group),".jpg"), width = 1200, height = 800)
    response(me@models[[top_model1$tune.args]])
    dev.off()
    
    # terra::plot(top_model)
    
    
    # Write out the predicted raster to be used in the excel doc as
    # the 'risk of introduction' variable.

    terra::writeRaster(top_model, paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk/introduction_risk_reduced_",snakecase::to_snake_case(the_group),".tif"), overwrite = T)
    file.copy(from = maxent_html, to = paste0(paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk/intro_MaxEnt_reduced_",snakecase::to_snake_case(the_group),".html")))
      
    })


