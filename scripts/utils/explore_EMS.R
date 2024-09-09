library(DBI)
library(RSQLite)
library(sf)
library(ggplot2)
library(dplyr)
library(bcmaps)
library(raster)
library(automap)
library(gstat)
library(stringr)
library(terra)
library(raster)
library(lubridate)
source("scripts/utils/pie_col.R")

var_name<-"Turbidity"
  
  bc = bcmaps::bc_bound() |> 
    dplyr::summarise() |> 
    terra::vect()
  
  path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
  onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/CNF/")
  
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
  bc_vect_alb = terra::vect(sf::st_transform(bcmaps::bc_bound(),3005))
  
  conn<-dbConnect(RSQLite::SQLite(),"../EMS/output/EMS.sqlite")
  
  raw_data<-dbGetQuery(conn, paste0("select * from results where parameter like '%",var_name,"%'"))
  
  dbDisconnect(conn)
  
  raw_data<-raw_data[!is.na(raw_data$LATITUDE),]
  raw_data<-raw_data[!is.na(raw_data$LONGITUDE),]
  
  data_sf<-st_as_sf(raw_data, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
  data_sf$COLLECTION_DATE<-as.Date(data_sf$COLLECTION_DATE)
  
  results<-data_sf %>%
    dplyr::select(c(RESULT,COLLECTION_DATE,LOCATION_TYPE,LOCATION_PURPOSE,MONITORING_LOCATION, geometry)) %>% 
    dplyr::filter(!is.na(RESULT)) %>% 
    dplyr::filter(LOCATION_TYPE == "RIVER,STREAM OR CREEK" |
                    LOCATION_TYPE == "LAKE OR POND")
  
  
  locplot<-ggplot(data = results, aes(x = COLLECTION_DATE, y = RESULT, color = as.factor(LOCATION_PURPOSE)))+
    geom_point()+
    scale_color_manual(values = c25[1:length(unique(results$LOCATION_PURPOSE))])+
    labs(color = "Purpose of \nlocation sample", x = "Date", y = "Temperature")+
    theme(plot.title = element_text(size = rel(2), face = "bold"),
          plot.subtitle = element_text(size = rel(1.8)),
          legend.title = element_text(size = rel(1.8)),
          legend.text = element_text(size = rel(1.2)),
          legend.position = 'right',
          panel.grid.major = element_blank(),
          strip.text.x = element_text(size = rel(1.2),face = "bold"),
          axis.text = element_text(size = rel(1.1)),
          axis.title = element_text(size = rel(1.3), face = "bold")
          )+
    facet_wrap( ~ LOCATION_TYPE, ncol = 3)
  locplot
  #ggsave("./images/locationSamples.png",locplot, height = 10, width = 12, units = "in")

  ggplot(data = results, aes(x = COLLECTION_DATE, y = RESULT, color = as.factor(LOCATION_TYPE)))+
    geom_point()+
    scale_color_manual(values = c25[1:length(unique(results$LOCATION_TYPE))])+
    facet_wrap( ~ LOCATION_PURPOSE, ncol = 3)
  
  