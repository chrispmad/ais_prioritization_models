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
library(stars)

variable_to_search <- "Temperature"
year_to_search<-2023

path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/CNF/")

bc_vect = bc_bound()
bc_vect_alb = terra::vect(sf::st_transform(bcmaps::bc_bound(),3005))
bc = bcmaps::bc_bound() |> 
  dplyr::summarise() |> 
  terra::vect()

conn<-dbConnect(RSQLite::SQLite(),"../EMS/output/EMS.sqlite")

#dbListTables(conn) # list the table(s)

varGet<-dbGetQuery(conn, paste0("select * from results where parameter like '%",variable_to_search,"%'"))

#remove where the coordinates are NA
varGet<- varGet %>% 
  filter_at(vars(LONGITUDE, LATITUDE), all_vars(!is.na(.)))

#create the sf object from the data
varSF<-st_as_sf(varGet, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
#set date
varSF$COLLECTION_DATE<-as.Date(varSF$COLLECTION_DATE)

varSF_filt_date<-varSF[varSF$COLLECTION_DATE >= paste0(as.character(year_to_search-2),"-01-01") &
                  varSF$COLLECTION_DATE < paste0(as.character(year_to_search+1),"-01-01"),] %>%
  filter(!is.na(MONITORING_LOCATION))



results <- varSF_filt_date %>%
  #dplyr::select(c(RESULT, COLLECTION_DATE, LOCATION_TYPE, LOCATION_PURPOSE, MONITORING_LOCATION)) %>%
  dplyr::filter(!is.na(RESULT)) %>%
  dplyr::filter(LOCATION_TYPE == "RIVER,STREAM OR CREEK" |
                  LOCATION_TYPE == "MONITORING WELL" |
                  LOCATION_TYPE == "LAKE OR POND") %>%
  dplyr::group_by(MONITORING_LOCATION) %>%
  dplyr::summarise(medianVal = median(RESULT))

highvals<-results %>% 
  dplyr::filter(medianVal >20)



plotting <- varSF_filt_date[varSF_filt_date$MONITORING_LOCATION %in% results$MONITORING_LOCATION,]

p1<-ggplot(plotting, aes(x = COLLECTION_DATE, y = RESULT, color = MONITORING_LOCATION)) +
  geom_point() +
  geom_line() +
  labs(title = "Result vs. Collection Date by Monitoring Location",
       x = "Collection Date",
       y = "Result") +
  facet_wrap(~ MONITORING_LOCATION, ncol = 5)

ggsave(p1, filename = "./images/monitoringlocs.png", height = 300,units = "cm", limitsize = F)


resultsalb<-st_transform(results, 3005)

tointerpPt<-st_centroid(resultsalb)

tointerpPt<- tointerpPt %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

#create interpolation grid encompassing Canada and USA
bbox <- sf::st_bbox(st_as_sf(bc))

# grid10km <- expand.grid(
#   X = seq(from = bbox["xmin"], to = bbox["xmax"], by = 10000),
#   Y = seq(from = bbox["ymin"], to = bbox["ymax"], by = 10000)) %>%
#   mutate(Z = 0)  %>% 
#   raster::rasterFromXYZ(crs = 3005) 
grid10km <- bc_vect %>% 
            st_bbox(.) %>% 
            st_as_stars(dx = 10000) %>% 
            st_crop(., st_bbox(bc_vect))

grid10km

idwgrid<-gstat::idw(medianVal~1, tointerpPt, grid10km)

highvals<- tointerpPt %>% 
  filter(medianVal >=20)

ggplot()+
  geom_stars(data = idwgrid, aes(fill = var1.pred, x = x, y = y))+
  geom_sf(data = st_cast(bc_vect, "MULTILINESTRING"))+
  geom_sf(data = tointerpPt)

v <- variogram(medianVal~1, tointerpPt)
plot(v)
v.f<-fit.variogram(v, vgm(1, "Exp", 50000,1))



