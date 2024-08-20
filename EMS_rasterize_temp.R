library(DBI)
library(RSQLite)
library(sf)
library(ggplot2)
library(dplyr)
library(bcmaps)

conn<-dbConnect(RSQLite::SQLite(),"../EMS/output/EMS.sqlite")

dbListTables(conn) # list the table(s)
test1<-dbGetQuery(conn, "select * from results where parameter like '%Temperature%'")

##date column - collection start and collection end - if need date specific - convert back to datetime

class(test1)
str(test1)
test1<-test1[!is.na(test1$LATITUDE),]
test1<-test1[!is.na(test1$LONGITUDE),]

tempSF<-st_as_sf(test1, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
ggplot()+
  geom_sf(data = tempSF)

tempSF$COLLECTION_DATE<-as.Date(tempSF$COLLECTION_DATE)
class(tempSF$COLLECTION_DATE[1])
summary(tempSF$COLLECTION_DATE)

temp2024<-tempSF[tempSF$COLLECTION_DATE >= "2024-01-01",]
results<-temp2024 %>% 
  select(c(RESULT,COLLECTION_DATE,LOCATION_TYPE,LOCATION_PURPOSE,MONITORING_LOCATION, geometry)) %>% 
  filter(!is.na(RESULT))

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)
#pie(rep(1, 22), col = c25)

barplot(table(tempSF$LOCATION_TYPE))

ggplot(data = results, aes(x = COLLECTION_DATE, y = RESULT, color = as.factor(LOCATION_PURPOSE)))+
  geom_point()+
  scale_color_manual(values = c25[1:length(unique(results$LOCATION_PURPOSE))])+
  facet_wrap( ~ LOCATION_TYPE, ncol = 3)

ggplot(data = results, aes(x = COLLECTION_DATE, y = RESULT, color = as.factor(LOCATION_TYPE)))+
  geom_point()+
  scale_color_manual(values = c25[1:length(unique(results$LOCATION_TYPE))])+
  facet_wrap( ~ LOCATION_PURPOSE, ncol = 3)

river2024<- results %>% 
  filter(LOCATION_TYPE == "RIVER,STREAM OR CREEK")




library(lubridate)
ggplot()+
  geom_sf(data = river2024, aes(color = RESULT)) +
  facet_wrap(~ month(COLLECTION_DATE), ncol = 3)


p1<-ggplot()+
  geom_point(data = river2024, aes(x = COLLECTION_DATE, y = RESULT, color = RESULT))+
  geom_line(data = river2024, aes(x = COLLECTION_DATE, y = RESULT, color = RESULT))+
  facet_wrap(~ (MONITORING_LOCATION), ncol = 5)
ggsave("./images/monitoringlocs.png",p1, height = 400, units = "cm", limitsize = F)


river2024TF<-st_transform(river2024, 3005)
plot(st_geometry(river2024TF))
tointerp<- river2024TF %>% 
  select(RESULT)

tointerp<- tointerp %>% 
            dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                          lat = sf::st_coordinates(.)[,2])

bc<-bc_bound()
library(gstat)
f1<-as.formula(RESULT ~ lon +lat)
var.smpl <- variogram(f1, data=tointerp, cutoff=1000000, width=89900)

dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=22, model="Sph", range=590000, nugget=0))
plot(var.smpl, dat.fit, xlim=c(0,1000000))
