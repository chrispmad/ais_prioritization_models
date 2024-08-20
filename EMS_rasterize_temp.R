library(DBI)
library(RSQLite)
library(sf)
library(ggplot2)
library(dplyr)
library(bcmaps)

conn<-dbConnect(RSQLite::SQLite(),"../EMS/output/EMS.sqlite")

dbListTables(conn) # list the table(s)

test1<-dbGetQuery(conn, "select * from results where parameter like '%Temperature%'")

pH <-dbGetQuery(conn, "select * from results where parameter like 'pH' and strftime('&Y', COLLECTION_DATE) >= 2022")

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
ggsave("./images/locationSamples.png",locplot, height = 10, width = 12, units = "in")





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

# How many monitoring locations have 3 or more data points in 2024?
river2024_data_rich = river2024 |> 
  dplyr::group_by(MONITORING_LOCATION) |> 
  dplyr::mutate(number_datapoints = n()) |> 
  dplyr::ungroup() |> 
  dplyr::filter(number_datapoints >= 3)

river2024TF<-st_transform(river2024_data_rich, 3005)
plot(st_geometry(river2024TF))
tointerp<- river2024TF %>% 
  select(RESULT)

tointerp<- tointerp %>% 
            dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                          lat = sf::st_coordinates(.)[,2])

bc<-bc_bound()

