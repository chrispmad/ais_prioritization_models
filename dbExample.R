library(DBI)
library(RSQLite)
library(sf)

conn<-dbConnect(RSQLite::SQLite(),"../EMS/output/EMS.sqlite")

dbListTables(conn) # list the table(s)
test1<-dbGetQuery(conn, "select * from results where parameter like '%Temperature%'")

##date column - collection start and collection end - if need date specific - convert back to datetime

class(test1)
str(test1)
test1<-test1[!is.na(test1$LATITUDE),]
test1<-test1[!is.na(test1$LONGITUDE),]

tempSF<-st_as_sf(test1, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
plot(st_geometry(tempSF))

