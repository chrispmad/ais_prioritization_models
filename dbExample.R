library(DBI)
library(RSQLite)
library(sf)
library(ggplot2)
library(dplyr)

conn<-dbConnect(RSQLite::SQLite(),"../EMS/output/EMS.sqlite")

dbListTables(conn) # list the table(s)
test1<-dbGetQuery(conn, "select * from results where parameter like '%Temperature%'")

##date column - collection start and collection end - if need date specific - convert back to datetime
