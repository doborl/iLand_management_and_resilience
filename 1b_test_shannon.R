
library(vegan)




file<-"E:/iLandDist - new version 2.0 - 2025/output/DISASTER_UNMANAGED_w5_EC-EARTH_RACMO22E-r1_rcp45.sqlite"




# connect to the database
sqlite.driver <- dbDriver("SQLite")
db1 <- dbConnect(sqlite.driver, dbname = file)
tables.in.the.file <- dbListTables(db1)
print(tables.in.the.file)

#-----------------------------------------------
landscape <- dbReadTable(db1,"landscape")
landscape.removed <- dbReadTable(db1,"landscape_removed")
if (mgm!="UNMANAGED") abeUnit <- dbReadTable(db1, "abeUnit")
#  abeStandRemoval <- dbReadTable(db1,"abeStandRemoval")
barkbeetle <- dbReadTable(db1,"barkbeetle")
wind <- dbReadTable(db1,"wind")
carbon <- dbReadTable(db1,"carbon")
carbonflow <- dbReadTable(db1,"carbonflow")

stand<- dbReadTable(db1,"stand")
dbDisconnect(db1)    # close the file