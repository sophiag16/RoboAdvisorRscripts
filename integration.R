setwd('~/Google Drive/Data at TWI/Pursuits/Robo advice')
library(RPostgreSQL)
library(data.table)
library(reshape)
library(igraph)

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="roboadvisordb", host= "localhost", port=5432, 
                user="robouser", password="password")

q <- "select * from asset"
assets <- data.table(dbGetQuery(db, q))

q <- "select * from assetdata";
stockInfo <- data.table(dbGetQuery(db, q))
dbDisconnect(db)
dbUnloadDriver(drv)

stockPrice <- stockInfo[,list(timeStamp,asset_id,price)]
stockPrice <- cast(stockPrice, timeStamp ~ asset_id)
stockPrice$timeStamp <- as.Date(stockPrice$timeStamp)
colnames(stockPrice)[1] <- 'Date'




# loop over all assets to predict

for (i in assets$id) {
  print(i)
}

# method to find prediction of an asset and write it to DB
putPredictionToDB <- function(asset){
  print(asset)
  print("######")
}




