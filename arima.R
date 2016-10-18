setwd("~/Google Drive/Data at TWI/Pursuits/Robo advice")

## Loading data and taking relevant subset (of stocks having all data points without NAs)
allData <- fread("nyseClosingPrice.csv")
allData$Date <- as.Date(allData$Date, "%m/%d/%y")
allData <- set(allData, j = which(colSums(is.na(allData)) > 0), value = NULL)
allData <- allData[with(allData, order(Date)), ]

## Finding normalized log return of all data points
stockPrices <- allData[, 2:ncol(allData), with = FALSE]
logStockPrices <- log10(stockPrices)
logReturn <- data.table(diff(as.matrix(logStockPrices)))
logReturn <- logReturn[, Date:= allData[2:nrow(allData), Date]]
logReturn <- logReturn[, c(ncol(logReturn),1:(ncol(logReturn)-1)), with = FALSE]
save(logReturn,file="logReturn.rda")

## function to create arima model
getArimaModel <- function(data,order){
  fit <- arima(data,order = c(order,0L,0L),method = "ML")
}

## create and save models for each stock
arimaModelDF <- lapply(logReturn[1:nrow(logReturn)], function(x){return(getArimaModel(x,5))})
arimaModelDF$Date <- NULL
save(arimaModelDF,file = "arimaModels.rda")

## get prediction using model
getTimeSeriesPrediction <- function(stockName,newdata) {
  refit <- Arima(newdata, model = arimaModelDF[[stockName]])
  prediction <- predict(refit,n.ahead=1)
  return(prediction$pred[1])
}

arimaModelDF <- lapply(logReturn[1:701], function(x){return(getArimaModel(x,5))})
arimaPredictions <- logReturn[1]
arimaPredictions$Date <- NULL
for (stockName in colnames(logReturn)[2:ncol(logReturn)]) {
  refit <- Arima(logReturn[[stockName]][702:757],model = arimaModelDF[[stockName]])
  prediction <- predict(refit,n.ahead=1)
  return(prediction$pred[1])
}

## find right order
# MEList <- c()
# for (i in 10:12) {
#   print(i)
#   arimaModelDF <- lapply(logReturn[1:701], function(x){return(getArimaModel(x,i))})
#   arimaModelDF$Date <- NULL
#   totalME <- 0
#   for (stockName in colnames(logReturn)[2:ncol(logReturn)]) {
#     refit <- Arima(logReturn[[stockName]][702:757],model = arimaModelDF[[stockName]])
#     totalME <- totalME + accuracy(refit)[1]
#   }
#   MEList[i] <- totalME
# }
