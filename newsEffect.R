setwd("~/Google Drive/Data at TWI/Pursuits/Robo advice")

#list of edges in mst
mstEdges <- read.csv("mstEdges.csv")
mstEdges$X <- NULL
closePriceLogDiff <- read.csv("closePriceLogDiff.csv")
closePriceLogDiff$X<- NULL
numStocks <- ncol(closePriceLogDiff)-1
#latestLogReturns <- closePriceLogDiff[1,1:40]
#latestLogReturns <- c(latestLogReturns[1,])

newsEffect <- function(stockName,effect=0.005) {
  #create list of 40 zeroes to depict effect due to news for each of 40 stocks
  currentNewsEffect <- c(closePriceLogDiff[1,1:numStocks])
  currentNewsEffect[1:numStocks] <- 0
  sourceList <- c(stockName)
  currentNewsEffect[stockName] <- effect
  while (length(sourceList)!=0) {
    sourceNode <- sourceList[1]
    print(sourceNode)
    destList <- as.list(as.character(mstEdges[mstEdges$X1==sourceNode,]$X2))
    while(length(destList)!=0) {
      dest <- destList[[1]]
      print(dest)
      sourceList <- c(sourceList, dest)
      model1 <- lm(as.formula(paste(dest, "~",sourceNode)),data=closePriceLogDiff)
      new <- data.frame(currentNewsEffect[sourceNode])
      colnames(new) <- c(sourceNode)
      result <- predict(model1,newdata = new)
      currentNewsEffect[dest] <- result
      destList <- destList[-1]
    }
    sourceList <- sourceList[-1]
  }
  return(currentNewsEffect)
}

c <- data.frame(newsEffect("ADX",-0.05))

## Convert result to price prediction
pricePrediction <- c
for (stockName in colnames(c)) {
  ## new closing price according to prediction
  pricePrediction[1,stockName] <- 10^(log10(closePrice[1,stockName]) + c[1,stockName])
  ## percentage change in price
  pricePrediction[2,stockName] <- 100*(pricePrediction[1,stockName] - 
                                     closePrice[1,stockName])/closePrice[1,stockName]
}


