library(igraph)
library(rgl)
library(data.table)
setwd("~/Google Drive/Data at TWI/Pursuits/Robo advice")

closePriceFileName <- 'nyseClosingPrice.csv'
takeDifferenceToggle <- TRUE

createMST <- function(closePriceFileName,takeDifferenceToggle) {
  ## get closing price data of required stocks
  rm(allData)
  allData <- read.csv(closePriceFileName)
  
  ## remove stocks with NA values
  rm(closePrice)
  closePrice <- set(allData, j = which(colSums(is.na(allData)) > 0), value = NULL)
  stockNum <- ncol(closePrice)-1
  rowNum <- nrow(closePrice)
   
  ## create log return
  rm(closePriceLog,closePriceLogDiff)
  closePriceLog <- closePrice[2:(stockNum+1)]
  closePriceLog <- log10(closePriceLog)
  closePriceLog$Date <- as.Date(closePrice$Date, "%m/%d/%y")
  if(takeDifferenceToggle) {
    closePriceLogDiff <- data.frame(closePriceLog[2:rowNum,]) - data.frame(closePriceLog[1:rowNum-1,])
    closePriceLogDiff$Date <- closePriceLog[1:rowNum-1,]$Date
  }else {
    closePriceLogDiff <- closePriceLog
  }
  
  ## create correlation matrix, and directed graph using correlation and causality
  rm(corMatrix,edges,weights,negToPosLagRatioMatrix)
  corMatrix <- cor(data.frame(closePriceLogDiff[1:stockNum]))
  edges <- c()
  weights <- c()
  negToPosLagRatioMatrix <- matrix(0, nrow = stockNum, ncol = stockNum)
  colnames(negToPosLagRatioMatrix) <- colnames(data.frame(closePriceLogDiff[1:stockNum]))
  rownames(negToPosLagRatioMatrix) <- colnames(data.frame(closePriceLogDiff[1:stockNum]))
  for (i in 1:stockNum) {
    for (j in 0+i:stockNum) {
      if (i!=j) {
        l = 100
        lagCorrel = ccf(closePriceLogDiff[i],closePriceLogDiff[j],l)
        sumSquaresNegtv = 0
        sumSquaresPostv = 0
        for (k in 1:l) {
          temp <- lagCorrel$acf
          sumSquaresPostv <- sumSquaresPostv + (temp[k+l+1] * temp[k+l+1])
          sumSquaresNegtv <- sumSquaresNegtv + (temp[k] * temp[k])
        }
        sumSquaresPostv <- sqrt(sumSquaresPostv)
        sumSquaresNegtv <- sqrt(sumSquaresNegtv)
        negToPosLagRatio <- (sumSquaresPostv - sumSquaresNegtv)*100/sumSquaresPostv
        negToPosLagRatioMatrix[i,j] <- negToPosLagRatio
        if (negToPosLagRatio < -4) {
          edges <- c(edges,i,j)
          weights <- c(weights,1-(corMatrix[i,j] * corMatrix[i,j]))
        }
        else if(negToPosLagRatio > 4){
          edges <- c(edges,j,i)
          weights <- c(weights,1-(corMatrix[i,j] * corMatrix[i,j]))
        }
      }
    }
  }
  
  ## create graph and mst
  g<-graph(edges, n=max(edges), directed = TRUE)
  g<-set.vertex.attribute(g, "name", value=colnames(closePriceLogDiff)[1:stockNum])
  gmst <- mst(g,weights = weights)
  plot(gmst,vertex.size = 8,edge.arrow.size=0.5)
  
  ## export results
  mstEdges <- data.frame(get.edgelist(gmst))
  write.csv(mstEdges,file = "mstEdges.csv")
  write.csv(closePriceLogDiff,file = "closePriceLogDiff.csv")
  write.csv(corMatrix,file = "correlationMatrix.csv")
}
  