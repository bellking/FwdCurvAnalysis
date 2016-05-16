getPriceChangeInPercentage <- function(startPrice,scaleFactor,priceData)
{
  originalStartPrice <- priceData[1]
  priceData <- rollapplyr(priceData
                          , width = 1  , function(x) startPrice + scaleFactor * (x[1] - originalStartPrice) , partial = F)
  as.vector(priceData)
}



minipuldateFwdPrice <- function(referenceFwdPrice,targetFwdPrice,scaleFactor)
{
  targetMMDiff <- c(0:max(referenceFwdPrice$MMDiff))
  targetFwdPrice <- targetFwdPrice[targetFwdPrice$MMDiff %in% targetMMDiff,]
  
  for(i in targetMMDiff)
  {
    currentFwdPrice <- targetFwdPrice[targetFwdPrice$MMDiff == i,]
    targetFwdPrice[targetFwdPrice$MMDiff == i,]$FwdPrice <- getPriceChangeInPercentage(referenceFwdPrice[i+1,]$FwdPrice, scaleFactor,currentFwdPrice$FwdPrice )
  }
  targetFwdPrice
}

createSimulationData <- function(simulationReference , historyPeriod.start, historyPeriod.end, startFwdData, referenceDate, scaleFactor , bchMark = 'DUBAI')
{
  
  if (missing(historyPeriod.start))
    historyPeriod.start <- min(simulationReference$QDate)
  if (missing(historyPeriod.end))
    historyPeriod.end <- max(simulationReference$QDate)
  if (missing(referenceDate))
    referenceDate <- max(startFwdData$QDate)
  
  simulationReference <- simulationReference[simulationReference$QDate >= historyPeriod.start & simulationReference$QDate <= historyPeriod.end, ]
  
  startFwdData <- startFwdData[startFwdData$QDate == referenceDate,]
  
  timediff <- as.Date(referenceDate) - as.Date(historyPeriod.start)
  
  simulationSpotPrice <- simulationReference[!duplicated(simulationReference$QDate),c('QDate','SpotPrice')]
  
  #shift date to simulation period

  startSpotPrice <- startFwdData$SpotPrice[1]
  referencePrice <- simulationSpotPrice$SpotPrice[1]
  
  if (missing(scaleFactor))
     scaleFactor <- startSpotPrice / referencePrice
  
  simulationSpotPrice$QDate <- simulationSpotPrice$QDate + timediff
  simulationReference$QDate <- simulationReference$QDate + timediff
  simulationReference$FwdYM <- getYearMonth(simulationReference$QDate , simulationReference$MMDiff)

  simulationSpotPrice$SpotPrice <-
    getPriceChangeInPercentage(startSpotPrice, scaleFactor , simulationSpotPrice$SpotPrice)

  simulationReference <- minipuldateFwdPrice(startFwdData,simulationReference[,c('QDate','FwdYM','FwdPrice','MMDiff')],scaleFactor)

  simulationReference$MMDiff <- NULL

  fwdStrBasicInfo.sim <- createFwdDataInfo(simulationSpotPrice,simulationReference,bchMark)
  #fwdStrBasicInfo.sim <- fwdStrBasicInfo.sim[fwdStrBasicInfo.sim$QDate > referenceDate,]
}


addFwdInfo <- function(currentFwdData,addedFwdData)
{
  referenceDate <- min(addedFwdData$QDate)
  currentFwdData <- currentFwdData[currentFwdData$QDate < referenceDate,] 
  rbind(currentFwdData,addedFwdData)
}
