historyPeriod.start <- '2009-06-01'
historyPeriod.end <- '2011-03-30'

ReferenceDate <- max(fwdStrBasicInfo$QDate)

startReferenceData <- fwdStrBasicInfo[fwdStrBasicInfo$QDate == ReferenceDate,]

timediff <- AttachedDate - as.Date(historyPeriod.start)

simulationFwdInfo <- fwdStrBasicInfo[fwdStrBasicInfo$QDate >= historyPeriod.start & fwdStrBasicInfo$QDate <= historyPeriod.end,]

historyPeriod.start <- min(simulationFwdInfo$QDate)
historyPeriod.end <- max(simulationFwdInfo$QDate)

simulationSpotPrice <- simulationFwdInfo[!duplicated(simulationFwdInfo$QDate),c('QDate','SpotPrice')]

#simulationSpotPrice$QDate <- simulationSpotPrice$QDate + timediff

referencePrice <- startReferenceData[1,]$SpotPrice
oldRferencePrice <- simulationSpotPrice[1,]$SpotPrice

scaleFactor <- referencePrice / oldRferencePrice

simulationSpotPrice$SpotPrice <- 
  getPriceChangeInPercentage(referencePrice, scaleFactor , simulationSpotPrice$SpotPrice)

simulationFwdInfo <- minipuldateFwdPrice(startReferenceData,simulationFwdInfo[,c('QDate','FwdYM','FwdPrice','MMDiff')],scaleFactor)

simulationFwdInfo$MMDiff <- NULL

fwdStrBasicInfo.sim <- createFwdDataInfo(simulationSpotPrice,simulationFwdInfo,'DUBAI')
fwdStrBasicInfo.sim <- createFwdStrInfo(fwdStrBasicInfo.sim)

print(drawFwdCurvesWithPrice(fwdStrBasicInfo.sim))
print(drawContourCurve(fwdStrBasicInfo.sim))

fwdStrBasicInfo.sim <- createSimulationData(fwdStrBasicInfo,historyPeriod.start,historyPeriod.end,startReferenceData)

