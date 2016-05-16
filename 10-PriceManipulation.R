#historyPeriod.start <- as.Date(c('2015-08-04','2009-06-01'))
#historyPeriod.end <- as.Date(c('2016-04-30','2011-03-30'))

historyPeriod.start <- as.Date(c('2013-11-04','2009-06-07'))
historyPeriod.end <- as.Date(c('2014-05-30','2011-03-30'))

#historyPeriod.start <- as.Date(c('2009-06-05'))
#historyPeriod.end <- as.Date(c('2011-03-30'))


referenceSimulationPeriod <- 
  data.frame( historyPeriod.start ,  historyPeriod.end)

# print(drawFwdPrice(fwdStrBasicInfo,referenceSimulationPeriod$historyPeriod.start[2],referenceSimulationPeriod$historyPeriod.end[2]))
 print(drawContourCurve(fwdStrBasicInfo,referenceSimulationPeriod$historyPeriod.start[2],referenceSimulationPeriod$historyPeriod.end[2]))
# print(drawFwdPrice(fwdStrBasicInfo.sim))

# create first section of simulation data
ReferenceDate <- max(fwdStrBasicInfo$QDate)
startReferenceData <- fwdStrBasicInfo[fwdStrBasicInfo$QDate == ReferenceDate,]
fwdStrBasicInfo.sim <- 
  createSimulationData(fwdStrBasicInfo,referenceSimulationPeriod$historyPeriod.start[1],
                       referenceSimulationPeriod$historyPeriod.end[1],startReferenceData)

if (nrow(referenceSimulationPeriod) > 1)
{
  for(i in c(2:nrow(referenceSimulationPeriod)))
  {
    fwdStrBasicInfo.sim <- addFwdInfo(fwdStrBasicInfo.sim,
      createSimulationData(fwdStrBasicInfo,referenceSimulationPeriod$historyPeriod.start[i],
                           referenceSimulationPeriod$historyPeriod.end[i],fwdStrBasicInfo.sim, scaleFactor = 0.55))
  }
}


fwdStrBasicInfo.sim <- createFwdStrInfo(fwdStrBasicInfo.sim)

print(drawFwdCurvesWithPrice(fwdStrBasicInfo.sim))
print(drawContourCurve(fwdStrBasicInfo.sim))
