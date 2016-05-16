createFwdStrInfo <- function(fwdStrBasicInfo)
{
  #structure for daily data
  fwdStrBasicInfo <- ddply(fwdStrBasicInfo, .(QDate), mutate,
                           FwdDelta = FwdPrice[1] - FwdPrice ,
                           FwdDelta2 =FwdPrice[2] - FwdPrice , 
                           LongTermDelta = FwdPrice[1] - FwdPrice[max(index(FwdPrice))] ,
                           FwdDeltaWPrev = c(0, - diff(FwdPrice, lag = 1))
                           #FwdDeltaWPrev = c(0,FwdPrice[1:max(index(FwdPrice))-1] - FwdPrice[2:max(index(FwdPrice))])
  )
  
  # add more structure data for daily average
  fwdStrBasicInfo$SlopeFromOrg <- 0
  fwdStrBasicInfo[fwdStrBasicInfo$MMDiff > 0 ,]$SlopeFromOrg <- fwdStrBasicInfo[fwdStrBasicInfo$MMDiff > 0 ,'FwdDelta'] / fwdStrBasicInfo[fwdStrBasicInfo$MMDiff > 0 ,'MMDiff']
  fwdStrBasicInfo$LongTermStr <- 'BKWD'
  fwdStrBasicInfo[fwdStrBasicInfo$LongTermDelta < 0,]$LongTermStr <- 'CNTG'
  #fwdStrBasicInfo[fwdStrBasicInfo$LongTermDelta > 0,]$LongTermStr <- 'BKWD'
  fwdStrBasicInfo
}

createCorrelation <- function(fwdStrBasicInfo, interestMMDiff ,historyPeriod.start, historyPeriod.end, wid = 30, freq = 5)
{
  if (missing(historyPeriod.start))
    historyPeriod.start <- min(fwdStrBasicInfo$QDate)
  if (missing(historyPeriod.end))
    historyPeriod.end <- max(fwdStrBasicInfo$QDate)

  curveChangeAnaylsis <- fwdStrBasicInfo[fwdStrBasicInfo$QYM >= historyPeriod.start & fwdStrBasicInfo$QYM <= historyPeriod.end & fwdStrBasicInfo$MMDiff %in% interestMMDiff,]

  curveChangeAnaylsis = ddply(curveChangeAnaylsis, .(FwdYM), mutate,
                              SpotPriceDif = c(0, diff(SpotPrice, lag =1)),
                              FwdPriceDif = c(0, diff(FwdPrice, lag = 1)))

# #  curveChangeAnaylsis$Direction <- 'UP'
# #  curveChangeAnaylsis[curveChangeAnaylsis$SpotPriceDif < 0 ,]$Direction <- 'DW'
#   curveChangeAnaylsis$
  
  processData <- curveChangeAnaylsis[curveChangeAnaylsis$MMDiff == interestMMDiff[1],]
  curveChangeAnaylsis.summary <- getCorrelationAnalysisInfo(processData$FwdPriceDif,processData$SpotPriceDif,processData$QDate, wid, freq)
  curveChangeAnaylsis.summary$MMDiff <- interestMMDiff[1]
  
  if (length(interestMMDiff) > 1)
  {
    for(i in c(2:length(interestMMDiff)))
    {
      processData <- curveChangeAnaylsis[curveChangeAnaylsis$MMDiff == interestMMDiff[i],]
      analysisResult <- getCorrelationAnalysisInfo(processData$FwdPriceDif,processData$SpotPriceDif,processData$QDate, wid, freq)
      analysisResult$MMDiff <- interestMMDiff[i]
      curveChangeAnaylsis.summary <- rbind(curveChangeAnaylsis.summary, analysisResult)
    }
  }
  return(curveChangeAnaylsis.summary)
  
  # curveChangeAnaylsis.summary <- ddply(curveChangeAnaylsis, .(MMDiff) , summarise,
  #                                           getCorrelationAnalysisInfo(FwdPriceDif,SpotPriceDif,QDate))
# 
#   curveChangeAnaylsis.summary <- ddply(curveChangeAnaylsis.summary, .(QYM) , mutate,
#                        a                 correlRatio = correl / correl[1])
}

getCorrelationAnalysisInfo <- function(col1, col2, qd, wid = 30, freq = 5)
{
 # browser()
  d <- data.frame(y = col1, x = col2)
  d <- zoo(d, qd)
  
  # if (length(col1) < freq)
  # {
  #   return(NULL)
  # }
  
    av <- rollapplyr(d,
               width = wid  , 
               FUN = function(zz) summary(lm(y ~ x, data = as.data.frame(zz),na.action = na.omit))$coefficients[2,],
               by = freq, by.column=FALSE,
               partial = F)

  
  av <- as.data.frame(av)
  av$QDate <- as.Date(rownames(av))
  return(av)
}

# 
# getCorrelationAnalysisInfo <- function(priceInfo, compCol, wid , freq)
# {
#   d <- zoo(priceInfo[,compCol], priceInfo$QDate)
#   colnames(d) <- c('y','x')
#   rollapplyr(d,
#              width = wid  , 
#              FUN = function(zz) summary(lm(y ~ x, data = as.data.frame(zz),na.action = na.omit))$coefficients[2,],
#              by = freq, by.column=FALSE,
#              partial = F)
# }


createSlopeAnalysis <- function(fwdStrBasicInfo , breakPoint , historyPeriod.start, historyPeriod.end)
{
  if (missing(historyPeriod.start))
    historyPeriod.start <- min(fwdStrBasicInfo$QDate)
  if (missing(historyPeriod.end))
    historyPeriod.end <- max(fwdStrBasicInfo$QDate)
 
  slopeTrend.Analy <- fwdStrBasicInfo[fwdStrBasicInfo$QYM >= historyPeriod.start & fwdStrBasicInfo$QYM <= historyPeriod.end & fwdStrBasicInfo$MMDiff %in% breakPoint,]
  
  missingQDates <- ddply(slopeTrend.Analy, .(QDate) , summarise, nn = length(FwdPrice))
  missingQDates <- missingQDates[missingQDates$nn < length(breakPoint),]$QDate
  
  slopeTrend.Analy <- slopeTrend.Analy[!(slopeTrend.Analy$QDate %in% missingQDates),]
  
  slopeTrend.Analy <- ddply(slopeTrend.Analy, .(QDate) , mutate,
                            PWSlope =  c(0, diff(FwdPrice, lag = 1) / diff(breakPoint, lag = 1)))
  
  slopeTrend.Analy.MAvg <- ddply(slopeTrend.Analy, .(QYM,MMDiff) , summarise,
                                 PWSlope =  mean(PWSlope),
                                 SpotPrice = mean(SpotPrice))
  slopeTrend.Analy.MAvg$QDate <- slopeTrend.Analy.MAvg$QYM
  
  return(slopeTrend.Analy)
}


