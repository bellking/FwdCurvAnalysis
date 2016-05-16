
summaryOfSlope <- aggregate(FwdDeltaWPrev ~ MMDiff + LongTermStr , summary, data=fwdStrCalInfo.MAvg)
summaryOfSlope.Detail <- summaryOfSlope$FwdDeltaWPrev

#Arima of FwdDelta???

# 1. plot of delta
testMMDiff <- 3
fwdDeltTS.Daily <- fwdStrBasicInfo[fwdStrBasicInfo$MMDiff == testMMDiff,c('QDate','FwdDelta')]
plot(fwdDeltTS.Daily, type = "l")

fwdDeltTS.MAv <- fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$MMDiff == testMMDiff,c('QDate','FwdDelta')]
plot(fwdDeltTS.MAv, type = "l")

displayColor <- rainbow(max(fwdStrCalInfo.MAvg$MMDiff)+1)

interestPeriod.start <- '2006-05-15'
interestPeriod.end <- '2100-05-15'


#regression fit for 
currentFwCurv <- fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$QDate == as.Date('2016-02-15') & fwdStrCalInfo.MAvg$MMDiff > 3 & fwdStrCalInfo.MAvg$MMDiff > 0,c('MMDiff','FwdDelta')]
#currentFwCurv$MMDiff <- currentFwCurv$MMDiff + 1 
#currentFwCurv$MMDiff <- log(currentFwCurv$MMDiff + 1, exp(1)) 
currentFwCurv$FwdDelta <- - currentFwCurv$FwdDelta

curveFit <- lm(currentFwCurv$FwdDelta ~ log(currentFwCurv$MMDiff), exp(1))
summary(curveFit)
curveFit$
currentFwCurv$FittedValue <- curveFit$fitted.values

#plot(currentFwCurv$MMDiff,currentFwCurv$FwdDelta)
#lines(currentFwCurv$MMDiff, currentFwCurv$FittedValue , col = "red" )

#fittedCurve <- ggplot(data = currentFwCurv , aes(x = MMDiff, y = FwdDelta)) +
  
  # geom_line(size=0.5,  aes(x = as.numeric(QDate), y = SpotPrice, group=NULL), color="red") +  
#  geom_point(size=2) +
#  geom_line(size = 0.5 , aes(x = MMDiff, y = FittedValue, group=NULL), color="red")


#print(fittedCurve)

#delta pattern analysis

#determine which dataset to use



curveChangeAnaylsis <- fwdStrBasicInfo[fwdStrBasicInfo$QYM > '2008-03-01',]
#curveChangeAnaylsis <- fwdStrCalInfo.MAvg
#
#curveChangeAnaylsis$
curveChangeAnaylsis = ddply(curveChangeAnaylsis, .(FwdYM), mutate,
                            SpotPriceDif = c(0, diff(SpotPrice, lag =1)),
                            FwdPriceDif = c(0, diff(FwdPrice, lag = 1)))

curveChangeAnaylsis$Direction <- 'UP'
curveChangeAnaylsis[curveChangeAnaylsis$SpotPriceDif < 0 ,]$Direction <- 'DW'


curveChangeAnaylsis.summary <- ddply(curveChangeAnaylsis, .(QYM,MMDiff,Direction) , summarise,
                                     correl = coefficients( lm(FwdPriceDif ~ SpotPriceDif) )[2],
                                     mvgPrice = mean(SpotPrice) / 100)

curveChangeAnaylsis.summary <- ddply(curveChangeAnaylsis.summary, .(QYM) , mutate,
                                     correlRatio = correl / correl[1])




#sp <- curveChangeAnaylsis[curveChangeAnaylsis$MMDiff == 24 ,]$SpotPriceDif
#fp <- curveChangeAnaylsis[curveChangeAnaylsis$MMDiff == 24 ,]$FwdPriceDif
#& curveChangeAnaylsis$QYM == '2016-04-15'
#moveFit <- lm(fp ~ sp)
#summary(moveFit)
#leveragePlots(moveFit)
#plot(moveFit)


#summary(moveFit)$coefficients[,4]
#ggplot(data = curveChangeAnaylsis.summary[curveChangeAnaylsis.summary$MMDiff %in% displayMMDiff,] , 
displayMMDiff <- c(32)

curveChangeAnaylsis.summary$CorrelType <- paste(curveChangeAnaylsis.summary$Direction,sprintf("%02d",curveChangeAnaylsis.summary$MMDiff))

corelationTrend <- ggplot(subset(curveChangeAnaylsis.summary, MMDiff %in% displayMMDiff), 
                          aes(x = QYM, y = correl , group = CorrelType, col = CorrelType)) +
  geom_line(size=0.5) +
  geom_point(size=1.2) +
  scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b") +
  geom_line(size = 1, aes(y = mvgPrice, group = NULL), col = 'red' )
  
 
#print(corelationTrend) #ppppp


curveChangeAnaylsis.daily <- curveChangeAnaylsis[curveChangeAnaylsis$MMDiff %in% displayMMDiff & abs(curveChangeAnaylsis$FwdPriceDif / curveChangeAnaylsis$SpotPriceDif) < 1.5,]

corelationTrend.daily <- ggplot(data = curveChangeAnaylsis.daily , 
          aes(x = QDate, y = FwdPriceDif / SpotPriceDif , group = MMDiff)) +
  geom_line(size=0.5) +
  geom_point(size=1.2) +
  scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b") +
  geom_line(size = 1, aes(y = SpotPrice / 100, group = NULL), col = 'red' )


print(corelationTrend.daily)


####
curveChangeAnaylsis.Aggre <- aggregate(correlRatio ~ MMDiff , summary, data = curveChangeAnaylsis.summary[curveChangeAnaylsis.summary$MMDiff <= 24,])
curveChangeAnaylsis.Aggre$MeanDiff <- - c(0,diff(curveChangeAnaylsis.Aggre$correlRatio[,c('Mean')], lag =1))

plot(curveChangeAnaylsis.Aggre$MMDiff,curveChangeAnaylsis.Aggre$MeanDiff)
correlationRegFit <- lm( MeanDiff ~ MMDiff, data = curveChangeAnaylsis.Aggre[curveChangeAnaylsis.Aggre$MMDiff > 2,])
summary(correlationRegFit)
leveragePlots(correlationRegFit)
boxplot(correl ~ MMDiff , data=curveChangeAnaylsis.summary[curveChangeAnaylsis.summary$MMDiff <= 24,])

########
#slope change analysis
breakPoint <- c(1,2,6,12,30)
defaultPlayCycle <- breakPoint[2:3]

slopeTrend.Analy <- fwdStrBasicInfo[fwdStrBasicInfo$QYM > '2008-03-01' & fwdStrBasicInfo$MMDiff %in% breakPoint,]

missingQDates <- ddply(slopeTrend.Analy, .(QDate) , summarise, nn = length(FwdPrice))
missingQDates <- missingQDates[missingQDates$nn < length(breakPoint),]$QDate

slopeTrend.Analy <- slopeTrend.Analy[!(slopeTrend.Analy$QDate %in% missingQDates),]

slopeTrend.Analy <- ddply(slopeTrend.Analy, .(QDate) , mutate,
                          PWSlope =  c(0, diff(FwdPrice, lag = 1) / diff(breakPoint, lag = 1)))

slopeTrend.Analy.MAvg <- ddply(slopeTrend.Analy, .(QYM,MMDiff) , summarise,
                               PWSlope =  mean(PWSlope),
                               SpotPrice = mean(SpotPrice))
slopeTrend.Analy.MAvg$QDate <- slopeTrend.Analy.MAvg$QYM

slopeTrend.Plot.Data <- slopeTrend.Analy

#slopeTrend.Plot.Data <- slopeTrend.Analy.MAvg;


slopeTrend.Plot <- ggplot(data = slopeTrend.Plot.Data , 
                          aes(x = QDate, y = PWSlope , group = MMDiff, col = sprintf("%02d" , MMDiff))) +
  geom_line(size=0.5) +
  #geom_point(size=1.2) +
  geom_line(size = 1, aes(y = SpotPrice / 100, group = NULL), col = 'red' ) +
  scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b")

#print(drawSlopeTrend(createSlopeAnalysis(fwdStrBasicInfo, '2008-03-01', breakPoint = breakPoint))) #ppppp

boxplot(PWSlope ~ MMDiff + LongTermStr, data=slopeTrend.Analy)
slopeAnalysis.summary <- aggregate(PWSlope ~ MMDiff + LongTermStr, summary ,data=slopeTrend.Analy)
#print(slopeAnalysis.summary)

bestDefaultCycle <- slopeAnalysis.summary[slopeAnalysis.summary$LongTermStr == 'CNTG',][3,]$PWSlope

