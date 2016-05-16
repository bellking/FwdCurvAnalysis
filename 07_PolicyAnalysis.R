#best cycle analysis

#'3rd Qu.', 'Mean'

threshHoldLevel <- mean(bestDefaultCycle[1,c('Mean','3rd Qu.')])


bestCycle.AnalysisData <- fwdStrBasicInfo[fwdStrBasicInfo$LongTermStr == 'CNTG',]
bestCycle.AnalysisData <- bestCycle.AnalysisData[bestCycle.AnalysisData$MMDiff <= defaultPlayCycle[2] | - bestCycle.AnalysisData$FwdDeltaWPrev >= threshHoldLevel,]

bestCycle.AnalysisData <- ddply(bestCycle.AnalysisData, .(QDate) , summarise, 
                                bestCycle = max(MMDiff), futureDelta = - FwdDelta[length(FwdDelta)]
                                , QYM = QYM[1])

plot(bestCycle.AnalysisData$QDate,bestCycle.AnalysisData$bestCycle)

#past earning
howFarLookback <- 6

realizedPastEarning <- fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$MMDiff == howFarLookback,c('FwdYM','FwdDelta')]
realizedPastEarning$PastFwdDelta <- - realizedPastEarning$FwdDelta
realizedPastEarning$QYM <- realizedPastEarning$FwdYM
realizedPastEarning$FwdDelta <- NULL
realizedPastEarning$FwdYM <- NULL

bestCycle.AnalysisData <- merge(bestCycle.AnalysisData, realizedPastEarning , by = "QYM" , all = F)


#open position volume analysis


openPosition.Analysis <- merge(bestCycle.AnalysisData, worstCasePrice, by = "QDate", all=F)
openPosition.Analysis <- openPosition.Analysis[!is.na(openPosition.Analysis$WorstPrice),]
openPosition.Analysis$OpenByFuture <- 100 * 1/ (1 +  (openPosition.Analysis$SpotPrice - openPosition.Analysis$WorstPrice) / openPosition.Analysis$futureDelta)
openPosition.Analysis$OPenByWhole <-  100 * 1/ (1 +  (openPosition.Analysis$SpotPrice - openPosition.Analysis$WorstPrice) / (openPosition.Analysis$futureDelta + openPosition.Analysis$PastFwdDelta))
row.names(openPosition.Analysis) <- openPosition.Analysis$QDate

plot(openPosition.Analysis$QDate,openPosition.Analysis$OpenPositionPortion)



displayVar <- c('QDate','SpotPrice','bestCycle','OpenByFuture','OPenByWhole','futureDelta','PastFwdDelta')
#displayVar <- c('QDate','SpotPrice','bestCycle','OpenByFuture','futureDelta')


openSpotionKeyInfo.Melt <- melt(openPosition.Analysis[,displayVar] , id = 'QDate')

policy.plot <- ggplot(data = openSpotionKeyInfo.Melt , aes(x = as.Date(QDate), y = value , group = variable, color = variable) ) +
  geom_line(size=0.3) +
  #geom_line(size = 0.5, aes(y = bestCycle ) , color = 'blue' ) +
  #geom_line(size = 0.5, aes(y = OpenPositionPortion ) , color = 'black' ) +
  #geom_line(size = 0.5, aes(y = totalDelta ) , color = 'purple' ) +
  #geom_line(data = fwdStrCalInfo.MAvg , aes(x = as.Date(QDate), y = FwdPrice, group= NULL, color= StrColor + as.numeric(format(QDate, "%d")))) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b")
print(policy.plot)

tail(openPosition.Analysis)
