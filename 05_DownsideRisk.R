# downside risk analysis
movingAvgPeriod <- 55
downRisk.LookAhead.Period <- 250
worstDrop.Percentage <- 0.6

worstCasePrice = fwdStrBasicInfo[fwdStrBasicInfo$MMDiff == 0, c('QDate','SpotPrice')]

worstCasePrice$RollMean <- rollapplyr(  data = worstCasePrice$SpotPrice , wid = movingAvgPeriod , fill = NA , mean)
worstCasePrice$WorstPrice <- worstCasePrice$RollMean * (1 - worstDrop.Percentage)


worstCasePrice$TargetDate <- c(worstCasePrice$QDate[-seq(downRisk.LookAhead.Period)], rep(NA,downRisk.LookAhead.Period))

downSideRisk.plot <- ggplot(data = worstCasePrice , aes(x = QDate, y = SpotPrice , group = NULL,color = 'red')  ) +
  geom_line(size=1) +
  geom_line(size = 1, aes(x = TargetDate, y = WorstPrice ) , color = 'blue' ) 

print(downSideRisk.plot)

downSideRisk.plot <- downSideRisk.plot +
geom_point(size = 0.5 ,  aes(x = QDate,y = SpotPrice - WorstPrice ) , color = 'black')
  

print(downSideRisk.plot)



