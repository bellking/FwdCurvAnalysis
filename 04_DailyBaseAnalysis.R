# specify the focusing period with smoothing factor

interestPeriod.start <- '2010-06-15'
interestPeriod.end <- '2011-07-15'

fwdStrBasicInfo.interest <- fwdStrBasicInfo[fwdStrBasicInfo$QDate >= interestPeriod.start & fwdStrBasicInfo$QDate <= interestPeriod.end ,]
smoothingPeriod <- 10
fwdStrBasicInfo.in.summary <- ddply(fwdStrBasicInfo.interest, .(FwdYM), summarise,
                                    FwdPrice = rollmean(FwdPrice,smoothingPeriod,na.pad = F),
                                    QDate = rollmax(QDate,smoothingPeriod,na.pad = F)
)

fwdStrBasicInfo.in.summary$MMDiff <- elapsed_months(fwdStrBasicInfo.in.summary$FwdYM,fwdStrBasicInfo.in.summary$QDate)

fwdStrBasicInfo.in.summary <- ddply(fwdStrBasicInfo.in.summary, .(QDate), mutate,
                                    FwdDelta = FwdPrice[1] - FwdPrice ,
                                    FwdDelta2 =FwdPrice[2] - FwdPrice , 
                                    LongTermDelta = FwdPrice[1] - FwdPrice[max(index(FwdPrice))] ,
                                    FwdDeltaWPrev = c(0,FwdPrice[1:max(index(FwdPrice))-1] - FwdPrice[2:max(index(FwdPrice))])
)

# add more structure data for daily average
fwdStrBasicInfo.in.summary$SlopeFromOrg <- 0
fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$MMDiff > 0 ,]$SlopeFromOrg <- fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$MMDiff > 0 ,'FwdDelta'] / fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$MMDiff > 0 ,'MMDiff']
fwdStrBasicInfo.in.summary$LongTermStr <- 'BKWD'
fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$LongTermDelta < 0,]$LongTermStr <- 'CNTG'
fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$LongTermDelta > 0,]$LongTermStr <- 'BKWD'

# color code
colorRange <- 700
fwdStrBasicInfo.in.summary$StrColor <- colorRange / 2
fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$LongTermStr == 'CNTG',]$StrColor <- 0
fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$LongTermStr == 'BKWD',]$StrColor <- colorRange

#draw a plot
plotFwdCurves <- ggplot(data = fwdStrBasicInfo.in.summary , aes(x = as.Date(FwdYM), y = FwdPrice, group=QDate, color=StrColor)) + #

  # geom_line(size=0.5,  aes(x = as.numeric(QDate), y = SpotPrice, group=NULL), color="red") +  
  geom_line(size=0.5) +
  geom_line(data = fwdStrBasicInfo.interest[fwdStrBasicInfo.interest$MMDiff == 0,], size=0.5,  aes(x = as.Date(QDate), y = FwdPrice, group=NULL), color="red")


print(plotFwdCurves)

fwdStrBasicInfo.in.summary$DisplayColor <- '1-BKWD'
fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$LongTermStr == 'CNTG',]$DisplayColor <- '3-CNTG'

# draw a contour plot

displayColor <- rainbow(max(fwdStrBasicInfo.in.summary$MMDiff))

plotFwdByMDiff.MovingAvg <- ggplot(data = fwdStrBasicInfo.in.summary , aes(x = QDate, y = FwdDelta, group=MMDiff, color= sprintf("%02d",MMDiff))) +
#plotFwdByMDiff.MovingAvg <- ggplot(data = fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$MMDiff > 0 , ] , aes(x = as.Date(QDate), y = FwdDelta2, group=MMDiff, color= displayColor[MMDiff+1])) +
  geom_line(size=0.3) +
  geom_point(size=0.5) + 
  geom_line( data = fwdStrBasicInfo.in.summary[fwdStrBasicInfo.in.summary$MMDiff < 15,],size = 0.7, aes(x = FwdYM, y = FwdPrice / 5, group = QDate, col = DisplayColor )) +
  scale_x_date(date_breaks = "6 months" , date_labels = "%y/%b")

print(plotFwdByMDiff.MovingAvg)


