# figure out fwdCurve Structure

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
fwdStrBasicInfo[fwdStrBasicInfo$LongTermDelta > 0,]$LongTermStr <- 'BKWD'




# structure for month data
fwdStrCalInfo.MAvg <- ddply(fwdStrBasicInfo.MonthAvg, .(QDate), mutate,
                                           FwdDelta = FwdPrice[1] - FwdPrice , 
                                            FwdDelta2 = FwdPrice[2] - FwdPrice ,
                            LongTermDelta = FwdPrice[1] - FwdPrice[max(index(FwdPrice))],
                            FwdDeltaWPrev = c(0, - diff(FwdPrice, lag = 1))
                            #FwdDeltaWPrev = c(0,FwdPrice[1:max(index(FwdPrice))-1] - FwdPrice[2:max(index(FwdPrice))])
                            )
#add more structure data for monthly average
fwdStrCalInfo.MAvg$SlopeFromOrg <- 0
fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$MMDiff > 0 ,]$SlopeFromOrg <- fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$MMDiff > 0 ,'FwdDelta'] / fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$MMDiff > 0 ,'MMDiff']
fwdStrCalInfo.MAvg$LongTermStr <- 'BKWD'
fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$LongTermDelta < 0,]$LongTermStr <- 'CNTG'
fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$LongTermDelta > 0,]$LongTermStr <- 'BKWD'






fwdStrCalInfo.MAvg$LongTermStr <- as.factor(fwdStrCalInfo.MAvg$LongTermStr)

colorRange <- 700
fwdStrCalInfo.MAvg$StrColor <- colorRange / 2
fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$LongTermStr == 'CNTG',]$StrColor <- 0
fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$LongTermStr == 'BKWD',]$StrColor <- colorRange

fwdStrBasicInfo$StrColor <- colorRange / 2
fwdStrBasicInfo[fwdStrBasicInfo$LongTermStr == 'CNTG',]$StrColor <- 0
fwdStrBasicInfo[fwdStrBasicInfo$LongTermStr == 'BKWD',]$StrColor <- colorRange


#+ as.numeric(format(QDate, "%d"))
#plotFwdCurves <- ggplot(data = fwdStrBasicInfo.in.summary , aes(x = as.Date(FwdYM), y = FwdPrice, group=QDate, color=StrColor)) + #
plotFwdCurves <- ggplot(data = fwdStrCalInfo.MAvg , aes(x = as.Date(FwdYM), y = FwdPrice, group=QDate, color= LongTermStr)) +
  
  # geom_line(size=0.5,  aes(x = as.numeric(QDate), y = SpotPrice, group=NULL), color="red") +  
  geom_line(size=0.5) +
  geom_line(data = fwdStrBasicInfo[fwdStrBasicInfo$MMDiff == 0,], size=0.5,  aes(x = as.Date(QDate), y = FwdPrice, group=NULL), color="blue") +
  scale_y_continuous(breaks = seq(0,150,10))

