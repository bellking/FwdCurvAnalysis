
#


# drawing the data
startRow <- as.integer(nrow(fwdStrBasicInfo)* 0 / 5)
endRow <- as.integer(nrow(fwdStrBasicInfo) * 5 /5)

#plotFwdCurves <- ggplot(data = fwdStrBasicInfo , aes(x = as.numeric(as.Date(FwdYM)), y = FwdPrice, group=QDate, color=as.numeric(format(QDate, "%d")))) +
plotFwdCurves <- ggplot(data = fwdStrBasicInfo.MonthAvg , aes(x = as.numeric(as.Date(FwdYM)), y = FwdPrice, group=QDate)) +
  
   # geom_line(size=0.5,  aes(x = as.numeric(QDate), y = SpotPrice, group=NULL), color="red") +  
  geom_line(size=0.5) +
  geom_line(data = fwdStrBasicInfo.MonthAvg[fwdStrBasicInfo.MonthAvg$FwdYM == fwdStrBasicInfo.MonthAvg$QDate,], size=1,  aes(x = as.numeric(as.Date(FwdYM)), y = FwdPrice, group=NULL), color="blue")
  

print(plotFwdCurves)



