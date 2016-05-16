boxplot(FwdDeltaWPrev ~ MMDiff + LongTermStr, data=fwdStrCalInfo.MAvg)
boxplot(FwdDeltaWPrev ~ MMDiff + LongTermStr, data=fwdStrBasicInfo)

#draw a forward curve
plotFwdCurves <- drawFwdCurvesWithPrice(fwdStrCalInfo.MAvg,fwdStrBasicInfo[,c('QDate','SpotPrice')])
print(plotFwdCurves)

plotFwdByMDiff.Daily <- drawContourCurve(fwdStrBasicInfo)
print(plotFwdByMDiff.Daily)


plotFwdByMDiff.Monthly <- drawContourCurve(fwdStrCalInfo.MAvg)
print(plotFwdByMDiff.Monthly)

print(corelationTrend)

print(drawCorrelationTrend(curveChangeAnaylsis.summary,fwdStrBasicInfo,c(3,24)))

print(drawSlopeTrend(slopeTrend.Analy))
