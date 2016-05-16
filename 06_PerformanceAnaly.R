
SpotPrice.Mvg <- fwdStrCalInfo.MAvg[fwdStrCalInfo.MAvg$MMDiff == 0, c('QDate','FwdPrice')]
n1 <- nrow(SpotPrice.Mvg)
n2 <- nrow(SpotPrice.Mvg) -1 

rownames(SpotPrice.Mvg) <- format(SpotPrice.Mvg$QDate, "%Y-%m-%d")
PriceChange.Return <-  SpotPrice.Mvg[2:n1,]


PriceChange.Return$Return <- (SpotPrice.Mvg$FwdPrice[2:n1] - SpotPrice.Mvg$FwdPrice[1:n2]) /SpotPrice.Mvg$FwdPrice[1:n2]

PriceChange.Return$QDate <- NULL
PriceChange.Return$FwdPrice <- NULL

SpotPrice.Mvg <- ts(SpotPrice.Mvg, start = c(2006,6), end = c(2016,2), frequency = 12)

chart.Drawdown(PriceChange.Return)
findDrawdowns(PriceChange.Return)

charts.PerformanceSummary(PriceChange.Return)
table.Stats(PriceChange.Return)

plot(managers)
plot(SpotPrice.Mvg)

data(edhec)
