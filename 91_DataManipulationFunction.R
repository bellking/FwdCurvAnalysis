createFwdDataInfo <- function(spotHistory,fwdHistory,bchMark)
{
  fwdHistory$FwdYM <- strtrim(fwdHistory$FwdYM,7)
  fwdHistory <- fwdHistory[fwdHistory$FwdPrice > 0 ,]
  
  #spotHistory <- read.csv(spotRawDateFile)
  spotHistory$YM <- as.yearmon(as.Date(spotHistory$QDate))
  spotHistory <- ddply(spotHistory, .(YM), mutate,
                       IndexFromStart = index(SpotPrice))
  
  spotHistory$MMvAvg <- rollapplyr(spotHistory$SpotPrice
                                   , width = spotHistory$IndexFromStart  , mean , partial = F)
  spotHistory$YM <- NULL
  spotHistory$IndexFromStart <- NULL
  
  # 2. combine the data
  
  fullHistory <- merge(fwdHistory, spotHistory, by = "QDate", all=F)
  
  # 3. add structure info
  
  
  fwdStrBasicInfo <- fullHistory
  
  fwdStrBasicInfo$BCHMark <- bchMark
  fwdStrBasicInfo$QDate <- as.Date(fwdStrBasicInfo$QDate , "%Y-%m-%d")
  fwdStrBasicInfo$FwdYM <- convertFYMStr2Date(fwdStrBasicInfo$FwdYM)
  
  fwdStrBasicInfo$MMDiff <- elapsed_months(fwdStrBasicInfo$FwdYM , fwdStrBasicInfo$QDate)
  
  
  # fill missing fwdprice of M0 with mav
  
  fwdStrBasicInfo$FwdPriceTy <- 'OR'
  FillMissingM0 <- ddply(fwdStrBasicInfo, .(QDate), summarise, QDate = QDate[1], FwdYM = convertFYMStr2Date(format(QDate[1], "%Y-%m")) , FwdPrice= MMvAvg[1], SpotPrice= SpotPrice[1], MMDiff = min(MMDiff), BCHMark = BCHMark[1], MMvAvg = MMvAvg[1], FwdPriceTy = 'CMV')
  FillMissingM0 <- FillMissingM0[FillMissingM0$MMDiff >0,]
  
  if (length(FillMissingM0$MMDiff) > 0)
  {
    FillMissingM0$MMDiff <- 0
    fwdStrBasicInfo <- rbind(fwdStrBasicInfo, FillMissingM0)
  }
  
  fwdStrBasicInfo <- fwdStrBasicInfo[!is.na(fwdStrBasicInfo$SpotPrice),]
  
  # order data by QDate and FwdMM
  fwdStrBasicInfo <- fwdStrBasicInfo[order(fwdStrBasicInfo$QDate,fwdStrBasicInfo$MMDiff),]
  rownames(fwdStrBasicInfo) <- index(fwdStrBasicInfo)
  fwdStrBasicInfo$QYM <- convertFYMStr2Date(format(fwdStrBasicInfo$QDate, "%Y-%m"))
  
  fwdStrBasicInfo <- fwdStrBasicInfo[as.yearmon(fwdStrBasicInfo$QDate) <= as.yearmon(fwdStrBasicInfo$FwdYM) ,]
}

createFwdDataInfo.MAvg <- function(fwdStrBasicInfo)
{
  fwdStrBasicInfo.MonthAvg <- ddply(fwdStrBasicInfo, .(QYM, FwdYM), summarise, QDate = QYM[1], FwdYM = FwdYM[1], FwdPrice = mean(FwdPrice), SpotPrice = mean(SpotPrice), BCHMark = BCHMark[1])
  fwdStrBasicInfo.MonthAvg$MMDiff <- elapsed_months(fwdStrBasicInfo.MonthAvg$FwdYM , fwdStrBasicInfo.MonthAvg$QDate)
  fwdStrBasicInfo.MonthAvg <- fwdStrBasicInfo.MonthAvg[!is.na(fwdStrBasicInfo.MonthAvg$QDate),]
}
