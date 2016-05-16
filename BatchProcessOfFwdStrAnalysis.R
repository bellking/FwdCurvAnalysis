#SetofAnalyzingPrice <- c('DUBAI') 'MOPS_FO380'
#SetofAnalyzingPrice <- c('WTI','BRENT','DUBAI')
SetofAnalyzingPrice <- c('WTI','BRENT','DUBAI','MOPS_FO180','MOPS_FO380')
valueScale <- c(1,1,1,1/6.5,1/6.5)
breakPoint <- c(1,2,6,12,30)
resultDF = data.frame()

source('0_LoadLibraries.R')

source('90_CommonUtilFunction.R')
source('91_DataManipulationFunction.R')
source('92_FwdStructureInfoFunction.R')

#length(SetofAnalyzingPrice)

for(i in 1:length(SetofAnalyzingPrice))
{
  currentPriceId <- SetofAnalyzingPrice[i]
  
  spotHistory <- getDataFromDB(currentPriceId,'S')
  spotHistory$SpotPrice <- valueScale[i] * spotHistory$SpotPrice
  fwdHistory <- getDataFromDB(currentPriceId,'F')
  fwdHistory$FwdPrice <- valueScale[i] * fwdHistory$FwdPrice
  
  #source('90_CommonUtilFunction.R')
  fwdStrBasicInfo <- createFwdDataInfo(spotHistory,fwdHistory,currentPriceId)
  fwdStrBasicInfo.MonthAvg <- createFwdDataInfo.MAvg(fwdStrBasicInfo)
  
  #source('91_DataManipulationFunction.R')
  fwdStrBasicInfo <- createFwdStrInfo(fwdStrBasicInfo)
  fwdStrCalInfo.MAvg <- createFwdStrInfo(fwdStrBasicInfo.MonthAvg)
  
  #source('92_FwdStructureInfoFunction.R')
  curveChangeAnaylsis.summary <- createCorrelation(fwdStrBasicInfo, c(1,3,30),'2008-03-01', wid = 40, freq = 3)
  slopeTrend.Analy <- createSlopeAnalysis(fwdStrBasicInfo, '2008-03-01', breakPoint = breakPoint)
  
  resultDF <- rbind(resultDF, writeResult(currentPriceId,fwdStrCalInfo.MAvg, fwdStrBasicInfo,curveChangeAnaylsis.summary,slopeTrend.Analy,
                                          c(1,2,6,12,18,24,30)))
}

uploadResultToDB(resultDF)