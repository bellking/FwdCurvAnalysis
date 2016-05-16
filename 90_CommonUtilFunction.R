elapsed_months <- function(end_date, start_date) {
  
  end_date <- as.Date(end_date)
  start_date <- as.Date(start_date)
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

getYearMonth <- function(currentDate, shiftMonth) {
  currentDate <- as.Date(currentDate)
  mon <- month(currentDate)
  mon <- mon + shiftMonth
  yr <- floor((mon -1) / 12)
  mon <- mon - yr * 12
  yr <- year(currentDate) + yr
  paste(yr, "-" , sprintf("%02d",mon) , sep = "")
}


getFwdStructure <- function(x, y) {
  if (x < y)
  {
    return ("CTNG")
  }
  return ("BKWD")  
}


convertFYMStr2Date <- function(x) {
  as.Date(paste(x, "-15",sep ="") , "%Y-%m-%d")
}

monthlyMeanSoFar <- function(p) {
  refYearMon <- max(p$QDate)
  #mean(p[as.yearmon(time(p)) == refYearMon])
}


getDataFromDB <- function(crudeCategory, priceCategory) {
  if (crudeCategory == 'DUBAI') {
    crudeCodeSpot <- '10M10010'
    crudeCodeFwd <- '90M90003'
  }
  else if (crudeCategory == 'WTI') {
    crudeCodeSpot <- '80C80001'
    crudeCodeFwd <- '90M90005'
  }
  else if (crudeCategory == 'BRENT'){
    crudeCodeSpot <- '81C81001'
    crudeCodeFwd <- '90M90002'
  }
  else if (crudeCategory == 'MOPS_FO180'){
    crudeCodeSpot <- '11M11032'
    crudeCodeFwd <- '90M90008'
  }
  else if (crudeCategory == 'MOPS_FO380'){
    crudeCodeSpot <- '11M11029'
    crudeCodeFwd <- '90M90032'
  }
  else
  {
    return
  }
  conn <-odbcConnect("IRMS_TEST", uid="QUOT_VIEW_FOR_OPI", pwd="456opi$%^")
  if(priceCategory == 'S') {
    sql_str = paste("SELECT LEFT(QUOT_DATE,4)+'-'+SUBSTRING(QUOT_DATE,5,2)+'-'+RIGHT(QUOT_DATE,2) AS QDate, QUOT_PRICE AS SpotPrice FROM [dbo].[VW_MST_QUOT_VIEW_FOR_OPI] WHERE QUOT_NO = '",crudeCodeSpot,"' AND QUOT_DATE NOT LIKE '2060%' ORDER BY QUOT_DATE",sep="")
  }
  else {
    sql_str = paste("SELECT LEFT(QUOT_DATE,4)+'-'+SUBSTRING(QUOT_DATE,5,2)+'-'+RIGHT(QUOT_DATE,2) AS QDate, LEFT(QUOT_PERIOD,4)+'-'+RIGHT(QUOT_PERIOD,2)+'-01' AS FwdYM, QUOT_PRICE AS FwdPrice FROM [dbo].[VW_MST_QUOT_VIEW_FOR_OPI] WHERE QUOT_NO = '",crudeCodeFwd,"' AND QUOT_PERIOD NOT LIKE '%13' AND LEN(QUOT_PERIOD) <= 6 ORDER BY QUOT_DATE, QUOT_PERIOD",sep="")
  }
  
  sql_ret = sqlQuery(conn,sql_str)
  odbcClose(conn)
  
  return (sql_ret)
}

writeResult <- function(priceId, fwdContour.Monthly, fwdContour.Daily, correlation.Monthly
                        ,fwdSlopeAnly.Daily, interestMMDiff) {
  analysisResult <- fwdContour.Daily[ fwdContour.Daily$MMDiff %in% interestMMDiff ,c('QDate','MMDiff','FwdDelta','FwdYM')]
  names(analysisResult) <- c('QDate','FwdMonth','CONTOUR_D','FwdYM')
  analysisResult$FwdYM <- format(analysisResult$FwdYM, "%Y-%m")
  
  analysisResult.T <- fwdContour.Monthly[ fwdContour.Monthly$MMDiff %in% interestMMDiff ,c('QDate','MMDiff','FwdDelta')]
  names(analysisResult.T) <- c('QDate','FwdMonth','CONTOUR_M')
  analysisResult <- merge(analysisResult,analysisResult.T, by = c('QDate','FwdMonth') , all = T)
  
  analysisResult.T <- correlation.Monthly[,c('QDate','MMDiff','Estimate')]
  names(analysisResult.T) <- c('QDate','FwdMonth','CORR_M')
  analysisResult <- merge(analysisResult,analysisResult.T, by = c('QDate','FwdMonth') , all = T)
  
  analysisResult.T <- fwdSlopeAnly.Daily[,c('QDate','MMDiff','PWSlope')]
  names(analysisResult.T) <- c('QDate','FwdMonth','SLOPE_D')
  analysisResult <- merge(analysisResult,analysisResult.T, by = c('QDate','FwdMonth') , all = T)
  
  #analysisResult$PriceId <- priceId
  analysisResult$FwdType <- paste(priceId , "_" ,sprintf("%02d",analysisResult$FwdMonth))
  
  #write.csv(file = paste(priceId,"_result.csv"), x = analysisResult, row.names = F, na = "")
  
  return(analysisResult)
}

uploadResultToDB <- function(resultDF) {
  resultDF$QDate <- as.character(resultDF$QDate)
  resultDF$FwdMonth <- as.integer(resultDF$FwdMonth)
  write.csv(file = "FwdCurveAnalysis.csv", x = resultDF, row.names = F, na = "")
  
  ftpUpload("FwdCurveAnalysis.csv","ftp://168.154.221.100/OPI/FwdCurveAnalysis.csv", userpw="irms_ftp:!@#qwe123")

  conn <-odbcConnect("IRMS_TEST", uid="QUOT_VIEW_FOR_OPI", pwd="456opi$%^")
  sqlQuery(conn, "exec [dbo].[USP_BATCH_FWDCURVE_TO_TB_MST_QUOT]")
  odbcClose(conn)
}
