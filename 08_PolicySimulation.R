simulationStartingDate <- as.Date('2008-05-20')
rawData.ForSimulation <- fwdStrBasicInfo[fwdStrBasicInfo$QYM > simulationStartingDate - 30,]
simulationPeriod <- unique(rawData.ForSimulation$QDate)

fwdStrBasicInfo$

policyBookKeeping <- 

openPosition.Analysis


runSimulation

PaperTrading.BookKeeping <- data.frame(Underlying.rAsset ="" , Contract.Type ="" ,Open.Date = date(), Close.Date = date(),FwdYM = date(), Postion = "", Open.Vol = 1, Close.Vol = 1, Open.Price = 1, Close.Price = 1, Status = "")
PaperTrading.BookKeeping <- PaperTrading.BookKeeping[-1,]
PhyiscalTrading.BookKeeping <- data.frame(Asset ="" , Buy.Date = date(), Sell.Date = date(), Buy.Price =1, Sell.Price = 1, Vol = 1 , Status = "")
PhyiscalTrading.BookKeeping <- PhyiscalTrading.BookKeeping[-1,]
PnL.BookKeeping<- data.frame(BQdate = date(), Appreiciated.PnL = 1, Realized.PnL = 1, Accumulated.APnL = 1 , Accumulated.RPnL = 1 , CashFlow = 1)
PnL.BookKeeping <- PnL.BookKeeping[-1,]

fwdStr.Previous <- 'CNTG'

for(i in 1:length(simulationPeriod))
{
  currentDate <- simulationPeriod[i]

  
  # contango ply policy
  
}



RollingPlay <- function (OpenPaper, PriceInfo.Avail)
{
  
}
