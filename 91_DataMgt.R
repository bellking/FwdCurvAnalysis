currentBCHMark <- 'DUBAI'
rawDataFolder <- "./RawData/"

# save key dataframe
dataFileName <- paste(rawDataFolder, "fwdStrBasicInfo" , currentBCHMark , ".csv" , sep ="" )
write.csv(fwdStrBasicInfo, dataFileName , row.names = FALSE)

dataFileName <- paste(rawDataFolder, "fwdStrBasicInfo.MonthAvg" , currentBCHMark , ".csv" , sep ="" )
write.csv(fwdStrBasicInfo.MonthAvg, dataFileName , row.names = FALSE)

dataFileName <- paste(rawDataFolder, "fwdStrCalInfo.MAvg" , currentBCHMark , ".csv" , sep ="" )
write.csv(fwdStrCalInfo.MAvg, dataFileName , row.names = FALSE)

# read key dataframe

dataFileName <- paste(rawDataFolder, "fwdStrBasicInfo" , currentBCHMark , ".csv" , sep ="" )
fwdStrBasicInfo <- read.csv(dataFileName)
read.zoo(dataFileName,sep=",",header=T, format = "%Y-%m-%d")

dataFileName <- paste(rawDataFolder, "fwdStrBasicInfo.MonthAvg" , currentBCHMark , ".csv" , sep ="" )
fwdStrBasicInfo.MonthAvg <- read.csv(dataFileName)


dataFileName <- paste(rawDataFolder, "fwdStrCalInfo.MAvg" , currentBCHMark , ".csv" , sep ="" )
fwdStrCalInfo.MAvg <- read.csv(dataFileName)