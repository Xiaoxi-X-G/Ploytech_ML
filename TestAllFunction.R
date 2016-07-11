rm(list = ls())
require(lubridate)

RScriptPath<-"C://Users/ptech3/Dropbox/Ploytech/Regression/AllFunctions"
DatabaseName<-"RetailDemo2"


######### link to drag all LocationID and corresponding SalesHistory
library(RODBC)
odbcDataSources()

conn<-odbcConnect(dsn="localdb") #
LocationIDResult <- sqlQuery(conn, "SELECT [LocationID] FROM [RetailDemo2].[dbo].[Location];")
#LocationIDResult <- sqlQuery(conn, "SELECT [LocationID] FROM [Time2Work_MultiRetailAU].[dbo].[Location];")
odbcClose(conn)


for (i in 1:nrow(LocationIDResult)){
#i <- 111 # LocationID = 110 (Store 112), 93 (Store 95)
DB_Path <- paste(LocationIDResult$LocationID[i], ";", sep="")
print(DB_Path)

conn<-odbcConnect(dsn="localdb") 
DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Sales] FROM [RetailDemo2].[dbo].[SalesHistory] WHERE LocationID = ", DB_Path, sep="" ))
#DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Sales] FROM [Time2Work_MultiRetailAU].[dbo].[SalesHistory] WHERE LocationID = ", DB_Path, sep="" ))
odbcClose(conn)

if (nrow(DataResult)< 300 || is.na(sum(DataResult[, 2]))){
  conn<-odbcConnect(dsn="localdb") #
  DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Transactions] FROM [RetailDemo2].[dbo].[SalesHistory] WHERE LocationID = ", DB_Path, sep="" ))
  #DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Transactions] FROM [Time2Work_MultiRetailAU].[dbo].[SalesHistory] WHERE LocationID = ", DB_Path, sep="" ))
  odbcClose(conn)
  if(nrow(DataResult)<300 || is.na(sum(DataResult[, 2]))){
    print("Not Data OR SMALL DATA")
    next
  }
}
VendData.stor<-DataResult[order(DataResult$FinishTime), ]

temp3<-tapply(VendData.stor[,2], format(VendData.stor$FinishTime, "%Y-%m-%d"),sum)
InputData <- data.frame(Dates = as.Date(names(temp3)), Values = unname(temp3))


################## Start Testing
StartDate<- as.character(tail(InputData$Dates, n =1) +1)
FinishDate<- as.character(as.Date(StartDate) + 32)

######
FirstDate <- as.character(format(VendData.stor$FinishTime[1],"%Y-%m-%d"))
LastDate <- as.character(format(tail(VendData.stor$FinishTime, n=1),"%Y-%m-%d"))

##### Figure out the true start date, StartDate.T, which is the first day after LastDate
FinishDate.T <- FinishDate
if (as.Date(StartDate) > (as.Date(LastDate)+1)){
  StartDate.T <-as.character(as.Date(LastDate) +1)
}else{
  StartDate.T <-StartDate
}




#### Connect to DB and find corresponding holidays and Saved as ExceptionalDatesRight.csv
source(paste(RScriptPath, "/Holidays2LocationID.R", sep=""))
Holidays2LocationID(as.character(LocationIDResult$LocationID[i]), RScriptPath, DatabaseName)
ExceptionalDatesCSV <- "/ExceptionalDatesRight.csv"

#### Calculate Full exceptional days and proximity days
source(paste(RScriptPath, "/ExceptionalDayandEffectFormatV2.R", sep=""))
ExceptionalDayandEffects<-ExceptionalDayandEffectFormatV2(ExceptionalDatesCSV, FirstDate, FinishDate.T)



#### Connect to DB and find corresponding Opening and Closing hours, the Closing dates are saved in CloseDatesRight.csv
source(paste(RScriptPath, "/OpenCloseDayTime.R", sep=""))
OpenDayTime <- OpenCloseDayTime(FirstDate, FinishDate.T, as.character(LocationIDResult$LocationID[i]), RScriptPath, DatabaseName)
CloseDatesCSV<-"/CloseDatesRight.csv"
print(OpenDayTime)



#### Data Preprocessing
source(paste(RScriptPath, "/PreDataPrecessing_MissTransNormV6.R", sep=""))
XXX<-PreDataPrecessing_MissTransNormV6(FinishDate.T, InputData, RScriptPath, ExceptionalDayandEffects, CloseDatesCSV)
print(XXX)


#### Daily prediction
source(paste(RScriptPath, "/DailyPred_PostProcessingV3.R", sep=""))
YYYY<-DailyPred_PostProcessingV3(FinishDate.T, XXX, ExceptionalDayandEffects, RScriptPath, CloseDatesCSV)
print(YYYY)




#### plots
historys <- XXX[["OutputData"]]
N=length(historys$Values)
PastData <- ts(c(tail(historys$Values, n =N), rep(0, as.integer(as.Date(FinishDate.T)-as.Date(StartDate.T))+1)))
PredictData <- c(rep(0, length = N), YYYY$Rev2_Orig)

plot(PastData, type="o" ,col = "red", main=DB_Path,xaxt = "n")
lines(PredictData, type="o", pch = 22,  col = "blue" )
axis(1, at=1:length(PredictData), labels=as.character(seq(as.Date(FirstDate), as.Date(FinishDate.T), by = "1 day")))


###########
source(paste(RScriptPath, "/ExponentialCoeff.R", sep=""))
XXXX<-XXX[[6]]

HistoryInfo <-data.frame(Dates = XXXX$Dates, Items=XXXX$Values,  DayofWeek = weekdays(XXXX$Dates),
                         OpenFrom = OpenDayTime$OpenFrom[1: which(OpenDayTime$Dates == as.Date(LastDate))], 
                         OpenTo = OpenDayTime$OpenTo[1: which(OpenDayTime$Dates == as.Date(LastDate))], 
                         SD.Type = XXXX$SD.Type, PD.Type = XXXX$PD.Type, Outlier = XXXX$Outliers
                         ,stringsAsFactors=FALSE)

PredictInfor <-data.frame(Dates = YYYY$Dates, Items=YYYY$Rev2_Orig, DayofWeek = weekdays(YYYY$Dates),
                          OpenFrom = OpenDayTime$OpenFrom[which(OpenDayTime$Dates == as.Date(StartDate.T)): nrow(OpenDayTime)], 
                          OpenTo = OpenDayTime$OpenTo[which(OpenDayTime$Dates == as.Date(StartDate.T)): nrow(OpenDayTime)], 
                          SD.Type = YYYY$SD.Type, PD.Type = YYYY$PD.Type, Outlier = rep(FALSE, length = nrow(YYYY))
                          ,stringsAsFactors=FALSE)


HistoryAndPredictInfo <- rbind(HistoryInfo, PredictInfor)


### format hourly data
source(paste(RScriptPath, "/DataHoursV2.R", sep=""))
HistoryAndPredictHourlyInfo<-DataHoursV2(VendData.stor, col=2, FirstDate, FinishDate.T)





source(paste(RScriptPath, "/NormalIntradayPrediction.R", sep=""))
HistoryAndPredictHourlyInfo_updated <- NormalIntradayPrediction(HistoryAndPredictHourlyInfo, HistoryAndPredictInfo, PredictInfor)
  
CheckDay <- "Sunday"
plot(tail(HistoryAndPredictHourlyInfo_updated$Items[which(weekdays(as.Date(format(HistoryAndPredictHourlyInfo_updated$Time, "%Y-%m-%d"))) == CheckDay)], n = 400), type="o", col="blue")




}





# find one-step-ahead prediction index, which always use the updated last 4 weeks data from the last prediction
NormalPredictionDayInd <- which(is.na(PredictInfor$SD.Type) & is.na(PredictInfor$PD.Type) & (!PredictInfor$Outlier))
NormalHistoryAndPredictInfo <-
  HistoryAndPredictInfo[which(is.na(HistoryAndPredictInfo$SD.Type) & is.na(HistoryAndPredictInfo$PD.Type) & (!HistoryAndPredictInfo$Outlier)), ]

for (i in 1:length(NormalPredictionDayInd)){
  UpdatedSameNormalDayInd<-tryCatch(
    {temp<-tail(which((NormalHistoryAndPredictInfo$DayofWeek[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$DayofWeek[NormalPredictionDayInd[i]]) 
                                      & (NormalHistoryAndPredictInfo$OpenFrom[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$OpenFrom[NormalPredictionDayInd[i]])
                                      & (NormalHistoryAndPredictInfo$OpenTo[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$OpenTo[NormalPredictionDayInd[i]])),
                                n = 4)
    if (length(temp)==0){stop("No 100% matched history")}
    UpdatedSameNormalDayInd <- temp
    },
    error = function(err){ # release constraint if no history
      UpdatedSameNormalDayInd<-tail(which((NormalHistoryAndPredictInfo$DayofWeek[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$DayofWeek[NormalPredictionDayInd[i]])),
                                    n = 4)
      return(UpdatedSameNormalDayInd)
    }
  )
  
  ### Hour-to-day ratio of last 4 weeks
  Hour2DayRatioHistory <-matrix(rep(0, 24*length(UpdatedSameNormalDayInd)), nrow=length(UpdatedSameNormalDayInd), ncol=24)
  for (j in 1:length(UpdatedSameNormalDayInd)){
    UpdatedSameNormalHourInd.temp <- sort(which(format(HistoryAndPredictHourlyInfo$Time, "%Y-%m-%d") == as.character(NormalHistoryAndPredictInfo$Dates[UpdatedSameNormalDayInd[j]])))
    Hour2DayRatioHistory[j,] <- HistoryAndPredictHourlyInfo$Items[UpdatedSameNormalHourInd.temp]/ sum(HistoryAndPredictHourlyInfo$Items[UpdatedSameNormalHourInd.temp])
  }
  
  ### Exponianl smoothing is used to predict + update 
  HistoryAndPredictHourlyInfo$Items[sort(which(format(HistoryAndPredictHourlyInfo$Time, "%Y-%m-%d") == as.character(PredictInfor$Dates[NormalPredictionDayInd[i]])))]<- 
    PredictInfor$Items[PredictInfor$Dates==PredictInfor$Dates[NormalPredictionDayInd[i]]]*(ExponentialCoeff(length(UpdatedSameNormalDayInd), 0.40)%*%Hour2DayRatioHistory)
}


CheckDay <- "Tuesday"
plot(tail(HistoryAndPredictHourlyInfo$Items[which(weekdays(as.Date(format(HistoryAndPredictHourlyInfo$Time, "%Y-%m-%d"))) == CheckDay)], n = 400), type="o", col="blue")












################################################################################



conn<-odbcConnect(dsn="localdb") 
OpenDayResults<-sqlQuery(conn, "SELECT [RetailDemo2].[dbo].[OpeningHours].[OpenFrom]
                         ,[RetailDemo2].[dbo].[OpeningHours].[OpenTo]
                         ,[RetailDemo2].[dbo].[OpeningHours].[EffectiveFrom]
                         ,[RetailDemo2].[dbo].[OpeningHours].[EffectiveTo]
                         ,[RetailDemo2].[dbo].[OpeningHours].[DayOfWeek]
                         FROM [RetailDemo2].[dbo].[OpeningHours]
                         WHERE  [RetailDemo2].[dbo].[OpeningHours].[HoursType] =1
                         AND [RetailDemo2].[dbo].[OpeningHours].[LocationID] = 95;")
odbcClose(conn)

OpenDayResults$OpenFrom <- format(OpenDayResults$OpenFrom, "%H:%M:%S")
OpenDayResults$OpenTo <- format(OpenDayResults$OpenTo, "%H:%M:%S")
OpenDayResults$EffectiveFrom <- as.Date(format(OpenDayResults$EffectiveFrom, "%Y-%m-%d"))
OpenDayResults$EffectiveTo <- as.Date(format(OpenDayResults$EffectiveTo, "%Y-%m-%d"))

AllDates <- seq(as.Date(FirstDate), as.Date(FinishDate.T), by ="1 day")
OpenDayTime <-data.frame(Dates = AllDates, 
                         OpenFrom = rep("00:00:00", length=length(AllDates)),
                         OpenTo = rep("00:00:00", length=length(AllDates)), stringsAsFactors=FALSE)


source(paste(RScriptPath, "/TranslateDayofWeek.R", sep=""))

### Find regular working hours
RegularHourInd <-which(is.na(OpenDayResults$EffectiveTo))
for (i in 1:length(RegularHourInd)){
  RegularDates<-TranslateDayofWeek(OpenDayResults$DayOfWeek[RegularHourInd[i]])
  for (j in 1:length(RegularDates)){
    OpenDayTime$OpenFrom[which(weekdays(OpenDayTime$Dates)==RegularDates[j])] <-OpenDayResults$OpenFrom[RegularHourInd[i]]
    OpenDayTime$OpenTo[which(weekdays(OpenDayTime$Dates)==RegularDates[j])] <-OpenDayResults$OpenTo[RegularHourInd[i]]
  }
}

### Find irregular working hours
IregularHourInd <-which(! is.na(OpenDayResults$EffectiveTo))
for (i in 1:length(IregularHourInd)){
  IregularDatesInd<-TranslateDayofWeek(OpenDayResults$DayOfWeek[IregularHourInd[i]])
  IregularDates <- seq(OpenDayResults$EffectiveFrom[IregularHourInd[i]], OpenDayResults$EffectiveTo[IregularHourInd[i]], by="1 day")
  for (j in 1:length(IregularDates)){
    if(weekdays(IregularDates[j]) %in% IregularDatesInd){
      OpenDayTime$OpenFrom[which(OpenDayTime$Dates==IregularDates[j])] <- OpenDayResults$OpenFrom[IregularHourInd[i]]
      OpenDayTime$OpenTo[which(OpenDayTime$Dates==IregularDates[j])] <- OpenDayResults$OpenTo[IregularHourInd[i]]
    }
  }
}

### Find Closing Days

CloseDays <- OpenDayTime$Dates[which((OpenDayTime$OpenFrom=="00:00:00") &(OpenDayTime$OpenTo=="00:00:00") )]

