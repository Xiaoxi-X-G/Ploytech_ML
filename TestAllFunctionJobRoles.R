rm(list = ls())
setMKLthreads(2) # set number of cores
getMKLthreads() # number of threads, needs to be less than number of cores 

require(lubridate)

RScriptPath<-"C://Users/ptech3/Dropbox/Ploytech/Regression/AllFunctions"
DatabaseName<-"RetailDemo2"


######### link to drag all LocationID and corresponding SalesHistory
library(RODBC)
odbcDataSources()

conn<-odbcConnect(dsn="localdb") #
LocationAndJobRoleIDResult <- sqlQuery(conn, "SELECT DISTINCT [JobRoleID], [LocationID] FROM [RetailDemo2].[dbo].[SalesHistory] Order by LocationID;;")
odbcClose(conn)


RMS.Hour<-c();
RMS.Day <-c();

for (i in 1:nrow(LocationAndJobRoleIDResult)){
  if (is.na(LocationAndJobRoleIDResult$LocationID[i]) || 
      is.na(LocationAndJobRoleIDResult$JobRoleID[i])){next}
  
  JobRoleIDs <- LocationAndJobRoleIDResult$JobRoleID[i]
  
  DB_Path <- paste(LocationAndJobRoleIDResult$LocationID[i], ";", sep="")
  #print(DB_Path)
  
  conn<-odbcConnect(dsn="localdb") 
  DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Sales] FROM [RetailDemo2].[dbo].[SalesHistory] WHERE JobRoleID = ", JobRoleIDs, " And LocationID = ", DB_Path, sep="" ))
  #DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Sales] FROM [Time2Work_MultiRetailAU].[dbo].[SalesHistory] WHERE LocationID = ", DB_Path, sep="" ))
  odbcClose(conn)
  
  if (nrow(DataResult)< 300 || is.na(sum(DataResult[, 2]))){
    conn<-odbcConnect(dsn="localdb") #
    DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Transactions] FROM [RetailDemo2].[dbo].[SalesHistory] WHERE JobRoleID = ", JobRoleIDs, " And LocationID = ", DB_Path, sep="" ))
    #DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Transactions] FROM [Time2Work_MultiRetailAU].[dbo].[SalesHistory] WHERE LocationID = ", DB_Path, sep="" ))
    odbcClose(conn)
    if(nrow(DataResult)<300 || is.na(sum(DataResult[, 2]))){
      print("Not Data OR SMALL DATA")
      next
    }
  }
  
  VendData.stor.temp0<-DataResult[order(DataResult$FinishTime), ]
  VendData.stor.temp <- data.frame(Time = VendData.stor.temp0$FinishTime, Items  = VendData.stor.temp0$Transactions)
  
  ################## Start Testing, with MSE calcualted
  NoDays <-20
  StartDate<- "2015-10-11"
  FinishDate<- as.Date(StartDate) + NoDays - 1
  # StartDate<- as.character(as.Date(format(tail(VendData.stor.temp$Time, n =1), "%Y-%m-%d")) - NoDays+1)
  # FinishDate<- as.character(format(tail(VendData.stor.temp$Time, n =1), "%Y-%m-%d"))
  
  VendData.stor <- VendData.stor.temp[which(as.Date(format(VendData.stor.temp$Time, "%Y-%m-%d")) < as.Date(StartDate) ), ]
  
  temp3<-tapply(VendData.stor[,2], format(VendData.stor$Time, "%Y-%m-%d"),sum)
  InputData <- data.frame(Dates = as.Date(names(temp3)), Values = unname(temp3))
  
  
  ##### Figure out the true start date, StartDate.T, which is the first day after LastDate
  FirstDate <- as.character(format(VendData.stor$Time[1],"%Y-%m-%d"))
  LastDate <- as.character(format(tail(VendData.stor$Time, n=1),"%Y-%m-%d"))
  
  FinishDate.T <- FinishDate
  if (as.Date(StartDate) > (as.Date(LastDate)+1)){
    StartDate.T <-as.character(as.Date(LastDate) +1)
  }else{
    StartDate.T <-StartDate
  }
  NoDays <- as.integer(as.Date(FinishDate.T) - as.Date(StartDate.T) + 1)
  
  ##### testing data
  source(paste(RScriptPath, "/DataHoursV2.R", sep=""))
  VendData.stor.testing0 <- VendData.stor.temp[which((as.Date(format(VendData.stor.temp$Time, "%Y-%m-%d")) >= as.Date(StartDate) )
                                               & (as.Date(format(VendData.stor.temp$Time, "%Y-%m-%d")) <= as.Date(FinishDate.T))), ]
  VendData.stor.testing <- DataHoursV2(VendData.stor.testing0, col=2, StartDate.T, FinishDate.T)
  
  temp4 <-tapply(VendData.stor.testing[,2], format(VendData.stor.testing$Time, "%Y-%m-%d"),sum)
  InputData.testing <- data.frame(Dates = as.Date(names(temp4)), Values = unname(temp4))
  
  
  ######
  

  
  
  
  #### Connect to DB and find corresponding holidays and Saved as ExceptionalDatesRight.csv
  source(paste(RScriptPath, "/Holidays2LocationID.R", sep=""))
  Holidays2LocationID(as.character(LocationAndJobRoleIDResult$LocationID[i]), RScriptPath, DatabaseName)
  ExceptionalDatesCSV <- "/ExceptionalDatesRight.csv"
  
  
  #### Calculate Full exceptional days and proximity days
  source(paste(RScriptPath, "/ExceptionalDayandEffectFormatV2.R", sep=""))
  ExceptionalDayandEffects<-ExceptionalDayandEffectFormatV2(ExceptionalDatesCSV, FirstDate, FinishDate.T)
  
  
  
  #### Connect to DB and find corresponding Opening and Closing hours, the Closing dates are saved in CloseDatesRight.csv
  source(paste(RScriptPath, "/OpenCloseDayTime.R", sep=""))
  OpenDayTime <- OpenCloseDayTime(FirstDate, FinishDate.T, as.character(LocationAndJobRoleIDResult$LocationID[i]), RScriptPath, DatabaseName)
  CloseDatesCSV<-"/CloseDatesRight.csv"
  print(OpenDayTime)
  
  #write.csv(OpenDayTime, file = paste(RScriptPath, "/OpenDayTime_FromCSharp.csv", sep=""), row.names = F)
  
  
  #### Connect to DB and find corresponding Opening and Closing hours, the Closing dates are saved in CloseDatesRight.csv
  source(paste(RScriptPath, "/RegularCloseDayofWeek.R", sep=""))
  RegularCloseDayofWeek(as.character(LocationAndJobRoleIDResult$LocationID[i]), RScriptPath)
  RegularCloseDayofWeekCSV<-"/RegularCloseDayofWeek.csv"

  
  #### Data Preprocessing
  source(paste(RScriptPath, "/PreDataPrecessing_MissTransNormV6.R", sep=""))
  XXX<-PreDataPrecessing_MissTransNormV6(FinishDate.T, InputData, RScriptPath, ExceptionalDayandEffects, CloseDatesCSV, RegularCloseDayofWeekCSV)
  print(XXX)
  
  
  #### Daily prediction
  source(paste(RScriptPath, "/DailyPred_PostProcessingV3.R", sep=""))
  YYYY<-DailyPred_PostProcessingV3(FinishDate.T, XXX, ExceptionalDayandEffects, RScriptPath, CloseDatesCSV)
  print(YYYY)
  
  
  
  
  #### plots
  historys <- XXX[["OutputData"]]
  N=length(historys$Values)
  PastData <- ts(c(historys$Values, InputData.testing$Values))
  PredictData <- c(rep(0, length = length(historys$Values)), YYYY$Rev2_Orig)
  
  plot(PastData, type="o" ,col = "red", main=paste(DB_Path, as.character(JobRoleIDs), sep="-"), xaxt = "n")
  lines(PredictData, type="o", pch = 22,  col = "blue" )
  axis(1, at=1:length(PredictData), labels=as.character(seq(as.Date(FirstDate), as.Date(FinishDate.T), by = "1 day")))

  RMS.Day[i] <- sqrt(mean((InputData.testing$Values-as.vector(YYYY$Rev2_Orig))^2))
  print(RMS.Day)
  
  plot(InputData.testing$Values, type="o" ,col = "red", main=paste(DB_Path, as.character(JobRoleIDs), sep="-"), xaxt = "n")
  lines(as.vector(YYYY$Rev2_Orig), type="o", pch = 22,  col = "blue" )
  axis(1, at=1:length(InputData.testing$Values), labels=as.character(seq(as.Date(StartDate.T), as.Date(FinishDate.T), by = "1 day")))
  
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

  
  
  ### Normal intraday prediction
  source(paste(RScriptPath, "/NormalIntradayPrediction.R", sep=""))
  HistoryAndPredictHourlyInfo_updated <- NormalIntradayPrediction(HistoryAndPredictHourlyInfo, HistoryAndPredictInfo, PredictInfor)
  
  
  
  
  
  ### Abnormal intraday prediction
  source(paste(RScriptPath, "/AbnormalIntradayPrediction.R", sep=""))
  HistoryAndPredictHourlyInfo_updated2 <- AbnormalIntradayPrediction(HistoryAndPredictHourlyInfo_updated, HistoryAndPredictInfo, PredictInfor)
  
  #CheckDay <- "Saturday"
  #plot(tail(HistoryAndPredictHourlyInfo_updated2$Items[which(weekdays(as.Date(format(HistoryAndPredictHourlyInfo_updated2$Time, "%Y-%m-%d"))) == CheckDay)], n = 400), type="o", col="blue")
  
  
  #plot(tail(HistoryAndPredictHourlyInfo_updated2$Items, n = NoDays*24), type="o", col="blue")
  
  plot(VendData.stor.testing$Items, type="o" ,col = "red", main=paste(DB_Path, as.character(JobRoleIDs), sep="-"), xaxt = "n")
  lines(tail(HistoryAndPredictHourlyInfo_updated2$Items, n = NoDays*24), type="o", pch = 22,  col = "blue" )

  RMS.Hour[i] <- sqrt(mean((VendData.stor.testing$Items-tail(HistoryAndPredictHourlyInfo_updated2$Items, n = NoDays*24))^2))
  print(RMS.Hour)
}


