rm(list = ls())

ptm <- proc.time()
library(plyr)
require(forecast)
require(MASS)

RScriptPath <- "C://Users/ptech3/Dropbox/Ploytech/Regression/AzureML/AZureML/upload2ML_Allfunction"
DataPath <- "C://Users/ptech3/Dropbox/Ploytech/Regression/AzureML/AZureML/AllData"

############## Load All R functions
source(paste(RScriptPath,"/DataHoursV2_ML.R", sep=""))
source(paste(RScriptPath,"/ExceptionalDayandEffectFormatV2_ML.R", sep=""))
source(paste(RScriptPath,"/FindQtrOutliers.R", sep=""))
source(paste(RScriptPath,"/PreDataPrecessing_MissTransNormV6_ML.R", sep=""))
source(paste(RScriptPath,"/ExponentialCoeff.R", sep=""))
source(paste(RScriptPath,"/DailyPred_PostProcessingV3_ML.R", sep=""))
source(paste(RScriptPath,"/NormalIntradayPrediction_ML.R", sep=""))
source(paste(RScriptPath,"/AbnormalIntradayPrediction_ML.R", sep=""))
source(paste(RScriptPath,"/RegularCloseDayofWeek_MLV2.R", sep=""))




StartDate <- "2016-01-01"
FinishDate <- "2016-01-21"


############################## Load all Data in the same format as AZure ML
salesHistories <- read.csv(paste(DataPath, "/salesHistoriesTest.csv", sep=""),header = T)

if (nrow(salesHistories)==0){
  PredictionResults <- data.frame(Time = NA, Items = NA)
  
}else{

  ExceptionalDates <- read.csv(paste(DataPath, "/ExceptionalDatesTest.csv", sep="") ,header = T) # class: data.frame
  openingHours2 <- read.csv(paste(DataPath, "/openingHoursTest.csv", sep=""),header = T)
  
  openingHours222 <- data.frame(Dates = as.Date(as.POSIXct(openingHours2$OpenFrom, origin = "1970-01-01", tz="GMT")),
                                OpenFrom =  format(as.POSIXct(openingHours2$OpenFrom, origin = "1970-01-01", tz="GMT"), "%H:%M:%S"),
                                OpenTo = format(as.POSIXct(openingHours2$OpenTo, origin = "1970-01-01", tz="GMT"), "%H:%M:%S"))
  
  StartFinishDate <- data.frame(StartDate, FinishDate)
  write.csv(StartFinishDate, file = paste(DataPath, "/StartFinishDateTest.csv", sep=""), row.names = F)
  ExceptionalDatesOpeningHours <-   rbind.fill(ExceptionalDates, openingHours222)
  
  
  
  OtherInfor <-  rbind.fill(StartFinishDate ,ExceptionalDatesOpeningHours)
  
  StartDate <- as.character(OtherInfor$StartDate[1])
  FinishDate <- as.character(OtherInfor$FinishDate[1])
  
  ExceptionalDatesCSV <- OtherInfor[c(!is.na(OtherInfor$ExceptionalDate)), c(3:7)]
  ExceptionalDatesCSV$ExceptionalDate <- format(as.POSIXct(ExceptionalDatesCSV$ExceptionalDate, origin = "1970-01-01", tz="GMT"), "%Y-%m-%d");
  
  OpenDayResults <- OtherInfor[c(!is.na(OtherInfor$OpenFrom)), c(8:10)]
  OpenDayResults$Dates <- as.Date(OpenDayResults$Dates)
  
  salesHistories$FinishTime <- as.POSIXct(salesHistories$FinishTime, origin = "1970-01-01", tz="GMT")
  
  VendData.stor.temp0 <- salesHistories[order(salesHistories$FinishTime), ] 
  VendData.stor.temp0$FinishTime <- as.POSIXct(VendData.stor.temp0$FinishTime, origin = "1970-01-01", tz="GMT") # Order changes DayTime format to Epoch
  
  
  
  ################## Daily aggregation
  temp3 <- tapply(VendData.stor.temp0$Transactions, format(VendData.stor.temp0$FinishTime, "%Y-%m-%d"), sum)
  InputData  <- data.frame(Dates = as.Date(names(temp3)), Values = unname(temp3))
  
  ######
  FirstDate <- as.character(format(VendData.stor.temp0$FinishTime[1],"%Y-%m-%d"))
  LastDate <- as.character(format(tail(VendData.stor.temp0$FinishTime, n=1),"%Y-%m-%d"))
  
  
  
  ##############################################################################################################
  ###  I: Reset the StartDate to the first unavailiable day
  FinishDateT <- FinishDate
  if (as.Date(StartDate) > (as.Date(LastDate)+1)){
    StartDateT <-as.character(as.Date(LastDate) +1)
  }else{
    StartDateT <-StartDate
  }
  
  # print("Start and Finish Date")
  # print(StartDateT)
  # print(FinishDateT) 
  # 
  # print("First and Last Date")
  # print(FirstDate)
  # print(LastDate)
  
  if ((as.integer(as.Date(StartDateT) - as.Date(FirstDate)) <= 8*7) || (as.integer(as.Date(FinishDateT) - as.Date(StartDateT)) <= 0)
      || (as.integer(as.Date(LastDate) - as.Date(FirstDate)) <= 8*7)){ # at least 8 weeks data are require 
    
    PredictionResults <- data.frame(Time = NA, Items = NA)
    
  }else{
    ##############################################################################################################
    ### II: Calculate Full exceptional days and proximity days
    ExceptionalDays <- ExceptionalDatesCSV
    ExceptionalDayandEffects<-ExceptionalDayandEffectFormatV2_ML(ExceptionalDatesCSV, FirstDate, FinishDateT)
    print(ExceptionalDayandEffects)
    
    
    
    ##############################################################################################################
    ###III: Find opening and closing day-time from first day in history to last day of prediction
    ### Change format to dataframe
    
    #OpenDayTime12 <- OpenCloseDayTime_ML(FirstDate, FinishDateT, OpenDayResults)
    # OpenDayTime12 <- OpenCloseDayTime(FirstDate, FinishDateT, OpenDayResults)
    # OpenDayResults = data.frame(col1 = OpenFrom, col2=OpenTo, col3 = EffectiveFrom, col4=EffectiveTo, col5= DayOfWeek)
    
    #CloseDays <- OpenDayTime12[[1]]
    #OpenDayTime <- OpenDayTime12[[2]]
    
    OpenDayTime <- OpenDayResults
    OpenDayTime <- OpenDayTime[which(OpenDayTime$Dates >= as.Date(FirstDate)),]
    CloseDays <- OpenDayTime$Dates[which((OpenDayTime$OpenFrom=="00:00:00") &(OpenDayTime$OpenTo=="00:00:00") )]
    OpenDayTime$Dates <- as.Date(OpenDayTime$Dates)
    
    ##############################################################################################################
    ### IV: Find weekly closing day 
    NoOfWeek <- (as.integer(as.Date(LastDate) - as.Date(FirstDate)) / 7)
    RegularCloseDayofWeekCSV <- RegularCloseDayofWeek_MLV2(CloseDays, NoOfWeek)
    
    
    ##############################################################################################################
    ### V: Data Preprocessing
    XXX <- PreDataPrecessing_MissTransNormV6_ML(FinishDateT, InputData, ExceptionalDayandEffects, CloseDays, RegularCloseDayofWeekCSV)# FristDate.T, LastDate.T = character 
    
    
    
    ##############################################################################################################
    ### VI: Daily prediction
    YYYY <- DailyPred_PostProcessingV3_ML(FinishDateT, XXX, ExceptionalDayandEffects, CloseDays)
    
    
    if (nrow(OpenDayTime) == 0){ #### No Openinghours information, only output daily forecasting
      
      PredictionResults.temp <- data.frame(Time = YYYY$Dates, Items=YYYY$Rev2_Orig)
      PredictionResults <- PredictionResults.temp[which(PredictionResults.temp$Time >= as.Date(StartDate)),]
      
    }else{
      
      ##############################################################################################################
      ### VII: Initialization intraday prediction
      XXXX<-XXX[[6]]
      
      HistoryInfo <-data.frame(Dates = XXXX$Dates, Items=XXXX$Values,  DayofWeek = weekdays(XXXX$Dates),
                               OpenFrom = OpenDayTime$OpenFrom[1: which(OpenDayTime$Dates == as.Date(LastDate))], 
                               OpenTo = OpenDayTime$OpenTo[1: which(OpenDayTime$Dates == as.Date(LastDate))], 
                               SD.Type = XXXX$SD.Type, PD.Type = XXXX$PD.Type, Outlier = XXXX$Outliers
                               ,stringsAsFactors=FALSE)
      
      PredictInfor <-data.frame(Dates = YYYY$Dates, Items=YYYY$Rev2_Orig, DayofWeek = weekdays(YYYY$Dates),
                                OpenFrom = OpenDayTime$OpenFrom[which(OpenDayTime$Dates == as.Date(StartDateT)): nrow(OpenDayTime)], 
                                OpenTo = OpenDayTime$OpenTo[which(OpenDayTime$Dates == as.Date(StartDateT)): nrow(OpenDayTime)], 
                                SD.Type = YYYY$SD.Type, PD.Type = YYYY$PD.Type, Outlier = rep(FALSE, length = nrow(YYYY))
                                ,stringsAsFactors=FALSE)
      
      HistoryAndPredictInfo <- rbind(HistoryInfo, PredictInfor) 
      
      
      ### format hourly data
      HistoryAndPredictHourlyInfo<-DataHoursV2_ML(VendData.stor, col=2, FirstDate, FinishDateT)
      ## VendData.stor is a dataframe =(FinishTime=as.POSIXct, Items = items/Tran, )
      ## col =  indicates what term to analyse
      ## FirstDate = character
      
      
      
      
      ##############################################################################################################
      ### VIII: Normal intraday prediction
      HistoryAndPredictHourlyInfo_updated <- NormalIntradayPrediction_ML(HistoryAndPredictHourlyInfo, HistoryAndPredictInfo, PredictInfor)
      # HistoryAndPredictHourlyInfo = data.frame(Time, Items)
      # HistoryAndPredictInfo = data.frame(Dates, Items, DayofWeek, OpenFrom, OpenTo, SD.Type, PD.Type, Outlier)
      # PredictInfor = data.frame(Dates, Items, DayofWeek, OpenFrom, OpenTo, SD.Type, PD.Type, Outlier)
      # Output = updated HistoryAndPredictHourlyInfo
      
      
      
      
      ##############################################################################################################
      ### IX: Abnormal intraday prediction
      HistoryAndPredictHourlyInfo_updated2 <- AbnormalIntradayPrediction_ML(HistoryAndPredictHourlyInfo_updated, HistoryAndPredictInfo, PredictInfor)
      # HistoryAndPredictHourlyInfo_updated = data.frame(Time, Items)
      # HistoryAndPredictInfo = data.frame(Dates, Items, DayofWeek, OpenFrom, OpenTo, SD.Type, PD.Type, Outlier)
      # PredictInfor = data.frame(Dates, Items, DayofWeek, OpenFrom, OpenTo, SD.Type, PD.Type, Outlier)
      # Output = updated HistoryAndPredictHourlyInfo data.frame(col1 = Time,  clo2 = Item)
      
      
      PredictionResults <- tail(HistoryAndPredictHourlyInfo_updated2, n = (24*as.integer(1+as.Date(FinishDateT)- as.Date(StartDate))))
    }
  }
}
proc.time() - ptm