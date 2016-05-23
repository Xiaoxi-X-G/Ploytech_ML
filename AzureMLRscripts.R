rm(list = ls())

library(plyr)
require(forecast)
require(MASS)
source("C://Users/ptech3/Desktop/AZureML/upload2ML_Allfunction/DataHoursV2_ML.R")
source("C://Users/ptech3/Desktop/AZureML/upload2ML_Allfunction/ExceptionalDayandEffectFormatV2_ML.R")
source("C://Users/ptech3/Desktop/AZureML/upload2ML_Allfunction/TranslateDayofWeek.R")
source("C://Users/ptech3/Desktop/AZureML/upload2ML_Allfunction/FindQtrOutliers.R")
source("C://Users/ptech3/Dropbox/Ploytech/Regression/AzureML/ML/OpenCloseDayTime_ML_InR.R")
source("C://Users/ptech3/Dropbox/Ploytech/Regression/AzureML/ML/RegularCloseDayofWeek_ML_InR.R")
source("C://Users/ptech3/Desktop/AZureML/upload2ML_Allfunction/PreDataPrecessing_MissTransNormV6_ML.R")
source("C://Users/ptech3/Desktop/AZureML/upload2ML_Allfunction/ExponentialCoeff.R")
source("C://Users/ptech3/Desktop/AZureML/upload2ML_Allfunction/DailyPred_PostProcessingV3_ML.R")



StartDate <- "2015-12-02"
FinishDate <- "2015-12-31"

salesHistories <- read.csv("C://Users/ptech3/Desktop/AZureML/salesHistoriesCSV.csv",header = T)
ExceptionalDates <- read.csv("C://Users/ptech3/Desktop/AZureML/ExceptionalDates.csv",header = T) # class: data.frame
openingHours <- read.csv("C://Users/ptech3/Desktop/AZureML/openingHoursCSV_right.csv",header = T)
StartFinishDate <- data.frame(StartDate, FinishDate)
ExceptionalDatesOpeningHours <-   rbind.fill(ExceptionalDates, openingHours)



OtherInfor <-  rbind.fill(StartFinishDate ,ExceptionalDatesOpeningHours)

StartDate <- as.character(OtherInfor$StartDate[1])
FinishDate <- as.character(OtherInfor$FinishDate[1])

ExceptionalDatesCSV <- OtherInfor[c(!is.na(OtherInfor$ExceptionalDate)), c(3:7)]
ExceptionalDatesCSV$ExceptionalDate <- format(as.POSIXct(ExceptionalDatesCSV$ExceptionalDate, origin = "1970-01-01", tz="GMT"), "%Y-%m-%d");

OpenDayResults <- OtherInfor[c(!is.na(OtherInfor$OpenFrom)), c(8:12)]
OpenDayResults$OpenFrom <- as.POSIXct(OpenDayResults$OpenFrom, origin = "1970-01-01", tz="GMT")
OpenDayResults$OpenTo <- as.POSIXct(OpenDayResults$OpenTo, origin = "1970-01-01", tz="GMT")
OpenDayResults$EffectiveFrom <- format(as.POSIXct(OpenDayResults$EffectiveFrom, origin = "1970-01-01", tz="GMT"), "%Y-%m-%d")



salesHistories$FinishTime <- as.POSIXct(salesHistories$FinishTime, origin = "1970-01-01", tz="GMT")

VendData.stor.temp0 <- salesHistories[order(salesHistories$FinishTime), ] 
VendData.stor.temp0$FinishTime <- as.POSIXct(VendData.stor.temp0$FinishTime, origin = "1970-01-01", tz="GMT") # Order changes DayTime format to Epoch



#VendData.stor.temp<- DataHoursV2_ML(VendData.stor.temp0, col=2, format(VendData.stor.temp0$FinishTime[1], "%Y-%m-%d"), format(tail(VendData.stor.temp0$FinishTime,n=1), "%Y-%m-%d"))

#### Daily aggregation
#temp3 <- tapply(VendData.stor.temp[,2], format(VendData.stor.temp[,1], "%Y-%m-%d"), sum)
temp3 <- tapply(VendData.stor.temp0$Transactions, format(VendData.stor.temp0$FinishTime, "%Y-%m-%d"), sum)
InputData  <- data.frame(Dates = as.Date(names(temp3)), Values = unname(temp3))

######
#FirstDate <- as.character(format(VendData.stor.temp$Time[1],"%Y-%m-%d"))
#LastDate <- as.character(format(tail(VendData.stor.temp$Time, n=1),"%Y-%m-%d"))
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

print("Start and Finish Date")
print(StartDateT)
print(FinishDateT) 

print("First and Last Date")
print(FirstDate)
print(LastDate)

##############################################################################################################
### II: Calculate Full exceptional days and proximity days
ExceptionalDays <- ExceptionalDatesCSV
ExceptionalDayandEffects<-ExceptionalDayandEffectFormatV2_ML(ExceptionalDatesCSV, FirstDate, FinishDateT)
print(ExceptionalDayandEffects)


##############################################################################################################
###III: Find opening and closing day-time from first day in history to last day of prediction
### Change format to dataframe

OpenDayTime12 <- OpenCloseDayTime_ML_InR(FirstDate, FinishDateT, OpenDayResults)
# OpenDayTime12 <- OpenCloseDayTime(FirstDate, FinishDateT, OpenDayResults)
# OpenDayResults = data.frame(col1 = OpenFrom, col2=OpenTo, col3 = EffectiveFrom, col4=EffectiveTo, col5= DayOfWeek)

CloseDays <- OpenDayTime12[[1]]
OpenDayTime <- OpenDayTime12[[2]]


##############################################################################################################
### IV: Find weekly closing day 
RegularCloseDayofWeekCSV <- RegularCloseDayofWeek_ML_InR(OpenDayResults)




##############################################################################################################
### V: Data Preprocessing
XXX <- PreDataPrecessing_MissTransNormV6_ML(FinishDateT, InputData, ExceptionalDayandEffects, CloseDays, RegularCloseDayofWeekCSV)# FristDate.T, LastDate.T = character 



##############################################################################################################
### VI: Daily prediction
YYYY <- DailyPred_PostProcessingV3_ML(FinishDateT, XXX, ExceptionalDayandEffects, CloseDays)




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