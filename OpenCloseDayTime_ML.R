OpenCloseDayTime_ML<-function(FirstDate, FinishDateT, OpenDayResults){
  # Pull data from DataBase with the Right LocationID
  # CloseDatesRight.csv is save in RScriptPath
  # Output = OpenDayTime, dataframe(Dates, OpenFrom, OpenTo)
  
  
  #########################################################################################
  ###1. Pull data from DataBase with the Right LocationID
  ###2. Find regular working hours
  ###3. Find irregular working hours
  ###4. Find Closing Days and save as CloseDatesRight.csv
  #########################################################################################
  
  #source(paste(RScriptPath, "/TranslateDayofWeek.R", sep=""))
  
  
  if (nrow(OpenDayResults)==0){
    CloseDays <- NA
    AllDates <- seq(as.Date(FirstDate), as.Date(FinishDateT), by ="1 day")
    OpenDayTime <-data.frame(Dates = AllDates, 
                             OpenFrom = rep(NA, length=length(AllDates)),
                             OpenTo = rep(NA, length=length(AllDates)), stringsAsFactors=FALSE)
    
    #print(OpenDayTime)                          
  }else{
    
    #print(class(OpenDayResults$EffectiveFrom))
    #print(class(OpenDayResults$EffectiveTo))
    OpenDayResults$OpenFrom <- format(OpenDayResults$OpenFrom, "%H:%M:%S")
    OpenDayResults$OpenTo <- format(OpenDayResults$OpenTo, "%H:%M:%S")
    #OpenDayResults$EffectiveFrom <- as.Date(format(OpenDayResults$EffectiveFrom, "%Y-%m-%d"))
    #OpenDayResults$EffectiveTo <- as.factor(format(OpenDayResults$EffectiveTo, "%Y-%m-%d"))
    
    ###2. Find regular working hours
    AllDates <- seq(as.Date(FirstDate), as.Date(FinishDateT), by ="1 day")
    OpenDayTime <-data.frame(Dates = AllDates, 
                             OpenFrom = rep("00:00:00", length=length(AllDates)),
                             OpenTo = rep("00:00:00", length=length(AllDates)), stringsAsFactors=FALSE)
    
    #print(OpenDayResults)
    #print(which(OpenDayResults$EffectiveTo=="NA"))
    RegularHourInd <-which(OpenDayResults$EffectiveTo=="NA")
    
    for (i in 1:length(RegularHourInd)){
      RegularDates<-TranslateDayofWeek(OpenDayResults$DayOfWeek[RegularHourInd[i]])
      print(RegularDates)
      for (j in 1:length(RegularDates)){
        OpenDayTime$OpenFrom[which(weekdays(OpenDayTime$Dates)==RegularDates[j])] <-OpenDayResults$OpenFrom[RegularHourInd[i]]
        OpenDayTime$OpenTo[which(weekdays(OpenDayTime$Dates)==RegularDates[j])] <-OpenDayResults$OpenTo[RegularHourInd[i]]
      }
    }
    
    
    IregularHourInd <-which(OpenDayResults$EffectiveTo != "NA")
    for (i in 1:length(IregularHourInd)){
      IregularDatesInd<-TranslateDayofWeek(OpenDayResults$DayOfWeek[IregularHourInd[i]])
      IregularDates <- seq(as.Date(OpenDayResults$EffectiveFrom[IregularHourInd[i]]), as.Date(OpenDayResults$EffectiveTo[IregularHourInd[i]]), by="1 day")
      for (j in 1:length(IregularDates)){
        if(weekdays(IregularDates[j]) %in% IregularDatesInd){
          OpenDayTime$OpenFrom[which(OpenDayTime$Dates==IregularDates[j])] <- OpenDayResults$OpenFrom[IregularHourInd[i]]
          OpenDayTime$OpenTo[which(OpenDayTime$Dates==IregularDates[j])] <- OpenDayResults$OpenTo[IregularHourInd[i]]
        }
      }
    }
    
    ###4. Find Closing Days and save as CloseDatesRight.csv
    CloseDays <- OpenDayTime$Dates[which((OpenDayTime$OpenFrom=="00:00:00") &(OpenDayTime$OpenTo=="00:00:00") )]
    #print(IregularHourInd)
    #print(CloseDays)   
  }
  return(list(CloseDays,OpenDayTime)) 
}