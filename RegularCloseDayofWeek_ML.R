RegularCloseDayofWeek_ML <-function(OpenDayResults){
  ###2. Find Regular Closing date and exclude from input


  ###I.2. Find Regular Closing date and exclude from input
  if (nrow(OpenDayResults)==0){
    Closedayofweek <-c()
  }else{
    
    RegularDates <- c()
    RegularHourInd <-which(OpenDayResults$EffectiveTo == "NA")
    if (length(RegularHourInd)>0){
      for (i in 1:length(RegularHourInd)){
        RegularDates<-c(RegularDates, TranslateDayofWeek(OpenDayResults$DayOfWeek[RegularHourInd[i]]))
      }
    }
    Closedayofweek <- setdiff(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), unique(RegularDates))
  }
   
  
  return (Closedayofweek)
}