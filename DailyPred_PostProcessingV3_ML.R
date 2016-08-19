DailyPred_PostProcessingV3_ML<-function(FinishDateT, StartDateT, InputData, ExceptionalDayandEffects, CloseDays){
  # FristDateT, StartDateT = character
  # ExceptionalDayandEffects = list(ExceptionalDays, ProximityDays)
  #          where ExceptionalDays = data.frame(ExceptionalDate, Annual, ExceptionalDayTypeID)
  #          where ProximityDays = data.frame(Dates, Annual, ProximityDaysTypeID)
  # InputData= list(lambda, NewMax, NewMin, OldMax, OldMin, OutputData); 
  #          where InputData[[lambda]] is the Box-Cox transformation index
  #          where InputData[[NewMax]] is the Max after [0 1] normalization, or 1
  #          where InputData[[NewMin]] is the Min after [0 1] normalization, or 0
  #          where InputData[[OldMax]] is the Max after Box-Cox transformation
  #          where InputData[[OldMin]] is the Min after Box-Cox transformation
  #          where InputData[[OutputData]] is the data.frame(Dates, Values(WithoutMissing), SD.Annual, SD.Type, ProximityDays, PD.Type, CloseDays, Values_BoxCox(without Outliers), 
  #                                                       Outliers, X_coeff(OutliterCoeff), Values_Scale01(NormalizedValued))
  
  # Output = data.frame(Dates, Preds(between 0, 1), Rev2_BoxCox, SepcialDays, SD.Annual, 
  #                     SD.Type, CloseDays, Rev2_Orig(convert back to original format))
  
  
  #####################################################################################################
  ##########  I: Daily prediction using cleaned data
  ########## II: Scaling: from normalized [0, 1] to Box-Cox format
  ##########III: Add Exceptional Days and Closing day information
  ########## IV: Deal with Exceptional days: using annual history or SpecialDayTypeID to groupe data
  ##########  V: Convert back from BoxCox transformation to original format
  ########## VI: Deal with CloseDays. Change to 0 if it's a close day; and repalced by mean if it's not but = 0 
  #####################################################################################################  
  
  
 
  
  ##########  I: Daily prediction using cleaned data
  #1. Try TABTS prediction which takes account of  trend, sensons(7, 365.25), irregular components
  #2. Switch to ETS if raising warnings or errors
  
  CleanedData <- InputData[[6]]
  lg <- as.Date(FinishDateT)-as.Date(StartDateT) +1
  fit<-tryCatch(
    {
      if (nrow(CleanedData) > 366){
      day.ts<-msts(CleanedData$Values_Scale01, seasonal.periods=c(7,365.25))
      fit <-tbats(day.ts, use.box.cox=FALSE) 
      }else{
        day.ts<-ts(CleanedData$Values_Scale01, frequency = 7)
        fit <-ets(day.ts, damped = TRUE)
      }
    },
    warning = function(cond){
      day.ts<-ts(CleanedData$Values_Scale01, frequency = 7)
      fit <-ets(day.ts)  
      return(fit)
    },
    error = function(cond){
      day.ts<-ts(CleanedData$Values_Scale01, frequency = 7)
      fit <-ets(day.ts)  
      return(fit)
    }
  )
  day.pred <- forecast(fit, h=as.integer(lg))
  day.pred$mean[which(day.pred$mean<0)]<-0
  Daypred.temp<-day.pred$mean
  
  Daypred <- data.frame(Dates=seq(as.Date(StartDateT), as.Date(FinishDateT), by = "1 day"),
                        Preds=Daypred.temp)
  
  
  #print(Daypred)
  ########## II: Scaling: from normalized [0, 1] to Box-Cox format
  NewMax<-InputData[[2]]
  NewMin<-InputData[[3]]
  OldMax<-InputData[[4]]
  OldMin<-InputData[[5]]
  Daypred$Rev2_BoxCox <- (Daypred$Preds - NewMin)/(NewMax-NewMin)*(OldMax - OldMin) + OldMin
  
  
  
  
  
  ########## III: Add Exceptional Days and Closing day information
  ########## 1)   Complement the Exceptional Days to a clean, full(from 1st Day), chronological format
  ########## 2)   Closing day
  ########## 3)   Proximity day
  ########## 4)   Add information 
  
  
  ########## 1)   Complement the Exceptional Days to a clean, full(from 1st Day), chronological format
  ExceptionalDays <- ExceptionalDayandEffects[[1]]
  
  
  ########## 2)   Closing day
  #CloseDays<-read.csv(paste(RScriptPath, CloseDatesCSV, sep=""), header = TRUE)
  
  ########## 3)   Proximity day
  ProximityDays <- ExceptionalDayandEffects[[2]]
  
  
  ########## 4)   Add information 
  Daypred$SpecialDays<-rep(FALSE, length = as.integer(tail(Daypred$Dates, n=1)-Daypred$Dates[1]+1))
  Daypred$SD.Annual <- rep(FALSE, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  Daypred$SD.Type <- rep(NA, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  Daypred$ProximityDays <- rep(FALSE, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  Daypred$PD.Type <- rep(NA, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  Daypred$CloseDays<-rep(FALSE, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  
  for (i in 1:nrow(Daypred)){
    if (Daypred$Dates[i] %in% ExceptionalDays$ExceptionalDate){
      Daypred$SpecialDays[i] <- TRUE
      IndAnn <- which(ExceptionalDays$ExceptionalDate == Daypred$Dates[i])
      Daypred$SD.Annual[i] <- ExceptionalDays$Annual[IndAnn]
      Daypred$SD.Type[i] <- ExceptionalDays$ExceptionalDayTypeID[IndAnn]
    }
    
    if (Daypred$Dates[i] %in% ProximityDays$Dates){
      Daypred$ProximityDays[i] <- TRUE
      PDInd <- which(ProximityDays$Dates == Daypred$Dates[i])
      Daypred$PD.Type[i] <- ProximityDays$ProximityDaysTypeID[PDInd]
    }
    
    if (as.character(Daypred$Dates[i]) %in% as.character(CloseDays)){
      Daypred$CloseDays[i] <- TRUE
    }
  }
  
  
  ########## IV: Deal with Exceptional&Proximity days: using annual history or DayTypeID to groupe data
  # 0: Deal with Proximity days first, which can be replace by exceptional day replacement
  # 1: Find history Exceptional days
  # 2: Create exponential factors
  # 3: Replace exceptional days with history
  
  
  # 0: Deal with Proximity days first, which can be replace by exceptional day replacement
  Ind <- c()
  Ind <- which(Daypred$ProximityDays)
  if (length(Ind)>0){
    for (j in 1:length(Ind)){
      ProximityDayTypeID <- Daypred$PD.Type[Ind[j]]
      Hist.Dates <- sort(CleanedData$Dates[which(CleanedData$PD.Type == ProximityDayTypeID)])
      
      if (length(Hist.Dates)>0){
        ProximityDayCoeff<-ExponentialCoeff(length(Hist.Dates), 0.65)
        Daypred$Rev2_BoxCox[Ind[j]]<-sum(ProximityDayCoeff*(CleanedData$X_coeff[which(CleanedData$Dates %in% Hist.Dates)]*CleanedData$Values_BoxCox[which(CleanedData$Dates %in% Hist.Dates)]))
      }
    }
  }
  
  # Special days
  Hist.Dates<-c()
  Ind <- c()
  Ind <- which(Daypred$SpecialDays)
  if (length(Ind)>0){
    for (i in 1:length(Ind)){
      if ((Daypred$SD.Annual[Ind[i]]) & (nrow(Daypred)>365) ){ 
        #Deal with annually repeated special days
        Dates<-Daypred$Dates[Ind[i]]
        
        # 1: Find history Exceptional days
        #Hist.Dates <-c(Dates %m-% years(1) ) 
        Hist.Dates <- seq(Dates, length=2, by="-1 year")[2]
        while (Hist.Dates[1] > as.Date(FirstDate)){
          Hist.Dates <- c(seq(Hist.Dates[1], length=2, by="-1 year")[2], Hist.Dates) #Old to New, i.e.[2013, 2014, 2015]
        }
        if (Hist.Dates[1] < as.Date(FirstDate)){
          Hist.Dates <-Hist.Dates[2:length(Hist.Dates)]
        }
      }else { 
        #Otherwise use SpecialDayTypeID to group and forecast
        SpecialDayTypeID <- Daypred$SD.Type[Ind[i]]
        Hist.Dates <- sort(CleanedData$Dates[which(CleanedData$SD.Type == SpecialDayTypeID)])
      } 
      
      # 2: Create exponential factors and Update the forecast if there is history
      if (length(Hist.Dates)>0){
        ExceptionalDayCoeff<-ExponentialCoeff(length(Hist.Dates), 0.65)
        
        # 3: Replace exceptinal days with history
        Daypred$Rev2_BoxCox[Ind[i]]<-sum(ExceptionalDayCoeff*(CleanedData$X_coeff[which(CleanedData$Dates %in% Hist.Dates)]*CleanedData$Values_BoxCox[which(CleanedData$Dates %in% Hist.Dates)]))
      }
    }
  }
  
  
  ##########  V: Convert back from BoxCox transformation to original format
  Lambda <- InputData[[1]]
  Daypred$Rev2_Orig <- InvBoxCox(Daypred$Rev2_BoxCox,Lambda)
  Daypred$Rev2_Orig[which(Daypred$Rev2_Orig<0)]<-0
  
  
  
  ########## VI: Deal with CloseDays. Change to 0 if it's a close day; and repalced by mean if it's not but = 0 
  Daypred$Rev2_Orig[which(Daypred$CloseDays)]<-0
  for (i in 1:nrow(Daypred)){
    if ((!Daypred$CloseDays[i]) & (Daypred$Rev2_Orig[i]==0) & (!Daypred$SpecialDays[i]) ){
      Daypred$Rev2_Orig[i]<- mean(Daypred$Rev2_Orig) 
    }
  }
  
  
  return(Daypred)
}