PreDataPrecessing_MissTransNormV6_ML<-function(FinishDateT, InputData, ExceptionalDayandEffects, CloseDays, RegularCloseDayofWeekCSV){
  # FristDate.T, LastDate.T = character 
  # ExceptionalDayandEffects = list(ExceptionalDays, ProximityDays)
  #          where ExceptionalDays = data.frame(ExceptionalDate, Annual, ExceptionalDayTypeID)
  #          where ProximityDays = data.frame(Dates, Annual, ProximityDaysTypeID)
  # InputData = data.frame(Dates(as.Date), Values)
  # Output = list(lambda, NewMax, NewMin, OldMax, OldMin, OutputData=data.frame); 
  #          where Output[[lambda]] is the Box-Cox transformation index
  #          where Output[[NewMax]] is the Max after [0 1] normalization, or 1
  #          where Output[[NewMin]] is the Min after [0 1] normalization, or 0
  #          where Output[[OldMax]] is the Max after Box-Cox transformation
  #          where Output[[OldMin]] is the Min after Box-Cox transformation
  #          where Output[[OutputData]] is the data.frame(Dates, Values(WithoutMissing), SD.Annual, SD.Types, ProximityDays, PD.Type, CloseDays, Values_BoxCox(without Outliers), 
  #                                                       Outliers, X_coeff(OutliterCoeff), Values_Scale01(NormalizedValued))
  
  
  
  ########### -I: Complement the Exceptional Days to a clean, full(from 1st Day), chronological format
  ExceptionalDays<-ExceptionalDayandEffects[[1]]
  ##########  0: Incorporate Proximity Day in a chronological format
  ProximityDays<- ExceptionalDayandEffects[[2]]
  
  print(ExceptionalDays)
  print(ProximityDays)
  ################### I: Format data started from FirstDate to LastDate
  #CloseDays<-read.csv(paste(RScriptPath, CloseDatesCSV, sep=""), header = TRUE)
  FirstDate <- as.character(InputData$Dates[1])
  LastDate <- as.character(tail(InputData$Dates, n=1))
  
  OutputData <- data.frame(Dates=seq(as.Date(FirstDate), as.Date(LastDate), by ="1 day"),
                           Values = rep(NA, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)), 
                           SpecialDays =rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           SD.Annual = rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           SD.Type = rep(NA, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           ProximityDays =rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           PD.Type = rep(NA, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           CloseDays =rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)))
  
  for (i in 1:nrow(OutputData)){
    if (OutputData$Dates[i] %in% InputData$Dates){
      OutputData$Values[i] <- InputData$Values[which(InputData$Dates == OutputData$Dates[i])]
    }
    if (OutputData$Dates[i] %in% ExceptionalDays$ExceptionalDate){
      OutputData$SpecialDays[i] <- TRUE
      IndAnn <- which(ExceptionalDays$ExceptionalDate == OutputData$Dates[i])
      OutputData$SD.Annual[i] <- ExceptionalDays$Annual[IndAnn]
      OutputData$SD.Type[i] <- ExceptionalDays$ExceptionalDayTypeID[IndAnn]
    }
    if (OutputData$Dates[i] %in% ProximityDays$Dates){
      OutputData$ProximityDays[i] <- TRUE
      PDInd <- which(ProximityDays$Dates == OutputData$Dates[i])
      OutputData$PD.Type[i] <- ProximityDays$ProximityDaysTypeID[PDInd]
    }
    if (as.factor(OutputData$Dates[i]) %in% CloseDays){
      OutputData$CloseDays[i] <- TRUE
    }
  }
  
  
  ################### II: Deal with missing values (or NAs)
  # 1) Initialization
  # 2) Fill in predicted value or 0 if it is a special day
  
  # 1) Initialization using first 3 weeks data
  wk <- 3
  for (i in 1:(wk*7)){
    if (is.na(OutputData$Values[i])&&(OutputData$SpecialDays[i] ||OutputData$CloseDays[i])){
      OutputData$Values[i] <-0
    }else if (is.na(OutputData$Values[i])){
      OutputData$Values[i] <- mean(OutputData$Values[1:(wk*7)], na.rm=TRUE)
    }
  }
  
  #2) Replacing the rest NAs with ETS or 0
  Ind<-which(is.na(OutputData$Values))
  if (length(Ind)>0){
    for (i in 1:length(Ind)){
      if (is.na(OutputData$Values[Ind[i]])&&(OutputData$SpecialDays[Ind[i]] ||OutputData$CloseDays[Ind[i]]) ){
        OutputData$Values[Ind[i]] <-0 #not data at a special day or close date, indicate 0 
      }else if (is.na(OutputData$Values[Ind[i]])){
        day.ts <- ts(OutputData$Values[(Ind[i]-wk*7):(Ind[i]-1)], frequency = 7)
        day.ets <- ets(day.ts)
        day.pred <- forecast(day.ets, h =1)
        OutputData$Values[Ind[i]] <- day.pred$mean
      }
    }  
  }
  
  ################# III: Data transformation, box-cox
  lmd<-BoxCox.lambda(OutputData$Values)
  OutputData$Values_BoxCox<-BoxCox(OutputData$Values, lmd)  
  
  
  ################## IV: Identify. Replace outliers and SpecialDay by prediction, not Regular closing days
  ##################     Quarterly Outliers & SpecialDay coeff are stored
  RegularCloseDayofWeek<-RegularCloseDayofWeekCSV
  
  OutputData$Outliers <- rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1))
  OutputData$X_coeff <-  rep(NA, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1))
  
  Data.temp <-data.frame(Dates=OutputData$Dates, Values=OutputData$Values_BoxCox)
  outliers <- unique(FindQtrOutliers(Data.temp))
  #outliers <- boxplot.stats(Data.temp$Values)$out  
  if (length(outliers)>0){
    for (i in 1:length(outliers)){
      OutputData$Outliers[which(OutputData$Values_BoxCox==outliers[i])] <- TRUE
    }
  }
  
  print(outliers)
  ################### Untick regular closing day
  if (length(RegularCloseDayofWeek)>0){
    for (j in 1:length(RegularCloseDayofWeek)){
      OutputData$Outliers[which(weekdays(OutputData$Dates)==as.character(RegularCloseDayofWeek$x[j]))] <- FALSE
    }
  }
  
  
  
  if ((length(outliers)>0) || (length(which(OutputData$SpecialDays)) >0) ||(length(which(OutputData$ProximityDays)) >0) ){
    Ind <-c()
    Ind<- sort(unique(c(which(OutputData$SpecialDays), which(OutputData$Outliers), which(OutputData$ProximityDays))))
    for (j in 1:length(Ind)){
      # Initialize if falls into first 3 weeks
      if (Ind[j] <=22){
        ReplaceValue <- mean(OutputData$Values_BoxCox[setdiff(c(1:22), Ind)], na.rm = T)
      }else{
        ReplaceValue<-tryCatch(
          {
            data.temp <- OutputData$Values_BoxCox[(Ind[j]-21):(Ind[j]-1)]
            day.ts<-ts(data.temp, frequency = 7)
            fit <-ets(day.ts) 
            ReplaceValuePred <- forecast(fit, h = 1)
            ReplaceValue <- ReplaceValuePred$mean
          },
          warning = function(cond){
            ReplaceValue <- mean(OutputData$Values_BoxCox)
            return(ReplaceValue)
          },
          error = function(cond){
            ReplaceValue <- mean(OutputData$Values_BoxCox)
            return(ReplaceValue)
          }
        )
      }
      OutputData$X_coeff[Ind[j]] <- OutputData$Values_BoxCox[Ind[j]]/ReplaceValue
      OutputData$Values_BoxCox[Ind[j]] <- ReplaceValue
    }
    
  }
  
  
  ################## V: Scaling: min-max normalization to [0, 1]
  new_max <- 1
  new_min <- 0 
  OutputData$Values_Scale01 <- new_min + (new_max-new_min)*
    (OutputData$Values_BoxCox - min(OutputData$Values_BoxCox))/(max(OutputData$Values_BoxCox) - min(OutputData$Values_BoxCox))
  
  return(list("lambda" = lmd, "NewMax"=new_max, "NewMin"=new_min,
              "OldMax" = max(OutputData$Values_BoxCox), "OldMin" = min(OutputData$Values_BoxCox),
              "OutputData" = OutputData))
}         
