FitModel <- function(survData, auxData){
# Kaplan Meier Estimator, which
    # Supports data              delta
    #    left censored,         -1
    #    right censored,        0
    #    interval censored,     -2
    #    event                  1
# Input: survData is a data.frame which has too variables
    # time: survival or censored time
    # delta: event indicator explaine above
    
    library("survival")
    # source the common.R library
    #script.dir <- dirname(sys.frame(1)$ofile)
    #file.name <- paste(sep="/", script.dir, "common.R")
    #source(file.name) 
        
    # convert dataset to left-right format
    survData <- ConvertToIntervalData(survData)
   
    # fit dataset with KM model
    kmModel <- survfit(Surv(left, right, type = c('interval2'))~1, survData)
    return(kmModel)
}

GetSurvFunc <- function(kmModel)
{
    surv <- kmModel$surv
    time <- kmModel$time
    
    if (kmModel$time[1] != 0)
    {
        surv <- c(1, kmModel$surv)
        time <- c(0, kmModel$time)
    }
    
    surv <- c(surv, 0)
    time <- c(time, .Machine$integer.max)
    
    return(list(timePnts = time, survFuncValues = surv))
}

Likelihood <- function(t, kmModel)
{
    #PMF

    kmModel <- GetSurvFunc(kmModel)
    
    indx <- max(which(t >= kmModel$timePnts))
    probability <- kmModel$survFuncValues[indx] - kmModel$survFuncValues[indx + 1]

    return (max(probability, 1e-8))
}

SurvFuncValue <-function(t, kmModel)
{
    kmModel <- GetSurvFunc(kmModel)
    s <- kmModel$survFuncValues[max(which(t >= kmModel$timePnts))]
    return (s)
}

PredictEventTime <- function(kmModel, flavor = "median")
{    
    if (flavor == "median")
    {
        indx <- max(which(kmModel$surv >= 0.5))
    }
    
    return (kmModel$time[indx])
}

