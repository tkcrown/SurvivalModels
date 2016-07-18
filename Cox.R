require(survival)
require(intcox)

# source the common.R library
script.dir <- dirname(sys.frame(1)$ofile)
commonLibFileName <- paste(sep="/", script.dir, "common.R")
source(commonLibFileName)

FitModel <- function(survData, auxData){
# cox2 is a function that implements iterative convex minorant algorithm, 
    # Support data              delta
    #    left censored,         -1
    #    right censored,        0
    #    interval censored,     -2
    #    event                  1
# INPUT: same with cox function    
# Note: cannot deal with data with data with some feature being the linear combinations of other features 
    
    survData <- ConvertToIntervalData(survData)
    featureNames <- GetRealFeatureNames(survData)
    
    formula = as.formula(paste("Surv(left, right, type='interval2') ~", paste(featureNames, collapse="+"))) 
    coxModel <- intcox(formula, data = survData)
        
    return (coxModel)
}

GetSurvFunc <- function(x, coxModel)
{
    features <- c(as.numeric(x[!(colnames(x) %in% defaultIgnoreVarNames)]))
    survValues <- exp(-coxModel$lambda0) ^ exp(sum(unname(coxModel$coeff) * features))    
    timePnts <- coxModel$time
    
    if (coxModel$time.point[1] != 0)
    {
        survValues <- c(1, survValues)
        timePnts <- c(0, timePnts <- coxModel$time)
    }
    
    survValues <- c(survValues, 0)
    timePnts <- c(timePnts, .Machine$integer.max)
    
    return(list(survValues = survValues, timePnts = timePnts))
}

Likelihood <- function(x, t, coxModel)
{
# Calculate the probability mass function of person x at time t

    survFunc <- GetSurvFunc(x, coxModel)
    timePntIndex <- max(which(t >= survFunc$timePnts))
    prob <- survFunc$survValues[timePntIndex] - survFunc$survValues[timePntIndex + 1]

    return (max(prob, 1e-8))
}

SurvFuncValue <- function(x, t = -1, coxModel){
# Calculate the survival function value
    # if t = -1 return the whole curve for x
    survFunc <- GetSurvFunc(x, coxModel)
    
    if (t == -1)
    {
        return (data.frame(timePnts = survFunc$timePnts, survValues = survFunc$survValues))   
    }
    
    survValue <- survFunc$survValues[max(which(t >= time))]
    
    return (survValue)
}

PredictEventTime <- function(x, coxModel, flavor = "median")
{
# Predict the median survival time for subject x with the median value
    survFunc <- GetSurvFunc(x, coxModel)

    if (flavor == "median")
    {
        indx <- max(which(survFunc$survValues >= 0.5))   
    }
    else
    {
        indx <- 1 # not implemented yet
    }

    return (survFunc$timePnts[indx])
}

RemoveLinearDependency <- function(dataset)
{
# Remove the linear dependency in the dataset
    require(survival)
    featureNames <- GetRealFeatureNames(dataset)
    
    formula = as.formula(paste("Surv(time, delta) ~ ", paste(listVarNames, collapse="+")))
    cox.fit <- coxph(formula, data = dataset)
    dataset <- dataset[, c(TRUE, TRUE, !is.na(cox.fit$coefficients))]
    
    return(dataset)
}