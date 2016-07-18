library(survival)    
# source the common.R library
script.dir <- dirname(sys.frame(1)$ofile)
commonLibFileName <- paste(sep="/", script.dir, "Common.R")
source(commonLibFileName)

FitModel <-function(survData, auxData)
{
# aft is a function that implements accelerated failure time model, which support
    # Support data              delta
    #    left censored,         -1
    #    right censored,        0
    #    interval censored,     -2
    #    event                  1
# Input: 
    #survData is a data.frame with two variables 
        # time: event time or censoring time, a numeric value
        # delta: event indicator, 1: event, 0: censoring    
    # dist(auxData): indicated the distribution you want use

    featureNames <- GetFeatureNames(survData)
    survData <- ConvertToIntervalData(survData)
    
    formula = as.formula(paste("Surv(left, right, type='interval2') ~", paste(featureNames, collapse="+")))            
    aftModel <- survreg(formula, dist = auxData[[1]], data = survData)
    aftModel$distName = auxData[[1]]

    return (aftModel)
}

Likelihood <-function(x, t, aftModel)
{
    # Continuos likelihood: likelihood of person x dying at time t
    # Formula: log T = X * Theta + scale * err
    
    theta <- unname(aftModel$coeff)
    features <- c(1, as.numeric(x[!(colnames(x) %in% defaultIgnoreVarNames)])) # with intercept
    mu <- sum(theta * features)
    likelihood <- dsurvreg(t, mean = mu, scale = aftModel$scale, distribution = aftModel$distName)
    
    return (likelihood)
}

SurvFuncValue <- function(x, t, aftModel)
{
    # Survival function value of person x at time t

    theta <- unname(aftModel$coeff)
    features <- c(1, as.numeric(x[!(colnames(x) %in% defaultIgnoreVarNames)])) # with intercept
    mu <- sum(theta * features)
    survFuncValue <- 1 - psurvreg(t, mean = mu, scale = aftModel$scale, distribution = aftModel$distName)

    return (survFuncValue)
}

PredictEventTime <- function(x, aftModel, flavor = "median")
{
    # The predicted median death time of person x
    
    if (flavor == "median")
    {
        pred <- predict(aftModel, newdata = x, type="quantile", p = 0.50)    
    }
    
    return (unname(pred))
}

HazardFuncValue <- function(x, t, aftModel)
{
    # Hazard function value of person x at time t    
    # not completed
    
    mu <- - sum(unname(aftModel$coeff) * c(1, x))
    h <- dsurvreg(t, mean = mu, scale = aftModel$scale, distribution = aftModel$distName)/
        (1-psurvreg(t, mean = mu, scale = aftModel$scale, distribution = aftModel$distName))
    return (h)
}