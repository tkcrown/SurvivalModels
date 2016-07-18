Calibration <-function(testSet, survFunc, isKM = TRUE, survModel, nBin)
{
# return the calibration stats
# isKM: indicate whether it is a personalized model
# Only right centered and events are allowed right now

    scale <- seq(0, 1 - 1/nBin, 1/nBin)
    calBin <- rep(0, nBin)
    nSample <- nrow(testSet)
    
    calibMatrix <- vapply(testSet, function(x)
        {
            isEvent <- x$delta == 1
            
            s <- if (isKM) survFunc(testSet$time[i], survModel) 
                else survFunc(testSet[i, ], testSet$time[i], survModel)

            binIndx <- if (isEvent) max(which(s >= scale)) else which(s >= scale)
            weights <- if (isEvent == 1) 1 else 1/length(binIndx)

            calibVec <- rep(0, nBin)
            calibVec[binIndx] <- weights
            
            return (calibVec)
        }, vector(nBin))
    
    calibBins <- colSums(calibMatrix)
    assertthat::assert_that(length(calibBins) == nBin)
    return (calibBins)
}

LNLoss <- function(n = 1)
{
    return(function(testSet, predictFunc, isKM = TRUE, survModel)
    {
        errors <- vapply(testSet, function(x)
        {
            predVal <- if(isKM) predictFunc(survModel) 
            else predictFunc(testSet[i, ], survModel)
            l1Loss <- if (testSet$delta[i]) abs(testSet$time[i] - predVal)
            else max(0, testSet$time[i] - predVal)
            return(l1Loss^n)
        })

        return(means(errors))
    }) 
}

L1Loss <- LNLoss(1)

L2Loss <- LNLoss(2)

ConcordanceIndex <- function(testSet, predictFunc, markerOrder = TRUE, survModel)
{
    predVal <- vapply(testSet, function(x) predictFunc(x, survModel) , numeric(0))
    sortTime <- sort(testSet$time, index.return = TRUE)
    predVal  <- predVal[sortTime$ix]
    delta <- testSet$delta[sortTime$ix]
    nSample <- nrow(testSet)
		
    nTotalPair <- 0
    nInversion <- 0
    for (i in 1:nSample)
    {
        if (delta[i] == 0) next
        
        for (j in (i+1):nrow(testSet))
				{
            nTotalPair <- nTotalPair + 1
            if (predVal[i] < predVal[j])
                nInversion <- nInversion + 1
        }        
    }
    
    return(1 - nInversion/nTotalPair)
}

AUC <- function(labels, scores)
{
    result  <- sort(scores, index.return = TRUE)
    nSample <- length(scores)
    I <- result$ix
    M <- sum(labels == 1)
    N <- nSample - M    
    sigma <- 0
    
    for (i in seq(M+N,1,-1))
    {
        if (labels[I[i]] == 1)
            sigma <- sigma + i;
    }
    auc <- (sigma-(M+1)*M/2)/(M*N);
    return (auc)
}

TimeAuc <- function(timePoints, testSet, score)
{
    nTime <- length(timePoints)
    nSample <- nrow(testSet)
    timeAUC <- rep(0, nTime)

    timeAUCs <- vapply(timePoints, function(timePnt)
    {
        selected <- (testSet$delta == 1)|(testSet$delta == 0 & testSet$time >= t )
        tLabel <- (testSet$time[selected] <= t);
        tScore <- score[selected]
        timeAUC[i] <- AUC(tLabel, tScore)
	})
    
    for (i in 1:nTime)
    {
        t <- timePoints[i]    
    }
 
    return (timeAUC)
}
    