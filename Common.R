ConvertToIntervalData <- function(survData)
{
    # Support data              delta
    #    left censored,         -1
    #    right censored,        0
    #    interval censored,     -2
    #    event                  1
    # convert data format from (t, delta) to (left, right)
    
    nSample <- length(survData$time)
    survData$left = vector(mode = 'numeric', length = nSample)
    survData$right = vector(mode = 'numeric', length = nSample)
    
    survData$left[survData$delta == -1] = 0
    survData$left[survData$delta != -1] = survData$time[survData$delta != -1]
    
    survData$right[survData$delta == 0] = NA
    survData$right[survData$delta != 0] = survData$time[survData$delta != 0]
    # intcox does not support uncensored data, i.e. left = right
    survData$right = survData$right + 0.001

    return (survData)
}

defaultIgnoreVarNames <- c("time", "delta", "left", "right")

Normalize <- function(survData, means, sds, ignoreVarNames = defaultIgnoreVarNames)
{
    # normalize survData with mean being means and standard deviation being sds
    # delta and time will not be normalized
    # INPUT:
        # survData: a data frame
        # means: named num, contains the means of each column
        # sds: named num, contains the standard deviations of each column
    featureNames <- GetRealFeatureNames(survData)
    
    listVarNames <- listVarNames[toKeepBool]
    for (i in 1:length(listVarNames))
    {
        mu <- means[[listVarNames[i]]]
        delta <- sds[[listVarNames[i]]]
        if (delta > 1e-5)
        {
            survData[[listVarNames[i]]] <- (survData[[listVarNames[i]]] - mu)/delta   
        }
    }
    
    return (survData)
}

library("cvTools")
SplitDatasetIntoTrainTestFold <- function(dataset, nFold, type = "consecutive")
{
    # Split dataset into nFolds of (trainset, testset) pairs
    nSample <- nrow(dataset)
    cvObj <- cvFolds(nSample, K = nFold, type = type)

    foldIndices <- 1:nFold
    
    trainSets <- lapply(foldIndices, function(foldIndex){

        
        return (trainSet)
    })
    
    testSets <- lapply(foldIndices, function(foldIndex){
        sampleIndices <- cvObj$subsets[cvObj$which == i]
        testSet <- dataset[sampleIndices, ]
        rownames(testSet) <- NULL
        
        sampleIndices <- -cvObj$subsets[cvObj$which == i]
        trainSet <- dataset[sampleIndices, ]
        rownames(trainSet) <- NULL
        
        return (list(trainSet = trainSet, testSet = testSet))
    })

    return (list(trainset, testset, cvObj))
}

GetRealFeatureNames <- function(dataset, ignoreVarNames = defaultIgnoreVarNames)
{
    # Get the feature names of dataset, excluding some var names
    # dataset can be (1) a data frame (2) a single named vector

    AllVarNames <- names(dataset)
    featureNames <- AllVarNames[!(AllVarNames %in% ignoreVarNames)]
    
    return(featureNames)
}

GetRealFeatures <- function(vars, ignoreVarNames = defaultIgnoreVarNames)
{
    # vars is a named vector or data frame
    features <- vars[[GetRealFeatureNames(vars, ignoreVarNames)]]

    return(features)
}




