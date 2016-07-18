currentDir <- dirname(sys.frame(1)$ofile)

#### Options Begin ####
source(paste(sep="/", currentDir, "Options.R"))
#### Options End ####

# Load dataset
dataset <- read.table(file = dataSetFileName, header = TRUE,
                      sep = ",", quote = "\"",dec = ".", stringsAsFactors=TRUE, fill = TRUE, comment.char = "", 
                      na.strings = c(" ",""))
colnames(dataset)[c(1,2)] <- c("time", "delta")
dataset$delta <- as.numeric(dataset$delta == 0); # in survival package, 0: right censored, 1: event data
dataset$time <- dataset$time + 0.01

# Load Survival package
library("survival")

# Load Model library
source(paste(sep="/", currentDir, paste(modelName, ".R", sep = "")))
source(paste(sep="/", currentDir, "common.R"))
source(paste(sep="/", currentDir, "preprocess/common.R"))
source(paste(sep="/", currentDir, "measurements.R"))


################Data Preprocessing#####################
# Remove linear dependency among features (cause problems in cox, using intcox package)
if (modelName == "cox"){
    tpDataset <- imputeMissingFeature(dataset, dataset)
    tpDataset <- binarizeFactorVar(tpDataset)
    tpDataset <- removeLinDep(tpDataset)
    dataset <- binarizeFactorVar(dataset)
    dataset <- dataset[, colnames(tpDataset)]
}

# split into nFold folds
trainTestSetPairs <- SplitDatasetIntoTrainTestFold(dataset, nFold, type = "random")

trainTestSetPairs <- lapply(trainTestSetPairs, function(trainTestSetPair)
    {
        trainSet <- trainTestSetPair$trainSet
        trainSet <- trainTestSetPair$testSet
        
        # impute the missing values with the median value of the variable
        trainSet <- imputeMissingFeature(trainSet, trainSet)
        testSet <- imputeMissingFeature(trainSet, testSet)
        
        # remove features with all the same values
        trainSet <- removeZeroVarFeature(trainSet, trainSet)
        testSet <- removeZeroVarFeature(trainSet, testSet)

        # binarize the categorical features
        if (shouldBinarize == TRUE || modelName == "cox")
        {
            trainSet <- binarizeFactorVar(trainSet);
            testSet <- binarizeFactorVar(testSet);
        }
        # normalize dataset
        if (option$shouldNormalize){
            means <- colMeans(trainsets[[i]], na.rm = TRUE)
            sds <- sapply(trainsets[[i]], sd, na.rm = TRUE)    
            trainSet <- normalize(trainSet, means, sds)
            testSet  <- normalize(testSet, means, sds)  
        }
    })

modelOptions <- if (options$modelName == "aft") list(options$aftDistName) else list()

## Training
models <- lapply(trainTestSetPairs, function(trainTestSetPair)
    {
        FitModel(survData = trainTestSetPair$trainSet, modelOptions)
    })

## Testing
nSample <- nrow(dataset)
nBin <- 20
calibHist <- rep(0, nBin)
for (i in 1:nFold){
    calibHist <- 
        calibHist + 
        calibration(testsets[[i]], get(paste(modelName, "_surv", sep = "")), (modelName == "km"), model[[i]], nBin)    
}

calibStat <- sum((calibHist - rep(nSample/nBin, nBin))^2　/　(nSample/nBin))

print(calibStat)





