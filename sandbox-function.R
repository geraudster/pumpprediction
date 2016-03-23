source('common.R')

library(plyr)
library(caret)

# Data preparation
trainset.values <- read.csv('trainset_values.csv', na.strings = "")
trainset.labels <- read.csv('trainset_labels.csv', na.strings = "")
testset.values <- read.csv('testset_values.csv', na.strings = "")

trainset.values <- prepareDataSet(trainset.values, 'train')
testset.values <- prepareDataSet(testset.values, 'test')

impute <- imputeOnTrainAndTestset(trainset.values, testset.values)
trainsetImpute <- subset(imputation, type == 'train')
testsetImpute <- subset(imputation, type == 'test')

factorDiff <- sapply(colnames(trainsetImpute), . %>% getDiffByCategory(testsetImpute, trainsetImpute, .))
# factorDiff %>% lapply(length) %>%
#   is_greater_than(0) %>%
#   factorDiff[.] %>%
#   names %>%
#   sapply(function(x) length(unlist(factorDiff[.])))

diffs <- sapply(names(factorDiff[lapply(factorDiff, length) > 0]), function(x) length(unlist(factorDiff[[x]])))
props <- diffs / sapply(impute[,names(diffs)], . %>% levels %>% length)
barplot(props * 100, ylab = 'Unknown values (%)')

rowcount <- sapply(colnames(testsetImpute), function(x)  (x != 'subvillage') & (testsetImpute[,x] %in% factorDiff[[x]]))
sum(rowSums(data.frame(rowcount))  > 0)

# Machine learning
set.seed(1234)
reducedTrainset <- trainsetImpute
inTrain <- createDataPartition(y=trainset.labels$status_group, p=0.8, list=FALSE)
training <- reducedTrainset[inTrain,]
trainingLabels <- trainset.labels[inTrain, 'status_group']
testing <- reducedTrainset[-inTrain,]

## Compute aggregations on training set
aggregForTrainset <- . %>% aggregByCategory(merge(training, trainset.labels), .)
trainsetAggregated <- sapply(colnames(training), aggregForTrainset)

corrForTrainset <- . %>% correlationAggregByCategory(merge(training, trainset.labels), .)
correlated <- ldply(colnames(training), corrForTrainset)

trainingFinal <- scaleMergeDataset(mergeAggregs(training, trainsetAggregated[subset(correlated, V2 == FALSE)$V1]))

## Apply on testset
testingFinal <- scaleMergeDataset(mergeAggregs(testing, trainsetAggregated[subset(correlated, V2 == FALSE)$V1]))


## Sort with label
trainingWithLabels <- merge(trainingFinal, trainset.labels)
testingWithLabels <- merge(testingFinal, trainset.labels)

predictors <- colnames(trainingWithLabels)[!(colnames(trainingWithLabels) %in% c('id', 'status_group', 'recorded_by', 'type','wpt_name',
                                                                                 'scheme_name', 'public_meeting',
                                                                                 'installer', 'subvillage', 'funder', 'permit'))]
predictors <- predictors[-(grep('\\.subvillage', predictors))]
predictorsTot <- predictors[grep('tot_', predictors)]
predictorsNotTot <- predictors[-grep('tot_', predictors)]

#predictors <- predictors[- grep('tot_', predictors)] # remove aggregate columns

trControl <- trainControl(method = 'repeatedcv', number = 5, preProcOptions = NULL, classProbs = TRUE, verboseIter = TRUE)
#trControl <- trainControl(method = 'none', number = 1, preProcOptions = NULL, classProbs = TRUE, verboseIter = TRUE)
library(doMC)
registerDoMC(3)
registerDoSEQ()
## Create model
set.seed(1234)

# preProc <- preProcess(trainingWithLabels[,predictors], method = c('medianImpute'))
rfModel <- train(trainingWithLabels[,predictors], factor(trainingWithLabels$status, 
                                                         labels = c('func', 'repair', 'nonfunc')), 
                 preProcess = c('medianImpute'),
                 method = 'ranger',
                 trControl = trControl, tuneLength = 3)

## Validation
validation <- predict(rfModel, newdata = testingWithLabels[,predictors])
confusionMatrix(validation, factor(testingWithLabels$status_group, 
                                   labels = c('func', 'repair', 'nonfunc')))

# Submission

validAggregs <- mergeAggregs(testsetImpute, trainsetAggregated[subset(correlated, V2 == FALSE)$V1])

validFinal <- scaleMergeDataset(validAggregs)
# setForSubmission <- cbind(validFinal[,c('id', predictorsNotTot)],
#                       predict(imputePreProc, validFinal[,predictorsTot]))
setForSubmission <- validFinal

rfProbs <- predict(rfModel, newdata = setForSubmission)
probs <- extractProb(list(rf = rfModel), unkX = setForSubmission)

rfDf <- data.frame(id = setForSubmission$id, status_group = factor(rfProbs, labels = levels(trainset.labels$status_group)))
rfSubmission <- join(testset.values, rfDf)[, c('id', 'status_group')]

write.table(rfSubmission, 
            paste0('rfSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            sep=',', quote = FALSE)
