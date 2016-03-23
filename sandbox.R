source('common.R')
trainset.values <- read.csv('trainset_values.csv', na.strings = "")
trainset.labels <- read.csv('trainset_labels.csv', na.strings = "")
testset.values <- read.csv('testset_values.csv', na.strings = "")

trainset.values <- prepareDataSet(trainset.values, 'train')
testset.values <- prepareDataSet(testset.values, 'test')

geraudScore <- function(x) {
  switch(as.character(x),
         'functional' = 0.5,
         'functional needs repair' = 1,
         'non functional' = 2)
}

setWithScore <- trainset.values
setWithScore$score <- sapply(trainset.labels$status_group, geraudScore)

library(plyr)
scoreByWard <- ddply(setWithScore, .(lga, ward), summarise, score = mean(score))


#library(mice)
#imp <- mice(trainset.values[,c('longitude', 'latitude', 'gps_height', 'ward', 'lga')])

library(Amelia)
# trainAndTestSet <- merge(rbind(trainset.values, testset.values), scoreByWard)
trainAndTestSet <- rbind(trainset.values, testset.values)

set.seed(2)
trainAndTestSetImpute <- amelia(x = trainAndTestSet, m = 5, idvars = c( "funder", "installer", "wpt_name", "basin", "subvillage", "region",  "lga", "ward", "public_meeting", "recorded_by", "permit",
                                                                "extraction_type", "extraction_type_group",  "extraction_type_class", "management", "management_group", "payment",  "payment_type",
                                                                "water_quality", "quality_group", "quantity",  "quantity_group", "source", "source_type", "source_class", "waterpoint_type",  "waterpoint_type_group",
                                                                "region_code", "district_code",  "scheme_name", "type", 'date_recorded'), ts = NULL, cs = NULL, priors = NULL,  
                         lags = NULL, empri = 0, intercs = FALSE, leads = NULL, splinetime = NULL,  
                         logs = NULL, sqrts = NULL, lgstc = NULL, ords = NULL, noms = c("scheme_management"),  
                         bounds = NULL, max.resample = 1000, tolerance = 1e-04) 
imputation <- trainAndTestSetImpute$imputations$imp5
imputation$construction_age <- 2015 - imputation$construction_year
trainsetImpute <- subset(imputation, type == 'train')
testsetImpute <- subset(imputation, type == 'test')


## clusters test

library(magrittr)
corrGroup <- ldply(colnames(trainsetImpute), function(col) {
  if(is.factor(trainsetImpute[,col])) {
    print(col)
    statsByFunderInstaller <- ddply(merge(trainsetImpute, trainset.labels), col, summarise,
                                    tot_functional = sum(status_group == 'functional') / length(status_group),
                                    tot_repair = sum(status_group == 'functional needs repair') / length(status_group),
                                    tot_nonfunctional = sum(status_group == 'non functional') / length(status_group))
    
    corMatrix <- cor(statsByFunderInstaller[,c('tot_functional', 'tot_repair', 'tot_nonfunctional')])
    print(corMatrix %>% .[upper.tri(corMatrix)] )
#     %>% is_greater_than(0.9) %>% sum %>% equals(3) %>%
#       c(col, .)
  }
})

trainsetImputWithLabel <- merge(trainsetImpute, trainset.labels)
cols <- colnames(trainsetImpute)
#cols <- colnames(trainsetImpute)[colnames(trainsetImpute) %in% subset(corrGroup, V2 == FALSE)$V1]
aggregs <- sapply(cols, function(col) {
  if(is.factor(trainsetImpute[,col])) {
    print(col)
    as.data.frame(ddply(trainsetImputWithLabel, col, summarise,
          tot_functional = sum(status_group == 'functional'),
          tot_repair = sum(status_group == 'functional needs repair'),
          tot_nonfunctional = sum(status_group == 'non functional')))
  }
})

mergeAggregs <- function (dataset, aggregs) {
  for(var in names(aggregs)) {
    if(!is.null(aggregs[[var]])) {
      print(var)
      dataset <- merge(dataset, aggregs[[var]], all.x = TRUE)
      # print(colnames(dataset[,c('tot_functional', 'tot_repair', 'tot_nonfunctional')]))
      colnames(dataset)[colnames(dataset) %in% c('tot_functional', 'tot_repair', 'tot_nonfunctional')] <- 
        make.names(paste(c('tot_functional', 'tot_repair', 'tot_nonfunctional'), var))
    }
  }
  dataset
}

trainsetWithAggregs <- mergeAggregs(trainsetImpute, aggregs[subset(corrGroup, V2 == FALSE)$V1])
predictorsAggregs <- colnames(trainsetWithAggregs)[!colnames(trainsetWithAggregs) %in% c('id', 'status_group')]
predictorsAggregsPos <- grep('tot_', colnames(trainsetWithAggregs))

trainsetWithAggregsScaled <- cbind(trainsetWithAggregs[, -predictorsAggregsPos],
                                   as.data.frame(scale(trainsetWithAggregs[,predictorsAggregsPos])))
                                   
#kmeansFit <- kmeans(trainsetImpute[, c('funder', 'installer', 'scheme_management', 'amount_tsh', 'latitude', 'longitude', 'gps_height', 'population', 'quantity')], 10)
kmeansFit <- kmeans(trainsetImpute[, subset(corrGroup, V2 == TRUE)$V1], 10)

library(caret)
set.seed(1234)
# reducedTrainset <- trainsetImpute
reducedTrainset <- trainsetWithAggregsScaled
inTrain <- createDataPartition(y=trainset.labels$status_group, p=0.8, list=FALSE)
training <- reducedTrainset[inTrain,]
trainingLabels <- trainset.labels[inTrain, 'status_group']
testing <- reducedTrainset[-inTrain,]
testingWithLabels <- merge(testing, trainset.labels)

# Whole dataset
training <- reducedTrainset
trainingLabels <- trainset.labels[, 'status_group']
trainingWithLabels <- merge(training, trainset.labels, by = c('id'))

allVariables <- colnames(reducedTrainset)
predictors <- colnames(reducedTrainset)[!(allVariables 
                                          %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'funder', 'installer', 'permit', 'public_meeting',
                                                 'status_group', 'functional', 'nonfunctional', 'needrepair', 'lga', 'num_private',
                                                 'recorded_by', 'quantity', 'ward', 'type',
                                                 'date_recorded', 'date_recorded_year', 'construction_year'
#                                                  'waterpoint_type_group', 'extraction_type_group', 'extraction_type_class', 'quantity_group', 'payment',
#                                                  'management_group', 'quality_group'
                                                 ))]

predictors <- predictorsAggregs
predictors <- colnames(reducedTrainset)[!(allVariables 
                                          %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'funder', 'installer', 'permit', 'public_meeting',
                                                 'recorded_by', 'type'))]
predictors <- predictors[- grep('tot_', predictors)]
trControl <- trainControl(method = 'repeatedcv', number = 10, preProcOptions = NULL, classProbs = TRUE, verboseIter = TRUE)
trControl <- trainControl(method = 'none', number = 1, preProcOptions = NULL, classProbs = TRUE, verboseIter = TRUE)
library(doMC)
registerDoMC(3)
set.seed(1234)
rfModel <- train(trainingWithLabels[,predictors], factor(trainingWithLabels$status, 
                                                         labels = c('func', 'repair', 'nonfunc')), 
                 method = 'ranger', 
                 trControl = trControl, tuneLength = 1)

# svmModel <- train(training[,predictors], factor(trainset.labels[inTrain, 'status_group'], 
#                                                labels = c('func', 'repair', 'nonfunc')), 
#                  method = 'svmLinear', 
#                  trControl = trControl, tuneLength = 1)
# 
# gbmModel <- train(training[,predictors], factor(trainset.labels[inTrain, 'status_group'], 
#                                                 labels = c('func', 'repair', 'nonfunc')), 
#                   method = 'gbm', 
#                   trControl = trControl, tuneLength = 1)

validation <- predict(rfModel, newdata = testingWithLabels[,predictors])
confusionMatrix(validation, factor(testingWithLabels$status_group, 
                                   labels = c('func', 'repair', 'nonfunc')))

summary(trainset.values)

library(ggplot2)
qplot(trainset.labels$status_group, fill = trainset.labels$status_group)

library(reshape2)
library(plyr)
library(dplyr)

trainset <- join(trainset.values, trainset.labels, 'id')

library(stringr)
histMissingValues <- function(dataset) {
  na.count <- lapply(dataset, function(x) { 
    if(is.numeric(x)) mean(x == 0) * 100
    else mean(str_trim(x, 'both') == '') * 100
    })
  na.count.melt <- melt(data.frame(na.count))
  qplot(x = variable, y = value, data = na.count.melt,
        geom = 'bar',
        stat = 'identity',
        fill = class(value)) + coord_flip()
}

histMissingValues(trainset)

library(psych)
describe(trainset)

getNumericColumns <- function(dataset) {
  sapply(dataset, is.numeric)
}

numericColumns <- getNumericColumns(trainset)[-c(1,41)]
charColumns <- !numericColumns
longtrainset <- melt(trainset, id.vars = c('id', 'status_group', names(which(charColumns))))

qplot(status_group, log(value + 0.1),
      data = longtrainset[longtrainset$variable == 'amount_tsh',],fill = status_group, geom = c('boxplot'))

qplot(status_group, value,
      data = longtrainset[longtrainset$variable == 'gps_height',],fill = status_group, geom = c('boxplot'))

qplot(status_group, value,
      data = longtrainset,fill = status_group, geom = c('boxplot')) + 
  facet_wrap(~ variable, scales = 'free_y')

summary(trainset)

selectedColumns <- c(
  'id',
  'gps_height',
  'basin',
  'extraction_type',
  'management',
  'payment_type',
  'quantity',
  'quality_group',
  'source_type',
  'waterpoint_type')
outcomeVar <- 'status_group'

reducedTrainset <- trainset[, c(selectedColumns, outcomeVar)]

library(caret)
library(modelUtils)
set.seed(1234)
inTrain <- createDataPartition(y=reducedTrainset$status_group, p=0.8, list=FALSE)
training <- reducedTrainset[inTrain,]
testing <- reducedTrainset[-inTrain,]
tc <- trainControl(classProbs = TRUE, method = "repeatedcv",
                   number = 5,
                   repeats = 10)

rpart.grid <- expand.grid(cp=seq(0.005, 0.05, 0.005))
rpartModel <- testModel(formula = NULL, training,
                        testing,
                        'status_group',
                        'rpart', trControl = tc, tuneGrid = rpart.grid)

rfModel <- testModel(formula = NULL, training,
                        testing,
                        'status_group',
                        'rf', trControl = tc)

set.seed(1234)
gbmModel <- testModel(formula = NULL, training,
                     testing,
                     'status_group',
                     'gbm')
                     # , trControl = tc)


rpartProbs <- extractProb(list(rpartModel$fit), unkX = testset.values)
table(rpartProbs$pred)

rpartSubmission <- data.frame(id = testset.values$id, status_group = rpartProbs$pred)
write.table(rpartSubmission, 
            paste0('rpartSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            sep=',', quote = FALSE)


rfProbs <- extractProb(list(rfModel$fit), unkX = testset.values[,selectedColumns])
table(rfProbs$pred)

rfProbs <- predict(rfModel, newdata = testsetImpute[,predictors])

rfSubmission <- data.frame(id = testset.values$id, status_group = factor(rfProbs, labels = levels(trainset.labels$status_group)))
write.table(rfSubmission, 
            paste0('rfSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            sep=',', quote = FALSE)

writeSubmission(rfModel, testset.values, 'id', 'status_group', 'pumpprediction')

## Impute try

library(mice)
pumps2 <- join(trainset.values, trainset.labels)
emptyCoords <- pumps2$longitude == 0
pumps2[emptyCoords, 'longitude'] <- NA
pumps2[emptyCoords, 'latitude'] <- NA
pumps2[emptyCoords, 'gps_height'] <- NA
imp.pimp <- mice(pumps2)


## submit with aggregs


#writeSubmission(rfModel, testset.values, 'id', 'status_group', 'pumpprediction')
