trainset.values <- read.csv('trainset_values.csv', na.strings = "")
trainset.labels <- read.csv('trainset_labels.csv', na.strings = "")
testset.values <- read.csv('testset_values.csv', na.strings = "")

emptyPos <- trainset.values$longitude == 0
trainset.values[emptyPos, 'longitude'] <- NA
trainset.values[emptyPos, 'latitude'] <- NA
trainset.values[emptyPos, 'gps_height'] <- NA

emptyConstrutionYear <- trainset.values$construction_year == 0
trainset.values[emptyConstrutionYear,'construction_year'] <- NA

trainset.values$wpt_name <- as.character(trainset.values$wpt_name)

emptyPos <- testset.values$longitude == 0
testset.values[emptyPos, 'longitude'] <- NA
testset.values[emptyPos, 'latitude'] <- NA
testset.values[emptyPos, 'gps_height'] <- NA

library(mice)
imp <- mice(trainset.values[,c('longitude', 'latitude', 'gps_height', 'ward', 'lga')])

library(Amelia)
imp <- amelia(head(trainset.values, 10000), idvars = c('ward', 'lga'))
imp <- amelia(trainset.values[,c('longitude', 'latitude', 'gps_height', 'ward', 'lga')], noms = c('ward', 'lga'))
extraction_type.levels <- levels(factor(trainset.values$extraction_type))
testset.values$extraction_type <- factor(testset.values$extraction_type, levels = extraction_type.levels)

trainset.impute.values <- read.csv('training-NA-imp5.csv', na.strings = '')

sapply(trainset.values, class)
cor(trainset.values)
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

rfSubmission <- data.frame(id = testset.values$id, status_group = rfProbs$pred)
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
