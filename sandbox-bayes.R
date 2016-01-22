source('common.R')

# Data preparation
trainset.labels <- read.csv('trainset_labels.csv', na.strings = "")
statusGroups <- dummyVars(~ status_group, data = trainset.labels)
trainset.labels <- cbind(trainset.labels, data.frame(predict(statusGroups, trainset.labels)))

trainset.values <- read.csv('trainset_values.csv', na.strings = "") %>%
  prepareDataSet('train')

testset.values <- read.csv('testset_values.csv', na.strings = "") %>% 
  prepareDataSet(testset.values, 'test')

reduceFactors <- function(x, keep = 20) {
  if(is.factor(x)) {
    factorTable <- table(x)
    head(factorTable[order(factorTable, decreasing = TRUE)], keep)
  }
}

reducedFactors <- sapply(trainset.values, reduceFactors)

applyFactorRedux <- function(xs, reducedLevels) {
  sapply(xs, function(x) if(x %in% names(reducedLevels)) as.character(x) else 'Others')
}


plotFactorRedux <- function(xs, label) {
  factorDf <- xs %>% table %>% as.data.frame
  colnames(factorDf) <- c('Factor', 'Freq')
  sortedfactorDf <- transform(factorDf, Factor = reorder(Factor, Freq))
  ggplot(sortedfactorDf, aes(x=Factor, fill=Factor, y=Freq)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    ggtitle(paste('Distribution for', label))
}

applyFactorRedux(trainset.values$lga, reducedFactors$lga) %>%
  plotFactorRedux('lga')

applyFactorRedux(trainset.values$ward, reducedFactors$ward) %>%
  plotFactorRedux('ward')

applyFactorRedux(trainset.values$installer, reducedFactors$installer) %>%
  plotFactorRedux('installer')

applyFactorRedux(trainset.values$funder, reducedFactors$funder) %>%
  plotFactorRedux('funder')

applyFactorRedux(trainset.values$scheme_management, reducedFactors$scheme_management) %>%
  plotFactorRedux('scheme_management')

applyFactorRedux(trainset.values$region_code, reduceFactors(trainset.values$region_code, 20)) %>%
  plotFactorRedux('region_code')

applyFactorRedux(trainset.values$management, reduceFactors(trainset.values$management, 10)) %>%
  plotFactorRedux('management')

applyFactorRedux(trainset.values$extraction_type, reduceFactors(trainset.values$extraction_type, 10)) %>%
  plotFactorRedux('extraction_type')

# Machine learning
library(caret)
set.seed(1234)
reducedTrainset <- head(join(trainset.values, trainset.labels), 10000)
reducedTrainset <- join(trainset.values, trainset.labels)
inTrain <- createDataPartition(y=reducedTrainset$status_group, p=0.8, list=FALSE)
training <- reducedTrainset[inTrain,]
testing <- reducedTrainset[-inTrain,]

nzvs <- nzv(training)
predictors <- colnames(training)[!(colnames(training) %in% c('id', 'status_group', 'recorded_by', 'type','wpt_name',
                                                             'scheme_name', 'public_meeting', 'gps_height', 'num_private',
                                                             'installer', 'subvillage', 'funder', 'permit')) &
                                   !grepl('status_group', colnames(training))]

sum(is.na(training[,predictors]))

## BART
fits <- list()
options(java.parameters="-Xmx4g")
library(bartMachine)
set_bart_machine_num_cores(1)
bart_machine <- bartMachine(training[,predictors], factor(training[,'status_group.functional']),
                            use_missing_data = TRUE)

preds <- predict(bart_machine, new_data = testing[,predictors])

train(training[,predictors], factor(training[,'status_group.functional']),
      method = 'bartMachine', use_missing_data = TRUE)


for(outcome in c('status_group.functional', 'status_group.non.functional', 'status_group.functional.needs.repair')) {
  fits[outcome] <- train(training[,predictors], training[,outcome],
                         method = 'bartMachine', use_missing_data = TRUE)
}

imputeModel <- preProcess(training[,predictors], method = c('medianImpute'))
training.imputed <- training[,c('id', predictors)] %>% predict(imputeModel, .) %>% 
  merge(trainset.labels, .)
training.imputed.withoutNA <- training.imputed[-which(is.na(training.imputed))]

## Variable selection

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
subsets <- c(1:5, 10, 15, 20, 25)
lmProfile <- rfe(training.imputed[,predictors], training.imputed$status_group,
                 sizes = subsets,
                 rfeControl = ctrl)

ctrl <- gafsControl(functions = rfGA, 
                    method = "cv",
                    number = 3)

obj <- gafs(x = training.imputed[,predictors],
            y = training.imputed$status_group,
            iters = 10, gafsControl = ctrl)

training.imputed.withoutNA$status_group_name <- factor(training.imputed.withoutNA$status_group, 
                                                       labels = c('func', 'repair', 'nonfunc'))
f <- predictors %>% paste0(collapse ='+') %>% paste('status_group_name', ., sep = '~') %>%
  as.formula

trControl <- trainControl(method = 'cv', number = '5', classProbs = TRUE, verboseIter = TRUE,
                          sampling = 'down')
fitNb <- train(f, training.imputed.withoutNA,
               method = 'nb',
               trControl= trControl)
