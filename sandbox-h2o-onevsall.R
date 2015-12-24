trainsetFull.hex$functional <- ifelse(trainsetFull.hex$status_group == 'functional', '1', '0')
trainsetFull.hex$nonfunctional <- ifelse(trainsetFull.hex$status_group == 'non functional', '1', '0')
trainsetFull.hex$needrepair <- ifelse(trainsetFull.hex$status_group == 'functional needs repair', '1', '0')

allVariables <- colnames(trainsetFull.hex)
modelsOneVsAll <- list()
for(outcome in c('functional', 'nonfunctional', 'needrepair')) {
  predictors <- colnames(trainsetFull.hex)[!(allVariables 
                                             %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'installer', 'funder',
                                                    'status_group', 'functional', 'nonfunctional', 'needrepair', 'recorded_by'))]
  
  fit <- h2o.deeplearning(
    x = predictors, y = outcome,
    training_frame = trainsetFull.hex,
    activation = "Tanh",
    autoencoder = FALSE, epochs = 5, nfolds = 10,
    l1 = c(1e-5), l2 = c(1e-5), hidden_dropout_ratios=c(0.1,0.05,0.05,0),
    hidden = c(50,50,25,25))
  modelsOneVsAll[outcome] <- fit
}

## Try to autoencode features
predictors <- colnames(trainsetFull.hex)[!(allVariables 
                                           %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'funder',
                                                  'status_group', 'functional', 'nonfunctional', 'needrepair', 'recorded_by'))]
modelFeatures <- h2o.deeplearning(predictors, training_frame = trainsetFull.hex, activation = 'Tanh',
                                  autoencoder = TRUE, hidden=c(50, 20, 10, 20, 50),epochs = 5)
features <- h2o.deepfeatures(modelFeatures, trainsetFull.hex)
predictors <- colnames(features)
features$status_group <- trainsetFull.hex$status_group
fitGbm <- h2o.gbm(predictors, 'status_group', features)
fitRf <- h2o.randomForest(predictors, 'status_group', features)

model <- fitRf
featuresValidation <- h2o.deepfeatures(modelFeatures, validation.hex)
preds <- h2o.predict(model, featuresValidation)
head(validation.hex$id)
head(preds)
submission <- data.frame(as.data.frame(validation.hex$id), as.data.frame(preds$predict))
colnames(submission) <- c('id', 'status_group')
write.table(submission, 
            paste0('deepH2oSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            sep=',', quote = FALSE)

