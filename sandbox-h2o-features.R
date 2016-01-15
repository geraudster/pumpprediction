## Deep features
library(h2o)
localH2O <- h2o.init(min_mem_size = '4G', nthreads = 4)
trainset.hex <- h2o.uploadFile(path = 'trainset_values.csv', destination_frame = 'trainset.hex', sep = ',', header = TRUE)
labels.hex <- h2o.uploadFile(path = 'trainset_labels.csv', destination_frame = 'labels.hex', sep = ',', header = TRUE)

trainsetFull.hex <- h2o.merge(trainset.hex, labels.hex)
trainsetFull.split <- h2o.splitFrame(trainsetFull.hex)

allVariables <- colnames(trainsetFull.hex)
predictors <- colnames(trainsetFull.hex)[!(allVariables 
                                           %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'installer', 'funder',
                                                  'status_group', 'ward_lga'))]

featuresNN <- h2o.deeplearning(x = predictors, training_frame = trainsetFull.split[[1]], activation = "Tanh",
                               autoencoder = TRUE, epochs = 7,
                               hidden = c(500, 200, 100, 200, 500))

featuresNN <- h2o.deeplearning(x = predictors, training_frame = trainsetFull.split[[1]], activation = "Tanh",
                               autoencoder = TRUE, epochs = 7,
                               hidden = c(50, 500, 1000))

featuresNN <- h2o.deeplearning(x = predictors, training_frame = trainsetFull.split[[1]], activation = "Tanh",
                               autoencoder = TRUE, epochs = 7,
                               hidden = c(50))

summary(featuresNN)

featuresNN.1 <- h2o.deepfeatures(featuresNN, trainsetFull.split[[1]], layer = 1)
head(featuresNN.1)
summary(featuresNN.1)

featuresPredictors <- colnames(featuresNN.1)
featuresNN.1$status_group <- trainsetFull.split[[1]]$status_group

modelRf <- h2o.randomForest(featuresPredictors, 'status_group', featuresNN.1, balance_classes = TRUE)

featuresNN.1.test <- h2o.deepfeatures(featuresNN, trainsetFull.split[[2]], layer = 1)
featuresNN.1.test$status_group <- trainsetFull.split[[2]]$status_group

h2o.performance(modelRf, data = featuresNN.1.test, valid = TRUE)
preds.test <- h2o.predict(modelRf, featuresNN.1.test)

deepModel <- h2o.deeplearning(featuresPredictors, 'status_group', featuresNN.1, balance_classes = FALSE,
                              activation = 'Tanh', autoencoder = FALSE, epochs = 7,
                              hidden = c(500, 100, 10))
h2o.performance(deepModel, data = featuresNN.1.test, valid = TRUE)

## Clustering

clusters <- h2o.kmeans(trainsetFull.split[[1]], x = c('longitude', 'latitude'), 10)


# Submit
validation.hex <- h2o.uploadFile(path = 'testset_values.csv', destination_frame = 'validation.hex', sep = ',', header = TRUE)
validation.features <- h2o.deepfeatures(featuresNN, validation.hex, layer = 1)
preds <- h2o.predict(modelRf, validation.features)
submission <- data.frame(as.data.frame(validation.hex$id), as.data.frame(preds$predict))
colnames(submission) <- c('id', 'status_group')
write.table(submission, 
            paste0('rfH2oSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            sep=',', quote = FALSE)


