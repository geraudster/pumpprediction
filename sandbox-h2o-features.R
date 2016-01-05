## Deep features

featuresNN <- h2o.deeplearning(x = predictors, training_frame = trainsetFull.split[[1]], activation = "Tanh",
                               autoencoder = TRUE, epochs = 100,
                               hidden = c(500, 200, 100, 200, 500))
summary(featuresNN)

featuresNN.1 <- h2o.deepfeatures(featuresNN, trainsetFull.split[[1]], layer = 1)
summary(featuresNN.1)
head(featuresNN.1)

featuresPredictors <- colnames(featuresNN.1)
featuresNN.1$status_group <- trainsetFull.split[[1]]$status_group

modelRf <- h2o.randomForest(featuresPredictors, 'status_group', featuresNN.1, balance_classes = TRUE)

featuresNN.1.test <- h2o.deepfeatures(featuresNN, trainsetFull.split[[2]], layer = 1)
featuresNN.1.test$status_group <- trainsetFull.split[[2]]$status_group

h2o.performance(modelRf, data = featuresNN.1.test, valid = TRUE)
preds.test <- h2o.predict(modelRf, featuresNN.1.test)