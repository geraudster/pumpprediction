library(h2o)
localH2O <- h2o.init()
trainset.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/trainset_values.csv', destination_frame = 'trainset.hex', sep = ',', header = TRUE)
labels.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/trainset_labels.csv', destination_frame = 'labels.hex', sep = ',', header = TRUE)

trainsetFull.hex <- h2o.merge(trainset.hex, labels.hex)

summary(trainsetFull.hex)
dim(trainset.hex)
summary(trainset.hex)
head(trainset.hex)
head(labels.hex)

trainsetFull.split <- h2o.splitFrame(trainsetFull.hex)
sapply(trainsetFull.split, dim)
sapply(trainsetFull.hex, class)
allVariables <- colnames(trainset.hex)

predictors <- colnames(trainset.hex)[!(allVariables %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'installer', 'funder'))]

hyper_params <- list(
  hidden=list(c(rep(50, 1), rep(25, 1)),
              c(rep(50, 2), rep(25, 2)),
              c(rep(200, 1), rep(100, 1)),
              c(rep(200, 2), rep(100, 2)))
)

grid <- h2o.grid('deeplearning',
                 x = predictors, y = 'status_group',
                 training_frame = trainsetFull.split[[1]],
                 validation_frame = trainsetFull.split[[2]],
                 activation = "Tanh",
                 autoencoder = FALSE, epochs = 100,
                 hyper_params = hyper_params)

models <- lapply(grid@model_ids, function(id) { h2o.getModel(id)})

deepModel <- h2o.deeplearning(predictors, 'status_group', trainsetFull.split[[1]], activation = "Tanh",
                              autoencoder = FALSE, epochs = 100,
                              hyper_params = hyper_params)

rfModel <- h2o.randomForest(predictors, 'status_group', trainsetFull.hex)
gbmModel <- h2o.gbm(predictors, 'status_group', trainsetFull.hex)
glmModel <- h2o.glm(predictors, 'status_group', trainsetFull.hex, family='multinomial',solver='L_BFGS')
# glrmModel <- h2o.glrm(predictors, 'status_group', trainsetFull.hex)

validation.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/testset_values.csv', destination_frame = 'validation.hex', sep = ',', header = TRUE)

# model <- models[[3]]
# model <- rfModel
model <- rfModel
preds <- h2o.predict(model, validation.hex)
head(validation.hex$id)
submission <- data.frame(as.data.frame(validation.hex$id), as.data.frame(preds$predict))
colnames(submission) <- c('id', 'status_group')
write.table(submission, 
            paste0('rfH2oSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            sep=',', quote = FALSE)

# Variable importance
varImpDf <- as.data.frame(rfModel@model$variable_importances)
ggplot(varImpDf, aes(reorder(variable, scaled_importance), scaled_importance)) +  geom_bar(stat = 'identity') + coord_flip()