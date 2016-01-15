library(h2o)
localH2O <- h2o.init(min_mem_size = '5G', nthreads = 4)
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

allVariables <- colnames(trainsetFull.hex)
predictors <- colnames(trainsetFull.hex)[!(allVariables 
                                              %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'installer', 'funder',
                                                     'status_group', 'ward_lga'))]

## Try to concat ward and lga
wardlga <- data.frame(id=as.vector(trainsetFull.hex$id), wardlga = paste(as.vector(trainsetFull.hex$ward), as.vector(trainsetFull.hex$lga), sep = '_'))
trainsetWardLga.hex <- h2o.merge(trainsetFull.hex, as.h2o(wardlga))

allVariables <- colnames(trainsetWardLga.hex)
predictors <- colnames(trainsetWardLga.hex)[!(allVariables 
                                       %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'installer', 'funder',
                                              'status_group', 'ward_lga'))]

hyper_params <- list(
  hidden=list(
    c(rep(25, 2), rep(10, 2)),
    c(rep(50, 2), rep(25, 2)),
    c(rep(100, 4)))
)

# grid <- h2o.grid('deeplearning',
#                  x = predictors, y = 'status_group',
#                  training_frame = trainsetFull.split[[1]],
#                  validation_frame = trainsetFull.split[[2]],
#                  activation = "Tanh",
#                  autoencoder = FALSE, epochs = 100,
#                  hyper_params = hyper_params)
grid <- h2o.grid('deeplearning',
                 x = predictors, y = 'status_group',
                 training_frame = trainsetFull.hex,
                 activation = "Tanh",
                 autoencoder = FALSE, epochs = 10, nfolds = 2,
                 l1 = c(1e-5), l2 = c(1e-5), hidden_dropout_ratios=c(0.1,0.05,0.05,0),
                 hyper_params = hyper_params)

models4 <- lapply(grid@model_ids, function(id) { h2o.getModel(id)})

library(plyr)
laply(models4, function (x) { list(hidden = paste(x@parameters$hidden, collapse = ' '), r2 = h2o.r2(x)) })
for(model in models4) {
  print(paste('Model: ', paste(model@parameters$hidden, collapse = ' ')))
  print(h2o.confusionMatrix(model))
}

deepModel <- h2o.deeplearning(predictors, 'status_group', trainsetFull.split[[1]], activation = "Tanh",
                              autoencoder = FALSE, epochs = 100,
                              hyper_params = hyper_params)

allVariables <- colnames(trainsetFull.hex)
predictors <- colnames(trainsetFull.hex)[!(allVariables 
                                              %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'funder',
                                                     'status_group', 'functional', 'nonfunctional', 'needrepair'))]

rfModel <- h2o.randomForest(predictors, 'status_group', trainsetFull.hex, nfolds = 10, nbins = 40)

allVariables <- colnames(trainsetWardLga.hex)
predictors <- colnames(trainsetWardLga.hex)[!(allVariables 
                                              %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'installer', 'funder',
                                                     'status_group', 'ward', 'lga'))]
rfModel2 <- h2o.randomForest(predictors, 'status_group', trainsetWardLga.hex)
gbmModel <- h2o.gbm(predictors, 'status_group', trainsetFull.hex)
glmModel <- h2o.glm(predictors, 'status_group', trainsetFull.hex, family='multinomial',solver='L_BFGS')
# glrmModel <- h2o.glrm(predictors, 'status_group', trainsetFull.hex)

validation.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/testset_values.csv', destination_frame = 'validation.hex', sep = ',', header = TRUE)
wardlga <- data.frame(id=as.vector(validation.hex$id), wardlga = paste(as.vector(validation.hex$ward), as.vector(validation.hex$lga), sep = '_'))
validationWardLga.hex <- h2o.merge(validation.hex, as.h2o(wardlga))

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
