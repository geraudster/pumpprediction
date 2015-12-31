trainsetImpute.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/training-NA-imp5.csv', sep = ',', header = TRUE)

# trainsetImpute.hex <- as.h2o(trainset.impute.values[,-1])
trainsetImputeFull.hex <- h2o.merge(trainsetImpute.hex[,-1], labels.hex)
trainsetImputeFull.hex$age <- 2015 - trainsetImputeFull.hex$construction_year

trainsetImputeFull.hex$functional <- ifelse(trainsetImputeFull.hex$status_group == 'functional', '1', '0')
trainsetImputeFull.hex$nonfunctional <- ifelse(trainsetImputeFull.hex$status_group == 'non functional', '1', '0')
trainsetImputeFull.hex$needrepair <- ifelse(trainsetImputeFull.hex$status_group == 'functional needs repair', '1', '0')

allVariables <- colnames(trainsetImputeFull.hex)
predictors <- colnames(trainsetImputeFull.hex)[!(allVariables 
                                           %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'funder',
                                                  'status_group', 'functional', 'nonfunctional', 'needrepair', 'lga', 'recorded_by', 'quantity'))]
trainsetImputeFull.split <- h2o.splitFrame(trainsetImputeFull.hex, 0.8, seed = 123)
ovaModels <- list()
ovaModels['needrepair'] <- h2o.deeplearning(predictors, outcome, trainsetImputeFull.split[[1]], balance_classes = TRUE, hidden = c(20, 20, 20))

for(outcome in c('functional', 'nonfunctional')) {
  #model <- h2o.deeplearning(predictors, outcome, trainsetImputeFull.split[[1]], balance_classes = FALSE, hidden = c(20, 20, 20))
  model <- h2o.randomForest(predictors, outcome, trainsetImputeFull.split[[1]], balance_classes = TRUE)
  # gbmModel <- h2o.gbm(predictors, outcome, trainsetImputeFull.split[[1]], balance_classes = FALSE)
#   glmModel <- h2o.glm(predictors, outcome, trainsetImputeFull.hex, family = 'binomial')
  ovaModels[outcome] <- model
}

# Append OVA predictors
ovaPreds <- sapply(names(ovaModels), function(x) { 
  as.vector(h2o.predict(ovaModels[[x]], trainsetImputeFull.split[[1]])$p1)
})
colnames(ovaPreds) <- gsub('^', 'pred_', colnames(ovaPreds))
ovaPredsDf <- cbind(as.data.frame(trainsetImputeFull.split[[1]][, c('id', 'status_group')]), as.data.frame(ovaPreds))
trainsetImputeFullWithPreds.hex <- as.h2o(ovaPredsDf)

rfModelWithOVAPreds <- h2o.gbm(c('pred_functional', 'pred_nonfunctional', 'pred_needrepair'), 'status_group', trainsetImputeFullWithPreds.hex)


# Append OVA pred to test set
lapply(ovaModels, h2o.confusionMatrix)
lapply(ovaModels, function(x) { h2o.performance(x, trainsetImputeFull.split[[2]]) })
ovaPreds <- sapply(names(ovaModels), function(x) { 
  as.vector(h2o.predict(ovaModels[[x]], trainsetImputeFull.split[[2]])$p1)
})
colnames(ovaPreds) <- gsub('^', 'pred_', colnames(ovaPreds))
validationImputeFullWithPreds.hex <- as.h2o(cbind(as.data.frame(trainsetImputeFull.split[[2]][, c('id', 'status_group')]), as.data.frame(ovaPreds)))

h2o.performance(rfModelWithOVAPreds, validationImputeFullWithPreds.hex)

ovaPredsDf$vote <- factor(apply(ovaPredsDf[, c('pred_functional', 'pred_nonfunctional', 'pred_needrepair')], 1, which.max),
                          levels = c(1, 3, 2), labels = c('functional', 'functional needs repair', 'non functional'))

table(ovaPredsDf$status_group, ovaPredsDf$vote)

score <- function (actual, preds) {
  t <- table(actual, preds)
  result <- list()
  result[['table']] <- t
  result[['scores']] <- diag(t) / rowSums(t)
  result[['errors']] <- 1 - diag(t) / rowSums(t)
  result[['total']] <- sum(diag(t)) / sum(t)
  result
}



rfModel3classes <- h2o.randomForest(predictors, 'status_group', trainsetImputeFull.hex, balance_classes = TRUE)

model <- rfModel
h2o.confusionMatrix(model)

needRepair <- h2o.predict(rfModel, trainsetImputeFull.hex)
trainsetImputeFull.hex$needRepair <- needRepair$p1

allVariables <- colnames(trainsetImputeFull.hex)
predictors <- colnames(trainsetImputeFull.hex)[!(allVariables 
                                                 %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'funder',
                                                        'status_group', 'functional', 'nonfunctional', 'needrepair', 'lga', 'recorded_by', 'quantity'))]
rfModel3classes <- h2o.randomForest(predictors, 'status_group', trainsetImputeFull.hex)

model <- rfModel3classes
h2o.confusionMatrix(model)

validation.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/testset_values.csv', destination_frame = 'validation.hex', sep = ',', header = TRUE)

needRepair <- h2o.predict(rfModel, validation.hex)
validation.hex$needRepair <- needRepair$p1

preds <- h2o.predict(model, validation.hex)
submission <- data.frame(as.data.frame(validation.hex$id), as.data.frame(preds$predict))
colnames(submission) <- c('id', 'status_group')
write.table(submission, 
            paste0('rfH2oSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            sep=',', quote = FALSE)

