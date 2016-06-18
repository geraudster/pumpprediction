library(h2o)
localH2O <- h2o.init(min_mem_size = '5G', nthreads = 4)
trainset.hex <- h2o.uploadFile(path = '~/pumpprediction/trainset_values.csv')

labels.hex <- h2o.uploadFile(path = '~/pumpprediction/trainset_labels.csv')

validation <- h2o.uploadFile('~/pumpprediction/testset_values.csv')
trainsetFull.hex <- h2o.merge(trainset.hex, labels.hex)

dim(trainsetFull.hex)
summary(trainsetFull.hex)

trainsetFull.split <- h2o.splitFrame(trainsetFull.hex)
sapply(trainsetFull.split, dim)

trainset <- trainsetFull.split[[1]]
testset <- trainsetFull.split[[2]]

allVariables <- colnames(trainsetFull.hex)
predictors <- colnames(trainsetFull.hex)[!(allVariables 
                                           %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'installer', 'funder',
                                                  'status_group', 'ward_lga'))]

# model <- models[[3]]
# model <- rfModel
model <- rfModel

submit <- function(model, dataset) {
  preds <- h2o.predict(model, dataset)
  submission <- data.frame(as.data.frame(dataset$id), as.data.frame(preds$predict))
  colnames(submission) <- c('id', 'status_group')
  write.table(submission, 
              paste0('submissions/rfH2oSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
              row.names = FALSE,
              sep=',', quote = FALSE)
}

# submit(model)
