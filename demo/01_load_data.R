library(h2o)
localH2O <- h2o.init(min_mem_size = '5G', nthreads = 4)
trainset.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/trainset_values.csv',
                               destination_frame = 'trainset.hex',
                               sep = ',', header = TRUE)
labels.hex <- h2o.uploadFile(path = '~/projects/pumpprediction/trainset_labels.csv',
                             destination_frame = 'labels.hex',
                             sep = ',', header = TRUE)

trainsetFull.hex <- h2o.merge(trainset.hex, labels.hex)

dim(trainsetFull.hex)
summary(trainsetFull.hex)

trainsetFull.split <- h2o.splitFrame(trainsetFull.hex)
sapply(trainsetFull.split, dim)

allVariables <- colnames(trainsetFull.hex)
predictors <- colnames(trainsetFull.hex)[!(allVariables 
                                           %in% c('id', 'wpt_name', 'subvillage', 'scheme_name', 'installer', 'funder',
                                                  'status_group', 'ward_lga'))]

# model <- models[[3]]
# model <- rfModel
model <- rfModel

submit <- function(model) {
  preds <- h2o.predict(model, validation.hex)
  head(validation.hex$id)
  submission <- data.frame(as.data.frame(validation.hex$id), as.data.frame(preds$predict))
  colnames(submission) <- c('id', 'status_group')
  write.table(submission, 
              paste0('rfH2oSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
              row.names = FALSE,
              sep=',', quote = FALSE)
}

# submit(model)
