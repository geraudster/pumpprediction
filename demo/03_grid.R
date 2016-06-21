## grid

# grid <- h2o.grid('deeplearning',
#                  x = predictors, y = 'status_group',
#                  training_frame = trainsetFull.split[[1]],
#                  validation_frame = trainsetFull.split[[2]],
#                  activation = "Tanh",
#                  autoencoder = FALSE, epochs = 100,
#                  hyper_params = hyper_params)
# hidden=list(rep(100, 2), rep(200, 2), rep(300, 2), rep(400, 2), rep(400, 3)), # logloss = 0.233676544101873



grid <- h2o.grid('deeplearning', x = predictors, y = 'status_group',
                 training_frame = trainset,
                 validation_frame = testset,
                 hyper_params = list(
                   hidden=list(rep(1024, 3), rep(400, 4), rep(400, 5)),
                   activation = c('Rectifier', 'RectifierWithDropout'),
                   l1 = c(0, 1e-5),
                   l2 = c(0, 1e-5),
                   epochs = 100))

models <- lapply(grid@model_ids, function(id) { h2o.getModel(id)})
h2o.saveModel(models[[1]], 'models')
submit(models[[1]], validation) # score 0.7931

library(plyr)
laply(models, function (x) { list(hidden = paste(x@parameters$hidden, collapse = ' '), r2 = h2o.r2(x)) })
for(model in models) {
  print(paste('Model: ', paste(model@parameters$hidden, collapse = ' ')))
  print(h2o.confusionMatrix(model))
  print(h2o.confusionMatrix(model, trainsetFull.split[[2]]))
}

modelsForPlot <- ldply(models, function (model) {
  print(!is.null(model))
  stopifnot(1 == 0)
  data.frame(nunits = first(model@parameters$hidden))
})

modelsForPlot <- ldply(models, function (model) {
  print(paste(model@parameters$activation, model@parameters$l1))
  data.frame(nunits = if(is.null(model@parameters$hidden)) 200
                      else first(model@parameters$hidden),
             activation = if(is.null(model@parameters$activation)) NA
                          else model@parameters$activation,
             l1 = if(is.null(model@parameters$l1)) NA
                  else model@parameters$l1,
             inSample = h2o.confusionMatrix(model),
             outOfSample = h2o.confusionMatrix(model, trainsetFull.split[[2]]),
             stringAsFactor = FALSE)
})

modelsForPlot$status <- c('functional', 'functional.needs.repair', 'non.functional', 'total')

qplot(nunits, outOfSample.Error, data = subset(modelsForPlot,
                                               is.na(activation) & is.na(l1)),
      color = status, geom = 'line') 

+
  geom_text(aes(x = 300, label = status), data = modelsForPlot[modelsForPlot$nunits == 300,])
