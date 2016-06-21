EPOCHS <- 1

args <- list(
  hidden = list(c(64),
  c(128),            
  c(256),            
  c(512),            
  c(1024),           
  c(2048),
  c(64,64),          
  c(128,128),        
  c(256,256),        
  c(512,512),        
  c(1024,1024),
  c(2048,2048),
  c(64,64,64),       
  c(128,128,128),    
  c(256,256,256),    
  c(512,512,512),    
  c(1024,1024,1024),
  c(2048,2048,2048)),
  l1 = c(0, 1e-5),
  activation = c('Rectifier', 'RectifierWithDropout'))

grid <- h2o.grid('deeplearning', x = predictors, y = 'status_group',
                training_frame = trainset,
                validation_frame = testset,
                hyper_params = args,
                epochs = EPOCHS)

models <- lapply(grid@model_ids, function(id) { h2o.getModel(id)})
h2o.saveModel(models[[1]], 'models')
submit(models[[1]], validation) # score 0.7931

sapply(models, function(model) model@parameters$hidden)

library(plyr)
stats <- laply(models, function (x) { list(hidden = paste(x@parameters$hidden, collapse = ' '),
                                           l1 = x@allparameters$l1,
                                           l2 = x@allparameters$l2,
                                           activation = x@allparameters$activation,
                                           r2 = h2o.r2(x),
                                           logloss = h2o.logloss(x),
                                           mse = h2o.mse(x)) })

write.csv(stats, paste0('stats/stat', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
          row.names = FALSE)
