deepModel <- h2o.deeplearning(predictors, 'status_group', trainsetFull.split[[1]])

h2o.confusionMatrix(deepModel)

# Predict on out of bag data
h2o.confusionMatrix(deepModel, trainsetFull.split[[2]])

deepModel.h100 <- h2o.deeplearning(predictors, 'status_group', trainsetFull.split[[1]],
                                   hidden = c(100, 100))
h2o.confusionMatrix(deepModel.h100)
h2o.confusionMatrix(deepModel.h100, trainsetFull.split[[2]])

library(plyr)
library(dplyr)

models <- ldply(c(50, 100, 200, 300), function(nunits)
{
  deepModel <- h2o.deeplearning(predictors, 'status_group', trainsetFull.split[[1]],
                                hidden = c(nunits, nunits))
  data.frame(nunits = nunits,
             inSample = h2o.confusionMatrix(deepModel),
             outOfSample = h2o.confusionMatrix(deepModel, trainsetFull.split[[2]]))
})

models$status <- c('functional', 'functional.needs.repair', 'non.functional', 'total')

library(ggplot2)
qplot(nunits, inSample.Error, data = models, color = status, geom = 'line') +
  geom_text(aes(x = 300, label = status), data = models[models$nunits == 300,])

qplot(nunits, outOfSample.Error, data = models, color = status, geom = 'line') +
  geom_text(aes(x = 300, label = status), data = models[models$nunits == 300,])
