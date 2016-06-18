set.seed(1234)
hidden_opt <- lapply(1:100, function(x) 100 * sample(10,
                                                sample(4), replace=TRUE))
l1_opt <- seq(1e-6,1e-3,1e-6)
hyper_params <- list(hidden = hidden_opt, l1 = l1_opt)
search_criteria <- list(strategy = 'RandomDiscrete',
                       max_models = 10,
                       seed=123456)

randomGrid <- h2o.grid('deeplearning', x = predictors, y = 'status_group',
                       training_frame = trainset,
                       validation_frame = testset,
                       hyper_params = hyper_params,
                       search_criteria = search_criteria,
                       epochs = 1000)

models <- lapply(randomGrid@model_ids, function(id) { h2o.getModel(id)})
h2o.saveModel(models[[1]], '.')
submit(models[[1]], validation)
