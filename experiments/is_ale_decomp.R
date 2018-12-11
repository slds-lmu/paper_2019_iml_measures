

# Get some data with correlated features and interactions
# mlbench
data("Sonar")



M = 1000

ale_cors = lapply(1:M, function(m) {
  # Subsample
  sonarx = Sonar[sample(1:nrow(Sonar), size = 0.5 * nrow(Sonar)), ]

  # Fit model
  tsk = makeClassifTask(data = sonarx, target = "Class")

  # Example with linear model ALE
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  mod = train(lrn, tsk)
  mod$learner.model
  pred = Predictor$new(mod, sonarx, class = 1)

  # Fit ALE first order model
  ale1st = get_ale_1stmodel(pred)


  prediction = pred$predict(sonarx)[[1]]
  # Get values of first order model
  ale1.values = ale1st(sonarx)

  # Get values of higher order model f - f1st
  ale2plus.values = prediction - ale1.values
  ale_cor = cor(ale1.values, ale2plus.values)
  print(ale_cor)
  ale_cor
})

# Measure correlation
plot(hist(unlist(ale_cors)))

# Repeat
