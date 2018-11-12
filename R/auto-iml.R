# Title: Multicrit performance and interpretability

## Build a model
library(data.table)
library(mlr)
library("iml")
library("ggplot2")
library(mlrMBO)
library(dplyr)

load("~/repos/interpretable-ml-book/data/bike.RData")
source("R/iml-measures.R")
source("R/xgboost.R")

bike.task = makeRegrTask(data = bike, target = "cnt")



mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.svm', id = 'bike-rf'), bike.task)
lm.bike = mlr::train(mlr::makeLearner(cl = 'regr.lm'), bike.task)
rpart.bike = mlr::train(mlr::makeLearner(cl = 'regr.rpart'), bike.task)

bike.x = bike[setdiff(names(bike), "cnt")]

pred = Predictor$new(mod.bike, bike.x, y = bike$cnt)
pred.lm = Predictor$new(lm.bike, bike.x, y = bike$cnt)
pred.rpart = Predictor$new(rpart.bike, bike.x, y = bike$cnt)


par.set = makeParamSet(
  makeIntegerParam ("max_depth" , lower = 1, upper = 10, requires = quote(booster == "gbtree")),
  makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
  makeNumericParam("alpha", lower = 0, upper = 100, requires = quote(booster == "gblinear")),
  makeIntegerParam("nrounds", lower = 1, upper = 1000)
)

rin = makeResampleInstance(cv2 , bike.task)

lrn = mlr::makeLearner(cl = 'regr.xgboost.mod')

fn = function(x){
  if(x$booster == "gbtree") x$alpha = NULL
  if(x$booster == "gblinear") x$max_depth = NULL
  lrn = setHyperPars(lrn, par.vals = x)
  perf = resample(learner = lrn, show.info = FALSE,
    task = bike.task , resampling = rin ,
    measures = list(mae))$aggr
  mod = train(lrn, bike.task)
  pred = Predictor$new(mod, bike.x)
  #sob = interaction.strength(pred)
  c(perf, n.features(pred), round(lin_or_bin_tree(pred), 2), max(0, round(interaction.strength(pred), 1)))
}
obj.fun = makeMultiObjectiveFunction(fn = fn, par.set = par.set, n.objectives = 4, has.simple.signature = FALSE)

ctrl = makeMBOControl(n.objectives = 4L)
ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
ctrl = setMBOControlMultiObj(ctrl, method = "parego")

mbo.lrn = makeLearner("regr.randomForest", predict.type = "se", ntree = 200)
mbo.lrn = makeImputeWrapper(mbo.lrn, classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__")))

design = generateDesign(n = 10L, par.set = par.set, fun = lhs::randomLHS)

mbo.iml = mbo(fun = obj.fun, design = design, learner = mbo.lrn, control = ctrl)


pareto.set = rbindlist(lapply(mbo.iml$pareto.set, data.frame))
best.models = cbind(round(mbo.iml$pareto.front, 2), pareto.set)
best.models %>%
  arrange(y_1)

mae_0 = measureMAE(truth = bike$cnt, response = mean(bike$cnt))

ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = (ncol(bike.x) - y_2) * (1 - y_3) * (1 - y_4))) + geom_point() +
  geom_label(aes(label = y_2, fill = booster)) +
  scale_x_continuous("Interpretability") +
  scale_y_continuous("Accuracy")


ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = 1 - y_3)) + geom_point() +
  geom_label(aes(label = y_2, fill = booster)) +
  scale_x_continuous("Interpretability") +
  scale_y_continuous("Accuracy")

