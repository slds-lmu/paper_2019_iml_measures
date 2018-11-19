# Title: Multicrit performance and interpretability

## Build a model
library(data.table)
library(mlr)
library("iml")
library("ggplot2")
library(mlrMBO)
library(dplyr)
library("OpenML")

set.seed(42)

load("~/repos/interpretable-ml-book/data/bike.RData")
source("R/iml-measures.R")
source("R/xgboost.R")


tasks = listOMLTasks()
tasks = tasks[grepl("Supervised Regression", tasks$task.type), ]
tasks = tasks[tasks$number.of.instances.with.missing.values == 0, ]
i = 206

task = getOMLTask(task.id = tasks$task.id[i])
task = convertOMLTaskToMlr(task)
task.dat = getTaskData(task$mlr.task)
print(dim(task.dat))


#bike.task = makeRegrTask(data = bike, target = "cnt")
#task = bike.task


# mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.svm'), task$mlr.task)
# lm.bike = mlr::train(mlr::makeLearner(cl = 'regr.lm'), task$mlr.task)
# rpart.bike = mlr::train(mlr::makeLearner(cl = 'regr.rpart'), task$mlr.task)
#
# pred = Predictor$new(mod.bike, task.dat, y = task$mlr.task$task.desc$target)
# pred.lm = Predictor$new(lm.bike, task.dat, y = task$mlr.task$task.desc$target)
# pred.rpart = Predictor$new(rpart.bike, task.dat, y = task$mlr.task$task.desc$target)


par.set = makeParamSet(
  makeDiscreteParam("learner", values = c("rpart", "xgboost.mod")),
  #makeDiscreteParam("learner", values = c("rpart", "xgboost.mod", "svm")),
  #makeNumericParam("cost", lower = 0, upper = 100, requires = quote(learner == "svm")),
  makeIntegerParam ("max_depth" , lower = 1, upper = 6, requires = quote(booster == "gbtree" & learner == "xgboost.mod")),
  makeIntegerParam ("maxdepth" , lower = 1, upper = 10, requires = quote(learner == "rpart")),
  makeDiscreteParam("booster", values = c("gbtree", "gblinear"), requires = quote(learner == "xgboost.mod")),
  makeNumericParam("alpha", lower = 0, upper = 100, requires = quote(booster == "gblinear" & learner == "xgboost.mod")),
  makeIntegerParam("nrounds", lower = 1, upper = 1000, requires = quote(learner == "xgboost.mod"))
)

rin = makeResampleInstance(cv2 , task$mlr.task)

lrn.xgboost = mlr::makeLearner(cl = 'regr.xgboost.mod')
lrn.rpart = mlr::makeLearner(cl = 'regr.rpart')
lrn.svm = mlr::makeLearner(cl = "regr.svm")

fn = function(x){
  if(x$learner == "rpart") {
    print("rpart learner")
    lrn = lrn.rpart
    x$max_depth = x$booster = x$alpha = x$nrounds = x$cost =  NULL
  } else if (x$learner == "xgboost.mod") {

    lrn = lrn.xgboost
    x$maxdepth = x$cost = NULL
    if(x$booster == "gbtree") {
      x$alpha = NULL
      print("gbtree learner")
    }
    if(x$booster == "gblinear") {
      x$max_depth = NULL
      print("gblinear learner")

    }
  } else {
    print("svm learner")
    x$max_depth = x$booster = x$alpha = x$nrounds = x$maxdepth = NULL
    lrn = lrn.svm
  }
  x$learner = NULL
  lrn = setHyperPars(lrn, par.vals = x)
  perf = resample(learner = lrn, show.info = FALSE,
    task = task$mlr.task , resampling = rin ,
    measures = list(mae))$aggr
  mod = train(lrn, task$mlr.task)
  pred = Predictor$new(mod, task.dat[sample(1:nrow(task.dat), size = 100),])
  #sob = interaction.strength(pred)
  c(perf, round(sum_df(pred), 2), max(0, round(interaction.strength(pred), 1)))
}
obj.fun = makeMultiObjectiveFunction(fn = fn, par.set = par.set, n.objectives = 3, has.simple.signature = FALSE)

ctrl = makeMBOControl(n.objectives = 3L)
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

y = task.dat[task$mlr.task$task.desc$target][[1]]
mae_0 = measureMAE(truth = y, response = mean(y))
#
# ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
#   x = (ncol(task.dat) - y_2) * (1 - y_3) * (1 - y_4))) + geom_point() +
#   geom_label(aes(label = y_2, fill = booster)) +
#   scale_x_continuous("Interpretability") +
#   scale_y_continuous("Accuracy")


ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = (max(y_2) - y_2) / max(y_2))) + geom_point() +
  geom_label(aes(label = learner, fill = y_3)) +
  scale_x_continuous("Interpretability") +
  scale_y_continuous("Accuracy")


ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = 1 - y_3)) + geom_point() +
  geom_label(aes(label = learner)) +
  scale_x_continuous("Interpretability") +
  scale_y_continuous("Accuracy")




ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = (max(y_2) - y_2) / max(y_2) * (1 - y_3))) + geom_point() +
  geom_label(aes(label = learner)) +
  scale_x_continuous("Interpretability") +
  scale_y_continuous("Accuracy")
