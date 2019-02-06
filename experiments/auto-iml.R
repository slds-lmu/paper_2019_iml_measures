devtools::load_all()
set.seed(42)



TASK_TYPE = "regr"

tasks = listOMLTasks()
if(TASK_TYPE == "regr") {
  tasks = tasks[grepl("Supervised Regression", tasks$task.type), ]
} else {
  tasks = tasks[grepl("Supervised Classification", tasks$task.type), ]
  tasks = tasks[tasks$number.of.classes  == 2, ]
}
tasks = tasks[tasks$number.of.instances.with.missing.values == 0, ]
tasks = tasks[tasks$number.of.symbolic.features > 0, ]
tasks = tasks[tasks$number.of.features < 150, ]
tasks = tasks[tasks$number.of.features > 4, ]
tasks = tasks[tasks$number.of.instances < 10000, ]
tasks = tasks[tasks$number.of.instances > 200, ]
tasks = tasks[!is.na(tasks$task.id), ]

i = 6


task = getOMLTask(task.id = tasks$task.id[i])
task = convertOMLTaskToMlr(task)
task = task$mlr.task
task.dat = getTaskData(task)
summary(task.dat)
print(dim(task.dat))



devtools::load_all()
set.seed(42)
wine = read.csv("./data/winequalityN.csv")
wine = na.omit(wine)

WINE_SAMPLE = sample(1:nrow(wine), size = 300)
wine = wine[WINE_SAMPLE, ]

task = makeRegrTask(data = wine, target = "quality")
task.dat = getTaskData(task)
summary(task.dat)
print(dim(task.dat))


# #
# n = 300
# dat = data.frame(mlbench::mlbench.friedman3(n))
#
# task = makeRegrTask(data = dat, target = "y")
# task.dat = dat
#
# lrn1 = makeLearner("regr.rpart")
# mod1 = train(lrn1, task)
# pred1 = Predictor$new(mod1, task.dat)
# fc1 = FunComplexity$new(pred1)
#
# fc1$complexity_total
# fc1$complexity_total
# fc1$complexity_total
# fc1$var_explained
# fc1$plot_complexity("x.1")
#
# FeatureEffects$new(pred1)$plot()
#
# f4 = FeatureEffect$new(pred1, "SEX")
# table(f4$predict(task.dat), task.dat$SEX)


base.learners.classif = list(
  makeLearner("classif.ksvm", predict.type = "prob"),
  makeLearner("classif.rpart", predict.type = "prob"),
  makeLearner("classif.gamboost", predict.type = "prob"),
  makeLearner("classif.kknn", predict.type = "prob"),
  makeLearner("classif.naiveBayes", predict.type = "prob")
)

base.learners.regr = list(
  makeLearner("regr.ksvm"),
  makeLearner("regr.xgboost.mod"),
  makeLearner("regr.rpart"),
  makeLearner("regr.gamboost")
)

lrn.regr = makeModelMultiplexer(base.learners.regr)
lrn.classif = makeModelMultiplexer(base.learners.classif)

ps.regr = makeModelMultiplexerParamSet(lrn.regr,
  makeNumericParam("C", lower = 0, upper = 100),
  makeIntegerParam ("max_depth" , lower = 1, upper = 6, requires = quote(booster == "gbtree")),
  makeIntegerParam ("maxdepth" , lower = 1, upper = 10),
  makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
  makeNumericParam("alpha", lower = 0, upper = 100, requires = quote(booster == "gblinear")),
  makeIntegerParam("nrounds", lower = 1, upper = 1000),
  makeIntegerParam("mstop", lower = 1, upper = 1000)
)

ps.classif = makeModelMultiplexerParamSet(lrn.classif,
  makeNumericParam("C", lower = 0, upper = 100),
  makeIntegerParam ("maxdepth" , lower = 1, upper = 10),
  makeIntegerParam("mstop", lower = 1, upper = 1000),
  makeIntegerParam("k", lower = 1, upper = 10),
  makeNumericParam("laplace", lower = 0, upper = 1)
)

rin = makeResampleInstance(cv2 , task)

if(TASK_TYPE == "classif") {
  lrn = lrn.classif
  loss = auc
  ps = ps.classif
} else {
  lrn = lrn.regr
  loss = mae
  ps = ps.regr
}


sample.size = min(nrow(task.dat), 300)
subset_index = sample(1:nrow(task.dat), size = sample.size)

fn = function(x){
  # removes unused params
  x = x[!is.na(x)]
  lrn = setHyperPars(lrn, par.vals = x)
  perf = resample(learner = lrn, show.info = FALSE,
    task = task , resampling = rin ,
    measures = list(loss))$aggr
  mod = train(lrn, task)
  pred = Predictor$new(mod, task.dat[subset_index,], y = task$task.desc$target)
  imeasure = FunComplexity$new(pred)
  c(round(perf, 2),
    round(imeasure$complexity_wavg2, 2),
    round(1 - imeasure$var_explained, 2))
}

obj.fun = makeMultiObjectiveFunction(fn = fn, par.set = ps, n.objectives = 3, has.simple.signature = FALSE)

ctrl = makeMBOControl(n.objectives = 3L)
ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
ctrl = setMBOControlMultiObj(ctrl, method = "parego")

mbo.lrn = makeLearner("regr.randomForest", predict.type = "se", ntree = 200)
mbo.lrn = makeImputeWrapper(mbo.lrn, classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__")))

design = generateDesign(n = 20L, par.set = ps, fun = lhs::randomLHS)

mbo.iml = mbo(fun = obj.fun, design = design, learner = mbo.lrn, control = ctrl)


pareto.set = rbindlist(lapply(mbo.iml$pareto.set, data.frame))
best.models = cbind(round(mbo.iml$pareto.front, 2), pareto.set)
best.models %>%
  arrange(y_1)


best.models %>%
  arrange(y_2)


best.models$overall_complexity = best.models$y_2 * (1 + best.models$y_3)
best.models$interpretability  = 1 - (best.models$overall_complexity / max(best.models$overall_complexity))

y = task.dat[task$task.desc$target][[1]]
mae_0 = measureMAE(truth = y, response = mean(y))
mae_0 = 0.5
ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = interpretability)) + geom_point() +
  geom_label(aes(label = selected.learner, fill = y_3)) +
  scale_x_continuous("Interpretability") +
  scale_y_continuous("Accuracy")


ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = 1 - y_3)) + geom_point() +
  geom_label(aes(label = selected.learner)) +
  scale_x_continuous("Interpretability") +
  scale_y_continuous("Accuracy")




ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = (max(y_2) - y_2) / max(y_2) * (1 - y_3))) + geom_point() +
  geom_label(aes(label = selected.learner)) +
  scale_x_continuous("Interpretability") +
  scale_y_continuous("Accuracy")
