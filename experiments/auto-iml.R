# Title: Multicrit performance and interpretability
devtools::load_all()
set.seed(42)



TASK_TYPE = "classif"

tasks = listOMLTasks()
tasks = tasks[grepl("Supervised Classification", tasks$task.type), ]
#tasks = tasks[grepl("Supervised Regression", tasks$task.type), ]
tasks = tasks[tasks$number.of.instances.with.missing.values == 0, ]
tasks = tasks[tasks$number.of.symbolic.features > 0, ]
tasks = tasks[tasks$number.of.features < 50, ]
tasks = tasks[tasks$number.of.features > 4, ]
tasks = tasks[tasks$number.of.instances < 5000, ]
tasks = tasks[tasks$number.of.instances > 400, ]
tasks = tasks[!is.na(tasks$task.id), ]
tasks = tasks[tasks$number.of.classes  == 2, ]
i = 26

task = getOMLTask(task.id = tasks$task.id[i])
task = convertOMLTaskToMlr(task)
task.dat = getTaskData(task$mlr.task)
summary(task.dat)
print(dim(task.dat))

# lrn = makeLearner("regr.gamboost", mstop = 171)
lrn = makeLearner("classif.rpart", maxdepth = 7, predict.type = 'prob')
lrn = makeLearner("classif.logreg", predict.type = 'prob')
lrn = makeLearner("classif.lda", predict.type = 'prob')

lrn = makeLearner("classif.naiveBayes", predict.type = 'prob')

# lrn = makeLearner("regr.lm")
# lrn = makeLearner("regr.ranger")
# lrn = makeLearner("regr.ksvm")

trained = train(lrn, task$mlr.task)
#
#
pred = Predictor$new(trained, data = task.dat, y = task$mlr.task$task.desc$target, class = 1)
# #
ale_fanova(pred)
sum_df(pred)



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

rin = makeResampleInstance(cv2 , task$mlr.task)

if(TASK_TYPE == "classif") {
  lrn = lrn.classif
  loss = auc
  ps = ps.classif
} else {
  lrn = lrn.regr
  loss = mae
  ps = ps.regr
}



fn = function(x){
  # removes unused params
  x = x[!is.na(x)]
  lrn = setHyperPars(lrn, par.vals = x)
  perf = resample(learner = lrn, show.info = FALSE,
    task = task$mlr.task , resampling = rin ,
    measures = list(loss))$aggr
  mod = train(lrn, task$mlr.task)
  pred = Predictor$new(mod, task.dat, y = task$mlr.task$task.desc$tar, class = 1)
  c(1 - perf, round(n_segs(pred), 2), max(0, ale_fanova(pred)))
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

y = task.dat[task$mlr.task$task.desc$target][[1]]
#mae_0 = measureMAE(truth = y, response = mean(y))
mae_0 = 0.5
ggplot(best.models, aes(y = (mae_0 - y_1)/( mae_0 - min(y_1)),
  x = (max(y_2) - y_2) / max(y_2))) + geom_point() +
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
