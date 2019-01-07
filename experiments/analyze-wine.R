devtools::load_all()

wdat = read.csv(file.path(data_dir, "winequalityN.csv"))
# Kicking out 36 wines with missing values
wdat = na.omit(wdat)
# Not sure if there is some order to it
set.seed(42)
wdat = wdat[sample(1:nrow(wdat)),]
n = nrow(wdat)


lrn = makeLearner("regr.ranger")
tsk = makeRegrTask(data = wdat, target = "quality")
mod = mlr::train(lrn, tsk,  subset = seq(1, n, 2))
preds = predict(mod, task = tsk, subset = seq(2, n, 2))
performance(preds, measures = list(mae, mse))
pred  = Predictor$new(mod, data = wdat, y = "quality")

n_segs(pred)
ale_fanova(pred)

plot_all_ale = function(pred) {
  pls = lapply(setdiff(colnames(wdat), c("quality")), function(fname) {
    fe = FeatureEffect$new(pred, fname, method = "ale")
    res = fe$results
    colnames(res)[colnames(res) == fname] = ".feature"
    res$.feature.name = fname
    res$.feature = as.numeric(res$.feature)
    res$.feature.type = fe$feature.type
    res
  })

  res  = rbindlist(pls, use.names = TRUE)
  res$.feature = as.numeric(res$.feature)

  ggplot(res, aes(x = as.numeric(.feature), y= .ale)) +
    geom_line(aes(alpha = .feature.type)) +
    geom_point() +
    facet_wrap(".feature.name", scales = "free_x") +
    scale_alpha_discrete(guide = "none")
}

plot_all_ale(pred)

fi = FeatureImp$new(pred, loss = "mae", n.repetitions = 10)
fi$plot()

x.interest = wdat[20,]
sh = Shapley$new(pred, x.interest = x.interest)
plot(sh)



## Simpler model


lrn = makeLearner("regr.rpart", maxdepth = 2)
tsk = makeRegrTask(data = wdat, target = "quality")
mod = mlr::train(lrn, tsk,  subset = seq(1, n, 2))
preds = predict(mod, task = tsk, subset = seq(2, n, 2))
performance(preds, measures = list(mae, mse))
pred.tree  = Predictor$new(mod, data = wdat, y = "quality")

n_segs(pred.tree)
ale_fanova(pred.tree)

plot_all_ale(pred.tree)

mod$learner.model


fi = FeatureImp$new(pred, loss = "mae", n.repetitions = 10)
plot(fi)

plot_segs_feature(pred, feature.name = "alcohol", cat.mode = FALSE)
plot_segs_feature(pred, feature.name = "volatile.acidity", cat.mode = FALSE)
n_segs_feature(pred, "alcohol", cat.mode = TRUE)
n_segs_feature(pred, "sulphates", cat.mode = FALSE)
sh = Shapley$new(pred, x.interest = x.interest)
plot(sh)
li = LocalModel$new(pred, x.interest = x.interest)
plot(li)

lrn = makeLearner("regr.lm")
tsk = makeRegrTask(data = wdat, target = "quality")
mod = mlr::train(lrn, tsk,  subset = seq(1, n, 2))
preds = predict(mod, task = tsk, subset = seq(2, n, 2))
performance(preds, measures = list(mae, mse))
pred  = Predictor$new(mod, data = wdat, y = "quality")


