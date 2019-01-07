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

fi = FeatureImp$new(pred, loss = "mae", n.repetitions = 10)
plot(fi)

fe = FeatureEffect$new(pred, "density")
plot(fe)


fe = FeatureEffect$new(pred, "type")
plot(fe)


fe = FeatureEffect$new(pred, "fixed.acidity")
plot(fe)


fe = FeatureEffect$new(pred, "citric.acid")
plot(fe)



x.interest = wdat[20,]
sh = Shapley$new(pred, x.interest = x.interest)
plot(sh)



## Simpler model


lrn = makeLearner("regr.rpart", maxdepth = 2)
tsk = makeRegrTask(data = wdat, target = "quality")
mod = mlr::train(lrn, tsk,  subset = seq(1, n, 2))
preds = predict(mod, task = tsk, subset = seq(2, n, 2))
performance(preds, measures = list(mae, mse))
pred  = Predictor$new(mod, data = wdat, y = "quality")

n_segs(pred)
ale_fanova(pred)


mod$learner.model
fe = FeatureEffect$new(pred, "alcohol")
plot(fe)

fe = FeatureEffect$new(pred, "volatile.acidity")
plot(fe)

fe = FeatureEffect$new(pred, "sulphates")
plot(fe)


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


