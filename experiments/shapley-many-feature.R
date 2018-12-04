# Many interpretability methods become unusable with many features.


# simulate dataset with many features with decreasing importance.

set.seed(42)
n = 300
n.features = 100

generate_data = function(n, n.features) {
  dat  = data.frame(round(matrix(rnorm(n * n.features), ncol = n.features), 2))
  f = function(newdata) {
    mat = as.matrix(newdata)
    weights1 = rnorm(n.features/2, sd = 0.5)
    weights2 = rnorm(n.features/2, mean = 1, sd = 2)
    rowSums(mat %*% diag(c(weights1, weights2)))
  }
  dat$y = f(dat) + rnorm(n)
  dat
}

dat = generate_data(n, n.features)

lrn = makeLearner("regr.lm")
tsk = makeRegrTask(data = dat, target = "y")
mod = train(lrn, tsk)
dat.x = dat[setdiff(colnames(dat), "y")]
pred = Predictor$new(mod, data = dat, y = "y")



explain_instance = dat.x[1,,drop = FALSE]

# Many features in Shapley and Feature importance plots

shap = Shapley$new(pred, x.interest = explain_instance)
plot(shap)

# fi= FeatureImp$new(pred, loss = "mae")
# plot(fi)



# Instead of the full model we train a sparse model
lrn = makeLearner("regr.glmnet", alpha = 1, lambda = 5)
tsk = makeRegrTask(data = dat, target = "y")
mod.net = train(lrn, tsk)
betas  = as.matrix(mod.net$learner.model$beta)
features_used = rownames(betas)[!betas[,1] == 0]


dat.reduced = dat[c("y", features_used)]
explain_instance_red = explain_instance[features_used]

lrn = makeLearner("regr.lm")
tsk = makeRegrTask(data = dat.reduced, target = "y")
mod.reduced = train(lrn, tsk)
pred.net = Predictor$new(mod.reduced, data = dat.reduced, y = "y")


shap = Shapley$new(pred.net, x.interest = explain_instance_red)
plot(shap)
#
fi = FeatureImp$new(pred.net, loss = "mae", n.repetitions = 100)
plot(fi)

n_segs(pred.net)






# Also lime has the problem of loosing fidelity



library(lime)
explainer = lime(dat.x, mod, bin_continuous = FALSE)
explanation = explain(explain_instance, explainer, n_features = 3)

explanation = explain(dat.x, explainer, n_features = 3)
explanation$model_r2
mean(explanation$model_r2)


explanation$prediction
explanation$model_prediction

explainer.net = lime(dat.reduced, mod.reduced, bin_continuous = FALSE)
explanation.net = explain(dat.reduced, explainer.net, n_features = 3)



explanation.net = explain(dat.reduced, explainer.net, n_features = 3)
explanation.net$model_r2
mean(explanation.net$model_r2)

explanation.net$prediction
explanation.net$model_prediction



