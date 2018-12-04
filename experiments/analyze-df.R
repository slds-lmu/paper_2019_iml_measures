# ----------------------------------------------------------------------------
# Analyze the degrees of freedom measure
# ----------------------------------------------------------------------------
devtools::load_all()
set.seed(42)

library(mlbench)
library(randomForest)
library(rpart)
library(partykit)

n = 500
dat = data.frame(mlbench::mlbench.friedman1(n))
dat$x.1 = cut(dat$x.1, breaks = c(0, 0.1, 0.2, 0.4, 0.7, 0.8, 1))


tsk = makeRegrTask(data = dat, target = "y")

# Example with linear model ALE
lrn = makeLearner("regr.lm")
mod = train(lrn, tsk)
mod$learner.model
pred = Predictor$new(mod, dat)
n_segs(pred)
n_segs_feature(pred, "x.1", cat.mode = FALSE, plot = TRUE, epsilon = 0.05)
n_segs_feature(pred, "x.4", cat.mode = FALSE, plot = TRUE, epsilon = 0.05)
n_segs_feature(pred, "x.4",  plot = TRUE, epsilon = 0.05, cat.mode = TRUE)

# Example with tree ALE
lrn = makeLearner("regr.rpart")
mod = train(lrn, tsk)
mod$learner.model
pred = Predictor$new(mod, dat)
n_segs(pred)
n_segs_feature(pred, "x.1", cat.mode = FALSE, plot = TRUE, epsilon = 0.001)
n_segs_feature(pred, "x.4",  plot = TRUE, epsilon = 0.05, cat.mode = FALSE)

# TODO: Check for errors in nodeprune
n_segs_feature(pred, "x.4",  plot = TRUE, epsilon = 0.05, cat.mode = TRUE)

n_segs_feature(pred, "x.2",  plot = TRUE, epsilon = 0.05, cat.mode = FALSE)
n_segs_feature(pred, "x.2",  plot = TRUE, epsilon = 0.05, cat.mode = TRUE)

n_segs_feature(pred, "x.3",  plot = TRUE, epsilon = 0.05, cat.mode = FALSE)
n_segs_feature(pred, "x.3",  plot = TRUE, epsilon = 0.1, cat.mode = TRUE)

# Example with SVM ALE
lrn = makeLearner("regr.ksvm")
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)
n_segs(pred)
n_segs_feature(pred, "x.1", cat.mode = FALSE, plot = TRUE, epsilon = 0.05)
n_segs_feature(pred, "x.2", cat.mode = FALSE, plot = TRUE, epsilon = 0.05)
n_segs_feature(pred, "x.6",  plot = TRUE, epsilon = 0.05, cat.mode = TRUE)
n_segs_feature(pred, "x.6",  plot = TRUE, epsilon = 0.05, cat.mode = FALSE)

# ----------------------------------------------------------------------------
# Effect of choosing different epsilons/alphas
# ----------------------------------------------------------------------------

n_segs_feature(pred, "x.6",  plot = TRUE, epsilon = 0.05, cat.mode = FALSE)
n_segs_feature(pred, "x.6",  plot = TRUE, epsilon = 0.1, cat.mode = FALSE)
n_segs_feature(pred, "x.6",  plot = TRUE, epsilon = 0.001, cat.mode = FALSE, max_df = 20)

# ----------------------------------------------------------------------------
# Effect of weighting vs. no weighting
# ----------------------------------------------------------------------------

# Example with one numerical feature
# Maybe another example with one categorical example where almost  everything is in one category
# TODO
