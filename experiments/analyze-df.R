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
n_segs_linear = n_segs(pred)


p1 = plot_segs_feature(pred, "x.1")
p2 = plot_segs_feature(pred, "x.2")
p3 = plot_segs_feature(pred, "x.4")

png(file = file.path(image_dir, "complexity-lm-friedman1.png"), res =  200, height = 400, width = 1000)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

# Example with tree ALE
lrn = makeLearner("regr.rpart")
mod = train(lrn, tsk)
mod$learner.model
pred = Predictor$new(mod, dat)

mod = fit_segs_feature(pred, "x.2")

p1 = plot_segs_feature(pred, "x.1")
p2 = plot_segs_feature(pred, "x.2", cat.mode = TRUE)
p2 = plot_segs_feature(pred, "x.2", cat.mode = FALSE)
p3 = plot_segs_feature(pred, "x.4")

png(file = file.path(image_dir, "complexity-rpart-friedman1.png"), res =  200, height = 400, width = 1000)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
dev.off()


# Example with SVM ALE
lrn = makeLearner("regr.ksvm")
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)
n_segs(pred)


p1 = plot_segs_feature(pred, "x.1")
p2 = plot_segs_feature(pred, "x.2")
p3 = plot_segs_feature(pred, "x.4")

png(file = file.path(image_dir, "complexity-svm-friedman1.png"), res =  200, height = 400, width = 1000)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

# ----------------------------------------------------------------------------
# Effect of choosing different epsilons/alphas
# ----------------------------------------------------------------------------

p1 = plot_segs_feature(pred, "x.6",  plot = TRUE, epsilon = 0.05, cat.mode = FALSE)
p2 = plot_segs_feature(pred, "x.6",  plot = TRUE, epsilon = 0.1, cat.mode = FALSE)
p3 = plot_segs_feature(pred, "x.6",  plot = TRUE, epsilon = 0.001, cat.mode = FALSE, max_df = 20)

png(file = file.path(image_dir, "complexity-svm-epsilons-friedman1.png"), res =  200, height = 400, width = 1000)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
dev.off()


# ----------------------------------------------------------------------------
# Showcase the minimum of cat.mode FALSE and TRUE
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Effect of weighting vs. no weighting
# ----------------------------------------------------------------------------

# Example with one numerical feature
# Maybe another example with one categorical example where almost  everything is in one category
# TODO
