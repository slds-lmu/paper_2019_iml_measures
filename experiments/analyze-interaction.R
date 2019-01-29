# ----------------------------------------------------------------------------
# Analyze the degrees of freedom measure
# ----------------------------------------------------------------------------
#devtools::load_all()
set.seed(42)

library(mlbench)
library(randomForest)
library(rpart)
library(partykit)
library(mlr)

devtools::load_all("~/repos/iml/")


# make all things readable in plots
theme_set(theme_gray(base_size=12*(81/169)))


n = 500
dat = data.frame(mlbench::mlbench.friedman2(n))
# dat$x.1 = cut(dat$x.1, breaks = c(0, 0.1, 0.2, 0.4, 0.7, 0.8, 1))
# dat$x.2 = cut(dat$x.2, breaks = c(0, 0.1, 0.2, 0.4, 0.7, 0.8, 1))


tsk = makeRegrTask(data = dat, target = "y")

# Example with linear model ALE
lrn = makeLearner("regr.ranger")
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)

ff = FunComplexity$new(pred)
ff$var_explained
ff$complexity_total
ff$complexities

# Example with GLM ALE
gm = mgcv::gam(y ~ s(x.1) + x.2, data = dat)
pred = Predictor$new(gm, dat)

fc = FunComplexity$new(pred)
fc$var_explained
fc$complexity_total
fc$complexities
fc$plot_feature("x.4")

# Example with tree ALE
lrn = makeLearner("regr.rpart")
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)

fc = FunComplexity$new(pred)
fc$var_explained
fc$complexities
fc$complexity_total
# Example with ranger ALE
lrn = makeLearner("regr.ranger")
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)


FunComplexity$new(pred)$var_explained

# ----------------------------------------------------------------------------
# Increasing interactions with increasing tree depth
# ----------------------------------------------------------------------------

for(mdepth in 1:5) {
  # Example with tree ALE
  lrn = makeLearner("regr.rpart", maxdepth = mdepth)
  mod = train(lrn, tsk)
  pred = Predictor$new(mod, dat)

  print(sprintf("Depth of tree %i, interaction strength: %.2f", mdepth, FunComplexity$new(pred)$var_explained
))
}

# Interesting example: Tree with depth 2 has two different features, but still no interaction since level 2 splits are both the same



# ----------------------------------------------------------------------------
# Building prediction function with ALE plots
# ----------------------------------------------------------------------------

# simulate 2d or 3d features set and a prediction function
n = 100
dat = expand.grid(x1 = seq(from = -2, to = 2, length.out = n),
  x2 = seq(from = -2, to = 2, length.out = n))

f_hat = function(newdata){
  sin(newdata$x1) + newdata$x1 * newdata$x2 + newdata$x2^2
}

dat$y = f_hat(dat)
p_predictor = ggplot(dat, aes(x = x1, y = x2, fill = y, z = y)) +
  geom_tile() + geom_contour() +
  scale_fill_continuous(guide = "none") +
  ggtitle("Prediction function (100% variance to be explained). SST.")

# plot ale for each component

pred = Predictor$new(predict.fun = f_hat, data = dat)

ale1 = FeatureEffect$new(predictor = pred, feature = "x1")
ale_f1 = get_ale_function(pred, "x1", 30)

ale2 = FeatureEffect$new(predictor = pred, feature = "x2")
ale_f2 = get_ale_function(pred, "x2", 30)


predict_ale = function(newdata) {
  mean(newdata$y) + ale_f1(newdata$x1) + ale_f2(newdata$x2)
}

dat3 = dat
dat3$y_ale = predict_ale(dat)
p_predictor_ale = ggplot(dat3, aes(x = x1, y = x2, fill = y_ale, z = y_ale)) +
  geom_tile() + geom_contour() +
  scale_fill_continuous(guide = "none") +
  ggtitle(sprintf("Prediction function first order ALE. Explains %.2f %s variance. SSM.", 100 * ale_fanova(pred), "%"))






p_predictor_diff = ggplot(dat3, aes(x = x1, y = x2, fill = y_ale - y)) +
  geom_tile() +
  scale_fill_gradient2(low = "yellow", mid = "white", high = "red", guide = "none") +
  ggtitle(sprintf("Interactions. Explains %.2f %s variance. SSE.", 100 - 100 * ale_fanova(pred), "%"))


p_predictor
xx2 = gridExtra::grid.arrange(plot(ale1), plot(ale2), nrow = 1)

xx1 = gridExtra::grid.arrange(p_predictor, p_predictor_ale,  nrow = 1)

ale_fanova_plot = gridExtra::grid.arrange(xx1, xx2)
png(file = file.path(image_dir, "ale_fanova.png"), width = 960, res = 200)
plot(ale_fanova_plot)
dev.off()


get.cor = function(x) {
  n = 100
  dat  = data.frame(x1 = rnorm(n))

  dat$x2 = 2 * dat$x1 + rnorm(n, sd = 1.7)
  dat$y = dat$x1 + dat$x2 + rnorm(n, sd = 0.2)

  mod = lm(y ~ ., data = dat)

  pred = Predictor$new(mod, dat)

  ale1 = FeatureEffect$new(pred, "x1")
  ale12 = FeatureEffect$new(pred, c("x1", "x2"))


  ale1$results$.ale2 = ale1$results$.ale
  ale1$results$.ale = NULL

  ale.all = left_join(ale1$results, ale12$results)
  cor(ale.all$.ale, ale.all$.ale2)
}


cors = lapply(1:100, get.cor)
mean(unlist(cors))
