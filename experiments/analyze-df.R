# ----------------------------------------------------------------------------
# Analyze the degrees of freedom measure
# ----------------------------------------------------------------------------
devtools::load_all()

library(mlbench)
library(randomForest)

n = 100
dat = data.frame(mlbench::mlbench.friedman1(n))

dat$x.1 = cut(dat$x.1, breaks = c(0, 0.1, 0.2, 0.4, 0.7, 0.8, 1))
dat$x.2 = cut(dat$x.2, breaks = c(0, 0.1, 0.2, 0.4, 0.7, 0.8, 1))

rf = randomForest(y ~ ., data = dat, ntree = 10000)

rf = rpart(y ~ ., data = dat, control = rpart.control(maxdepth = 2))

pred = Predictor$new(rf, dat)


sum_df(pred)

FeatureEffect$new(pred, "x.1")$plot()
df_feature(pred, "x.1")
df_tree_feature(pred, "x.1")
df_spline_feature(pred, "x.1")


FeatureEffect$new(pred, "x.2")$plot()
df_feature(pred, "x.2")
df_tree_feature(pred, "x.2")
df_spline_feature(pred, "x.2")


plot_feature = function(fname, eps = 0.01, plot = FALSE) {
  print(df_feature(pred, fname, eps = eps))
  print(df_tree_feature(pred, fname, eps = eps, plot = plot))
  print(df_spline_feature(pred, fname, eps = eps, plot = plot))
}

plot_feature("x.1", plot = TRUE, eps = 0.01)
plot_feature("x.2", plot = TRUE, eps = 0.01)
plot_feature("x.3", plot = TRUE, eps = 0.01)
plot_feature("x.4", plot = TRUE)
plot_feature("x.5", plot = TRUE, eps = 0.1)
plot_feature("x.6", plot = TRUE)
plot_feature("x.7", plot = TRUE, eps = 0.1)
plot_feature("x.8", plot  = TRUE, eps = 0.05)
plot_feature("x.9", plot = TRUE, eps = 0.01)
plot_feature("x.10")


ale_length(pred, "x.1", grid.size = 50)
ale_length(pred, "x.2", grid.size = 50)
ale_length(pred, "x.3", grid.size = 50)
ale_length(pred, "x.4", grid.size = 50)
ale_length(pred, "x.5", grid.size = 50)
ale_length(pred, "x.6", grid.size = 50)
ale_length(pred, "x.7", grid.size = 50)
ale_length(pred, "x.8", grid.size = 50)
ale_length(pred, "x.9", grid.size = 50)
ale_length(pred, "x.10", grid.size = 50)



n_segs(pred)
n_segs_feature(pred, "x.2", cat.mode = TRUE, plot = FALSE)





# ----------------------------------------------------------------------------
# Effect of choosing different epsilons
# ----------------------------------------------------------------------------




# TODO

# ----------------------------------------------------------------------------
# Effect of weighting vs. no weighting
# ----------------------------------------------------------------------------

# Example with one numerical feature
# Maybe another example with one categorical example where almost  everything is in one category
# TODO
