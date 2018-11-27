# ----------------------------------------------------------------------------
# Analyze the degrees of freedom measure
# ----------------------------------------------------------------------------
devtools::load_all()

library(mlbench)

n = 100
dat = data.frame(mlbench::mlbench.friedman1(n))


library(randomForest)
rf = randomForest(y ~ ., data = dat, ntree = 10000)

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


n_segs(pred, "x.1")
n_segs(pred, "x.2")
n_segs(pred, "x.3")
n_segs(pred, "x.4")
n_segs(pred, "x.5")
n_segs(pred, "x.6")
n_segs(pred, "x.7")
n_segs(pred, "x.8")
n_segs(pred, "x.9")
n_segs(pred, "x.10")





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
