devtools::load_all()
n = 30
dat = mlbench::mlbench.xor(n, d=2)
dat  = data.frame(dat$x, y =  dat$classes)
colnames(dat) = c("feature_1", "feature_2", "y")

pred.fun = function(newdata) {
  1 * (newdata$feature_1 > 0 & newdata$feature_2 <= 0 |
    newdata$feature_1 <= 0 & newdata$feature_2 > 0)
}


gr = 25
pred = iml::Predictor$new(predict.fun = pred.fun, data = dat)
FeatureEffect$new(pred, "feature_1", grid.size = gr)$plot()
FeatureEffect$new(pred, "feature_2", grid.size = gr)$plot()
ale_fanova(pred, gr)
measureMAE(truth = dat$y == 1, response = pred$predict(dat)[[1]])

xx = mean(unlist(lapply(1:50, function(i) {
  x = ale_fanova(pred, grid.size = i)
  print(paste(i, x))
  x
})))
median(xx)

sum_df(pred)
