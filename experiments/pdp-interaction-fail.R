# Partial dependence plots fail with interactions


# simulate dataset with three features. one for the lines, one for the interaction, one for variance.

set.seed(42)
n = 400

x1 = sample(c(0, 1), size = n, replace = TRUE)
x2 = rnorm(n)
x3 = rnorm(n)

dat = data.frame(x1,x2,x3)

pred.fun = function(newdata) {
  slope = -2 + 4 * newdata$x1
  slope * newdata$x2 + newdata$x3
}

# show pdp and ice.

pred = Predictor$new(predict.fun = pred.fun, data = dat)

FeatureEffect$new(pred, "x2", method = "pdp+ice")$plot()

FeatureEffect$new(pred, "x2")$plot() +
  scale_y_continuous(limits = c(-10, 10))

# show interaction measure
ale_fanova(pred)


# Other features
FeatureEffect$new(pred, "x1", method = "pdp+ice")$plot()
FeatureEffect$new(pred, "x3", method = "pdp+ice")$plot()
