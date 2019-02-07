#
library(iml)
library(mlbench)
library(mlr)

set.seed(42)
n = 300
dat = data.frame(mlbench::mlbench.friedman3(n))
dat$x.5 = factor(1:10)
y.name = "y"
task = makeRegrTask(data = dat, target = y.name)
task.dat = dat

lrn1 = makeLearner("regr.lm")
mod1 = train(lrn1, task)
pred1 = Predictor$new(mod1, task.dat)
fc1 = FunComplexity$new(pred1, epsilon = 0.001)




test_that("Interaction in linear case", {
  expect_equal(fc1$var_explained, 1)
})


test_that("Complexity in linear case", {

  complexity = 2 * (ncol(dat) - 2) + length(unique(dat$x.5))
  expect_equal(fc1$complexity_total, complexity)

  expect_equal(names(fc1$complexities), setdiff(colnames(task.dat), y.name))

  fc1.1 = FunComplexity$new(pred1, base_feat_cost = 0, epsilon = 0.001)
  expect_equal(fc1.1$complexity_total, length(coef(mod1$learner.model)) - 1)
  expect_equal(sum(unlist(fc1.1$complexities)), fc1.1$complexity_total)

  p = fc1$plot_complexity("x.1")
  expect_class(p, "ggplot")
  print(p)
})


lrn2 = makeLearner("regr.rpart")
mod2 = train(lrn2, task)
pred2 = Predictor$new(mod2, task.dat)
fc2 = FunComplexity$new(pred2, epsilon = 0.01)




test_that("Interaction in tree case", {
  expect_true(fc2$var_explained <= 1 & fc2$var_explained >= 0)
})



test_that("Complexity in tree case", {
  expect_equal(fc2$complexities, list(x.1 = 5, x.2 = 4, x.3 = 6, x.4 = 0, x.5 = 0))
)}