#
context("FunComplexity")
devtools::load_all()



set.seed(42)
n = 300
dat = data.frame(mlbench::mlbench.friedman3(n))
task = makeRegrTask(data = dat, target = "y")

lrn1 = makeLearner("regr.lm")
mod1 = train(lrn1, task)
pred1 = Predictor$new(mod1, dat)
fc1 = FunComplexity$new(pred1, epsilon = 0.1)


test_that("Interaction in linear case", {
  expect_equal(fc1$r2, 1, tolerance = 0.001)
})


test_that("Complexity in linear case", {
  expect_equal(fc1$c_wmean, 1)
  p = fc1$plot_complexity("x.1")
  expect_class(p, "ggplot")
  print(p)
})

