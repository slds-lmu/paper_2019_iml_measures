devtools::load_all()



#==============================================================================
context("segment_fn")
#==============================================================================


set.seed(42)
n = 300
dat = data.frame(mlbench::mlbench.friedman3(n))
task = makeRegrTask(data = dat, target = "y")

lrn.lm = makeLearner("regr.lm")
lm.mod = train(lrn.lm, task)
pred.lm = Predictor$new(lm.mod, dat)
ale.lm1 = FeatureEffect$new(pred.lm, feature = "x.1")
sst.ale.x1 = var(ale.lm1$predict(dat$x.1))


test_that("does not accept out of range pars", {
  expect_error(segment_fn(-10, ale = ale.lm1, SST=sst.ale.x1, x = dat$x.1, ale_prediction = ale1$predict(dat$x.1)))
})

test_that("cost always 0 for lm", {
  expect_equal(0, segment_fn(2,                 ale = ale.lm1, SST=sst.ale.x1, x = dat$x.1, ale_prediction = ale.lm1$predict(dat$x.1)))
  expect_equal(0, segment_fn(c(10, 20, 50, 88), ale = ale.lm1, SST=sst.ale.x1, x = dat$x.1, ale_prediction = ale.lm1$predict(dat$x.1)))
  expect_equal(0, segment_fn(c(1, 99),          ale = ale.lm1, SST=sst.ale.x1, x = dat$x.1, ale_prediction = ale.lm1$predict(dat$x.1)))
})


lrn2 = makeLearner("regr.ksvm")
mod2 = train(lrn2, task)
pred2 = Predictor$new(mod2, dat)
ale2 = FeatureEffect$new(pred2, feature = "x.1")
sst2 = ssq(ale2$predict(dat$x.1))


test_that("cost matches correct cost", {
  pars = c(10, 30)
  x.cut = cut(dat$x.1, breaks = c(min(dat$x.1), pars, max(dat$x.2)), include.lowest = TRUE)
  pred = ale2$predict(dat$x.1)
  mod = lm(pred ~ dat$x.1 * x.cut)
  eps = sum(ssq(mod$residuals)) / sst2
  expect_equal(eps,segment_fn(pars, ale = ale2, SST=sst2, x = dat$x.1, ale_prediction = ale2$predict(dat$x.1)))


  pars = c(66)
  x.cut = cut(dat$x.1, breaks = c(min(dat$x.1), pars, max(dat$x.2)), include.lowest = TRUE)
  pred = ale2$predict(dat$x.1)
  mod = lm(pred ~ dat$x.1 * x.cut)
  eps = sum(ssq(mod$residuals)) / sst2
  expect_equal(eps,segment_fn(pars, ale = ale2, SST=sst2, x = dat$x.1, ale_prediction = ale2$predict(dat$x.1)))


  pars = 1:10
  x.cut = cut(dat$x.1, breaks = c(min(dat$x.1), pars, max(dat$x.2)), include.lowest = TRUE)
  pred = ale2$predict(dat$x.1)
  mod = lm(pred ~ dat$x.1 * x.cut)
  eps = sum(ssq(mod$residuals)) / sst2
  expect_equal(eps,segment_fn(pars, ale = ale2, SST=sst2, x = dat$x.1, ale_prediction = ale2$predict(dat$x.1)))
})


test_that("order of pars irrelevant", {
  expect_equal(segment_fn(c(10, 50, 60), ale = ale2, SST=sst2, x = dat$x.1, ale_prediction = ale2$predict(dat$x.1)),
    segment_fn(c(50, 10, 60), ale = ale2, SST=sst2, x = dat$x.1, ale_prediction = ale2$predict(dat$x.1)))

  ale2 = FeatureEffect$new(pred2, feature = "x.2")
  sst2 = var(ale2$predict(dat$x.2))
  expect_equal(segment_fn(c(1000, 500, 600), ale = ale2, SST=sst2, x = dat$x.2, ale_prediction = ale2$predict(dat$x.2)),
    segment_fn(c(500, 1000, 600), ale = ale2, SST=sst2, x = dat$x.2, ale_prediction = ale2$predict(dat$x.2)))
})



#==============================================================================
context("step_fn")
#==============================================================================

set.seed(42)
n = 300
dat = data.frame(mlbench::mlbench.friedman3(n))
x2_continuous = dat$x.2
dat$x.2 = cut(dat$x.2, breaks = c(min(dat$x.2), 200, 300, 500, 1000, 1200, max(dat$x.2)), include.lowest = TRUE)
task = makeRegrTask(data = dat, target = "y")

lrn3 = makeLearner("regr.lm")
mod3 = train(lrn3, task)
pred3 = Predictor$new(mod3, dat)
ale3 = FeatureEffect$new(pred3, feature = "x.2")
sst3 = ssq(ale3$predict(dat$x.2))

tab3 = left_join(ale3$results, data.frame(table(dat$x.2)), by = c("x.2"= "Var1"))
colnames(tab3) = c("ale", "x.2", "type", "n")
tab3 = data.table(tab3)

test_that("does not accept out of range pars",{
  expect_error(step_fn(-10, ale = ale3, SST=sst3, x = dat$x.2, ale_prediction = ale1$predict(dat$x.2)))
})


test_that("cost matches correct cost", {
  pars = c(1)

  mean2 = weighted.mean(tab3$ale[-1], w = tab3$n[-1])
  eps = (0 + ssq(sqrt(tab3$n[-1]) * (tab3$ale[-1] - mean2))) / sst3
  expect_equal(eps,step_fn(pars, tab3, sst3))


  pars = c(2,3)
  mean1 = weighted.mean(tab3$ale[1:2], w = tab3$n[1:2])
  mean3 = weighted.mean(tab3$ale[4:6], w = tab3$n[4:6])
  eps = (ssq(sqrt(tab3$n[1:2]) * (tab3$ale[1:2] - mean1)) + 0 +
      ssq(sqrt(tab3$n[4:6]) * (tab3$ale[4:6] - mean3))) / sst3
  expect_equal(eps,step_fn(pars, tab3, sst3))

  pars = 1:nrow(tab3)
  expect_equal(0,step_fn(pars, tab3, sst3))
})


test_that("order of pars irrelevant", {
  expect_equal(step_fn(c(1,4), tab3, sst3),step_fn(c(4,1), tab3, sst3))
})


