# ----------------------------------------------------------------------------
# Analyze the degrees of freedom measure
# ----------------------------------------------------------------------------
devtools::load_all()

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
pred = Predictor$new(mod, dat)

# Example with tree ALE
lrn = makeLearner("regr.rpart")
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)

# Example with SVM ALE
lrn = makeLearner("regr.svm")
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)



# ----------------------------------------------------------------------------
# Increasing interactions with increasing tree depth
# ----------------------------------------------------------------------------




