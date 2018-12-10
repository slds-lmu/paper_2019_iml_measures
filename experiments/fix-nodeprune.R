# ----------------------------------------------------------------------------
# Analyze the degrees of freedom measure
# ----------------------------------------------------------------------------

#  It works since I use my own nodeprune locally, ... where I didn't really change anything.
# It remains a mystery
# TODO: checkout why it doesn't work when using partykit::nodeprune.party

devtools::load_all()
set.seed(2)

library(mlbench)
library(randomForest)
library(rpart)
library(partykit)

n = 500
dat = data.frame(mlbench::mlbench.friedman1(n))
dat$x.1 = cut(dat$x.1, breaks = c(0, 0.1, 0.2, 0.4, 0.7, 0.8, 1))


tsk = makeRegrTask(data = dat, target = "y")


# Example with tree ALE
lrn = makeLearner("regr.rpart")
mod = train(lrn, tsk)
mod$learner.model
pred = Predictor$new(mod, dat)


feature.name = "x.4"
grid.size = 50
ale.fun = get_ale_function(pred, "x.4", grid.size = 50)
feature = pred$data$get.x()[[feature.name]]
ale.values = ale.fun(feature)
mod = partykit::ctree(ale.values ~ feature,  control = ctree_control(alpha = 0.001, minbucket = 1))
mod
prune_tree_1n(mod)
