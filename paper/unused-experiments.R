library(Cubist)

dat = data.frame(x = fc2$approx_models$x1$.__enclos_env__$private$x)
y = fc2$approx_models$x1$.__enclos_env__$private$ale_values
model_tree <- cubist(x = dat, y = y, control = cubistControl(rules = 2, unbiased = TRUE, extrapolation = 0))
pred = predict(model_tree, dat, extra)
var(pred - y)/var(y)

ddf = data.frame(x = dat$x, y = y, pred = pred)
ggplot(ddf) + geom_line(aes(x = x, y = y)) + geom_line(aes(x = x, y = pred), color = "red") +
  scale_y_continuous(limits = c(-4, 5))


dat = data.frame(x = rnorm(100))
y = dat$x^2
y[2] = -2

model_tree <- cubist(x = dat, y = y, control = cubistControl(rules = 2, unbiased = TRUE, extrapolation = 100))
pred = predict(model_tree, dat)
#var(pred - y)/var(y)

ddf = data.frame(x = dat$x, y = y, pred = pred)
ggplot(ddf) + geom_line(aes(x = x, y = y)) + geom_line(aes(x = x, y = pred), color = "red")



aic = function(tab){
  k = nrow(tab) + 1
  2 * k - 2 * ln(lf)
}

library(Cubist)

dat = data.frame(x = fc2$approx_models$x$.__enclos_env__$private$x)
y = fc2$approx_models$x$.__enclos_env__$private$ale_values
model_tree <- cubist(x = dat, y = y, control = cubistControl(rules = 3))
pred = predict(model_tree, dat)
var(pred - y)/var(y)

ddf = data.frame(x = dat$x, y = y, pred = pred)
ggplot(ddf) + geom_line(aes(x = x, y = y)) + geom_line(aes(x = x, y = pred), color = "red")

# TEst splines
n.segs = 10
x = rnorm(1000)
y = x^2
bbreaks =  seq(from = min(x), to = max(x), length.out = 1 + n.segs)
segs = cut(x, breaks = bbreaks, include.lowest = TRUE)
x.intercept = model.matrix(y ~ segs - 1)
x.intercept = t(apply(x.intercept, 1, function(x) cummax(x)))
x.slope = x.intercept * x
colnames(x.slope) = sprintf("slope:%s", colnames(x.slope))
X = cbind( x.intercept, x.slope)
head(X)


library(glmnet)
glmmod = glmnet(X,y)
plot(glmmod)

lambda = 0.02

pred = predict(glmmod,newx=X,s=c(lambda))[,1]
var(pred - y)/var(y)

coef(glmmod,s=lambda) # extract coefficients at a single value of lambda
plot(x, pred)









Modeled with Random Forest \citep{Breiman2001}

We compare random forest with linear model in terms of performance

<<adult-data>>=
  SAMPLE = TRUE

adult = read.csv("../data/adult.data")
if(SAMPLE) {
  adult = adult[sample(1:nrow(adult), size = 1000),]
  print("ADULT DATA WAS SAMPLED")
}

colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'educatoin',
  'educatoin_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex',
  'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
tsk = makeClassifTask(data = adult, target = "income")
@

  <<adult-tune, dependson="adult-data", results = "asis">>=
  # Benchmark both xgboost and linear model
  ctrl = makeTuneControlRandom(maxit = 10)
ps_ranger = makeParamSet(
  makeIntegerParam("num.trees", lower = 100, upper = 500),
  makeIntegerParam("mtry", lower = 1, upper = sum(tsk$task.desc$n.feat))
)
lrn_ranger = makeTuneWrapper(makeLearner("classif.ranger", predict.type = "prob"), resampling = cv3,
  measures = list(acc), par.set = ps_ranger, control = ctrl)
ps_ctree = makeParamSet(
  makeIntegerParam("maxdepth", lower = 1, upper = 10)
)
lrn_tree = makeTuneWrapper(makeLearner("classif.ctree", predict.type = "prob"), resampling = cv3,
  measures = list(mmce), par.set = ps_ctree, control = ctrl)
lrn_fl = makeLearner("classif.featureless")

rdesc = cv5
lrns = list(lrn_ranger, lrn_tree, lrn_fl)
bmr = benchmark(lrns, tsk, rdesc, measures = list(acc), show.info = FALSE)
bmr_tab = getBMRAggrPerformances(bmr, as.df = TRUE)

xtable::xtable(bmr_tab)
@


  Now we compare it in terms of interpretability

<<adult-ranger, dependson = "adult-tune">>=
  mod = train(lrn_ranger, tsk)
pred_rf = Predictor$new(mod, getTaskData(tsk),class = 1)
fc_xg = FunComplexity$new(pred_rf, epsilon = 0.05)
plot(fc_xg, nrow = 2)
@

  The ranger model is rather complex, as visible in the plots in \ref{fig:wine-xgboost}.
Average weighted complexity is \Sexpr{fc_xg$c_wmean} and the $R^2$ of the first order ALE approximation is only \Sexpr{fc_xg$r2}, which means that a lot of interactions are modeled.
Of the available \Sexpr{sum(tsk$task.desc$n.feat)} features, \Sexpr{fc_xg$n_features} were used.


<<adult-tree, dependson = "adult-tune">>=
  mod_tree = train(lrn_tree, tsk)
pred_tree = Predictor$new(mod_tree, getTaskData(tsk), class  = 1 )
fc_tree = FunComplexity$new(pred_tree, epsilon = 0.05)
plot(fc_tree, nrow = 2)
fc_tree$c_wmean
fc_tree$n_features
fc_tree$r2
@

  The xgboost model is rather complex, as visible in the plots in \ref{fig:wine-xgboost}.
Average weighted complexity is \Sexpr{fc_tree$c_wmean} and the $R^2$ of the first order ALE approximation is only \Sexpr{fc_tree$r2}, which means that a lot of interactions are modeled.

Of the available \Sexpr{sum(tsk$task.desc$n.feat)} features, \Sexpr{fc_tree$n_features} were used.




<<sbrl>>=
  library("sbrl")
library("arules")

adult2 = as.data.frame(lapply(adult, function(x) {
  if(is.factor(x) || length(unique(x)) < 5) {
    as.factor(x)
  } else {
    discretize(x, method = "interval", 3)
    #discretize(x, breaks = max(length(unique(x))-1, 5))
  }
}))

get.sbrl.rules = function(x) {
  res = lapply(1:nrow(x$rs), function(i) {
    if (i == 1)
      sprintf("If      %s (rule[%d]) then positive probability = %.8f\n",
        x$rulenames[x$rs$V1[i]], x$rs$V1[i], x$rs$V2[i])
    else if (i == nrow(x$rs))
      sprintf("else  (default rule)  then positive probability = %.8f\n",
        x$rs$V2[nrow(x$rs)])
    else sprintf("else if %s (rule[%d]) then positive probability = %.8f\n",
      x$rulenames[x$rs$V1[i]], x$rs$V1[i], x$rs$V2[i])
  })
  data.frame(rules = unlist(res))
}


adult2$label = adult2$income
adult2 = droplevels.data.frame(adult2)
rules = sbrl(adult2[setdiff(colnames(adult2), c("income"))], pos_sign = " >50K", neg_sign = " <=50K", rule_maxlen = 2)
pred = Predictor$new(rules, data = adult2, class = 1)
fc = FunComplexity$new(pred)
fc$c_wmean
fc$r2
rules = sbrl(adult2[setdiff(colnames(adult2), c("income"))], pos_sign = " >50K", neg_sign = " <=50K", rule_maxlen = 3)
pred = Predictor$new(rules, data = adult2, class = 1)
fc = FunComplexity$new(pred)
fc2$c_wmean
fc2$r2


rules = sbrl(adult2[setdiff(colnames(adult2), c("income"))], pos_sign = " >50K", neg_sign = " <=50K", lambda = )
pred = Predictor$new(rules, data = adult2, class = 1)
fc = FunComplexity$new(pred)
fc2$c_wmean
fc2$r2
@




# GAM and degrees of freedom
  set.seed(123)
n = 500
dat = mlbench::mlbench.friedman3(n, sd = 0.3)
datx = data.frame(cbind(dat$x, "y" = dat$y))
task = makeRegrTask(data  = datx, target = "y")
library("mgcv")


gammas = c(0.1, 0.5, 1, 2, 5, 10, 20, 100)
edf = vector()
cs = vector()
epsilons = vector()
for (eps in c(0.1, 0.05, 0.01, 0.001)) {
  for(gamma in gammas) {
    gm = gam(y ~ s(V1) + s(V2) + s(V3) + s(V4), data = datx, gamma = gamma)
    edf  = c(edf, sum(gm$edf)/4)
    pred = Predictor$new(gm, datx)
    fc = FunComplexity$new(pred, epsilon = eps)
    cs = c(cs, fc$c_wmean)
    epsilons = c(epsilons, eps)
  }
}
ggplot(data.frame(x = edf, y = cs, e = factor(epsilons))) +
  geom_line(aes(x=x,y=y, group = e, color = e))  +
  scale_x_continuous(limits = c(0,NA)) +
  scale_y_continuous(limits = c(0,NA))



\subsubsection{LIME unreliable when C high}

In this example we show that the LIME explanations become less reliable with high interactions and high complexity.

We use the Boston Housing dataset and various models to show this.

<<tab-fidelity>>=
  library("lime")
set.seed(123)
task = bh.task
llearner = list(makeLearner("regr.lm"),
  makeLearner("regr.gamboost"),
  makeLearner("regr.ranger"),
  makeLearner("regr.kknn"),
  makeLearner("regr.ksvm"),
  makeLearner("regr.earth"),
  makeLearner("regr.rpart"))
res = rbindlist(lapply(llearner, function(lrn){
  mod = train(lrn, task)
  pred = Predictor$new(mod, getTaskData(bh.task), class = 1)
  fc = FunComplexity$new(pred, grid.size = 100)
  exp = lime(getTaskData(bh.task), mod, bin_continuous = FALSE)
  x = explain(getTaskData(bh.task)[sample(1:nrow(getTaskData(bh.task)), size = 50),], exp, n_features = 2)
  data.frame(c = fc$c_wmean, IA = 1  - fc$r2, model = lrn$id, fidelity = median(x$model_r2))
}))
xtable::xtable(res[order(res$fidelity),])
ggplot(res) + geom_point(aes(x = c, y = fidelity, size = IA))
@

  The table \ref{tab-fidelity} shows that with increasing complexity and interactions, the mean fidelity of the explanation falls.
