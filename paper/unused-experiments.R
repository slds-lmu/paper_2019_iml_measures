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



\subsection{Interaction in logistic regression}

In this example we demonstrate how deceptive model formulization can be and that we need a measure to really compare if one or the other model relies more heavily on interactions.
Interaction means that the output of the model can't be fully explained by the main effects of the model.
The logistic regression model might be a linear model on the level of the log odds, but not on the probability level.

<<interaction-demo-data>>=
library(rpart)
library(partykit)
library(ranger)
set.seed(42)
n = 500
dat = data.frame(x1 = rnorm(n), x2 = rnorm(n), x3  = rnorm(n))
dat$y = ifelse(exp(dat$x1 +  dat$x2 + dat$x3^2)> 0.1, 1, 0)
# to remove linear separability
change_prob = 0.01
change_index = which(rbinom(size = 1, n = n, prob = change_prob) == 1)
dat$y[change_index] = 1 - dat$y[change_index]
dat$y = factor(dat$y)

grid.size = 100
epsilon = 0.05
@

In the following example we simulate 3 features each is normal distributed with mean zero and standard deviation of 1.
We sample \Sexpr{n} data points from this distribution and simulate the target y as follows:

$$\eta = exp(x_1 + x_2 + x_3)$$
$$y = \begin{cases}\eta>0.5&1\\\eta \leq 0.5 0\\\end{cases}$$
We flip each of the computed y's with a probability of \Sexpr{change_prob * 100} \% to avoid linear separability.

We train a logistic regression model.

<<interaction-demo>>=
  rp = glm(y ~ ., data = dat, family = "binomial")
pred = Predictor$new(rp, data = dat, predict.fun = function(model, newdata) predict(model, newdata, type = "response"))
fc = FunComplexity$new(pred)
@

  The main effect model explains \Sexpr{100 * fc2$r2}\% of output for the logistic regression model.
Measures have advantage to operate on the level of outcome.
Usual interpretation of parameters in logistic regression, which are linear within the non-linear transformation function (logit), hides that logistic regression models interactions.
The interactions come from the logistic function and saturation.
When an instance has already 0.995 probability, changing a feature by one unit might increase probability to 0.996, but if probability would be lower, like 0.5, an increase might change it to 0.8.

\subsection{Complexity lm, gam, interactions}

In this section we show that even for models from statistical modeling that come with a lot of tools (tests for nested models, degrees of freedom, AIC) our proposed measures are useful for comparing models.

<<lm-gam-prepare>>=
  library(mgcv)
library(Metrics)
set.seed(12)
n.train = 1000
n.test = 5000
create_dat = function(n) {
  dat = data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  dat$y = dat$x1 * dat$x2 +  sin(dat$x3)  + rnorm(n, sd = 0.3)
  dat
}
dat = create_dat(n.train)
newdat = create_dat(n.test)
@

  We simulate a very simple dataset:

  $$y = x_1 \cdot x_2 + x_3^2 + \epsilon$$

  where $x_1, x_2, x_3, \epsilon \sim N(0,1)$

  We draw \Sexpr{n.train} points from this distribution.

We fit 4 models:

  \begin{enumerate}
\item A vanilla linear regression model
\item A linear regression model with an additional interaction between $x_1$ and $x_2$ and a quadratic term for $x_3^2$.
\item A Generalized Additive model (GAM) with a non-linear effect (using thin plate splines) for each feature, but no interaction terms.
\item A GAM with a non-linear handling of $x_1$, $x_2$ and their interaction.
\item GAM with non-linear interaction between all three features
\end{enumerate}

Apart from measuring the mean absolute error (based on \Sexpr{n.test} unseen samples), how would we traditionally compare the models?
  One commonly used complexity measure is the degrees of freedom and closely linked to this Akaikes Information criterion which gives a tradeoff between in-sample fit and degrees of freedom used by the model.

<<lm-gam, results="asis">>=
  mod1 = lm(y ~ x1 + x2 + x3, data = dat)
mod2 = lm(y ~ x1 * x2 + x3, data = dat)
mod3 = gam(y ~ s(x1) + s(x2) + s(x3), data = dat)
mod4 = gam(y ~ s(x1, x2) + s(x3), data = dat)
mod5 = gam(y ~ s(x1, x2, x3), data = dat)

analyze_lin_mod  = function(mod, fc, newdat) {
  data.frame(
    c = fc$c_wmean,
    IA = 1 - fc$r2,
    df = mod$rank,
    aic = AIC(mod),
    mae = mae(newdat$y, predict(mod, newdat)))
}

res = lapply(list(mod1, mod2, mod3, mod4, mod5), function(mod) {
  pred = Predictor$new(mod, newdat)
  fc = FunComplexity$new(pred)
  analyze_lin_mod(mod, fc, newdat)
})

xtable::xtable(rbindlist(res))
@

  While the AIC is a good measure here for assessing the goodness of fit and complexity tradeoff.
But IA and AMEC give us a more detailed view.
One thing is that we are actually not adding much IA between model 5 and 6  and also the MAE stays roughly the same.
Curiously while the degrees of model 3 are very low, the IA is extremely high.

