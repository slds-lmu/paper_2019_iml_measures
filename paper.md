# Introduction
Motivation: User should decide on interpretability  / accuracy tradeoff
Novelty: New model-agnostic measures for interpretability. End-to-end, multicrit optimization for accuracy and interpretability (interpretability not measured with intermediary number from model.)

# Review

Multicrit often looks at complexity: Examples
But nowhere are explicit, model-agnostic interpretability measures used
AIC, BIC is a tradeoff measure between accuracy and interpretability

# Prerequisites

- ALE plots
- Multicrit, Pareto Front

# Motivation

## Limitations of interpretable models
Short chapter. Limits to certain model class. sub-optimal performance.

## Limitations of post-hoc methods

**Effect of high number of features**
Example with few and many features and showing Shapley explanations and many partial dependence plots.

**Effect of interactions**
Show ICE / PDP example.
Show problem with local and global linear surrogate models.

**Effect of complex non-linear feature effects**
Show crazy pdp.
Show problems with LIME.

# Interpretability measures


Motivation: 
- ALE and functional decomposition of \hat{f}(x)
- Definition via existing model-agnostic criteria (e.g. ALE)
- Sources of lack of interpretability: many features, complex relationship with prediction and interactions -> interpretability measure should capture those
- why all with ALE? -> Works also with correlated inputs
- Why not functional Anova a la hooker? computationally ineffective and his decomposition properties not needed. Also good software with ALE.

$$\hat{f}(x) = f_1(x) + f_2(x_2) + \ldots + f_p(x_p) + f_{12} ...$$

Examples on which measures are demonstrated:
- XOR problem with perfect predictor
- Simulated dataset
  - 3 features, 2 numerical (one linear, one more complex), 1 cateogrical
  - one version with and one without interactions
  - categorical features with more 5 or so levels, only 3 df needed

One of the measures describes the complexity of first-order model with the degrees of freedom. 
The other measure describes how big the higher-order portion of the model is, and with that kind of equating anything higher order as non-interpretable.

## First order ALE decomposition and SSE

Use fanova decomposition, but with ALE.

Discussion
- When true marginal function is step function, then it adds to the SSE unecessarily
- Unclear how many intervals
- Weighted by data sensity (can be unintuitive when looking at plot) or all plot point same weight (probably very wrong). Solution: Make clear in ALE plot where most data is with rug or alpha.
- Short-comings: not tested with humans, but could be adapted and used with the same mult-crit framework. 
## Degrees of freedom univariate

R square controlled degrees of freedom approximation.
Version for cubic splines approx
Version for tree split approx

2 or 3 PDP/ALE examples.
1. Almost linear curve
2. Curve with jump
3. More complex function: Show different R squared as group,color

0 if not used, 1 if linear or binary split, ...

Discussion 
- Implicit comparison between features: reducing a feature from 3 to 2 df is same as removing a linear feature. 
- can be adjusted to needs. like maybe only interested in log(df) per feature?
- When ALE is used, correlation of features is ok.
- 


## Others

- R squared for linear PDP approx
- Feature Importance Gini?
- Number of features
- Average/median number of features needed to explain individual predictions to user-defined degree of certainty. Or with Shapley value: Order shapley value by absolute value. Cumsum of abs.value divided by total value > 0.95. how many features needed? -> lm would have very bad number here, trees probably really good.
-How strongly are features (that are used in the model) correlated / associated .
- Alternative variance: Difference between product of pdps and comp

- Sobol interaction strength. Based on first-order indices

# The Optimization problem and MBO
Mathematically writing down objectives

Short mbo summary and setup.

as alternative: pareto front per learner and later combine or do something else

# Experiments

## Alternative way to tune LASSO
one dataset.
instead of best fitting model, you get pareto front.
maybe example is not so relevant.

## Tune xgboost gbtree/gblinear
One dataset?
Shows how many different solution and tradeoffs you get within xgboost

## Tune across many learners (xgboost, svm, lm, rpart, ...)
Many datasets
Show Pareto Front for some datasets.

Aggregated results per learner (maybe do mbo per learner) and recreate accuracy / interpretability figures


## Benchmarking methods that claim to be interpretable

Linear model, LASSO, rpart, SLIM, SBRL, ...

Use a handful of datasets.
How to tune the models? Maybe different setups: With defaults, only tuned for performance, tuned for performance and interpretability (and report pareto sets for all methods).
Compare in table:
  rows: dataset x tuning method
  columns: methods
  cells: [performance, interaction.strength, degrees of freedom]


# Formatting
http://www.jmlr.org/format/format.html


