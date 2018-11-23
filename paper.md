

# Introduction
Motivation: User should decide on interpretability  / accuracy tradeoff
Novelty: New model-agnostic measures for interpretability. End-to-end, multicrit optimization for accuracy and interpretability (interpretability not measured with intermediary number from model.)

# Review
Multicrit often looks at complexity: Examples
But nowhere are explicit, model-agnostic interpretability measures used


# Prerequisites
little bit about multicrit


# Interpretability measures
Discuss pros and cons, show examples. 
Motivation: PDP interpretability


## Sobol interaction strength
Definition
2d Heatmap examples. Interaction of zero. High interaction.
Example motivated by ICE curves.
Discussion: Instable, Needs uncorrelated features.

## Degrees of freedom univariate

R square controlled degrees of freedom approximation.
Version for cubic splines approx
Version for tree split approx

2 or 3 PDP/ALE examples.
1. Almost linear curve
2. Curve with jump
3. More complex function: Show different R squared as group,color

0 if not used, 1 if linear or binary split, ...

Discuss: Implicit comparison between features: reducing a feature from 3 to 2 df is same as removing a linear feature. 
Discuss: When ALE is used, correlation of features is ok.
Discuss: 


## Others

- R squared for linear PDP approx
- Feature Importance Gini?
- Number of features
- Average/median number of features needed to explain individual predictions to user-defined degree of certainty. Or with Shapley value: Order shapley value by absolute value. Cumsum of abs.value divided by total value > 0.95. how many features needed? -> lm would have very bad number here, trees probably really good.
-How strongly are features (that are used in the model) correlated / associated .
- Alternative variance: Difference between product of pdps and comp
- Use fanova decomposition. For first-order, it should be simple to construct it. Make sure to weight data points when marginalizing, based on density. Use grid for function. For each feature: get fine grid, get $\hat{f}$ for grid based on PDP estimate. make function by interpolation. make sure to weight by probability. not sure fit that will work. maybe do it the Hooker-Generalized fanova way. Built grid over all features.

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



# Formatting
http://www.jmlr.org/format/format.html


