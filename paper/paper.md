# Introduction
Motivation: We want to be able to compare interpretability across models.
Problem: No measure available.
Novelty: New model-agnostic measures for interpretability. End-to-end, multicrit optimization for accuracy and interpretability (interpretability not measured with intermediary number from model.)
We define interpretability as "How well do post-hoc methods work" and how sparse are they.
For us the model is interpretable, when I have to look at as little as post-hoc analysis as possible and when those post-hoc analyses are as truthful to the model as possible.

Argumemntation: We can decompose the function.
Individual components ccan be approximated by ALE. 
We measure the complexity and all the interactions we did not approximate.
We show that when interaction is small, we have an additive model.
And when other measure is minimized, we get linear effects or sparsity.
All the post-hoc methods tend to work better when the two criteria are optimized.


Some authors suggest that we need to have some interpretability across different models. 
Bibal, Adrien, and Benoît Frénay. "Interpretability of machine learning models and representations: an introduction." Proceedings on ESANN. 2016.
We address this by using model-agnostic methods that can be applied to the model. 
They also call for multi-objective optimization "In contrast, heuristics
can be integrated in learning through multi-objective optimisation techniques."

We also are based on theory (functional ANOVA decomposition)

# Review

Multicrit often looks at complexity: Examples
But nowhere are explicit, model-agnostic interpretability measures used
AIC, BIC is a tradeoff measure between accuracy and interpretability

Other approach, but only for decision tree and logistic regression: "Measuring Interpretability for Different Types
of Machine Learning Models" by Qing Zhou 1 , Fenglu Liao 1 , Chao Mou 1(&) , and Ping Wang 2

# Prerequisites

- Functional decomposition
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

Desiderata:
- Information should be contained in as few plots as possible.
- We prefer marginal relationships between features and prediction that can be explained with few parameters
- Plots and summary statistics should show as much information as possible of the black box



Paper says that desiderata are 
- Accuracy (of interpretation method), which matches that we look at R squared. Predictive accuracy is measured as usual. Descriptive accuracy is measured with novel measures
- Relevancy: Show only relevant information. With our measures we can decide which plots to show. Remove when effect is zero. Also we can measure variance of each of the 1st order components and only show the most relevant ones.
- Sparsity: Directly optimized with our measures
- Simulatability: Can human internally simulate and reason about 
- Modularity: Can model parts be interpreted independently? Interaction measure allows us to determine how independently we can analyze the individual features with their ALE plots
- 
They also say: "Moreover, it is unclear if any of the current inter-pretation forms can fully capture a model’s behaviour, or if a new format altogether is needed. How to close that gap, while producing outputs relevant to a particular audience/problem, is an open problem."
"Interpretable machine learning: definitions,methods, and applications"
W. James Murdoch a,1 , Chandan Singh b,1 , Karl Kumbier a,2 , Reza Abbasi-Asl b,c,2 , and Bin Yu a,b

The approach we take (functional decomposition) is flexible enough to adapt to different desideratea (e.g. favor different functional forms over others).

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
While the decomoosition of variance does not work with correlated features (1st order correlated with higher order) it should be still ok to just describe the SSE I get when using first order ALE model.

Making no assumptions, but simply describe how much residuals are left after modeling with first order ALE.

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

For categorical features we have to count how many degrees of freedom the model burns.
E.g. if month is a feature, it makes a difference if the model just distinguishes between two groups of months or fits a node for each month.


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

## Show with tree how an additive model is formed with 1st ALE approx

Show all 1st ALE and for one prediction the explanation?

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



## Proof that some interpretable models already optimize some measures

- LASSO directly optimizes both measures (interaction is zero), linear models, few weights.
- Tree does not, because of interactions
- knn not interpretable based on this, but  could be on a different level when instances interpretable


# Formatting
http://www.jmlr.org/format/format.html


