# Quantifying Model Complexity

This repository contains the code, files and data for the paper *Quantifying Model Complexity via Functional Decomposition for Better Post-Hoc Interpretability* 


## Reproduce

All experiments are implemented with the R language, the paper is written using R, Latex and the `knitr` R package.

To reproduce the results, first clone the repository:

```bash
git clone git@github.com:compstat-lmu/paper_2019_iml_measures.git
```

Assuming you have R installed, you can install the package dependencies with R via:

```r
install.packages("devtools")
devtools::install_dev_deps()
```

To reproduce the paper, go to the folder `paper/` and run follwing command in  R:

```r
knitr::knit2pdf('paper.Rnw')
```

## Abstract

To obtain interpretable machine learning models, either interpretable models are constructed from the outset - e.g. shallow decision trees, rule lists, or sparse generalized linear models - or post-hoc interpretation methods - e.g. partial dependence or ALE plots - are employed. Both approaches have disadvantages.
While the former can restrict the hypothesis space too conservatively, leading to potentially suboptimal solutions, the latter can produce too verbose or misleading results if the resulting model is too complex, especially w.r.t. feature interactions. We propose to make the compromise between predictive power and interpretability explicit by quantifying the complexity / interpretability of machine learning models. Based on functional decomposition, we propose measures of the number of features used, interaction strength and main effect complexity. We show that post-hoc interpretation of models that minimize the three measures becomes more reliable and compact.
Furthermore, we demonstrate the application of such measures in a multi-objective optimization approach which considers predictive power and interpretability at the same time.




