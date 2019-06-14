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
Post-hoc model-agnostic interpretation methods such as partial dependence plots can be employed to interpret complex machine learning models.
While these interpretation methods can be applied regardless of model complexity, they can produce misleading and verbose results if the model is too complex, especially w.r.t. feature interactions.
To quantify the complexity of arbitrary machine learning models, we propose model-agnostic complexity measures based on functional decomposition: number of features used, interaction strength and main effect complexity.
We show that post-hoc interpretation of models that minimize the three measures is more reliable and compact.
Furthermore, we demonstrate the application of these measures in a multi-objective optimization approach which simultaneously minimizes loss and complexity.



