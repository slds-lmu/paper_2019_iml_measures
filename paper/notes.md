

- minimum complexity: 1
- maximum: number of data points??
- AIC / BIC criterium TODO check
- TODO: Checkout how in boosting the degrees of freedom are computed


# Problems
- For tree based methods and other models with step curves, the ale fanova decomp never goes to perfectly zero, because it interpolates a little bit between the steps.



# Thoughts:

- Create the interpretability vs accuracy figure
- Title: Optimizing for interpretability and predictive performance
- publish in JMLR
- DO an analysis whether ale plots grid.size changes results of tuning
- Do analysis how tuning parameters map to interpretability
- Maybe separate n.features and n.segs in measure chapter and combine the in a third explanation.
- Add table for how post-hoc methods scale with number of features. rows: methods, cols: scalings, like O() of comp, size of output scaling with p, fidelity.
  - methods: featureimp, pdp, ale, lime, shapley, shap, 2way interaction, ice
- Show negative correlation between LIME R squared and both of the interpretability measures.
- Soft monotonicity constraints can be incorporated into the model as well with my complexity measure framework
- Another great measurement would be the correlation between the features that are used in the model. 
The less correlated, the better the interpretation methods work.
But don't measure with pearson correlation but maybe use notion of "predictability"

# Table, Figures, Experiments for Paper
- Simulate 3 different uni-dimensional example without interaction for showcasing the degrees-of-freedom measure. x: feature, y: prediction/ale, geom:line, 
  - Linear
  - stepwise 
  - zick-zack
- Examples for the measures
  - 

# Questions

- What does generateDesign could only produce 138 points instead of 1000?
- How to combine interpretability measure into one measure?
- How to get interaction measure more stable?
- Optimize over all learners? Or per learner and combine pareto sets in the end?
- How do you (in code) correctly do  hyperparameter tuning across different learners and parameters?
- First a paper with only the measures? Or already with multicrit optimization?


# Novelty
- Suggesting (novel) interpretability measures and testing them
- First paper to suggest to multicrit optimize for interpretability and accuracy at the same time, without going through intermediary measure, like number of nodes in a neural network
- The first thorough quantitative evidence of accuracy vs. interpretability tradeoff.
- End-to-end optimization. Also possible for pipelines with feature selection.


# Assumptions
- Fixed dataset (i.e. not going back and improving data as part of process, like Rudin suggests)
- 


# Citing Stuff
- Tradeoff between accuracy and interpretability has to be decided by the user. / Arguments for Multiobjective instead of weighted single objective. [A Critical Review of Multi-Objective Optimization in Data Mining: a position paper Alex A.] Freitas
- TIP: Typifying the Interpretability of Procedures
- Towards A rigorous science ... : we use the task level evaluation that can be automated
- TODO: Cite a few examples where interpretability is measured with human experiments.
- TIP: Typifying the Interpretability of Procedures => They suggest to measure improvement in helping with some kind of task. My critique: Always needs some baseline which might not exist. For example discovering new knowledge. Also, target model is fixed, can't decide accuracy interpretability tradeoff.  sparsity is not rewarded.
