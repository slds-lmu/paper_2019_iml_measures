# TODOs

- Improve computational speed by using density function instead of data for computing R2 for complexity approximation.
- Brainstorm: How to combine interpretability measure into one measure?
- Find out why  generateDesign could only produce 138 points instead of 1000?
- Cite a few examples where interpretability is measured with human experiments
- Formally define: Sparsity, Additivity, Performance, Simplicity (only possible if you have mutliple models)
- Call $R^2$ approximation of first order model "Fidelity"?
- Give names for all three measures. CAndidates: Average Main Effect Complexity (AMEC), Main Effect Interaction Residuals (MEIR) and Number of Features Used for Prediction (NFUP).
- Write tests for spark function
- Cite ECML 2019 paper about importance and ICE curves
- Decrease figure height to save space
- In multi-obj: Show tradeoff simplicity/performance, interaction/performance, n.features/performance
- For multi-obj: Show for 2-5 good models table with: rows feature/C/IA/NF/MAE, cols models, cells sparkLine and measures.
- Kick out decision tree examle? or maybe greatly shorten and move to curve-complexity

# Ideas

- Do analysis how tuning parameters map to interpretability
- Add table for how post-hoc methods scale with number of features. rows: methods, cols: scalings, like O() of comp, size of output scaling with p, fidelity.methods: featureimp, pdp, ale, lime, shapley, shap, 2way interaction, ice
- Show negative correlation between LIME R squared and both of the interpretability measures.
- Soft monotonicity constraints can be incorporated into the model as well with my complexity measure framework
The less correlated, the better the interpretation methods work.
But don't measure with pearson correlation but maybe use notion of "predictability"/dependence
- Benchmarking methods that claim to be interpretable
- Show for a few ML methods how parameters map to the measures.
