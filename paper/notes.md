# TODOs

- Improve computational speed by using density function instead of data for computing R2 for complexity approximation.
- Find out why  generateDesign could only produce 138 points instead of 1000?
- Write tests for spark function
- Cite ECML 2019 paper about importance and ICE curves
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
