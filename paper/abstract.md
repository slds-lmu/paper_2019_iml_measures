# Measuring and optimizing machine learning interpretability


One of the biggest disadvantages of machine learnings is the lack of interpretability.
Scientists proposed intrinsically interpretable models such as decision trees and post-hoc interpretation methods such as partial dependence plots as solutions.
Interpretable models limit their users to a certain model class and often have a lower accuracy.
Post-hoc methods are applied after the model has been optimized for performance and can help to better understand the model, but the relationships modelled can be complex and have interactions.

We propose model-agnostic measures of interpretability for machine learning models.
These measures can be used to compare interpretability for different hyperparameter configurations and across different model classes.

We show an application of these measures in multi-criteria optimization.
We optimize model hyperparameters across different types of models for predictive performance and interpretability together, yielding a Pareto front of models with different tradeoffs between interpretability and accuracy.
