# Thoughts:

- Create the interpretability vs accuracy figure
- Title: Optimizing for interpretability and predictive performance
- publish in JMLR

# Table, Figures, Experiments for Paper
- Simulate 3 different uni-dimensional example without interaction for showcasing the degrees-of-freedom measure. x: feature, y: prediction/ale, geom:line, 
  - Linear
  - stepwise 
  - zick-zack
- Examples for the measures
  - 

# Questions

- What does generateDesign could only produce 138 points instead of 1000?
- Is there a better way for conditional subsetting of Params?
- How to combine interpretability measure into one measure?
- How to get interaction measure more stable?
- Optimize over all learners? Or per learner and combine pareto sets in the end?
- How do you (in code) correctly do  hyperparameter tuning across different learners and parameters?

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
