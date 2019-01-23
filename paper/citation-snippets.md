
# User should decide on tradeoff

"Note also that it is very hard to define a priori which model
size would be considered “too large” to be analyzed by a user.
Hence, instead of specifying the maximum size of a classification
model as a parameter of a classification algorithm, we prefer a
more principled approach to cope with the accuracy-
comprehensibility trade-off, such as a multi-objective approach
based on Pareto dominance or lexicographic optimization"" - Comprehensible Classification Models – a position paper


# Use monotonicity

Monotonicity constraints can be considered as hard constraints
(which can never be violated during model induction) or soft
constrains (which can be violated if this leads to significantly
higher predictive accuracy), depending on the application domain
and the user’s interests. Monotonicity constraints can be specified
by an expert on the application domain or automatically extracted
from (high-quality) data, depending on the application. More
research is needed to determine to what extent an automated
approach to extract semantic monotonicity constraints from data
leads to improved classification models.


# Distinguish between interpretability measure and representation

In the literature, there is no clear-cut distinction between the interpretability
measure of models and representations. The two research questions "what is an
interpretable model?" and "what is an interpretable representation?" need to be
investigated independently. Furthermore, many papers rely on intuition in the
use of interpretability, which leads to a focus on "white-boxes" (decision trees,
decision rules, etc.) and a lack of consideration of "black-boxes" (SVM, neu-
ral networks, etc.). -- Interpretability of Machine Learning Models
and Representations: an Introduction


# We need model-agnostic measures

One drawback of formal complexity measures is that they are either only
applicable for a specific class of models, e.g. the depth of a decision tree, the
norm of a parameter vector or the number of prototypes, or provide only a very
coarse measure of complexity, like the number of attributes used. Hence, they
provide little help in choosing the right hypothesis space in the first place. People
sometimes argue that one hypothesis language is “obviously” more intuitive than
others, but this is of course highly subjective and hence useless. -- Rüping
