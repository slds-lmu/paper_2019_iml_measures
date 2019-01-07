
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


#
