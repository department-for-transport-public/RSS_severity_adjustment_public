This is the public version of this code that reads in our open data. 

Why was regression chosen?

There are two potential outcomes for a casualty severity - either serious or slight. Due to this binary outcome a binomial logistic regression was chosen.

Also many explanatory variables can be used in these models to explain this binary outcome - which is useful as many factors could potentially influence the severity of injury that a casualty sustains.

It estimates potential severity adjustments to each reported casualty that was reported on a non-injury based reporting system. It allows the approach for the adjustment to be consistent over time. The transition probabilities to slight to serious or serious to slight are the output from this model. These transition probabilities are then used to uplift/downlift casualties given severities.

Assumptions made in this methodology:

Assumes the total number of casualties is not effected by the transition to CRASH
Assumes that the fatal severity assigned is correct - so only serious and slight are changing
Assumes that police forces not on IBRS will have a similar effect moving to IBRS that is seen at the median police force
As there was no collection where both systems were in use cannot know the correlation - assumes correlation coefficient between them is 1 (this assumes CRASH is correct)
Severities assigned under IBRS are kept the same (assume CRASH is correct)
Limitations

This model does highlight the explanatory variables that have the most importance in predicting the severity of the casualty and which values of variables are associated with higher or lower risk of being serious. However, there is no data on the extent of exposure or the risk of each factor.

The data used is observational, any relationships seen are associations not necessarily causation

For police forces with no IBRS, assumes that these forces will have a similar effect to the median police force

Theory

Logistic regression models the probability of a binary outcome (serious or slight) given the variables linked to the event. Result of this is a set of coefficients which predict the probability of the severity being ‘serious’ give a set of variables in the model.

One of the variables is whether its IBRS or NIBRS – by switching this value for each casualty (but keeping everything else fixed) the model provides two probabilities. Serious on IBRS and serious of NIBRS – marginal probabilities (unconditional margins).

What is required is the conditional probability that the result would have been different using the other system (the transition probabilities).

Best way – of both methods used in parallel. In the absence of both – probabilities are used. In the absence of this direct evidence – base on assumptions. There is some evidence to suggest that IBRS and NIBRS are maximally correlated, though with likely different marginal probabilities. In other words, while the alternative recording systems may assess a different proportion of casualties as ‘serious’, this will arise from as few casualties as possible having their severity status changed.

3a methodology (finding the median police force)

This work finds the median police force using the exponential values of the coefficients from the regression model
These coefficient values are outputted from the model and are the transitional probabilities for each of the police forces currently using IBRS – the likelihood of that casualty being classified differently
The transition probability is the effect since CRASH came in and the uplift generally seen since the introduction for each police force – though for some police forces seen a downlift when CRASH introduced.
These transition probabilities are then ranked and median value chosen – this is then used to use this transition value on the police forces that are yet to introduce crash but will uplift their results to be comparable to CRASH forces in the model.