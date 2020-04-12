#  Use Stacking to Average Models for Time Series and Panel Data Under Continuous Ranked Probability Score (CRPS)

**Target prediction**: y_{rt}. The outcome (new cases, new hospitalization) at day t and site r.  

**Input**: an array x_{rtks}, the (out-of-sample/one-day-ahead/several-day-ahead prediction) prediction of the k-th model for the outcome in day t and site r.  To incoprort both Bayeian and non bayesiain predicitons, the prediction is recorded by S draws for each y_{rt}.

**Extra tuning**: the time variying weight, the site varying weight.

**Output**: model weights for all models; final prediction as an mixture of all individual models.




