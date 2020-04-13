#  Use Stacking to Average Models for Time Series and Panel Data Under Continuous Ranked Probability Score (CRPS)

## Summary of the algorithm 

**Target prediction**: y_{rt}. The outcome (new cases, new hospitalization) at day t and site r.  

**Input**: an array x_{rtks}, the (out-of-sample/one-day-ahead/several-day-ahead prediction) prediction of the k-th model for the outcome in day t and site r.  To incorporate both Bayesian and non Bayesiain predictions, the prediction is recorded by S draws for each y_{rt}.

**Extra tuning**: the time-varying weight, the site-varying weight.

**Output**: model weights for all models; final prediction as a mixture of all individual models.

For computation details, see 'method.pdf'.

## Usage
Run `source ("stacking_function.R")` to load necessary functions.

The main function `stacking_weight` takes input:
* Dimension, must be integers 
  * `K`>=2: number of models;
  * `R`>=1: number of sites;
  * `T`>=1: number of days in the existing predictions;
  * `S`>=2: number of simulation draws per prediction.
* `predict_sample`: an array with dimension (T, R, S, K). Its (t, r, s, k) element refers to the s-th simulation draws of the leave-future-out prediction for the r-th site, t-th day in the k-th model.
* `y`: a matrix with dimension (R, T). The observed outcome on the r-th site and t-th day.
* `lambda` (optional): a vector of length T that specifies the time-weight. The default is lambda(t)= 2 - (1 - t  /T)^2.
* `gamma` (optional): a vector of length R that specifies the site-weight. The default is uniform.
* `dirichlet_alpha` (optional): a positive real value that specifies the Dirichlet prior we place on model weights. The default is 1.001. 

The function returns the stan optimization result including the model weights. 

## Examples
see `example_R_script.R`.
