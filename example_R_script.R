library(rstan)

K = 4 # number of models
R = 1 # number of regions
T = 100 # number of timesteps
S = 80 # number of predictive samples

# create an example array
predict_sample_mat <- array(NA, c(T, R, S,K))
for (r in 1:R) {
  for (t in 1:T) {
    for (s in 1:S) {
      predict_sample_mat[t, r, , ] <- cbind(rnorm(S, 1.4, 1), 
                                        rnorm(S, 0.5, 1), 
                                        rnorm(S, 1, 4),  
                                        rnorm(S, 2, 2.4))
    }
  }
}


# create observed true values
y_mat <- array(rnorm(S * R), c(R, T))


model <- rstan::stan_model("stan/crps_test.stan")
standata <- list(K = 4,
                 R = 1,
                 T = 100,
                 S = 80,
                 predict_sample_mat = predict_sample_mat, 
                 y = y_mat)

rstan::sampling(model, data = standata)



rstan::optimizing(model, data = standata)
