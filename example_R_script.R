library(rstan)

seed = 1

K = 5 # number of models
R = 1 # number of regions
T = 100 # number of timesteps
S = 200 # number of predictive samples

# create an example array
predict_sample_mat <- array(NA, c(T, R, S,K))
for (r in 1:R) {
  for (t in 1:T) {
      predict_sample_mat[t, r, , ] <- cbind(rnorm(S, 2, 1), 
                                        rnorm(S, 0.5, 1),
                                        rnorm(S),
                                        rnorm(S, 1, 4),  
                                        rnorm(S, 2, 2.4))
  }
}


# create observed true values
y_mat <- array(rnorm(S * R), c(R, T))

model <- rstan::stan_model("stan/crps_test.stan")
standata <- list(K = K,
                 R = R,
                 T = T,
                 S = S,
                 predict_sample_mat = predict_sample_mat, 
                 y = y_mat)

opt=rstan::optimizing(model, data = standata,seed=20)

