K = 4 # number of models
R = 1 # number of regions
T = 100 # number of timesteps
S = 80 # number of predictive samples


predict_sample_mat <- array(rnorm(K * R * T * S), c(T, R, S,K))

y_mat <- array(rnorm(S), c(R, T))

model <- rstan::stan_model("stan/crps_test.stan")

standata <- list(K = 4,
                 R = 1,
                 T = 100,
                 S = 80,
                 predict_sample_mat = predict_sample_mat, 
                 y = y_mat)

optimizing(m, standata)