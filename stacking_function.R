library("rstan")
stacking_opt_model <- stan_model("stan/crps_test.stan")

stacking_weight = function(predict_sample, y,   K,  R, T, S,
													 lambda=NULL, gamma=NULL, dirichlet_alpha=1){
	if ( dim(predict_sample)!=c(T, R, S,K) | dim(y)!=c(R, T) ) 
		stop("Input dimensions do not match")
	if ( K < 2 ) 
		stop("At least two models are required model averaging")
	if( is.null(lambda) )
		for (t in 1:T) 
			lambda[t] = 2 - (1 - t  /T)^2
	if( is.null(gamma) )
		gamma=rep(1/R, R)
	standata <- list(K = K,
									 R = R,
									 T = T,
									 S = S,
									 predict_sample_mat = predict_sample, 
									 y = y,
									 lambda=lambda,
									 gamma=gamma,
									 dirichlet_alpha=dirichlet_alpha)
	opt <- rstan::optimizing(stacking_opt_model, data = standata)
	return(opt)
}