library("rstan")
stacking_opt_model <- stan_model("../stan/crps_test.stan")

stacking_weight = function(predict_sample, y,   K,  R, T, S,
													 lambda=NULL, gamma=NULL, dirichlet_alpha=1.001){
	if ( dim(predict_sample)!=c(T, R, S,K) | dim(y)!=c(R, T) )  
		# TODO: fix the dimension of predict_sample and y on R-T.
		stop("Input dimensions do not match.")
	if ( K < 2 ) 
		stop("At least two models are required model averaging.")
	if( is.null(lambda) )
		lambda = 2 - (1 - c(1:T)  /T)^2
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


##compute pointwise CRPS when the prediction comes from a mixtures
CRPS_pointwise=function(predict_sample_pointwise, y,  w){
	S = nrow(predict_sample_pointwise)
	K = ncol(predict_sample_pointwise)
	if ( length(y)!=1 ) 
		stop("Input dimensions do not match.")
	if ( length(w)!=K | min(w)<0 | sum(w)!=1  )
		stop("The weight has to be a simplex.")
	mean_bias= apply(abs(predict_sample_pointwise-y), 2, mean)
	entropy= matrix(0,K,K)  
	for( k1 in 1:K)
		for(k2 in 1:k1)
			for(s1 in 1:S)
					entropy[k1, k2] = entropy[k1, k2] + 1/(S^2)* sum( abs( predict_sample_pointwise[s1, k1] - predict_sample_pointwise[s2, ]))
	for( k1 in 1:(K-1) )
		for(k2 in (k1+1):K)	
			entropy[k1, k2]=entropy[k2, k1]
	entropy_aggregrate=0
	for( k1 in 1:K)
		for(k2 in 1:K)
			entropy_aggregrate=entropy_aggregrate+entropy[k1, k2]*w[k1]*w[k2]
	return( mean_bias %*% w - 1/2* entropy_aggregrate )
}

##compute CRPS over panel data when the prediction comes from a mixtures
CRPS_mixture=function(predict_sample, y,  
											K, R, T, S,w, lambda=NULL, gamma=NULL){
	if ( dim(predict_sample)!=c(T, R, S,K) | dim(y)!=c(R, T) ) 
			stop("Input dimensions do not match.")
	if ( length(w)!=K | min(w)<0 | sum(w)!=1  )
		  stop("The weight has to be a simplex.")
	if( is.null(lambda) )
			lambda = 2 - (1 - c(1:T)  /T)^2
	if( is.null(gamma) )
			gamma=rep(1/R, R)
	lambda=lambda/sum(lambda)
	gamma=gamma/sum(gamma)
	CRPS_sum=0
	for(t in 1:T)
		for(r in 1:R)
			CRPS_sum=CRPS_sum+lambda[t]*gamma(r)*CRPS_pointwise(predict_sample[t,r,,], y[r, t], w)
  return(CRPS_sum)
}


