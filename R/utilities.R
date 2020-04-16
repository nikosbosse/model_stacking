## turn predictive samples into array. 
## needs future work to be quicker
create_array <- function(predictions) {
  
  # number of models
  models <- predictions$model %>%
    unique()
  K <- length(models)
  
  regions <- predictions$region %>%
    unique() 
  R <- length(regions)
  
  S <- predictions %>%
    dplyr::select(-c(region, model, date)) %>%
    ncol()
  
  dates <- predictions$date %>%
    unique()
  
  T <- length(dates)
  
  predict_sample_arr <- array(NA, c(T, R, S, K))
  for (r in 1:R) {
    for (t in 1:T) {
      predict_sample_arr[t, r, , ] <- predictions %>% 
        dplyr::filter(date == dates[t], region == regions[r]) %>%
        pivot_longer(cols = grep("sample", colnames(.))) %>%
        pivot_wider(names_from = model, values_from = value) %>%
        dplyr::select(-c(region, date, name)) %>%
        as.matrix()
    }
  }
  
  return(predict_sample_arr)
}



## (from scoringRules::crps_sample)
crps_scoringRules <- function (y, dat) 
{
  c_1n <- 1/length(dat)
  x <- sort(dat)
  a <- seq.int(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = length(dat))
  f <- function(s) 2 * c_1n * sum(((s < x) - a) * (x - s))
  sapply(y, f)
}



## make mixture model
# input: list of predictions of equal lengths
# mixture weights
make_mixture_model <- function(prediction_list, weights) {
  
  K <- length(weights) # number of models
  T <- nrow(prediction_list[[1]]) # number of time steps
  S <- ncol(prediction_list[[1]]) # number of samples
  
  mixture_ensemble <- matrix(NA, nrow = T, ncol = K * S)
  
  for (i in 1:T) {
    mixture_ensemble[t, ] <- sapply(1:(K*S), 
                                    FUN = function(x) {
                                      m <- sample(1:K, size = 1)
                                      draw <- prediction_list[[m]][t, ] %>%
                                        sample(size = 1) %>% 
                                        as.numeric()
                                      return(draw)
                                    }, simplify = T)
  }  
  return(mixture_ensemble)
}

