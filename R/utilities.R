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