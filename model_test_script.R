library(tidyverse)
source("R/utilities.R")
seed = 1


## ========================================================================== ##
##      Test optimization versus a simple grid search and plot results        ##
## ========================================================================== ##

# compare only two models to allow for simple grid search
K = 2 # number of models
R = 1 # number of regions
T = 30 # number of timesteps
S = 100 # number of predictive samples

# initialize empty list and number of simulation runs
plotlist <- list()
n_runs <- 9
for (run in 1:n_runs) {
  
  # simulate true observations and bring in correct format
  y_true <- rnorm(T, mean = 0)
  y_mat <- array(y_true, c(1, T))
  
  # simulate predictive samples
  pred1 <- data.frame(region = "A", 
                      model = "1", 
                      date = as.Date("2020-01-01") + 1:T,
                      samples = replicate(S, rnorm(T, mean = rnorm(1))))
  
  pred2 <- data.frame(region = "A", 
                      model = "2", 
                      date = as.Date("2020-01-01") + 1:T,
                      samples = replicate(S, rnorm(T, mean = rnorm(1), sd = 1)))
  
  # bring predictive samples in correct format for stan optimizer
  predictions <- rbind(pred1, pred2)
  predict_sample_mat <- create_array(predictions)
  
  # define all needed parameters and store in list for stan
  lambda <- array(1, T)
  gamma <- array(1, R)
  standata <- list(K = K,
                   R = R,
                   T = T,
                   S = S,
                   predict_sample_mat = predict_sample_mat, 
                   y = y_mat,
                   lambda = lambda,
                   gamma = gamma,
                   dirichlet_alpha = 1.01
  )
  
  # determine optimal weights, create an ensemble accordingly and score
  opt <- rstan::optimizing(model, data = standata,seed=20)
  
  
  prediction_list <- 
    list(pred1 %>% dplyr::select(-c(region, date, model)) %>% as.matrix(), 
         pred2 %>% dplyr::select(-c(region, date, model)) %>% as.matrix())

  ensemble_forecast <- make_mixture_model(prediction_list, 
                                          weights = c(opt$par[1], opt$par[2]))
    
  crps_optim <- crps_scoringRules(y_true, ensemble_forecast) %>% mean()
  
  # maually run several differently weigted ensembles and check their CRPS
  crps_test <- numeric(101)
  p <- seq(0, 1, 0.01)
  for (i in 1:101) {
    weights <- c(p, 1-p)
    
    test_ensemble <- make_mixture_model(prediction_list, 
                                        weights = weights)
    
    crps_test[i] <- (crps_scoringrules(y_true, test_ensemble) %>% mean())
  }
  
  # combine results of grid search and optimization in df for plotting
  gridtest <- data.frame(p = p, 
                         crps = crps_test, 
                         type = "tested")
  plot_df <- rbind(gridtest, 
                   data.frame(p = opt$par[1], 
                              crps = crps_optim, 
                              type = "optimized"))
  
  # plot
  plotlist[[run]] <- ggplot(data = plot_df, aes(x = p, y = crps, color = type)) + 
    geom_vline(aes(xintercept = p[crps == min(.data$crps)])) + 
    geom_vline(aes(xintercept = p[type == "optimized"]), color = "blue", alpha = 0.6) + 
    geom_point(size = 0.3) + 
    theme(text = element_text(family = 'Sans Serif'))
}

plot <- patchwork::wrap_plots(plotlist)

ggsave(filename = "results/plots/test_optim_vs_grid_weights.png", 
       height = 10, width = 10)
















## ========================================================================== ##
##      Work in progress, ignore for now.         ##
## ========================================================================== ##




pred <- rbind(predictions, 
              data.frame(region = "A", 
                         model = "estimated_ensemble", 
                         date = as.Date("2020-01-01") + 1:100,
                         ensemble_forecast), 
              data.frame(region = "A", 
                         model = "test_ensemble", 
                         date = as.Date("2020-01-01") + 1:100,
                         test_ensemble))

pl <- pred %>% 
  pivot_longer(cols = grep("sample", colnames(.)))


true_dat <- data.frame(value = y_true, 
                       date = as.Date("2020-01-01") + 1:100, 
                       model = "true")

ggplot(pl, aes(x = model, y = value, group = model, color = model), alpha = 0.5) + 
  geom_boxplot(alpha = 0.8) + 
  geom_boxplot(data = true_dat) + 
  geom_hline(yintercept = 1) + 
  theme(text = element_text(family = 'Sans Serif'))

mean(test_ensemble)
var(as.numeric(test_ensemble))

pred1 %>% 
  dplyr::select(-c(region, date, model)) %>%
  as.matrix() %>% 
  as.numeric() %>%
  mean()

pred2 %>% 
  dplyr::select(-c(region, date, model)) %>%
  as.matrix() %>% 
  as.numeric() %>%
  var()

mean(y_true)


x = 0.6
p <- 1-x
0.5 * p + 2 * p

