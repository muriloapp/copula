library(rugarch)
library(quantmod)
library(readxl)
library(dplyr)
library(mvtnorm)
library(ghyp)
library(zoo)
library(here)
library('VineCopula')
library("rvinecopulib")

source(here('src','utils.R'))
source(here('src','core_functions.R'))


set.seed(1234)


xts_data <- load_rets_data() #Load rets
data <- load_pit_data()      #Load PIT

days = unique(format(index(xts_data), '%Y-%m-%d')) # Total number of days
d <-  10                            # Training
d_forecast <- 5                     # Test
familyset_l <- list(1, 2, 3, 4, 7, "SET1", NA) # Define the list of family sets


train_data <- select_train_data(d, days, data)               # Select training data
test_data <- select_test_data(d, d_forecast = 5, days, data) # Select testing data

# Compute C-vine
cvm_ll <- compute_CVM(data, familyset_l)

# Compute metrics based on fitted C-vine model
metrics_ll <- compute_CVM_metrics(test_data, familyset_l, d)

# Simulate EW portfolio returns based on fitted C-vine model
simlution_ll <- simulate_ew_port_rets(1500, test_data, familyset_l, d, d_forecast)

# Evaluate the number of VaR vsiolations
hit_rates_list <- evaluate_simulations(xts_data, familyset_l, d, d_forecast)






