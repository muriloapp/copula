
library(rugarch)
library(quantmod)
library(readxl)
library(dplyr)
library(zoo)
library(here)

set.seed(1234)

SET1 <- c(1,2,3,4,7,9,10,13,14,17,19,20,23,24,27,29,30,33,34,37,39,40)


compute_CVM <- function(data, familyset_l){

  CVM_ll <- list()
  
  directory <- "dataset/dependence/CVM/"
  
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  # Run loop over families
  for (i in seq_along(familyset_l)) {
    # Select the family set
    family_set <- familyset_l[[i]]
    
    # Define the file path for saving/loading the model
    file_path <- file.path(directory, paste0("CVM_train_", d, "_family_", family_set, ".rds"))
    
    # Check if the model already exists
    if (file.exists(file_path)) {
      # Load the precomputed model
      CVM <- readRDS(file_path)
      print(paste("Loaded precomputed model for family:", family_set))
    } else {
        if (identical(family_set, "SET1")) {
          f <- SET1
        } else {
          f <- family_set
        }
            CVM <- RVineStructureSelect(data = data, 
                                        type = "CVine", 
                                        familyset = f, 
                                        selectioncrit = "AIC", 
                                        indeptest = TRUE, 
                                        level = 0.1,
                                        trunclevel = 4
      )
      
      # Save the computed model
      saveRDS(CVM, file_path)
      print(paste("Computed and saved model for family:", family_set))
    }
    
    CVM_ll[[i]] <- CVM
  }
  return(CVM_ll)
}


compute_CVM_metrics <- function(test_data, familyset_l, d) {
  
  logLik_list <- list()
  aic_list <- list()
  n_param_list <- list()
  vine_structure_list <- list()
  logLik_oos_list <- list() 
  
  for (i in seq_along(familyset_l)) {
    family_set <- familyset_l[[i]]
    directory = "dataset/dependence/CVM"
    file_path <- file.path(directory, paste0("CVM_train_", d, "_family_", family_set, ".rds"))
    
    # Check if the model already exists
    if (file.exists(file_path)) {
      # Load the precomputed model
      CVM <- readRDS(file_path)
      print(paste("Loaded precomputed model for family:", family_set))
      # Compute different metrics and store them
      logLik_list[[as.character(family_set)]] <- CVM$logLik
      aic_list[[as.character(family_set)]] <- CVM$AIC
      n_param <- sum(CVM$par!=0) + sum(CVM$par2!=0)
      n_param_list[[as.character(family_set)]] <- n_param
      vine_structure_list[[as.character(family_set)]] <- CVM$Matrix  # Stores the vine structure matrix
      logLik_oos_list[[as.character(family_set)]] <- RVineLogLik(test_data, CVM, calculate.V = FALSE)$loglik
      
    } else {
      print(paste("CVM model not found for family:", family_set, ". Skipping..."))
    }
  }
  
  # Return a list containing all computed metrics
  return(list(logLik = logLik_list, 
              AIC = aic_list, 
              VineStructure = vine_structure_list,
              logLik_oos = logLik_oos_list,
              n_param = n_param_list))
}


residuals_to_rets <- function(data, order, d, d_forecast){
  
  quantile_fun <- function(data, fit_list, col){
    distribution <- fit_list[[col]]@model$modeldesc$distribution
    if (distribution == "norm") {
      x <- qnorm(data)
    } else if (distribution == "std") {
      dof <- as.numeric(fit_list[[col]]@fit$coef["shape"])
      x <- qt(data, dof)
    } 
    return(x)
  }
  
  directory = "dataset/margins"
  file_path <- file.path(directory, "fit_list")
  fit_list <- readRDS(file_path)
  
  aux_mat <- matrix(nrow = dim(data)[1], ncol = dim(data)[2])
  
  init <-  d*390 + 1
  end <- init + d_forecast*390 - 1
  
  for (i in 1:length(order)) {
    cond_mean <- (fitted(fit_list[[order[i]]]@fit))[init:end] 
    cond_var <- fit_list[[order[i]]]@fit$sigma[init:end]    
    aux_mat[,order[i]] <- sweep(cond_var * matrix(quantile_fun(data[,order[i]], fit_list, order[i])), 1, cond_mean, "+")
  }
  return(aux_mat)
}


simulate_ew_port_rets <- function(n_samples = 1000, test_data, familyset_l, d, d_forecast) {
  
  sim_directory <- "dataset/simulations"
  if (!dir.exists(sim_directory)) {
    dir.create(sim_directory, recursive = TRUE)
  }
  
  simulation_results <- list()
  
  # Loop over family sets
  for (i in seq_along(familyset_l)) {
    family_set <- familyset_l[[i]]
    
    # Define file paths
    CVM_file_path <- file.path("dataset/dependence/CVM", paste0("CVM_train_", d, "_family_", family_set, ".rds"))
    sim_file_path <- file.path(sim_directory, paste0("simulations_train_", d, "_family_", family_set, ".rds"))
    
    # Check if the simulation file already exists
    if (file.exists(sim_file_path)) {
      print(paste("Already simulated for family:", family_set, "at", sim_file_path))
      # Load existing simulation
      sim_port_agg <- readRDS(sim_file_path)
    } else {
      # Check if the CVM model exists
      if (!file.exists(CVM_file_path)) {
        warning("CVM model file not found for family ", family_set, ". Skipping simulation...")
        next
      }
      
      CVM <- readRDS(CVM_file_path)
    
      print(paste("Computing simulation for family:", family_set, "with", n_samples, "samples..."))
      
      # Initialize matrix for storing simulated portfolio returns
      sim_port_agg <- matrix(ncol = n_samples, nrow = dim(test_data)[1])
      
      # Run simulations
      for (s in 1:n_samples) {
        # Simulate data from CVM model
        sim_mat_t <- RVineSim(dim(test_data)[1], CVM, U = NULL)
        
        # Convert simulated residuals to returns
        matrix <- CVM$Matrix
        order <- rev(matrix[,1])
        aux_mat <- residuals_to_rets(data = sim_mat_t, order = order, d = d, d_forecast = d_forecast)
        
        # Compute the equal-weighted portfolio mean
        sim_port <- matrix(apply(aux_mat, 1, mean))
        colnames(sim_port) <- s
        
        # Store in aggregated matrix
        sim_port_agg[, s] <- sim_port
        
        # Print progress every 100 samples
        if (s %% 100 == 0) {
          print(paste("Finished sample:", s, "for family:", family_set))
        }
      }
      
      # Save simulation results
      saveRDS(sim_port_agg, sim_file_path)
      print(paste("Saved simulation for family:", family_set, "at", sim_file_path))
    }
    
    # Store in the results list
    simulation_results[[as.character(family_set)]] <- sim_port_agg
  }
  
  return(simulation_results)
}


evaluate_simulations <- function(xts_data, familyset_l, d, d_forecast) {
  
  tickers <- list("AIG", "AXP", "BAC", "C", "COF", "GS", "JPM", "MS", "WFC")
  
  # Extract market data and format time index
  xts_data <- matrix(xts_data[, 2:10], ncol = 9)
  colnames(xts_data) <- tickers
  
  # Compute equal-weighted portfolio returns
  port_rets <- apply(xts_data, 1, mean)
  init <-  d*390 + 1
  end <- init + d_forecast*390 - 1
  port_rets <- port_rets[init:end]  # Adjust the time window as required
  
  # Initialize list to store hit rates for each family specification
  hit_rates_list <- list()
  
  # Define the directory where the simulations are stored
  sim_directory <- "dataset/simulations"
  
  # Loop over family sets
  for (i in seq_along(familyset_l)) {
    family_set <- familyset_l[[i]]
    
    # Define the file path to load the simulated portfolio
    sim_file_path <- file.path(sim_directory, paste0("simulations_train_", d, "_family_", family_set, ".rds"))
    
    if (!file.exists(sim_file_path)) {
      warning("Simulation file not found for family ", family_set, ". Skipping...")
      next
    }
    
    # Load simulated portfolio returns
    sim_port_agg <- readRDS(sim_file_path)
    
    # Compute quantiles (10%, 5%, 1%)
    quantiles <- apply(sim_port_agg, 1, function(x) quantile(x, probs = c(0.1, 0.05, 0.01)))
    
    # Calculate hit rates for each quantile
    hit_rates <- list(
      var1 = mean(port_rets < quantiles[1, 1:length(port_rets)]),
      var05 = mean(port_rets < quantiles[2, 1:length(port_rets)]),
      var01 = mean(port_rets < quantiles[3, 1:length(port_rets)])
    )
    
    # Store results
    hit_rates_list[[as.character(family_set)]] <- hit_rates
    
    print(paste("Computed hit rates for family:", family_set))
  }
  
  return(hit_rates_list)
}






