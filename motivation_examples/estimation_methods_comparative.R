library(VineCopula)

run_scenario <- function(mat, families, par, par2, var_names,
                         n_sim = 1000, seed = 1234) {
  
  # Create the R-vine object (which encodes the full simulation model)
  RVM <- RVineMatrix(
    Matrix = mat,
    family = families,
    par    = par,
    par2   = par2,
    names  = var_names
  )
  
  # Simulate data
  simdata <- RVineSim(n_sim, RVM)
  
  # Measure time for Maximum Likelihood Estimation (MLE)
  time_mle <- system.time({
    mle <- RVineMLE(simdata, RVM)
  })["elapsed"]  # only store elapsed time
  
  # Measure time for Sequential Estimation (Itau)
  time_itau <- system.time({
    itau <- RVineSeqEst(simdata, RVM, method = "itau")
  })["elapsed"]
  
  # Measure time for Sequential Estimation 
  time_seqmle <- system.time({
    seqmle <- RVineSeqEst(simdata, RVM, method = "mle")
  })["elapsed"]
  
  # Extract Log-Likelihood
  loglik_mle    <- mle$value
  loglik_itau   <- itau$logLik
  loglik_seqmle <- seqmle$logLik
  
  # Organize results into a list
  return(list(
    times  = c(MLE = time_mle, Itau = time_itau, SeqMLE = time_seqmle),
    loglik = c(MLE = loglik_mle, Itau = loglik_itau, SeqMLE = loglik_seqmle)
  ))
}




replicate_scenario <- function(mat, families, par, par2, var_names,
                               n_sim = 1000, n_reps = 100) {
  results_list <- vector("list", n_reps)
  
  for (i in seq_len(n_reps)) {
    results_list[[i]] <- run_scenario(
      mat, families, par, par2, var_names,
      n_sim = n_sim
    )
  }
  
  # Extract times, loglik, AIC, BIC from each replicate
  times_mat  <- t(sapply(results_list, function(res) res$times))
  loglik_mat <- t(sapply(results_list, function(res) res$loglik))

  mean_times  <- colMeans(times_mat)
  mean_loglik <- colMeans(loglik_mat)

  return(list(
    mean_times  = mean_times,
    mean_loglik = mean_loglik
  ))
}





scenario_1 <- list(
  name     = "Scenario 1 (3D)",
  mat      = matrix(c(1,1,2,
                      0,2,1,
                      0,0,3), nrow = 3, byrow = TRUE),
  families = matrix(c(0,1,1,
                      0,0,1,
                      0,0,0), nrow = 3, byrow = TRUE),
  par      = matrix(c(0,0.5,0.8,
                      0,0,-0.2,
                      0,0,0), nrow = 3, byrow = TRUE),
  par2     = matrix(0, 3, 3),
  var_names = c("X1","X2","X3")
)

scenario_2 <- list(
  name     = "Scenario 2 (6D)",
  mat      = matrix(c(1,1,2,3,4,5,
                      0,2,1,2,3,4,
                      0,0,3,1,2,3,
                      0,0,0,4,1,2,
                      0,0,0,0,5,1,
                      0,0,0,0,0,6),
                    nrow = 6, byrow = TRUE),
  families = matrix(c(0, 1, 3, 13, 1, 1,
                      0, 0, 4, 1, 33, 1,
                      0, 0, 0, 1, 1, 23,
                      0, 0, 0, 0, 1, 3,
                      0, 0, 0, 0, 0, 14,
                      0, 0, 0, 0, 0, 0),
                    nrow = 6, byrow = TRUE),
  par      = matrix(c(0, 0.6, 0.7,  0.8,  -0.7,  0.65,
                      0, 0,   1.5, 0.41, -0.6,  0.37,
                      0, 0,   0,    0.26, -0.26, -0.56,
                      0, 0,   0,    0,     0.13,  0.2,
                      0, 0,   0,    0,     0,    1.5,
                      0, 0,   0,    0,     0,    0),
                    nrow = 6, byrow = TRUE),
  par2     = matrix(0, 6, 6),
  var_names = c("X1","X2","X3","X4","X5","X6")
)

scenario_3 <- list(
  name     = "Scenario 3 (9D)",
  mat      = matrix(
                    c(1,1,2,3,4,5,6,7,8,
                      0,2,1,2,3,4,5,6,7,
                      0,0,3,1,2,3,4,5,6,
                      0,0,0,4,1,2,3,4,5,
                      0,0,0,0,5,1,2,3,4,
                      0,0,0,0,0,6,1,2,3,
                      0,0,0,0,0,0,7,1,2,
                      0,0,0,0,0,0,0,8,1,
                      0,0,0,0,0,0,0,0,9),
    nrow = 9, byrow = TRUE
  ),
  families = matrix(c(0, 1, 3, 13, 1,  1,  3, 1,   1,
                      0, 0, 4, 1,  33, 1,  1,  33, 1,
                      0, 0, 0, 1,  1,  23, 1,  1,  23,
                      0, 0, 0, 0,  1,  3,  1,  1,  3,
                      0, 0, 0, 0,  0,  14, 1,  1,  14,
                      0, 0, 0, 0,  0,  0,  3,  1,  1,
                      0, 0, 0, 0,  0,  0,  0,  1,  3,
                      0, 0, 0, 0,  0,  0,  0,  0,  14,
                      0, 0, 0, 0,  0,  0,  0,  0,  0),
                    nrow = 6, byrow = TRUE),
  par      = matrix(c(0, 0.6, 0.7,  0.8,  -0.7,   0.65, 0.8,  -0.7,   0.65,
                      0, 0,   1.5,  0.41, -0.6,   0.37, 0.41, -0.6,   0.37,
                      0, 0,   0,    0.26, -0.26, -0.56, 0.26, -0.26, -0.56,
                      0, 0,   0,    0,     0.13,  0.2,  0.8,   0.13,  0.2,
                      0, 0,   0,    0,     0,     1.5, -0.8,   0.2,   1.5,
                      0, 0,   0,    0,     0,     0,    0.5,   0.9,  -0.3,
                      0, 0,   0,    0,     0   ,  0,    0,     0.13,  0.2,
                      0, 0,   0,    0,     0,     0,    0,     0,     1.5,
                      0, 0,   0,    0,     0,     0,    0,     0,     0
                      ),
                    nrow = 6, byrow = TRUE),
  par2     = matrix(0, 9, 9),
  var_names = paste0("X", 1:9)
)



scenarios <- list(scenario_2)

# Run generate 100 data sets consisting of 500 independent samples
n_reps <- 100   # e.g. 100 replications per scenario
n_sim  <- 500  # e.g. sample size per replication

results_list <- lapply(scenarios, function(sc) {
  cat("\nRunning", sc$name, "for", n_reps, "replications...\n")
  
  replicate_scenario(
    mat       = sc$mat,
    families  = sc$families,
    par       = sc$par,
    par2      = sc$par2,
    var_names = sc$var_names,
    n_sim     = n_sim,
    n_reps    = n_reps
  )
})


# Print results for scenario 1
results_list[[1]]
# Print results for scenario 2
results_list[[2]]

























