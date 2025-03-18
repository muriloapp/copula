library(copula)  
library(MASS)    

set.seed(123)

rho <- 0.6  # Common correlation for both copulas

# Compute the corresponding Clayton copula parameter (theta)
theta_clayton <- 2 * (rho / (1 - rho))  

# Number of samples
n <- 5000

# Generate data from the Gaussian copula
gaussian_cop <- normalCopula(param = rho, dim = 2)  # Define Gaussian copula
U_gauss <- rCopula(n, gaussian_cop)  # Sample from the Gaussian copula

# Generate data from the Clayton copula
clayton_cop <- claytonCopula(param = theta_clayton, dim = 2)  # Define Clayton copula
U_clayton <- rCopula(n, clayton_cop)  # Sample from the Clayton copula

# Transform to standard normal marginals using the inverse CDF (qnorm)
Z_gauss <- qnorm(U_gauss)
Z_clayton <- qnorm(U_clayton)

# Identify points in the lower tail (both values < -1)
lower_tail_gauss <- Z_gauss[,1] < -1 & Z_gauss[,2] < -1
lower_tail_clayton <- Z_clayton[,1] < -1 & Z_clayton[,2] < -1

# Plot Gaussian Copula
png("motivation_examples/Figures/Example_same_corr.png", width = 1200, height = 600)
par(mfrow = c(1, 2))  # Set plotting layout for side-by-side plots


cex.axis.size <- 1.8  
cex.lab.size <- 1.8   
cex.main.size <- 1.8  
thickness <- 1.8

plot(Z_gauss, col = ifelse(lower_tail_gauss, "black", "grey"),
     pch = 19, cex = 0.6, 
     cex.axis = cex.axis.size, 
     cex.lab = cex.lab.size,    
     cex.main = cex.main.size,  
     main = expression(~ X %~% N(0, 1) ~ ",  " ~ Y %~% N(0, 1)~ " - Gaussian dependence"),
     xlab = "X", 
     ylab = "Y")
     

# Plot Clayton Copula
plot(Z_clayton, col = ifelse(lower_tail_clayton, "black", "grey"),
     pch = 19, cex = 0.6, 
     cex.axis = cex.axis.size, 
     cex.lab = cex.lab.size,    
     cex.main = cex.main.size,  
     main = expression(~ X %~% N(0, 1) ~ ",  " ~ Y %~% N(0, 1) ~ " - Clayton dependence"),
     xlab = "X", 
     ylab = "Y")

dev.off()
# Reset plotting layout
par(mfrow = c(1, 1))






















