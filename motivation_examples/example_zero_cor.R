library(MASS)  
library(here)

set.seed(1234)

n <- 2500
Sigma <- matrix(c(1, 0, 0, 1), 2, 2)  # Zero correlation
norm_data <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)

X_raw <- norm_data[, 1]
Y_raw <- norm_data[, 2]

U <- pnorm(X_raw)
V <- pnorm(Y_raw)


X_exp <- qexp(U, rate = 0.5) 
Y_exp <- qexp(V, rate = 0.25) 

# Transform to Beta with distinct shape parameters
X_beta <- qbeta(U, shape1 = 0.3, shape2 = 0.3)
Y_beta <- qbeta(V, shape1 = 2,   shape2 = 5)


png("motivation_examples/Figures/Example_zero_cor.png", width = 900, height = 600)

par(mfrow = c(2, 3))  # Arrange plots in 2 rows, 3 columns

cex.axis.size <- 1.8  # Size for axis labels (numbers on axes)
cex.lab.size <- 1.8   # Size for x and y axis labels
cex.main.size <- 1.8  # Size for main title
thickness <- 1.8      # Thickness for axis lines, titles, etc.

# Plot (a)
plot(X_raw, Y_raw, 
     pch = 16, col = rgb(0, 0, 1, 0.5),
     cex = 0.7,
     cex.axis = cex.axis.size, 
     cex.lab = cex.lab.size,    
     cex.main = cex.main.size,  
     main = expression(~ X %~% N(0, 1) ~ ",  " ~ Y %~% N(0, 1)),
     xlab = "X", 
     ylab = "Y")
mtext("(a)", side = 3, line = 1.3, adj = 0, cex = 1.4, font = 2) 

# Plot (b)
plot(X_exp, Y_exp, 
     pch = 16, col = rgb(1, 0, 0, 0.5),
     cex = 0.7, 
     cex.axis = cex.axis.size,  
     cex.lab = cex.lab.size,    
     cex.main = cex.main.size,  
     main = expression(~ X %~% Exp(2) ~ ",  " ~ Y %~% Exp(4)),
     xlab = "X", 
     ylab = "Y")
mtext("(b)", side = 3, line = 1.3, adj = 0, cex = 1.4, font = 2) 

# Plot (c)
plot(X_beta, Y_beta, 
     pch = 16, col = "black",
     cex = 0.7, 
     cex.axis = cex.axis.size,  
     cex.lab = cex.lab.size,    
     cex.main = cex.main.size,  
     main = expression(~ X %~% B(0.3, 0.3) ~ ",  " ~ Y %~% B(2, 5)),
     xlab = "X", 
     ylab = "Y")
mtext("(c)", side = 3, line = 1.3, adj = 0, cex = 1.4, font = 2) 

# Plot (d)
plot(U, V, 
     pch = 16, col = rgb(0, 0, 1, 0.5),
     cex = 0.7, 
     cex.axis = cex.axis.size,  
     cex.lab = cex.lab.size,    
     cex.main = cex.main.size,  
     main = expression("U = F(X), V = F(Y)"),
     xlab = "X", 
     ylab = "Y")
mtext("(d)", side = 3, line = 1.3, adj = 0, cex = 1.4, font = 2) 

# Plot (e)
plot(U, V, 
     pch = 16, col = rgb(1, 0, 0, 0.5),
     cex = 0.7, 
     cex.axis = cex.axis.size,  
     cex.lab = cex.lab.size,    
     cex.main = cex.main.size,  
     main = expression("U = F(X), V = F(Y)"),
     xlab = "U", 
     ylab = "V")
mtext("(e)", side = 3, line = 1.3, adj = 0, cex = 1.4, font = 2) 

# Plot (f)
plot(U, V, 
     pch = 16, col = "black",
     cex = 0.7, 
     cex.axis = cex.axis.size,  
     cex.lab = cex.lab.size,    
     cex.main = cex.main.size,  
     main = expression("U = F(X), V = F(Y)"),
     xlab = "U", 
     ylab = "V")
mtext("(f)", side = 3, line = 1.3, adj = 0, cex = 1.4, font = 2) 

dev.off()


# Print correlation values
cat("Sample correlation (Exponential X', Y'):", cor(X_beta, Y_beta), "\n")
cat("Sample correlation (Restored X'', Y''):", cor(X_restored, Y_restored), "\n")




