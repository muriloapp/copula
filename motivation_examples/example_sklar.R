library(MASS)

set.seed(123)
P <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
n <- 2000
samples <- mvrnorm(n, mu = c(0, 0), Sigma = P)
highlight_indices <- c(100, 500, 900)  # Arbitrary chosen indices
highlight_points <- samples[highlight_indices, ]

# Compute the Gauss copula transform
Phi <- pnorm  # Standard normal CDF
copula_samples <- apply(samples, 2, Phi)


png("Example_Sklar_1.png", width = 1200, height = 600)
par(mfrow = c(1, 2))  # Set up two side-by-side plots
par(mar = c(5,5,5,5))  # Set margins: bottom, left, top, right


highlight_colors <- c("red", "orange", "blue")
cex.axis.size <- 1.8  # Size for axis labels
cex.lab.size <- 1.8   # Size for x and y axis main labels
cex.main.size <- 1.8  # Size for the main title
thickness <- 1.8

plot(samples, main = "(X,Y) for a joint normal with rho=0.5",
     col = "black", pch = 19, cex = 0.6, cex.axis = cex.axis.size, cex.lab=cex.lab.size, cex.main=cex.main.size,
     xlab = "X", ylab = "Y") 
points(highlight_points, col = c("red", "orange", "blue"),bg = highlight_colors, pch = 21, cex = 1.3, lwd=1)
text(highlight_points, labels = c("A", "B", "C"),font = 1, pos = 1, col = c("red", "orange", "blue"), cex = 1.5)

# Plot transformed copula scatter plot
plot(copula_samples, main = "(U,V)=(F(X),F(Y)) for (X,Y) a joint normal", 
     col = "black", pch = 19, cex = 0.6, cex.axis = cex.axis.size, cex.lab=cex.lab.size, cex.main=cex.main.size,
     xlab = "U", ylab = "V") 
points(copula_samples[highlight_indices, ], col = c("red", "orange", "blue"), bg = highlight_colors, pch = 21, cex = 1.3, lwd=1)
text(copula_samples[highlight_indices, ], labels = c("A", "B", "C"),font = 1, pos = 1, col = c("red", "orange", "blue"), cex = 1.5)

dev.off()


#-------------------------------------------------------------
# Part 2 
#-------------------------------------------------------------

# Define the inverse CDF (quantile function) of Exp(2)
exp_quantile <- function(u) { -log(1 - u) / 2 }

# Apply Exponential(2) transformation
exp_transformed_samples <- apply(copula_samples, 2, exp_quantile)
highlight_transformed <- apply(copula_samples[highlight_indices, ], 2, exp_quantile)


png("Example_Sklar_2.png", width = 1200, height = 600)
# Set up two side-by-side plots
par(mfrow = c(1, 2), mar = c(5,5,5,5))  # Set margins: bottom, left, top, right)

# Colors for highlighted points
highlight_colors <- c("red", "orange", "blue")

# Left: Gauss copula scatter plot
plot(copula_samples, main = "(U,V)=(F(X),F(Y)) for (X,Y) a joint normal",
     col = "black", pch = 19, cex = 0.6, cex.axis = cex.axis.size, cex.lab=cex.lab.size, cex.main=cex.main.size,  xlab = "U", ylab = "V")
points(copula_samples[highlight_indices, ], col = highlight_colors, bg = highlight_colors, pch = 21, cex = 1.3, lwd = 1)
text(copula_samples[highlight_indices, ], labels = c("A", "B", "C"), pos = 1, col = highlight_colors, font = 1, cex = 1.5)

# Right: Transformed Exponential(2) scatter plot
plot(exp_transformed_samples, main = "(G^{-1}(U),H^{-1}(V)) for dfs G, H exp with mean 0.5", xlab = "X", ylab = "Y",
     col = "black", pch = 19, cex = 0.6, cex.axis = cex.axis.size, cex.lab=cex.lab.size, cex.main=cex.main.size,)
points(highlight_transformed, col = highlight_colors, bg = highlight_colors, pch = 21, cex = 1.3, lwd = 1)
text(highlight_transformed, labels = c("A", "B", "C"), pos = 1, col = highlight_colors, font = 1, cex = 1.5)

dev.off()

