library(rugarch)
library(quantmod)
library(readxl)
library(dplyr)
library(zoo)
library(ghyp)
library(stats)
library(GeneralizedHyperbolic)



#-------------------------------------------------------------------------------
# Marginals
#-------------------------------------------------------------------------------

options(scipen = 999)
Sys.setenv(TZ = 'GMT')

df <- read_excel("dataset/df_oct_nov.xlsx")
df$hour <- as.numeric(df$hour)
df$hour_formatted <- sprintf("%06d", df$hour)
df$datetime <- as.POSIXct(strptime(paste(df$day, df$hour_formatted), format="%Y%m%d %H%M"))
start_date <- as.POSIXct("2019-11-01", tz="GMT")
end_date <- as.POSIXct("2019-11-30 23:59:59", tz="GMT")
df <- df[df$datetime >= start_date & df$datetime <= end_date, ]
xts_data <- xts(x = df[, -which(names(df) %in% c("date", "hour", "hour_formatted", "datetime"))], order.by = df$datetime)
tickers <- list("AIG", "AXP", "BAC", "C", "COF", "GS",  "JPM", "MS", "WFC")


# Fit the best MC-Garch specification
fit_best_spec <- function(data, sigma_d ) {
  best_aic <- Inf
  best_fit <- NULL
  best_spec <- NULL
  
  # Define ranges for GARCH(p,q)
  p_range <- 1:1
  q_range <- 1:1
  distributions <- list("norm")
  for (distr in distributions) {
      for (p in p_range) {
        for (q in q_range) {
          spec_try <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                                 variance.model = list(model = 'mcsGARCH', garchOrder = c(p, q)), distribution = distr)
          fit_try <- ugarchfit(spec = spec_try, data = data, DailyVar = sigma_d)
          if (!is.null(fit_try)) {
            aic <- infocriteria(fit_try)[1, ]
            print(aic)
            print(p)
            print(q)
            if (aic < best_aic) {
              best_aic <- aic
              best_fit <- fit_try
              best_spec <- spec_try
            }
          }
        }
      }
  }
  
  list(best_fit = best_fit, best_spec = best_spec)
}

stdResidualsList <- list()
fit_list <- list()
spec_list <- list()
distr_list <- list()


# Specification to capture daily var
spec_d <- ugarchspec(mean.model = list(armaOrder = c(1, 1)), 
                     variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
                     distribution = 'norm')


# Run for each stock
for (ticker in tickers) {
  R <- getSymbols((ticker), from = '2018-03-31', auto.assign = FALSE)
  R <- adjustOHLC(R, use.Adjusted = TRUE)
  R_d <- ROC(Cl(R), na.pad = FALSE)
  R_d <- R_d['/2019-11-30']
  
  fitted_model <- ugarchfit(spec = spec_d, data = R_d)
  f_sigma <- sigma(fitted_model)
  f_sigma <- f_sigma['2019-11-01/']
  
  best_spec_results <- fit_best_spec(data = xts_data[,tolower(ticker)], sigma_d = f_sigma^2 )
  fit = ugarchfit(data = xts_data[,tolower(ticker)], spec = best_spec_results$best_spec, DailyVar = f_sigma^2)
  
  
  stdResidualsList[[ticker]] <- fit@fit$z
  fit_list[[ticker]] <- best_spec_results$best_fit
  spec_list[[ticker]] <- best_spec_results$best_spec
  distr_list[[ticker]] <- best_spec_results$best_spec@model$modeldesc$distribution
  
  print(paste("Finished asset: ", ticker))
}



#-------------------------------------------------------------------------------
# Stand residuals
#-------------------------------------------------------------------------------
stdResiduals <- data.frame(stdResidualsList)
print(colMeans(stdResiduals))
print(apply(stdResiduals, 2, sd))


#-------------------------------------------------------------------------------
# Data transformation (PIT)
#-------------------------------------------------------------------------------
#data <- apply(stdResiduals, 2, rank)/length(stdResiduals$aig)

data2 <- list()
for (col in names(stdResiduals)) {
  if (fit_list[[col]]@model$modeldesc$distribution == "norm"){
    data2[[col]] <- pnorm(stdResiduals[[col]])
  }
  else if (fit_list[[col]]@model$modeldesc$distribution == "std"){
    dof <- as.numeric(fit_list[[col]]@fit$coef["shape"])
    data2[[col]] <- pt(stdResiduals[[col]], df = dof)
  }
}

data <- as.data.frame(data2)
row.names(data) <- row.names(stdResiduals)


directory = "dataset/margins"
file_path <- file.path(directory, "z")
saveRDS(stdResidualsList, file_path)
file_path <- file.path(directory, "spec_list")
saveRDS(spec_list, file_path)
file_path <- file.path(directory, "pit")
saveRDS(data, file_path)
file_path <- file.path(directory, "fit_list")
saveRDS(fit_list, file_path)
file_path <- file.path(directory, "distr_list")
saveRDS(distr_list, file_path)




#-------------------------------------------------------------------------------
# Plot marginals
#-------------------------------------------------------------------------------

# Daily vol
spec_d = ugarchspec(mean.model = list(armaOrder = c(1, 1)), 
                    variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)), distribution = 'std')
spec = ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                  variance.model = list(model = 'mcsGARCH'), distribution = 'norm')

ticker <- "c"
R = quantmod::getSymbols(toupper(ticker), from = '2017-01-01',auto.assign=FALSE)
R = quantmod::adjustOHLC(R, use.Adjusted = TRUE)
R_d = TTR::ROC(Cl(R), na.pad = FALSE)
R_d <- R_d['/2019-11-22']

roll = ugarchroll(spec_d, data = R_d, forecast.length = 252 , refit.every = 10, refit.window = 'moving', moving.size = 756, calculate.VaR = FALSE)
aux = as.data.frame(roll)
f_sigma_plot = as.xts(aux[, 'Sigma', drop = FALSE])


# Intraday plots
ticker <- "c"
R = quantmod::getSymbols(toupper(ticker), from = '2017-01-01',auto.assign=FALSE)
R = quantmod::adjustOHLC(R, use.Adjusted = TRUE)
R_d = TTR::ROC(Cl(R), na.pad = FALSE)
R_d <- R_d['/2019-11-22']

subset_data <- xts_data['/2019-11-22',ticker]
n = length(unique(format(index(subset_data), '%Y-%m-%d')))
roll = ugarchroll(spec_d, data = R_d, forecast.length = n , refit.every = 10, refit.window = 'moving', moving.size = 756, calculate.VaR = FALSE)
aux = as.data.frame(roll)
f_sigma = as.xts(aux[, 'Sigma', drop = FALSE])

fit = ugarchfit(data = subset_data, spec = spec, DailyVar = f_sigma^2)

#Plot
setwd("dataset/margins/Figure")
png("c_marginal.png", width=900, height=600)

cex.axis.size <- 1.5  # Size for axis labels
cex.lab.size <- 1.5   # Size for x and y axis main labels
cex.main.size <- 1.5  # Size for the main title
thickness <- 1.8
ep_d <- axTicksByTime(f_sigma_plot, 'auto')
ep <- axTicksByTime(fit@model$DiurnalVar)
par(mfrow = c(2, 2), mar = c(2.5, 2.5, 2.5, 2.5))
plot(as.numeric(f_sigma_plot), type = 'l', main = '(a) Daily Volatility', col = 'blue3', xaxt = 'n', ylab = 'sigma', xlab = ' ', lwd = thickness,
     cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.main.size)
axis(1, at = ep_d, labels = names(ep_d), tick = TRUE, cex.axis = cex.axis.size)
grid()

plot(as.numeric(fit@model$DiurnalVar^0.5), type = 'l', main = '(b) Seasonality', col = 'blue3', xaxt = 'n', ylab = 'sigma', xlab = ' ', lwd = thickness,
     cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.main.size)
axis(1, at = ep, labels = names(ep), tick = TRUE, cex.axis = cex.axis.size)
grid()

plot(fit@fit$q, type = 'l', main = '(c) Stochastic Component', col = 'blue3', xaxt = 'n', ylab = 'sigma', xlab = ' ', lwd = thickness,
     cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.main.size)
axis(1, at = ep, labels = names(ep), tick = TRUE, cex.axis = cex.axis.size)
grid()

plot(as.numeric(sigma(fit)), type = 'l', main = '(d) Total Volatility', col = 'blue3', xaxt = 'n', ylab = 'sigma', xlab = ' ', lwd = thickness,
     cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.main.size)
axis(1, at = ep, labels = names(ep), tick = TRUE, cex.axis = cex.axis.size)
grid()
dev.off()























