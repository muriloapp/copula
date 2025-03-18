library(rugarch)
library(quantmod)
library(readxl)
library(dplyr)
library(zoo)
library(here)

options(scipen = 999)
Sys.setenv(TZ = 'GMT')


load_rets_data <- function(){
  df <- read_excel("dataset/df_oct_nov.xlsx")
  df$hour <- as.numeric(df$hour)
  df$hour_formatted <- sprintf("%06d", df$hour)
  df$datetime <- as.POSIXct(strptime(paste(df$day, df$hour_formatted), format = "%Y%m%d %H%M"))
  start_date <- as.POSIXct("2019-11-01", tz = "GMT")
  end_date <- as.POSIXct("2019-11-30 23:59:59", tz = "GMT")
  df <- df[df$datetime >= start_date & df$datetime <= end_date, ]
  xts_data <- xts(x = df[, -which(names(df) %in% c("date", "hour", "hour_formatted", "datetime"))], order.by = df$datetime)
  return(xts_data)
}


load_pit_data <- function(){
  directory = "dataset/margins"
  file_path <- file.path(directory, "pit")
  data <- readRDS(file_path)
  return(data)
}


select_train_data <- function(d, days, data){
  # Inputs:
  #    d (Int): Number of training days  
  desired_dates <- days[(d - 9):d]
  pattern <- paste(desired_dates, collapse = "|")
  matching_rows <- grep(pattern, rownames(data))
  train_data <- data[matching_rows, ]
  return(train_data)
}


select_test_data <- function(d, d_forecast, days, data){
  # Inputs:
  #    d (Int): Number of training days   
  #    d (Int): Number of forecasting days
  test_days <- days[(d + 1):(d + d_forecast)]
  pattern <- paste(test_days, collapse = "|")
  matching_rows <- grep(pattern, rownames(data))
  test_data <- data[matching_rows, ]
  return(test_data)
}