# Set working directory


# Installing packages

options(repos = c(CRAN = "https://cloud.r-project.org"))  # set CRAN mirror

pkgs <- c("tidyverse", "tidyquant", "quantmod", "dplyr", "readr",
          "DBI", "jsonlite", "RSQLite", "readxl", "jsonlite",
          "httr", "ggplot2", "lubridate", "tseries", "scales",
          "gt")

# install only the ones not already installed

invisible(lapply(setdiff(pkgs, rownames(installed.packages())), install.packages))

# (optional) load them
# invisible(lapply(pkgs, require, character.only = TRUE))

# Load libraries

library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tidyquant)
library(tseries)
library(scales)
library(gt)

## Load data set from CSV ##
# City Bank

city_data <- read.csv("City Bank Stock Price History.csv")
city_data

## Handle missing values ##
# City Bank

city_data_na_omit <- na.omit(city_data)
city_data_na_omit

## Compute log returns of all the companies ##
# City Bank

# Create a structured data frame with cleaned variables

city_prices <- data.frame(
  Date     = mdy(city_data_na_omit$Date),      # Convert date column to Date format
  Price = city_data_na_omit$Price
)
city_prices

# Daily log returns

city_daily_log_ret <- city_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                              # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: daily return
    period      = "daily",                            # Frequency
    type        = "log"                               # Logarithmic returns
  ) %>%
  filter(Date != as.Date("2020-09-01"))
city_daily_log_ret

## Visualization ##
# City Bank

# Raw prices

city_prices |>
  ggplot(aes(Date, Price)) +
  geom_line() +
  labs(
    title = "Price of city Bank",
    x = "Date",
    y = "Price"
  )

# Log returns

city_daily_log_ret |>
  ggplot(aes(Date, daily.returns)) +
  geom_line() +
  labs(
    title = "Log Returns of city",
    x = "Date",
    y = "Return"
  )

## Risk measures ##
# City Bank

# Annualized log returns

city_annual_log_ret <- city_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                           # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: annual return
    period      = "yearly",                            # Frequency
    type        = "log"                               # Logarithmic returns
  )
city_annual_log_ret

# Mean return of annual log returns

city_mean_return <- mean(city_annual_log_ret$yearly.returns)
city_mean_return

# Volatility (standard deviation) of annual log returns

city_sd <-
  city_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = table.Stats
  ) |>
  select(Stdev)
city_sd

# Downside deviation of annual log returns

city_downside <-
  city_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = DownsideDeviation
  )
city_downside

# VaR of annual log returns

city_var <-
  city_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = VaR,
    p = 0.95,
    method = "historical"
  )
city_var

# Expected shortfall of annual log returns

city_es <-
  city_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = ES,
    p = 0.95,
    method = "historical"
  )
city_es

## ACF and PACF plots for prices and returns ##
# City Bank

# k value

city_k_price <- floor((nrow(city_prices) - 1)^(1/3))
city_k_price
city_k_daily_ret <- floor((nrow(city_daily_log_ret) - 1)^(1/3))
city_k_daily_ret
city_k_annual_ret <- floor((nrow(city_annual_log_ret) - 1)^(1/3))
city_annual_log_ret

# ACF and PACF value

city_adf_price  <- adf.test(city_prices$Price, k = city_k_price)
city_adf_price
city_adf_daily_return <- adf.test(city_daily_log_ret$daily.returns, k = city_k_daily_ret)
city_adf_daily_return
city_adf_annual_return <- adf.test(city_annual_log_ret$yearly.returns, k = city_k_annual_ret)
city_adf_annual_return

# ACF and PACF plots

acf(city_prices$Price)
pacf(city_prices$Price)
acf(city_daily_log_ret$daily.returns)
pacf(city_daily_log_ret$daily.returns)
acf(city_annual_log_ret$yearly.returns)
pacf(city_annual_log_ret$yearly.returns)
