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
# Dhaka Bank

dhaka_data <- read.csv("Dhaka Bank Stock Price History.csv")
dhaka_data

## Handle missing values ##
# Dhaka Bank

dhaka_data_na_omit <- na.omit(dhaka_data)
dhaka_data_na_omit

## Compute log returns of all the companies ##
# Dhaka Bank

# Create a structured data frame with cleaned variables

dhaka_prices <- data.frame(
  Date     = mdy(dhaka_data_na_omit$Date),      # Convert date column to Date format
  Price = dhaka_data_na_omit$Price
)
dhaka_prices

# Daily log returns

dhaka_daily_log_ret <- dhaka_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                              # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: daily return
    period      = "daily",                            # Frequency
    type        = "log"                               # Logarithmic returns
  ) %>%
  filter(Date != as.Date("2020-09-01"))
dhaka_daily_log_ret

## Visualization ##
# Dhaka Bank

# Raw prices

dhaka_prices |>
  ggplot(aes(Date, Price)) +
  geom_line() +
  labs(
    title = "Price of Dhaka Bank",
    x = "Date",
    y = "Price"
  )

# Log returns

dhaka_daily_log_ret |>
  ggplot(aes(Date, daily.returns)) +
  geom_line() +
  labs(
    title = "Log Returns of Dhaka",
    x = "Date",
    y = "Return"
  )

## Risk measures ##
# Dhaka Bank

# Annualized log returns

dhaka_annual_log_ret <- dhaka_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                           # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: annual return
    period      = "yearly",                            # Frequency
    type        = "log"                               # Logarithmic returns
  )
dhaka_annual_log_ret

# Mean return of annual log returns

dhaka_mean_return <- mean(dhaka_annual_log_ret$yearly.returns)
dhaka_mean_return

# Volatility (standard deviation) of annual log returns

dhaka_sd <-
  dhaka_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = table.Stats
  ) |>
  select(Stdev)
dhaka_sd

# Downside deviation of annual log returns

dhaka_downside <-
  dhaka_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = DownsideDeviation
  )
dhaka_downside

# VaR of annual log returns

dhaka_var <-
  dhaka_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = VaR,
    p = 0.95,
    method = "historical"
  )
dhaka_var

# Expected shortfall of annual log returns

dhaka_es <-
  dhaka_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = ES,
    p = 0.95,
    method = "historical"
  )
dhaka_es

## ACF and PACF plots for prices and returns ##
# Dhaka Bank

# k value

dhaka_k_price <- floor((nrow(dhaka_prices) - 1)^(1/3))
dhaka_k_price
dhaka_k_daily_ret <- floor((nrow(dhaka_daily_log_ret) - 1)^(1/3))
dhaka_k_daily_ret
dhaka_k_annual_ret <- floor((nrow(dhaka_annual_log_ret) - 1)^(1/3))
dhaka_annual_log_ret

# ACF and PACF value

dhaka_adf_price  <- adf.test(dhaka_prices$Price, k = dhaka_k_price)
dhaka_adf_price
dhaka_adf_daily_return <- adf.test(dhaka_daily_log_ret$daily.returns, k = dhaka_k_daily_ret)
dhaka_adf_daily_return
dhaka_adf_annual_return <- adf.test(dhaka_annual_log_ret$yearly.returns, k = dhaka_k_annual_ret)
dhaka_adf_annual_return

# ACF and PACF plots

acf(dhaka_prices$Price)
pacf(dhaka_prices$Price)
acf(dhaka_daily_log_ret$daily.returns)
pacf(dhaka_daily_log_ret$daily.returns)
acf(dhaka_annual_log_ret$yearly.returns)
pacf(dhaka_annual_log_ret$yearly.returns)
