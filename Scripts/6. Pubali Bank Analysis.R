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
# Pubali Bank

pubali_data <- read.csv("Pubali Bank Stock Price History.csv")
pubali_data

## Handle missing values ##
# Pubali Bank

pubali_data_na_omit <- na.omit(pubali_data)
pubali_data_na_omit

## Compute log returns of all the companies ##
# Pubali Bank

# Create a structured data frame with cleaned variables

pubali_prices <- data.frame(
  Date     = mdy(pubali_data_na_omit$Date),      # Convert date column to Date format
  Price = pubali_data_na_omit$Price
)
pubali_prices

# Daily log returns

pubali_daily_log_ret <- pubali_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                              # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: daily return
    period      = "daily",                            # Frequency
    type        = "log"                               # Logarithmic returns
  ) %>%
  filter(Date != as.Date("2020-09-01"))
pubali_daily_log_ret

## Visualization ##
# Pubali Bank

# Raw prices

pubali_prices |>
  ggplot(aes(Date, Price)) +
  geom_line() +
  labs(
    title = "Price of Pubali Bank",
    x = "Date",
    y = "Price"
  )

# Log returns

pubali_daily_log_ret |>
  ggplot(aes(Date, daily.returns)) +
  geom_line() +
  labs(
    title = "Log Returns of Pubali",
    x = "Date",
    y = "Return"
  )

## Risk measures ##
# Pubali Bank

# Annualized log returns

pubali_annual_log_ret <- pubali_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                           # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: annual return
    period      = "yearly",                            # Frequency
    type        = "log"                               # Logarithmic returns
  )
pubali_annual_log_ret

# Mean return of annual log returns

pubali_mean_return <- mean(pubali_annual_log_ret$yearly.returns)
pubali_mean_return

# Volatility (standard deviation) of annual log returns

pubali_sd <-
  pubali_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = table.Stats
  ) |>
  select(Stdev)
pubali_sd

# Downside deviation of annual log returns

pubali_downside <-
  pubali_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = DownsideDeviation
  )
pubali_downside

# VaR of annual log returns

pubali_var <-
  pubali_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = VaR,
    p = 0.95,
    method = "historical"
  )
pubali_var

# Expected shortfall of annual log returns

pubali_es <-
  pubali_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = ES,
    p = 0.95,
    method = "historical"
  )
pubali_es

## ACF and PACF plots for prices and returns ##
# Pubali Bank

# k value

pubali_k_price <- floor((nrow(pubali_prices) - 1)^(1/3))
pubali_k_price
pubali_k_daily_ret <- floor((nrow(pubali_daily_log_ret) - 1)^(1/3))
pubali_k_daily_ret
pubali_k_annual_ret <- floor((nrow(pubali_annual_log_ret) - 1)^(1/3))
pubali_annual_log_ret

# ACF and PACF value

pubali_adf_price  <- adf.test(pubali_prices$Price, k = pubali_k_price)
pubali_adf_price
pubali_adf_daily_return <- adf.test(pubali_daily_log_ret$daily.returns, k = pubali_k_daily_ret)
pubali_adf_daily_return
pubali_adf_annual_return <- adf.test(pubali_annual_log_ret$yearly.returns, k = pubali_k_annual_ret)
pubali_adf_annual_return

# ACF and PACF plots

acf(pubali_prices$Price)
pacf(pubali_prices$Price)
acf(pubali_daily_log_ret$daily.returns)
pacf(pubali_daily_log_ret$daily.returns)
acf(pubali_annual_log_ret$yearly.returns)
pacf(pubali_annual_log_ret$yearly.returns)
