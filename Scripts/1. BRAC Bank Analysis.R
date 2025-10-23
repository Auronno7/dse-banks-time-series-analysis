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
# BRAC Bank

brac_data <- read_csv("Brac Bank Stock Price History.csv", show_col_types = FALSE)
brac_data

## Handle missing values ##
# BRAC Bank

brac_data_na_omit <- na.omit(brac_data)
brac_data_na_omit

## Compute log returns of all the companies ##
# BRAC Bank

# Create a structured data frame with cleaned variables

brac_prices <- data.frame(
  Date     = mdy(brac_data_na_omit$Date),      # Convert date column to Date format
  Price = brac_data_na_omit$Price
)
brac_prices

# Daily log returns

brac_daily_log_ret <- brac_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                              # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: daily return
    period      = "daily",                            # Frequency
    type        = "log"                               # Logarithmic returns
  ) %>%
  filter(Date != as.Date("2020-09-01"))
brac_daily_log_ret

## Visualization ##
# BRAC Bank

# Raw prices

brac_prices |>
  ggplot(aes(Date, Price)) +
  geom_line() +
  labs(
    title = "Price of BRAC Bank",
    x = "Date",
    y = "Price"
  )

# Log returns

brac_daily_log_ret |>
  ggplot(aes(Date, daily.returns)) +
  geom_line() +
  labs(
    title = "Log Returns of BRAC",
    x = "Date",
    y = "Return"
  )

## Risk measures ##
# BRAC Bank

# Annualized log returns

brac_annual_log_ret <- brac_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                           # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: annual return
    period      = "yearly",                            # Frequency
    type        = "log"                               # Logarithmic returns
  )
brac_annual_log_ret

# Mean return of annual log returns

brac_mean_return <- mean(brac_annual_log_ret$yearly.returns)
brac_mean_return

# Volatility (standard deviation) of annual log returns

brac_sd <-
  brac_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = table.Stats
  ) |>
  select(Stdev)
brac_sd

# Downside deviation of annual log returns

brac_downside <-
  brac_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = DownsideDeviation
  )
brac_downside

# VaR of annual log returns

brac_var <-
  brac_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = VaR,
    p = 0.95,
    method = "historical"
  )
brac_var

# Expected shortfall of annual log returns

brac_es <-
  brac_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = ES,
    p = 0.95,
    method = "historical"
  )
brac_es

## ACF and PACF plots for prices and returns ##
# BRAC Bank

# k value

brac_k_price <- floor((nrow(brac_prices) - 1)^(1/3))
brac_k_price
brac_k_daily_ret <- floor((nrow(brac_daily_log_ret) - 1)^(1/3))
brac_k_daily_ret
brac_k_annual_ret <- floor((nrow(brac_annual_log_ret) - 1)^(1/3))
brac_annual_log_ret

# ACF and PACF value

brac_adf_price  <- adf.test(brac_prices$Price, k = brac_k_price)
brac_adf_price
brac_adf_daily_return <- adf.test(brac_daily_log_ret$daily.returns, k = brac_k_daily_ret)
brac_adf_daily_return
brac_adf_annual_return <- adf.test(brac_annual_log_ret$yearly.returns, k = brac_k_annual_ret)
brac_adf_annual_return

# ACF and PACF plots

acf(brac_prices$Price)
pacf(brac_prices$Price)
acf(brac_daily_log_ret$daily.returns)
pacf(brac_daily_log_ret$daily.returns)
acf(brac_annual_log_ret$yearly.returns)
pacf(brac_annual_log_ret$yearly.returns)
