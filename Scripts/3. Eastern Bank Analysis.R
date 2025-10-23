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
# Eastern Bank

eastern_data <- read.csv("Eastern Bank Stock Price History.csv")
eastern_data

## Handle missing values ##
# Eastern Bank

eastern_data_na_omit <- na.omit(eastern_data)
eastern_data_na_omit

## Compute log returns of all the companies ##
# Eastern Bank

# Create a structured data frame with cleaned variables

eastern_prices <- data.frame(
  Date     = mdy(eastern_data_na_omit$Date),      # Convert date column to Date format
  Price = eastern_data_na_omit$Price
)
eastern_prices

# Daily log returns

eastern_daily_log_ret <- eastern_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                              # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: daily return
    period      = "daily",                            # Frequency
    type        = "log"                               # Logarithmic returns
  ) %>%
  filter(Date != as.Date("2020-09-01"))
eastern_daily_log_ret

## Visualization ##
# Eastern Bank

# Raw prices

eastern_prices |>
  ggplot(aes(Date, Price)) +
  geom_line() +
  labs(
    title = "Price of Eastern Bank",
    x = "Date",
    y = "Price"
  )

# Log returns

eastern_daily_log_ret |>
  ggplot(aes(Date, daily.returns)) +
  geom_line() +
  labs(
    title = "Log Returns of Eastern Bank",
    x = "Date",
    y = "Return"
  )

## Risk measures ##
# Eastern Bank

# Annualized log returns

eastern_annual_log_ret <- eastern_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                           # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: annual return
    period      = "yearly",                            # Frequency
    type        = "log"                               # Logarithmic returns
  )
eastern_annual_log_ret

# Mean return of annual log returns

eastern_mean_return <- mean(eastern_annual_log_ret$yearly.returns)
eastern_mean_return

# Volatility (standard deviation) of annual log returns

eastern_sd <-
  eastern_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = table.Stats
  ) |>
  select(Stdev)
eastern_sd

# Downside deviation of annual log returns

eastern_downside <-
  eastern_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = DownsideDeviation
  )
eastern_downside

# VaR of annual log returns

eastern_var <-
  eastern_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = VaR,
    p = 0.95,
    method = "historical"
  )
eastern_var

# Expected shortfall of annual log returns

eastern_es <-
  eastern_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = ES,
    p = 0.95,
    method = "historical"
  )
eastern_es

## ACF and PACF plots for prices and returns ##
# Eastern Bank

# k value

eastern_k_price <- floor((nrow(eastern_prices) - 1)^(1/3))
eastern_k_price
eastern_k_daily_ret <- floor((nrow(eastern_daily_log_ret) - 1)^(1/3))
eastern_k_daily_ret
eastern_k_annual_ret <- floor((nrow(eastern_annual_log_ret) - 1)^(1/3))
eastern_annual_log_ret

# ACF and PACF value

eastern_adf_price  <- adf.test(eastern_prices$Price, k = eastern_k_price)
eastern_adf_price
eastern_adf_daily_return <- adf.test(eastern_daily_log_ret$daily.returns, k = eastern_k_daily_ret)
eastern_adf_daily_return
eastern_adf_annual_return <- adf.test(eastern_annual_log_ret$yearly.returns, k = eastern_k_annual_ret)
eastern_adf_annual_return

# ACF and PACF plots

acf(eastern_prices$Price)
pacf(eastern_prices$Price)
acf(eastern_daily_log_ret$daily.returns)
pacf(eastern_daily_log_ret$daily.returns)
acf(eastern_annual_log_ret$yearly.returns)
pacf(eastern_annual_log_ret$yearly.returns)
