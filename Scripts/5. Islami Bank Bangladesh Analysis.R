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
# Islami Bank Bangladesh

islami_data <- read.csv("Islami Bank Bangladesh Stock Price History.csv")
islami_data

## Handle missing values ##
# Islami Bank Bangladesh

islami_data_na_omit <- na.omit(islami_data)
islami_data_na_omit

## Compute log returns of all the companies ##
# Islami Bank Bangladesh

# Create a structured data frame with cleaned variables

islami_prices <- data.frame(
  Date     = mdy(islami_data_na_omit$Date),      # Convert date column to Date format
  Price = islami_data_na_omit$Price
)
islami_prices

# Daily log returns

islami_daily_log_ret <- islami_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                              # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: daily return
    period      = "daily",                            # Frequency
    type        = "log"                               # Logarithmic returns
  ) %>%
  filter(Date != as.Date("2020-09-01"))
islami_daily_log_ret

## Visualization ##
# Islami BANK Bangladesh

# Raw prices

islami_prices |>
  ggplot(aes(Date, Price)) +
  geom_line() +
  labs(
    title = "Price of Islami Bank",
    x = "Date",
    y = "Price"
  )

# Log returns

islami_daily_log_ret |>
  ggplot(aes(Date, daily.returns)) +
  geom_line() +
  labs(
    title = "Log Returns of Islami",
    x = "Date",
    y = "Return"
  )

## Risk measures ##
# Islami Bank Bangladesh

# Annualized log returns

islami_annual_log_ret <- islami_prices %>%
  arrange(Date, .by_group = TRUE) %>%                 # Ensure dates are ordered
  tq_transmute(
    select      = Price,                           # Input: adjusted price
    mutate_fun  = periodReturn,                       # Function: annual return
    period      = "yearly",                            # Frequency
    type        = "log"                               # Logarithmic returns
  )
islami_annual_log_ret

# Mean return of annual log returns

islami_mean_return <- mean(islami_annual_log_ret$yearly.returns)
islami_mean_return

# Volatility (standard deviation) of annual log returns

islami_sd <-
  islami_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = table.Stats
  ) |>
  select(Stdev)
islami_sd

# Downside deviation of annual log returns

islami_downside <-
  islami_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = DownsideDeviation
  )
islami_downside

# VaR of annual log returns

islami_var <-
  islami_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = VaR,
    p = 0.95,
    method = "historical"
  )
islami_var

# Expected shortfall of annual log returns

islami_es <-
  islami_annual_log_ret|>
  tq_performance(
    Ra = yearly.returns,
    performance_fun = ES,
    p = 0.95,
    method = "historical"
  )
islami_es

## ACF and PACF plots for prices and returns ##
# Islami Bank Bangladesh

# k value

islami_k_price <- floor((nrow(islami_prices) - 1)^(1/3))
islami_k_price
islami_k_daily_ret <- floor((nrow(islami_daily_log_ret) - 1)^(1/3))
islami_k_daily_ret
islami_k_annual_ret <- floor((nrow(islami_annual_log_ret) - 1)^(1/3))
islami_annual_log_ret

# ACF and PACF value

islami_adf_price  <- adf.test(islami_prices$Price, k = islami_k_price)
islami_adf_price
islami_adf_daily_return <- adf.test(islami_daily_log_ret$daily.returns, k = islami_k_daily_ret)
islami_adf_daily_return
islami_adf_annual_return <- adf.test(islami_annual_log_ret$yearly.returns, k = islami_k_annual_ret)
islami_adf_annual_return

# ACF and PACF plots

acf(islami_prices$Price)
pacf(islami_prices$Price)
acf(islami_daily_log_ret$daily.returns)
pacf(islami_daily_log_ret$daily.returns)
acf(islami_annual_log_ret$yearly.returns)
pacf(islami_annual_log_ret$yearly.returns)
