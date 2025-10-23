# Set working directory

setwd("D:\\Programing Languages\\R\\Working Directory")

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

banks_data <- read_csv("Combined Banks Stock Price History.csv", show_col_types = FALSE)
data

## Handle missing values ##

banks_data_na_omit <- na.omit(banks_data)
banks_data_na_omit

## Compute log returns of all the companies ##
# BRAC Bank

# Create a structured data frame with cleaned variables

brac_prices <- data.frame(
  Ticker = banks_data_na_omit$Ticker,
  Date     = mdy(banks_data_na_omit$Date),      # Convert date column to Date format
  Price = banks_data_na_omit$Price
) %>%
  filter(banks_data_na_omit$Ticker == "Brac")
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

## Compute log returns of all the companies ##
# Eastern Bank

# Create a structured data frame with cleaned variables

eastern_prices <- data.frame(
  Ticker = banks_data_na_omit$Ticker,
  Date     = mdy(banks_data_na_omit$Date),      # Convert date column to Date format
  Price = banks_data_na_omit$Price
) %>%
  filter(banks_data_na_omit$Ticker == "Eastern")
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

## Compute log returns of all the companies ##
# City Bank

# Create a structured data frame with cleaned variables

city_prices <- data.frame(
  Ticker = banks_data_na_omit$Ticker,
  Date     = mdy(banks_data_na_omit$Date),      # Convert date column to Date format
  Price = banks_data_na_omit$Price
) %>%
  filter(banks_data_na_omit$Ticker == "City")
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

## Compute log returns of all the companies ##
# Dhaka Bank

# Create a structured data frame with cleaned variables

dhaka_prices <- data.frame(
  Ticker = banks_data_na_omit$Ticker,
  Date     = mdy(banks_data_na_omit$Date),      # Convert date column to Date format
  Price = banks_data_na_omit$Price
) %>%
  filter(banks_data_na_omit$Ticker == "Dhaka")
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

## Compute log returns of all the companies ##
# Islami BANK Bangladesh

# Create a structured data frame with cleaned variables

islami_prices <- data.frame(
  Ticker = banks_data_na_omit$Ticker,
  Date     = mdy(banks_data_na_omit$Date),      # Convert date column to Date format
  Price = banks_data_na_omit$Price
) %>%
  filter(banks_data_na_omit$Ticker == "Islami")
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

## Compute log returns of all the companies ##
# Pubali Bank

# Create a structured data frame with cleaned variables

pubali_prices <- data.frame(
  Ticker = banks_data_na_omit$Ticker,
  Date     = mdy(banks_data_na_omit$Date),      # Convert date column to Date format
  Price = banks_data_na_omit$Price
) %>%
  filter(banks_data_na_omit$Ticker == "Pubali")
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

## Price ##

# Separate charts

brac_chart_prices   <- brac_prices   %>% transmute(
  bank  = "Brac Bank",
  date  = as.Date(Date, tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d-%b-%Y","%d.%m.%Y")),
  price = coalesce(suppressWarnings(as.numeric(Price)),
                   parse_number(as.character(Price)))
)

eastern_chart_prices <- eastern_prices %>% transmute(
  bank  = "Eastern Bank",
  date  = as.Date(Date, tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d-%b-%Y","%d.%m.%Y")),
  price = coalesce(suppressWarnings(as.numeric(Price)),
                   parse_number(as.character(Price)))
)

city_chart_prices   <- city_prices   %>% transmute(
  bank  = "City Bank",
  date  = as.Date(Date, tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d-%b-%Y","%d.%m.%Y")),
  price = coalesce(suppressWarnings(as.numeric(Price)),
                   parse_number(as.character(Price)))
)

dhaka_chart_prices  <- dhaka_prices  %>% transmute(
  bank  = "Dhaka Bank",
  date  = as.Date(Date, tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d-%b-%Y","%d.%m.%Y")),
  price = coalesce(suppressWarnings(as.numeric(Price)),
                   parse_number(as.character(Price)))
)

islami_chart_prices <- islami_prices %>% transmute(
  bank  = "Islami Bank",
  date  = as.Date(Date, tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d-%b-%Y","%d.%m.%Y")),
  price = coalesce(suppressWarnings(as.numeric(Price)),
                   parse_number(as.character(Price)))
)

pubali_chart_prices <- pubali_prices %>% transmute(
  bank  = "Pubali Bank",
  date  = as.Date(Date, tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d-%b-%Y","%d.%m.%Y")),
  price = coalesce(suppressWarnings(as.numeric(Price)),
                   parse_number(as.character(Price)))
)

# Single chart

single_chart_price <- bind_rows(brac_chart_prices, eastern_chart_prices, city_chart_prices, dhaka_chart_prices, islami_chart_prices, pubali_chart_prices) %>%
  filter(!is.na(date), !is.na(price)) %>%
  distinct(bank, date, .keep_all = TRUE) %>%   # drop duplicate dates if any
  arrange(bank, date)

# Line charts (separate charts)

ggplot(single_chart_price, aes(x = date, y = price)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ bank, nrow = 2, ncol = 3, scales = "free_y") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Bank Prices Comparison", x = NULL, y = "Price") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Line chart (single chart)

ggplot(single_chart_price, aes(x = date, y = price, color = bank)) +
  geom_line(linewidth = 0.5) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Bank Prices Comparison", x = NULL, y = "Price", color = "Bank") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Daily log returns ##

# Separate charts 

combined_daily_log_ret <- bind_rows(
  brac_daily_log_ret   %>% rename(Returns = daily.returns)   %>% mutate(Bank = "Brac Bank"),
  eastern_daily_log_ret%>% rename(Returns = daily.returns)   %>% mutate(Bank = "Eastern Bank"),
  city_daily_log_ret%>% rename(Returns = daily.returns)   %>% mutate(Bank = "City Bank"),
  dhaka_daily_log_ret%>% rename(Returns = daily.returns)   %>% mutate(Bank = "Dhaka Bank"),
  islami_daily_log_ret%>% rename(Returns = daily.returns)   %>% mutate(Bank = "Islami Bank"),
  pubali_daily_log_ret%>% rename(Returns = daily.returns)   %>% mutate(Bank = "Pubali Bank")  # or "City Bank"
) %>%
  select(Bank, Date, Returns) %>%
  filter(!is.na(Date), !is.na(Returns)) %>%
  arrange(Bank, Date)

# Line charts

ggplot(combined_daily_log_ret, aes(x = Date, y = Returns)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Bank, nrow = 2, ncol = 3, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Daily Log Returns Comparison", x = "Date", y = "Return") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram charts

ggplot(combined_daily_log_ret, aes(x = Returns)) +
  # show share of days on y (not raw counts)
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 bins = 50, binwidth = 0.001, color = "white") +
  facet_wrap(~ Bank, nrow = 2, ncol = 3, scales = "free_y") +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Daily Log Returns Comparison",
       x = "Return", y = "% of days") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Box plot charts
# (Optional) order banks by median return for nicer reading:

combined_daily_log_ret_boxplot <- combined_daily_log_ret %>% mutate(Bank = reorder(Bank, Returns, median, na.rm = TRUE))

ggplot(combined_daily_log_ret_boxplot, aes(x = Bank, y = Returns, fill = Bank)) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Daily Log Returns Comparison",
       x = NULL, y = "Return") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Single chart

single_daily_log_ret <- list(
  "Brac Bank"    = brac_daily_log_ret,
  "Eastern Bank" = eastern_daily_log_ret,
  "City Bank"    = city_daily_log_ret,
  "Dhaka Bank"   = dhaka_daily_log_ret,
  "Islami Bank"  = islami_daily_log_ret,
  "Pubali Bank"  = pubali_daily_log_ret
)

single_chart_return <- do.call(bind_rows, lapply(names(single_daily_log_ret), function(nm) {
  single_dataframe <- single_daily_log_ret[[nm]]
  transmute(single_dataframe,
            bank = nm,
            date = as.Date(Date),
            ret  = as.numeric(daily.returns)) %>%
    filter(!is.na(date), !is.na(ret)) %>%
    arrange(date)
}))

# Line chart

ggplot(single_chart_return, aes(x = date, y = ret, color = bank)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Daily Log Returns Comparison",
       x = NULL, y = "Return", color = "Bank") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Scatter plot of mean return vs volatility (standard deviation)

# Single plot

as_scalar_num <- function(x) {
  if (is.data.frame(x)) return(as.numeric(x[[1]][1]))
  if (is.list(x))       return(as.numeric(x[[1]]))
  return(as.numeric(x)[1])
}

mean_vec <- sapply(
  list(brac_mean_return, city_mean_return, dhaka_mean_return,
       islami_mean_return, eastern_mean_return, pubali_mean_return),
  as_scalar_num
)

sd_vec <- sapply(
  list(brac_sd, city_sd, dhaka_sd, islami_sd, eastern_sd, pubali_sd),
  as_scalar_num
)

single_plot <- tibble::tibble(
  Bank         = c("Brac Bank","City Bank","Dhaka Bank","Islami Bank","Eastern Bank","Pubali Bank"),
  mean_return  = as.numeric(mean_vec),
  sd_volatility= as.numeric(sd_vec)
)

stopifnot(is.numeric(single_plot$mean_return), is.numeric(single_plot$sd_volatility))

# Scatter plot

ggplot(single_plot, aes(x = sd_volatility, y = mean_return, color = Bank)) +
  geom_point(size = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1), name = "Volatility (Std. Dev.)") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), name = "Mean Return") +
  labs(title = "Risk-Return Comparison") +
  theme_minimal(base_size = 11)

# Scatter plots

ggplot(single_plot, aes(x = sd_volatility, y = mean_return)) +
  geom_point(size = 3, color = "steelblue") +
  facet_wrap(~ Bank, nrow = 2, ncol = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1), name = "Volatility (Std. Dev.)") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), name = "Mean Return") +
  labs(title = "Risk-Return Comparison") +
  theme_minimal(base_size = 11)

## ADF p-value table

# Price

brac_adf_p_value_price <- 0.08006
eastern_adf_p_value_price <- 0.04799
city_adf_p_value_price <- 0.01
dhaka_adf_p_value_price <- 0.2911
islami_adf_p_value_price <- 0.08321
pubali_adf_p_value_price <- 0.01

# Daily Return

brac_adf_p_value_daily_return <- 0.01
eastern_adf_p_value_daily_return <- 0.01
city_adf_p_value_daily_return <- 0.01
dhaka_adf_p_value_daily_return <- 0.01
islami_adf_p_value_daily_return <- 0.01
pubali_adf_p_value_daily_return <- 0.01

# Creating the table

banks_list_adf_p_value <- c("BRAC Bank","Eastern Bank","City Bank","Dhaka Bank","Islami Bank","Pubali Bank")

# ADF p-value table (Price)

banks_adf_p_value_price <- c(
  brac_adf_p_value_price,
  eastern_adf_p_value_price,
  city_adf_p_value_price,
  dhaka_adf_p_value_price,
  islami_adf_p_value_price,
  pubali_adf_p_value_price
)

# ADF p-value table (Daily Return)

banks_adf_p_value_daily_return <- c(
  brac_adf_p_value_daily_return,
  eastern_adf_p_value_daily_return,
  city_adf_p_value_daily_return,
  dhaka_adf_p_value_daily_return,
  islami_adf_p_value_daily_return,
  pubali_adf_p_value_daily_return
)

# Summary table

banks_adf_p_value_gt <- data.frame(
  Bank = banks_list_adf_p_value,
  check.names = FALSE,
  "Price"         = banks_adf_p_value_price,
  "Daily Return"  = banks_adf_p_value_daily_return
)

banks_adf_p_value_table <- banks_adf_p_value_gt |>
  gt() |>
  tab_header(title = "ADF p-values Summary") |>
  fmt_number(columns = 2:3, decimals = 4) |>
  # highlight significant results (p < 0.05) in each column
  tab_style(
    style = cell_fill(color = "#e8f5e9"),
    locations = cells_body(rows = `Price` < 0.05, columns = "Price")
  ) |>
  tab_style(
    style = cell_fill(color = "#e8f5e9"),
    locations = cells_body(rows = `Daily Return` < 0.05, columns = "Daily Return")
  ) |>
  tab_source_note(md("H₀: unit root (non-stationary). p < 0.05 ⇒ reject H₀ (evidence of stationarity)."))
banks_adf_p_value_table

## Building risk-return table

risk_table <- tibble::tibble(
  `Banks/Risk Measure` = c("BRAC Bank", "Eastern Bank", "City Bank", "Dhaka Bank", "Islami Bank", "Pubali Bank"),
  `Mean Return`        = c(brac_mean_return, eastern_mean_return, city_mean_return, dhaka_mean_return, islami_mean_return, pubali_mean_return),
  `Standard Deviation` = c(brac_sd, eastern_sd, city_sd, dhaka_sd, islami_sd, pubali_sd),
  `Downside Deviation` = c(brac_downside, eastern_downside, city_downside, dhaka_downside, islami_downside, pubali_downside),
  `VaR`                = c(brac_var, eastern_var, city_var, dhaka_var, islami_var, pubali_var),
  `Expected Shortfall` = c(brac_es, eastern_es, city_es, dhaka_es, islami_es, pubali_es)
)

risk_table %>%
  gt() %>%
  fmt_percent(columns = 6:6, decimals = 2) %>%
  tab_header(title = "Risk Summary") %>%
  tab_source_note("VaR/ES based on our chosen confidence level")
