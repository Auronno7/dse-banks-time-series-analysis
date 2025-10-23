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
  labs(title = "Bank Prices Comparisonv", x = NULL, y = "Price", color = "Bank") +
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
