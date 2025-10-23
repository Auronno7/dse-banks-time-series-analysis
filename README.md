Project Overview

This repository contains an end-to-end, reproducible pipeline for financial analysis on Dhaka Stock Exchange bank equities. The project ingests daily CSV files (see `Datasets/`), cleans dates/prices, and builds a complete R workflow covering: daily & annual log returns (via `tq_transmute(periodReturn)`), stationarity diagnostics (ADF + ACF/PACF), and risk metrics (Mean, Standard Deviation, Downside Deviation, VaR, ES). Outputs include line charts (faceted 2×3 and combined), histograms, box plots, a risk-return scatter, and gt table summarizing ADF p-values and risk.

I implemented the full pipeline in `R/RStudio` with `tidyverse`, `tidyquant`, `quantmod`, `tseries`, `PerformanceAnalytics` (via `tq_performance`), `ggplot2`, `scales`, and `gt`. You can run the numbered scripts individually in order (`1_…`, `2_…`, `3_…`, etc.) to inspect each bank, or execute `Combined_Banks_Analysis.R` which stitches everything together as a single “mother script.”

The analysis uses CSVs exported from https://www.investing.com. Results reflect the specified sample window (September 2020 - August 2025); insights are generated for educational purposes and should be validated on operational data before deployment.

Requirements
- `R ≥ 4.3` and `RStudio`
- Packages: `tidyverse`, `tidyquant`, `quantmod`, `dplyr`, `readr`, `lubridate`, `tseries`, `scales`, `ggplot2`, `gt`.


Data Files You Must Have (Important)

- The R scripts will not run without the files in `Datasets/`.

How to reproduce the analysis

Option A: One-click Run (Mother Script)

1. Ensure `Datasets/Combined Banks Stock Price History.csv` exists in your `RStudio` Working Directory.
2. Open `RStudio`
3. Run `Combined Banks Analysis.R` script This executes all numbered steps that are in the Scripts folder in one go (1…7), all six banks analysis and visualization can be done with a single script this way.

Option B: Run Step-by-step

1. Ensure `Datasets/Brac Bank Stock Price History, City Bank Stock Price History.csv` exists in your `RStudio` Working Directory. Basically put the csv file of those banks you want to analyze.
2. Open `RStudio`
3. Run each scripts individually in chronological order like I labled them `1. BRAC Bank Analysis.R`, 4. Dhaka Bank Analysis.R, 7. Visualization.R etc. This is useful if you don't need/want to do all the banks analysis (e.g. you can choose to do analysis of BRAC Bank but ignore Dhaka Bank). If you decide to not run six banks for analysis than you need to edit the visualization script to your corresponding number of banks (e.g. 4, 5, 8).
