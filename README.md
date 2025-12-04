# Quantitative Market Risk Analysis: PKO BP

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Finance](https://img.shields.io/badge/Finance-VaR%20%26%20ES-green?style=for-the-badge)


##  Project Overview

This project performs a quantitative analysis of market risk for **PKO BP S.A.**, one of Poland's largest financial institutions. Using daily stock data from **2016 to 2025**, the study estimates potential investment losses using **Value at Risk (VaR)** and **Expected Shortfall (ES)**.

The analysis focuses on comparing the effectiveness of standard Historical Simulation against Weighted Historical Simulation, particularly during periods of high volatility (e.g., the COVID-19 pandemic and the 2022 geopolitical instability).

## Objectives

* **Data Analysis:** Examination of daily closing prices, simple returns, and logarithmic returns.
* **Risk Estimation:** Calculation of 99% VaR and 99% ES using a 250-day rolling window.
* **Methodology Comparison:** * Standard **Historical Simulation**.
    * **Weighted Historical Simulation** (using a decay factor $q=0.995$).
* **Backtesting:** Verification of model accuracy using:
    * **Kupiec Test** (Proportion of Failures).
    * **Christofferson Test** (Independence of Failures).

##  Methodology

### 1. Data Preprocessing
The analysis is based on daily OHLC data. Returns are calculated as:
* Simple Returns: $R_t = \frac{P_t}{P_{t-1}} - 1$
* Logarithmic Returns: $r_t = \ln\left(\frac{P_t}{P_{t-1}}\right)$

### 2. VaR & ES Models
* **Historical Method:** Treats all past observations in the window ($N=250$) equally.
* **Weighted Historical Method:** Assigns weights to observations based on recency, allowing the model to react faster to changing market conditions.
    * Weight formula: $p_j = \frac{q^{n - j} (1 - q)}{1 - q^n}$

### 3. Backtesting
The models were validated to ensure statistical significance:
* **Kupiec Test:** Checks if the frequency of exceptions matches the expected confidence level (1%).
* **Christofferson Test:** Checks if the exceptions are independent (i.e., they do not cluster).

##  Key Findings

* **Volatility:** The analysis highlighted significant volatility clusters during early 2020 (COVID-19) and 2022 (War in Ukraine).
* **Model Performance:** The **Weighted Historical Simulation** proved superior to the standard method. It adapted more quickly to market shocks, producing fewer clustered exceptions.
* **Statistical Validity:** Both models passed the Kupiec test, but the Weighted method showed much stronger independence in the Christofferson test ($p \approx 0.37$ vs $p \approx 0.059$).

## Tech Stack

* **Language:** R (RMarkdown)
* **Libraries:**
    * `tidyverse` (Data manipulation and visualization)
    * `moments` (Statistical moments: skewness, kurtosis)
    * `psych` (Descriptive statistics)
    * `kableExtra` (HTML table formatting)

## File Structure

* `projekt.Rmd` – Main R Markdown source file containing the full analysis and calculations.
* `projekt.html` – Rendered HTML report with tables and charts (requires downloading to view).
* `pko_d.csv` – Historical price data (OHLCV) for PKO BP used in the analysis.
* `projekt_md.md` – Rendered Markdown report with tables and charts (viewable directly on GitHub).
* `projekt_md_files/figure-gfm` – Folder with PNG plots needed for correct rendering of `projekt_md.md` on GitHub.

