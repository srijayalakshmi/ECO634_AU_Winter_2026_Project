# ECO634_AU_Winter_2026_Project
# Impact of COVID-19 on Indian State Capital Expenditure

This repository contains the full analytical pipeline—from raw data extraction to econometric modeling—used to study the impact of the COVID-19 pandemic on state-level fiscal health and capital expenditure in India.

## Repository Structure

### 1. Data Extraction (R Scripts)
These scripts automate the collection of fiscal indicators from PDF reports and web pages:
* `data_extraction_rbi_3.R`: Uses `pdftools` to extract GSDP and population growth data from RBI "State Finances: A Study of Budgets" reports (2018–2025).
* `data_extraction_prs.R`: Scrapes state-wise fiscal indicators (Revenue Expenditure, Capital Expenditure, and Fiscal Deficit) from PRS Legislative Research.
* `data_extraction_prs_debt.R`: Specifically targets and cleans outstanding debt data as a percentage of GSDP across various state budget reports.

### 2. Econometric Analysis (Stata)
* `ECO634_analysis.do`: The core analysis script. Key procedures include:
    * **Data Cleaning**: Standardizing state names and merging RBI GSDP data with PRS fiscal panels.
    * **Feature Engineering**: Creating revenue/expenditure shares and identifying "Actuals" vs. "Budget Estimates."
    * **Regression Analysis**: Fixed-effects models to estimate the impact of COVID-19 using year-wise effects and post-2021 trends.
    * **Visualization**: Generating trend lines for Fiscal Deficit as a % of GSDP for selected states (Maharashtra, Tamil Nadu, UP, etc.).

## Setup & Requirements

### Software
- **R (v4.0+)**: Required libraries: `tidyverse`, `pdftools`, `rvest`, `stringr`.
- **Stata (v16+)**: Required packages: `estout` (for LaTeX table exports).

### Configuration
> **Note on File Paths:** The current scripts contain absolute local paths (e.g., `D:/Srijayalakshmi/...`). 
> To reproduce the results, you must:
> 1. Update the `cd` and `import` paths in `ECO634_analysis.do`.
> 2. Update the `rbi_folder` and `OUT_PATH` variables in the R scripts to point to your local directories.

## Execution Workflow
1. **Extraction**: Run the R scripts to generate the intermediate CSV files (`prs_state_panel.csv`, `prs_state_debt.csv`).
2. **Analysis**: Run the Stata `.do` file to clean the merged panel and generate the results for the final report.

## 📊 Data Sources
- **Reserve Bank of India (RBI)**: State Finances: A Study of Budgets (Editions 2018-19 to 2024-25).
- **PRS Legislative Research**: State Budget Analysis reports.
