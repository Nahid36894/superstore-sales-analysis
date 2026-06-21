# Superstore Sales Analysis

An end-to-end data analysis project exploring 9,994 retail orders to identify the key drivers of sales and profitability — built entirely in R.

[Live Interactive Dashboard](https://mahedihasan97.shinyapps.io/superstore_app/)

---

## Overview

This project analyzes the Sample Superstore dataset (2015–2018) to answer a central business question: *where is the company making money, where is it losing money, and why?*

The workflow covers the full data analysis pipeline — cleaning, exploratory analysis, statistical hypothesis testing, regression modeling, and an interactive dashboard for stakeholders to explore the findings themselves.

## Key Findings

- **Regional gap**: The West region is the most profitable (avg. profit $33.80/order); the Central region is the least profitable ($17.10/order) despite having the second-highest sales volume — driven by a discount rate more than double the West's (24.0% vs 10.9%).
- **Category gap:** Furniture generates nearly as much revenue as Technology ($742K vs $836K) but only 13% of the profit ($18K vs $145K).
- **Root cause:** Drilling into sub-categories revealed Tables and Bookcases are the only net loss-making product lines, losing $17,725 and $3,473 respectively — almost entirely due to discounts above ~20%.
- **Statistical confirmation:** A significant negative correlation exists between discount and profit (r = −0.22, p < 0.001). A multiple linear regression (R² = 0.27) confirms discount as the strongest negative predictor of profit among Sales, Discount, and Quantity.
- **Growth:** Sales grew 51% and profit grew 89% from 2015 to 2018, with consistent Q4 seasonality each year.

## Tools & Methods

- **R** — dplyr, ggplot2, Shiny, shinythemes
- **Statistical methods** — descriptive statistics, Pearson correlation, one-way ANOVA, multiple linear regression
- **Data cleaning** — handling missing values, duplicate removal, type conversion, logical consistency checks

## Repository Contents

| File | Description |
| `app.R` | Interactive Shiny dashboard (6 tabs: Overview, Region, Category, Discount Impact, Segment, Time Trend) |

## Dashboard Preview

The dashboard lets users filter by region, category, and sub-category to explore the data interactively, with live statistical test outputs (ANOVA, correlation, regression) included alongside the visualizations.

**[→ Try it live](https://mahedihasan97.shinyapps.io/superstore_app/)**

## Data Source

[Sample Superstore dataset](https://github.com/leonism/sample-superstore), a widely-used retail dataset for analytics practice.

## Limitations

- This is observational data — the discount–profit relationship is a strong association, not proven causation.
- The regional ANOVA result (p = 0.0489) is only marginally significant.
- The regression model explains 27% of profit variation; other unmeasured factors (shipping cost, supplier cost) likely play a role.
