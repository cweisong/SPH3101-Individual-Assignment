# SPH3101 Individual Assignment - Bangladesh Demographic and Health Survey (BDHS) 2022 Analysis

## Project Overview
This project analyzes data from the 2022 Bangladesh Demographic and Health Survey, focusing on child health outcomes (specifically underweight status) and their relationship with various maternal and social determinants of health among children under five years of age.

## Primary Research Question
How are maternal and socio-demographics factors associated with underweight children under five in Bangladesh?

## Repository Structure
```
├── Codes/
│   ├── 0-overall.R                          # Master script to run all analyses
│   ├── 01-data_prep.R                       # Data cleaning and preparation
│   ├── 02-missingness.R                     # Missingness analysis
│   ├── 03-descriptive.R                     # Descriptive statistics
│   ├── 04-regression_prep.R                 # Regression helper functions
│   ├── 04a-logistic_regression.R            # Logistic regression analysis
│   ├── 04b-linear_regression.R              # Linear regression analysis
│   ├── 05-regression_visualisation.R    # Logistic regression plots
├── Data/
│   └── bdhs.csv                             # Raw provided BDHS dataset
└── Plots/                                   # Visualizations
```

## Dependencies
```r
library(car)
library(corrplot)
library(dpylr)
library(finalfit)
library(flextable)
library(forcats)
library(forestplot)
library(ggplot2)
library(grid)
library(gt)
library(magick)
library(tableone)
library(tidyverse)
library(vcd)
```
