# Aarhus Univesity | Machine Learning Exam | January 2025

This repository contains my exam solutions for the Machine Learning for Business Intelligence course. The exam demonstrated proficiency in both classification and regression tasks using R.

## Methods covered

### Problem 1: Classification Task
- Data preprocessing and cleaning
- Feature engineering (including factor level handling)
- Implementation of multiple classification models:
  - Logistic Regression
  - K-Nearest Neighbors (KNN)
  - Linear Discriminant Analysis (LDA)
- Model evaluation using metrics like ROC, Sensitivity, and Specificity
- Creation of a stacked model combining predictions from multiple models
- Feature importance analysis
- Marketing segmentation analysis based on model insights

### Problem 2: Regression Task
- Data preprocessing and handling missing values
- Exploratory Data Analysis (EDA) with ggplot2:
  - Histogram analysis
  - Box plot analysis
- Implementation of multiple regression models:
  - Baseline intercept-only model
  - Multiple Linear Regression
  - Ridge Regression with cross-validation
  - Lasso Regression with cross-validation
  - Model averaging using median-based predictions
- Model evaluation using RMSE and MSE metrics
- Feature selection using Lasso
- Model comparison and interpretation

### Technical Skills Demonstrated
- R programming
- Data manipulation
- Statistical modeling
- Cross-validation techniques
- Visualization with ggplot2
- Model evaluation and comparison
- Handling imbalanced data
- Feature selection and dimensionality reduction

## Libraries Used
```R
library(DataExplorer)
library(rsample)
library(recipes)
library(caret)
library(ggplot2)
library(glmnet)
library(Metrics)
```

## Data
The analysis was performed on a dataset containing socio-demographic and economic factors, with two main tasks:
1. Predicting online food purchasing behavior (classification)
2. Predicting purchase values (regression)
