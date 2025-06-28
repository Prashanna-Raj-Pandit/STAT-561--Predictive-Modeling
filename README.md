# Predictive Modeling and Visualization
## Overview
This repository contains R scripts and analyses for predictive modeling and data visualization, developed as part of coursework for STAT 561 at SIUE. The projects cover a range of statistical and machine learning techniques applied to diverse datasets, including absenteeism at work, customer churn, patient satisfaction, muscle mass, county demographic information (CDI), flu shot uptake, student performance, and loan approval prediction. The analyses include regression models, regularization techniques, and diagnostic visualizations to explore relationships and predict outcomes.
Topics Covered
The repository encompasses the following topics in predictive modeling and visualization:

**Link to the dataset:** 

*Iranian Churn:* https://archive.ics.uci.edu/dataset/563/iranian+churn+dataset

*Patient satisfaction:*https://github.com/Prashanna-Raj-Pandit/STAT-Predictive-Modeling/blob/main/Linear%20Regression%20Model/pat_stat.txt

*Absenteeism at work:*https://github.com/Prashanna-Raj-Pandit/STAT-Predictive-Modeling/blob/main/Logistic%20Regression/Absenteeism_at_work.xls

*Flu shot:*https://github.com/Prashanna-Raj-Pandit/STAT-Predictive-Modeling/blob/main/Logistic%20Regression/flu%20shot.txt
*Student Performance:* https://github.com/Prashanna-Raj-Pandit/STAT-Predictive-Modeling/blob/main/Model%20diagnostic/Student_Performance.csv

*Loan Data:*https://github.com/Prashanna-Raj-Pandit/STAT-Predictive-Modeling/blob/main/Model%20diagnostic/loan_data.csv

*muscle mass:* https://github.com/Prashanna-Raj-Pandit/STAT-Predictive-Modeling/blob/main/Linear%20Regression%20Model/muscle_mass.txt


## Iranian Churn Analysis (Tableau):

<img width="1396" alt="project1" src="https://github.com/user-attachments/assets/5537771c-f775-4474-b77b-18f9b34d50bd" />

<img width="1396" alt="project3" src="https://github.com/user-attachments/assets/a9d2f116-e819-44f8-9dc1-9bd3cf81ead9" />


## Data Preprocessing:

* Handling missing values (e.g., imputation in loan data)
* Converting variables to factors for categorical analysis
* Data splitting into training, test, and holdout sets
* Recoding variables (e.g., absenteeism categories)


## Exploratory Data Analysis (EDA):

Histograms, boxplots, and scatter plots for distribution and relationship analysis Correlation matrices to assess relationships between numeric variables Contingency tables and bar plots for categorical variables


## Regression Models:

Multiple Linear Regression: Predicting continuous outcomes (e.g., student performance, patient satisfaction)
Poisson Regression: Modeling count data (e.g., frequency of use in customer churn)
Negative Binomial Regression: Addressing overdispersion in count data
Logistic Regression: Binary classification (e.g., loan approval, flu shot uptake)
Multinomial Logistic Regression: Multi-class classification (e.g., absenteeism categories)


## Regularization Techniques:

Lasso Regression: Feature selection and shrinkage (e.g., absenteeism, customer churn)
Ridge Regression: Handling multicollinearity (e.g., absenteeism, customer churn)
Elastic Net Regression: Combining Lasso and Ridge with tuned alpha and lambda parameters


## Model Selection:

Forward and backward stepwise selection for variable selection
Cross-validation (e.g., 5-fold, 10-fold) to optimize model parameters


## Model Diagnostics:

Residual plots (vs. fitted values, predictors, observation order) to check linearity, homoscedasticity, and independence
Q-Q plots and histograms for normality of residuals
Variance Inflation Factor (VIF) for multicollinearity checks
Overdispersion checks for Poisson models


## Model Evaluation:

Metrics: RMSE, R-squared, accuracy, sensitivity, specificity, AUC, and confusion matrices
ROC curves for binary classification models
Comparison of model performance across different techniques


## Visualization:

Scatter plot matrices (pairs plots)
Residual diagnostic plots
Bar plots and stacked bar plots for categorical data
Regression line and curve fitting (e.g., linear, quadratic, cubic models for muscle mass)


## Applications:

Predicting absenteeism time and categories in workplace data
Modeling customer churn and frequency of use in telecom data
Analyzing patient satisfaction based on age, severity, and anxiety
Studying muscle mass decline with age using polynomial regression
Predicting physician counts based on demographic and regional factors
Modeling flu shot uptake based on age, awareness, and sex
Predicting student performance using academic preparation factors
Predicting loan approval based on applicant characteristics



## Repository Structure

**LinearRegression.R:**  Linear regression models for patient satisfaction, muscle mass (with polynomial terms), and physician counts, including forward/backward selection.
**logistics_regression.R:** Multinomial logistic regression for absenteeism categories and binary logistic regression for flu shot uptake, with EDA and model selection.
**Model diagnostics.pdf:** Analysis of student performance (linear regression) and loan approval (logistic regression), with detailed model diagnostics and proposed interventions.
**Regularization.R:** Analysis of absenteeism data using Lasso, Ridge, and Elastic Net regression for continuous outcomes and Lasso for multinomial classification.
**Project.R:** Customer churn analysis with Poisson, Negative Binomial, Ridge, and Lasso regression, plus logistic regression for complaint prediction with SMOTE for class imbalance.

## Installation
To run the scripts in this repository, ensure you have R installed along with the required packages. Follow these steps:

## Install R from CRAN.
Install the necessary R packages by running the following commands in R:

install.packages(c("dplyr", "readxl", "ggplot2", "corrplot", "nnet", "caret", "MASS", "pROC", "car", "glmnet", "lattice", "DMwR2", "themis", "recipes"))


## Usage
Each script is self-contained and can be run independently in R or RStudio. Ensure the required datasets (e.g., Absenteeism_at_work.xls, Customer Churn.csv, pat_stat.txt, muscle_mass.txt, cdi.txt, flu shot.txt, Student_Performance.csv, loan_data.csv) are in the working directory or update the file paths in the scripts.
To run a script:

## Open the script in R or RStudio (e.g., HW4.R).
Ensure the required dataset is available.
Run the script to perform the analysis and generate outputs (e.g., model summaries, plots, performance metrics).

Example:
source("HW4.R")

This will load the absenteeism dataset, preprocess it, fit regression models, and output test MSEs and classification errors.
Datasets
The analyses use the following datasets:

Absenteeism_at_work.xls: Workplace absenteeism data with variables like hours absent, age, distance, and education.
Customer Churn.csv: Telecom customer data with usage, demographic, and churn information.
pat_stat.txt: Patient satisfaction data with age, severity, and anxiety.
muscle_mass.txt: Muscle mass data with age and mass measurements.
cdi.txt: County demographic data with population, income, and physician counts.
flu shot.txt: Flu shot uptake data with age, awareness, and sex.
Student_Performance.csv: Student performance data with study hours, scores, and other factors.
loan_data.csv: Loan approval data with applicant income, credit history, and more.

Note: Datasets are not included in this repository due to potential size or privacy constraints. Ensure you have access to these files or similar datasets to replicate the analyses.
Example Outputs

Absenteeism Analysis (HW4.R): Compares Lasso, Ridge, and Elastic Net models for predicting absenteeism hours (Test MSEs) and Lasso for classifying absenteeism categories (classification error).
Customer Churn (Project.R): Compares Poisson, Negative Binomial, Ridge, and Lasso models for frequency of use (RMSE) and logistic regression for complaints (AUC, confusion matrix).
Patient Satisfaction (LinearRegression.R): Linear regression model with forward/backward selection, predicting satisfaction based on age, severity, and anxiety.
Muscle Mass (LinearRegression.R): Linear, quadratic, and cubic models for muscle mass vs. age, with visualization of regression fits.
Flu Shot (logistics_regression.R): Logistic regression for flu shot uptake, with forward/backward selection and predicted probabilities.
Student Performance (Model diagnostics.pdf): Linear regression with cross-validation, achieving an R-squared of 0.9893 and RMSE of 1.99, with residual diagnostics confirming model assumptions.
Loan Approval (Model diagnostics.pdf): Logistic regression with 85.53% accuracy, 100% sensitivity, and 50% specificity, with ROC curve analysis.

Contributing
Contributions are welcome! If you have suggestions for improving the analyses, adding new models, or enhancing visualizations, please:

**Reference:**
Iranian Churn [Dataset]. (2020). UCI Machine Learning Repository. https://doi.org/10.24432/C5JW3Z.
