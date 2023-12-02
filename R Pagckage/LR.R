# Load required libraries
library("glmnet")
library("ggplot2")
library("reshape2")

# Function to fit Lasso model
fit_lasso <- function(x, y, alpha) {
  lasso_model <- glmnet(x, y, alpha = alpha, family = "gaussian")
  return(lasso_model)
}

# Define fit_ridge function
fit_ridge <- function(x, y, alpha, lambda_seq) {
  model <- glmnet(x, y, alpha = alpha, lambda = lambda_seq)
  return(model)
}

# Function to visualize Lasso coefficient paths
visualize_coefficient_paths_lasso <- function(lasso_model) {
  plot(lasso_model, xvar = "lambda", label = TRUE)
}

# Function to visualize Ridge coefficient paths
visualize_coefficient_paths_ridge <- function(ridge_model) {
  plot(ridge_model, xvar = "lambda", label = TRUE)
}

# Function to plot cross-validation results for Lasso
cross_validation_plot_lasso <- function(lasso_model) {
  plot(lasso_model)
}

# Function to plot cross-validation results for Ridge
cross_validation_plot_ridge <- function(ridge_model) {
  plot(ridge_model)
}


