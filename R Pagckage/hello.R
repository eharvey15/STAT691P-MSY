# Load required libraries
library(testthat)
library("glmnet")
library(ggplot2)

context("Testing the glmnet functions")

test_that("fit_lasso function returns a valid model", {
  # Example data for testing
  set.seed(123)
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  alpha <- 1  # Lasso regression
  lasso_model <- fit_lasso(x, y, alpha)
  expect_true(is(lasso_model, "glmnet"))
})

test_that("fit_ridge function returns a valid model", {
  # Example data for testing
  set.seed(123)
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  lambda <- 0.1  # Ridge regression
  ridge_model <- fit_ridge(x, y, lambda)
  expect_true(is(ridge_model, "glmnet"))
})

test_that("visualize_coefficient_paths_lasso function produces a plot", {
  # Example data for testing
  set.seed(123)
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  alpha <- 1  # Lasso regression
  lasso_model <- fit_lasso(x, y, alpha)
  # Create a temporary file to save the plot
  tmp_file <- tempfile(fileext = ".png")
  # Save the plot to the temporary file
  ggsave(tmp_file, visualize_coefficient_paths_lasso(lasso_model))
  # Check if the file is created
  expect_true(file.exists(tmp_file))
})

test_that("visualize_coefficient_paths_ridge function produces a plot", {
  # Example data for testing
  set.seed(123)
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  alpha <- 0.01  # Ridge regression
  lambda_seq <- 10^seq(10, -2, length = 60)  # Example lambda sequence
  ridge_model <- fit_ridge(x, y, alpha, lambda_seq)
  # Create a temporary file to save the plot
  tmp_file <- tempfile(fileext = ".png")
  # Save the plot to the temporary file
  ggsave(tmp_file, visualize_coefficient_paths_ridge(ridge_model))
  # Check if the file is created
  expect_true(file.exists(tmp_file))
})

test_that("cross_validation_plot_lasso function produces a plot", {
  # Example data for testing
  set.seed(123)
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  alpha <- 1  # Lasso regression
  lasso_model <- fit_lasso(x, y, alpha)
  # Create a temporary file to save the plot
  tmp_file <- tempfile(fileext = ".png")
  # Save the plot to the temporary file
  ggsave(tmp_file, cross_validation_plot_lasso(lasso_model))
  # Check if the file is created
  expect_true(file.exists(tmp_file))
})

test_that("cross_validation_plot_ridge function produces a plot", {
  # Example data for testing
  set.seed(123)
  n <- 100
  p <- 5
  x <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  alpha <- 0.1  # Ridge regression
  lambda_seq <- 10^seq(10, -2, length = 60)  # Example lambda sequence
  ridge_model <- fit_ridge(x, y, alpha, lambda_seq)
  # Create a temporary file to save the plot
  tmp_file <- tempfile(fileext = ".png")
  # Save the plot to the temporary file
  ggsave(tmp_file, cross_validation_plot_ridge(ridge_model))
  # Check if the file is created
  expect_true(file.exists(tmp_file))
})
