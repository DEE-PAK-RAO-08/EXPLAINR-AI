# =============================================================================
# EXPLAINR-AI: Explainable AI Web Application
# Global Configuration and Package Loading
# =============================================================================
# Setup local library path if it exists
if (dir.exists("c:/Users/LENOVO/Downloads/EXPLAINR-AI/r_library")) {
  .libPaths(c("c:/Users/LENOVO/Downloads/EXPLAINR-AI/r_library", .libPaths()))
}
# Author: EXPLAINR-AI Development Team
# Purpose: Academic Capstone Project - Explainable Machine Learning
# Technology: R Shiny with tidyverse, caret, DALEX, iml, lime, shapviz
# =============================================================================

# -----------------------------------------------------------------------------
# Package Installation Helper
# -----------------------------------------------------------------------------
# Run this section once to install all required packages:
# 
# install.packages(c(
#   "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "shinyBS",
#   "tidyverse", "dplyr", "tidyr", "ggplot2", "plotly",
#   "DT", "scales", "corrplot", "reshape2",
#   "caret", "rpart", "rpart.plot", "randomForest", "e1071",
#   "DALEX", "iml", "lime", "shapviz",
#   "pROC", "Metrics"
# ))

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------

# Shiny Framework
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyjs)
  library(shinyBS)
})

# Data Manipulation & Visualization
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(scales)
  library(corrplot)
  library(reshape2)
})

# Machine Learning
suppressPackageStartupMessages({
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(randomForest)
  library(e1071)
})

# Explainable AI
suppressPackageStartupMessages({
  library(DALEX)
  library(iml)
  library(lime)
  library(shapviz)
})

# Metrics & Evaluation
suppressPackageStartupMessages({
  library(pROC)
  library(Metrics)
})

# -----------------------------------------------------------------------------
# Global Theme Configuration
# -----------------------------------------------------------------------------

# Custom ggplot2 theme for consistent visualization
theme_explainr <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, color = "#2c3e50"),
      plot.subtitle = element_text(hjust = 0.5, color = "#7f8c8d"),
      axis.title = element_text(face = "bold", color = "#34495e"),
      axis.text = element_text(color = "#2c3e50"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#ecf0f1"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Set as default theme
theme_set(theme_explainr())

# Custom color palettes
explainr_colors <- list(
  primary = "#3498db",
  secondary = "#2ecc71",
  warning = "#f39c12",
  danger = "#e74c3c",
  info = "#9b59b6",
  dark = "#2c3e50",
  light = "#ecf0f1",
  gradient = c("#3498db", "#2ecc71", "#f39c12", "#e74c3c", "#9b59b6"),
  heatmap = colorRampPalette(c("#3498db", "#ecf0f1", "#e74c3c"))(100)
)

# -----------------------------------------------------------------------------
# Global Utility Functions
# -----------------------------------------------------------------------------

#' Safe data type detection
#' @param x A vector to analyze
#' @return Character string indicating data type
detect_data_type <- function(x) {
  if (is.numeric(x)) {
    if (length(unique(x)) <= 10) return("numeric_discrete")
    return("numeric_continuous")
  } else if (is.factor(x) || is.character(x)) {
    return("categorical")
  } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return("datetime")
  } else if (is.logical(x)) {
    return("logical")
  }
  return("unknown")
}

#' Calculate missing value statistics
#' @param df A data frame
#' @return Data frame with missing value statistics
calculate_missing_stats <- function(df) {
  data.frame(
    Variable = names(df),
    Missing_Count = sapply(df, function(x) sum(is.na(x))),
    Missing_Percent = sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 2)),
    Data_Type = sapply(df, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(Missing_Percent))
}

#' Detect problem type based on target variable
#' @param target_vector The target variable vector
#' @return Character string: "classification" or "regression"
detect_problem_type <- function(target_vector) {
  if (is.factor(target_vector) || is.character(target_vector)) {
    return("classification")
  }
  n_unique <- length(unique(na.omit(target_vector)))
  if (n_unique <= 10 || n_unique / length(target_vector) < 0.05) {
    return("classification")
  }
  return("regression")
}

#' Safe model prediction wrapper
#' @param model Trained model object
#' @param newdata Data for prediction
#' @param type Prediction type
#' @return Predictions or NULL on error
safe_predict <- function(model, newdata, type = "raw") {
  tryCatch({
    predict(model, newdata = newdata, type = type)
  }, error = function(e) {
    warning(paste("Prediction error:", e$message))
    return(NULL)
  })
}

#' Format numeric values for display
#' @param x Numeric value
#' @param digits Number of decimal places
#' @return Formatted string
format_metric <- function(x, digits = 4) {
  if (is.na(x) || is.null(x)) return("N/A")
  formatC(x, digits = digits, format = "f")
}

# -----------------------------------------------------------------------------
# Application Constants
# -----------------------------------------------------------------------------

# Maximum file size (10MB)
MAX_FILE_SIZE <- 10 * 1024^2

# Supported file types
SUPPORTED_FILE_TYPES <- c("text/csv", "application/vnd.ms-excel", ".csv")

# Model configurations
MODEL_TYPES <- list(
  classification = c(
    "Logistic Regression" = "glm",
    "Decision Tree" = "rpart",
    "Random Forest" = "rf",
    "Naive Bayes" = "naive_bayes"
  ),
  regression = c(
    "Linear Regression" = "lm",
    "Decision Tree" = "rpart",
    "Random Forest" = "rf",
    "Support Vector Regression" = "svmRadial"
  )
)

# Default train-test split ratio
DEFAULT_SPLIT_RATIO <- 0.7

# Maximum dataset size for performance
MAX_ROWS <- 10000

# -----------------------------------------------------------------------------
# Sample Dataset Generator (for demo purposes)
# -----------------------------------------------------------------------------

#' Generate sample classification dataset
#' @param n Number of observations
#' @return Data frame with sample data
generate_sample_classification <- function(n = 500) {
  set.seed(42)
  data.frame(
    Age = round(runif(n, 18, 70)),
    Income = round(runif(n, 20000, 150000)),
    Education_Years = round(runif(n, 8, 20)),
    Experience = round(runif(n, 0, 40)),
    Credit_Score = round(runif(n, 300, 850)),
    Loan_Amount = round(runif(n, 1000, 50000)),
    Employment_Type = sample(c("Full-time", "Part-time", "Self-employed"), n, replace = TRUE),
    Approved = factor(sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.6, 0.4)))
  )
}

#' Generate sample regression dataset
#' @param n Number of observations
#' @return Data frame with sample data
generate_sample_regression <- function(n = 500) {
  set.seed(42)
  data.frame(
    Size_sqft = round(runif(n, 500, 5000)),
    Bedrooms = sample(1:6, n, replace = TRUE),
    Bathrooms = sample(1:4, n, replace = TRUE),
    Age_Years = round(runif(n, 0, 50)),
    Distance_Downtown = round(runif(n, 0.5, 30), 1),
    Neighborhood = sample(c("Urban", "Suburban", "Rural"), n, replace = TRUE),
    Has_Garage = sample(c("Yes", "No"), n, replace = TRUE),
    Price = round(runif(n, 100000, 1000000))
  )
}

# -----------------------------------------------------------------------------
# Application Information
# -----------------------------------------------------------------------------

APP_INFO <- list(
  name = "EXPLAINR-AI",
  version = "1.0.0",
  description = "Explainable AI Web Application for Machine Learning Interpretation",
  author = "Academic Capstone Project",
  year = 2026
)

cat("✓ EXPLAINR-AI Global Configuration Loaded Successfully\n")
cat(paste("  Version:", APP_INFO$version, "\n"))
cat(paste("  Packages loaded:", length(sessionInfo()$otherPkgs), "\n"))
