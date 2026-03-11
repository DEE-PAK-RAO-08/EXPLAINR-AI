# =============================================================================
# EXPLAINR-AI: Package Installation & Setup Script
# =============================================================================
#
# Run this script ONCE before launching the application to ensure
# all required packages are installed.
#
# Usage: source("setup.R")
#
# =============================================================================

cat("
================================================================================
                        EXPLAINR-AI Setup Script
================================================================================

This script will install all required R packages for EXPLAINR-AI.
Please wait while packages are being checked and installed...

")

# -----------------------------------------------------------------------------
# Required Packages List
# -----------------------------------------------------------------------------

required_packages <- c(
  # Shiny Framework
  "shiny",           # Core Shiny
  "shinydashboard",  # Dashboard layout
  "shinyWidgets",    # Additional widgets
  "shinyjs",         # JavaScript operations
  "shinyBS",         # Bootstrap modals & tooltips
  
  # Data Manipulation
  "tidyverse",       # Core data science packages
  "dplyr",           # Data manipulation
  "tidyr",           # Data tidying
  "readr",           # Data reading
  
  # Visualization
  "ggplot2",         # Static plotting
  "plotly",          # Interactive plotting
  "corrplot",        # Correlation matrices
  "scales",          # Axis scaling
  "reshape2",        # Data reshaping for heatmaps
  "RColorBrewer",    # Color palettes
  
  # Tables
  "DT",              # Interactive data tables
  
  # Machine Learning
  "caret",           # ML training framework
  "rpart",           # Decision trees
  "rpart.plot",      # Decision tree visualization
  "randomForest",    # Random forest
  "e1071",           # SVM, Naive Bayes
  
  # Explainable AI
  "DALEX",           # Model explanation framework
  "iml",             # Interpretable ML
  "lime",            # Local explanations
  
  # Metrics
  "pROC",            # ROC curves
  "Metrics"          # Regression metrics
)

# Optional packages (install if needed for SHAP visualizations)
optional_packages <- c(
  "shapviz",      # SHAP visualization (may require additional dependencies)
  "kernelshap"   # Kernel SHAP
)

# -----------------------------------------------------------------------------
# Installation Function
# -----------------------------------------------------------------------------

install_if_missing <- function(packages, optional = FALSE) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(new_packages) > 0) {
    cat(paste("\nInstalling", length(new_packages), "packages:\n"))
    cat(paste("  -", new_packages, collapse = "\n"), "\n\n")
    
    for (pkg in new_packages) {
      tryCatch({
        cat(paste("Installing:", pkg, "... "))
        install.packages(pkg, dependencies = TRUE, quiet = TRUE)
        cat("Done!\n")
      }, error = function(e) {
        if (optional) {
          cat(paste("Skipped (optional):", e$message, "\n"))
        } else {
          cat(paste("ERROR:", e$message, "\n"))
        }
      })
    }
  } else {
    cat("\nAll required packages are already installed!\n")
  }
  
  return(length(new_packages))
}

# -----------------------------------------------------------------------------
# Run Installation
# -----------------------------------------------------------------------------

cat("Checking required packages...\n")
installed_count <- install_if_missing(required_packages)

cat("\nChecking optional packages (for advanced SHAP features)...\n")
install_if_missing(optional_packages, optional = TRUE)

# -----------------------------------------------------------------------------
# Verify Installation
# -----------------------------------------------------------------------------

cat("\n\nVerifying installation...\n")
cat("================================================================================\n")

verification_results <- data.frame(
  Package = required_packages,
  Installed = sapply(required_packages, function(x) x %in% installed.packages()[, "Package"]),
  stringsAsFactors = FALSE
)

# Display results
for (i in 1:nrow(verification_results)) {
  status <- if(verification_results$Installed[i]) "✓" else "✗"
  color <- if(verification_results$Installed[i]) "" else " <- MISSING"
  cat(paste(status, verification_results$Package[i], color, "\n"))
}

# Summary
n_installed <- sum(verification_results$Installed)
n_total <- nrow(verification_results)

cat("\n================================================================================\n")
cat(paste("Installation Summary:", n_installed, "/", n_total, "packages installed\n"))

if (n_installed == n_total) {
  cat("\n✓ All packages installed successfully!")
  cat("\n✓ EXPLAINR-AI is ready to run!")
  cat("\n\nTo start the application, run:")
  cat("\n  source('app.R')")
  cat("\n  OR")
  cat("\n  shiny::runApp()")
} else {
  cat("\n✗ Some packages failed to install.")
  cat("\nPlease install missing packages manually and try again.")
}

cat("\n================================================================================\n\n")

# -----------------------------------------------------------------------------
# Optional: Load packages to verify they work
# -----------------------------------------------------------------------------

cat("Testing package loading (this may take a moment)...\n")

test_result <- tryCatch({
  suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
    library(ggplot2)
    library(plotly)
    library(caret)
    library(DALEX)
  })
  cat("✓ Core packages load successfully!\n")
  TRUE
}, error = function(e) {
  cat(paste("✗ Error loading packages:", e$message, "\n"))
  FALSE
})

if (test_result) {
  cat("\n================================================================================")
  cat("\n                    SETUP COMPLETE - READY TO LAUNCH")
  cat("\n================================================================================\n")
}
