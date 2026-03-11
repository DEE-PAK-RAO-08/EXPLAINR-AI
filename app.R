# =============================================================================
# EXPLAINR-AI: Main Application Entry Point (app.R)
# =============================================================================
# 
# EXPLAINR-AI: Explainable AI Web Application
# 
# Description:
#   A comprehensive R Shiny web application for:
#   - Data Upload & Exploratory Data Analysis
#   - Machine Learning Model Training
#   - Explainable AI with Global and Local Interpretations
#
# Author: Academic Capstone Project
# Version: 1.0.0
# Year: 2026
#
# Usage:
#   1. Set working directory to this folder
#   2. Run: shiny::runApp()
#   OR
#   3. Source this file: source("app.R")
#
# =============================================================================

# -----------------------------------------------------------------------------
# Set Options
# -----------------------------------------------------------------------------

# Increase upload file size limit
options(shiny.maxRequestSize = 10 * 1024^2)  # 10MB

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Suppress warnings during loading
options(warn = -1)

# -----------------------------------------------------------------------------
# Load Global Configuration
# -----------------------------------------------------------------------------

cat("=== EXPLAINR-AI Initialization ===\n")
cat("Loading global configuration...\n")

source("global.R")

cat("Global configuration loaded successfully!\n\n")

# -----------------------------------------------------------------------------
# Load UI and Server
# -----------------------------------------------------------------------------

cat("Loading UI components...\n")
source("ui.R")

cat("Loading Server logic...\n")
source("server.R")

cat("\n=== EXPLAINR-AI Ready ===\n")
cat("Run shinyApp(ui, server) to start the application.\n\n")

# -----------------------------------------------------------------------------
# Run Application
# -----------------------------------------------------------------------------

# Create and run the Shiny application
shinyApp(ui = ui, server = server)
