# =============================================================================
# EXPLAINR-AI: Module 1 - Data Preparation & Exploratory Data Analysis
# =============================================================================
# This module handles:
# - CSV file upload and validation
# - Automatic data type detection
# - Missing value analysis and visualization
# - Summary statistics for numeric and categorical variables
# - Interactive visualizations (histograms, boxplots, correlation heatmap)
# - Data preprocessing (imputation, normalization, scaling)
# =============================================================================

# -----------------------------------------------------------------------------
# UI Components for Data & EDA Module
# -----------------------------------------------------------------------------

#' Data Upload UI Component
dataUploadUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = span(icon("upload"), " Data Upload"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(6,
          fileInput(
            ns("file_upload"),
            label = "Upload CSV File",
            accept = c(".csv", "text/csv"),
            placeholder = "Choose a CSV file...",
            buttonLabel = icon("folder-open")
          ),
          helpText(icon("info-circle"), "Maximum file size: 10MB. Only CSV format supported.")
        ),
        column(6,
          h5("Or use a sample dataset:"),
          actionButton(ns("load_classification"), "Load Classification Sample",
                      icon = icon("tags"), class = "btn-success btn-block"),
          br(), br(),
          actionButton(ns("load_regression"), "Load Regression Sample",
                      icon = icon("chart-line"), class = "btn-info btn-block")
        )
      )
    ),
    
    box(
      title = span(icon("table"), " Data Preview"),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      uiOutput(ns("data_info")),
      br(),
      DTOutput(ns("data_preview"))
    )
  )
}

#' Data Summary UI Component
dataSummaryUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = span(icon("info-circle"), " Dataset Overview"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      fluidRow(
        valueBoxOutput(ns("vb_rows"), width = 3),
        valueBoxOutput(ns("vb_cols"), width = 3),
        valueBoxOutput(ns("vb_numeric"), width = 3),
        valueBoxOutput(ns("vb_categorical"), width = 3)
      )
    ),
    
    box(
      title = span(icon("columns"), " Column Information"),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      DTOutput(ns("column_info"))
    ),
    
    box(
      title = span(icon("question-circle"), " Missing Values Analysis"),
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(6, DTOutput(ns("missing_table"))),
        column(6, plotlyOutput(ns("missing_plot"), height = "350px"))
      )
    )
  )
}

#' Summary Statistics UI Component
summaryStatsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = span(icon("calculator"), " Numeric Variable Statistics"),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      DTOutput(ns("numeric_stats"))
    ),
    
    box(
      title = span(icon("list"), " Categorical Variable Statistics"),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      DTOutput(ns("categorical_stats"))
    )
  )
}
