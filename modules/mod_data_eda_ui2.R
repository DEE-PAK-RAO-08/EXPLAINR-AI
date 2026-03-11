# =============================================================================
# EXPLAINR-AI: Module 1 Part 2 - EDA Visualizations UI
# =============================================================================

#' EDA Visualizations UI Component
edaVisualizationsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Histogram Section
    box(
      title = span(icon("chart-bar"), " Distribution Analysis (Histograms)"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(4, selectInput(ns("hist_var"), "Select Numeric Variable:", choices = NULL)),
        column(4, sliderInput(ns("hist_bins"), "Number of Bins:", min = 5, max = 50, value = 30)),
        column(4, checkboxInput(ns("hist_density"), "Show Density Curve", value = TRUE))
      ),
      plotlyOutput(ns("histogram_plot"), height = "400px"),
      downloadButton(ns("download_hist"), "Download Plot", class = "btn-sm btn-primary")
    ),
    
    # Boxplot Section
    box(
      title = span(icon("box"), " Outlier Detection (Boxplots)"),
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(6, selectInput(ns("box_var"), "Select Numeric Variable:", choices = NULL)),
        column(6, selectInput(ns("box_group"), "Group By (Optional):", choices = NULL))
      ),
      plotlyOutput(ns("boxplot_plot"), height = "400px"),
      downloadButton(ns("download_box"), "Download Plot", class = "btn-sm btn-warning")
    ),
    
    # Correlation Heatmap
    box(
      title = span(icon("th"), " Correlation Heatmap"),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(6, selectInput(ns("corr_method"), "Correlation Method:",
                            choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"))),
        column(6, checkboxInput(ns("corr_values"), "Show Correlation Values", value = TRUE))
      ),
      plotlyOutput(ns("correlation_heatmap"), height = "500px"),
      downloadButton(ns("download_corr"), "Download Plot", class = "btn-sm btn-info")
    ),
    
    # Bar Charts for Categorical Variables
    box(
      title = span(icon("chart-pie"), " Categorical Variable Distribution"),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(6, selectInput(ns("bar_var"), "Select Categorical Variable:", choices = NULL)),
        column(6, radioButtons(ns("bar_type"), "Chart Type:",
                              choices = c("Bar Chart" = "bar", "Pie Chart" = "pie"), inline = TRUE))
      ),
      plotlyOutput(ns("bar_chart"), height = "400px"),
      downloadButton(ns("download_bar"), "Download Plot", class = "btn-sm btn-success")
    )
  )
}

#' Data Preprocessing UI Component
preprocessingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = span(icon("cogs"), " Data Preprocessing"),
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      
      fluidRow(
        column(4,
          h5(icon("eraser"), "Missing Value Treatment"),
          selectInput(ns("missing_method"), "Method:",
            choices = c("Remove Rows with Missing" = "remove",
                       "Mean Imputation (Numeric)" = "mean",
                       "Median Imputation (Numeric)" = "median",
                       "Mode Imputation (All)" = "mode")),
          actionButton(ns("apply_missing"), "Apply Treatment",
                      icon = icon("check"), class = "btn-warning btn-block")
        ),
        column(4,
          h5(icon("compress-arrows-alt"), "Normalization"),
          selectInput(ns("norm_method"), "Method:",
            choices = c("None" = "none",
                       "Min-Max Scaling (0-1)" = "minmax",
                       "Z-Score Standardization" = "zscore",
                       "Log Transformation" = "log")),
          selectInput(ns("norm_vars"), "Select Variables:", choices = NULL, multiple = TRUE),
          actionButton(ns("apply_norm"), "Apply Normalization",
                      icon = icon("check"), class = "btn-info btn-block")
        ),
        column(4,
          h5(icon("filter"), "Feature Selection"),
          checkboxGroupInput(ns("exclude_vars"), "Exclude Variables:", choices = NULL),
          actionButton(ns("apply_exclude"), "Remove Selected",
                      icon = icon("trash"), class = "btn-danger btn-block")
        )
      ),
      
      hr(),
      
      fluidRow(
        column(12,
          h4(icon("history"), "Preprocessing Log"),
          verbatimTextOutput(ns("preprocess_log")),
          br(),
          actionButton(ns("reset_data"), "Reset to Original Data",
                      icon = icon("undo"), class = "btn-secondary"),
          downloadButton(ns("download_processed"), "Download Processed Data", class = "btn-success")
        )
      )
    )
  )
}
