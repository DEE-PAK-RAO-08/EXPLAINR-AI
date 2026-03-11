# =============================================================================
# EXPLAINR-AI: User Interface (ui.R)
# =============================================================================
# Shiny Dashboard UI with sidebar navigation for all three modules
# =============================================================================

# Source module UI files
source("modules/mod_data_eda.R")
source("modules/mod_data_eda_ui2.R")
source("modules/mod_ml_model.R")
source("modules/mod_xai.R")

# -----------------------------------------------------------------------------
# Dashboard Header
# -----------------------------------------------------------------------------

header <- dashboardHeader(
  title = tagList(
    tags$img(src = "logo.png", height = "32px", style = "margin-right: 12px; vertical-align: middle;"),
    tags$span("EXPLAINR-AI", style = "font-weight: 700; letter-spacing: 0.8px; vertical-align: middle; color: white;")
  ),
  titleWidth = 280,
  
  # Right-side dropdown menus
  dropdownMenu(
    type = "notifications",
    headerText = "App Information",
    icon = icon("info-circle"),
    badgeStatus = NULL,
    notificationItem(
      text = "Version 1.0.0",
      icon = icon("tag"),
      status = "info"
    ),
    notificationItem(
      text = "Academic Capstone Project",
      icon = icon("graduation-cap"),
      status = "success"
    )
  )
)

# -----------------------------------------------------------------------------
# Dashboard Sidebar
# -----------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 280,
  
  sidebarMenu(
    id = "sidebar_menu",
    
    # App branding
    div(
      style = "padding: 20px; text-align: center; border-bottom: 1px solid #444;",
      h4(style = "color: #3498db; margin: 0;", "Explainable AI Platform"),
      p(style = "color: #888; font-size: 12px; margin-top: 5px;", 
        "Machine Learning Interpretation")
    ),
    
    br(),
    
    # Module 1: Data & EDA
    menuItem(
      "Data Upload & EDA",
      icon = icon("database"),
      
      menuSubItem("Upload Data", tabName = "data_upload", icon = icon("upload")),
      menuSubItem("Dataset Overview", tabName = "data_summary", icon = icon("info-circle")),
      menuSubItem("Summary Statistics", tabName = "summary_stats", icon = icon("calculator")),
      menuSubItem("Visualizations", tabName = "eda_viz", icon = icon("chart-bar")),
      menuSubItem("Preprocessing", tabName = "preprocessing", icon = icon("cogs"))
    ),
    
    # Module 2: ML Model
    menuItem(
      "Model Training",
      icon = icon("brain"),
      
      menuSubItem("Configure & Train", tabName = "model_config", icon = icon("sliders-h")),
      menuSubItem("Performance Metrics", tabName = "model_metrics", icon = icon("chart-line"))
    ),
    
    # Module 3: Explainable AI
    menuItem(
      "Explainable AI",
      icon = icon("lightbulb"),
      
      menuSubItem("Global Explanations", tabName = "xai_global", icon = icon("globe")),
      menuSubItem("Local Explanations", tabName = "xai_local", icon = icon("user"))
    ),
    
    hr(),
    
    # Help & Documentation
    menuItem(
      "Help & Guide",
      tabName = "help",
      icon = icon("question-circle"),
      badgeLabel = "?",
      badgeColor = "purple"
    ),
    
    # About
    menuItem(
      "About",
      tabName = "about",
      icon = icon("info"),
      badgeLabel = "i",
      badgeColor = "teal"
    )
  ),
  
  # Footer info
  div(
    style = "position: absolute; bottom: 10px; width: 100%; text-align: center; color: #666; font-size: 11px;",
    "© 2026 EXPLAINR-AI",
    br(),
    "Academic Capstone Project"
  )
)

# -----------------------------------------------------------------------------
# Dashboard Body
# -----------------------------------------------------------------------------

body <- dashboardBody(
  # Custom CSS and Favicon for enhanced styling
  tags$head(
    tags$link(rel = "shortcut icon", href = "logo.png"),
    tags$style(HTML("
      /* Custom Theme Styling */
      .skin-blue .main-header .logo {
        background-color: #2c3e50;
        font-weight: bold;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #34495e;
      }
      .skin-blue .main-header .navbar {
        background-color: #2c3e50;
      }
      .skin-blue .main-sidebar {
        background-color: #1a252f;
      }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a {
        background-color: #3498db;
        border-left-color: #3498db;
      }
      
      /* Box styling */
      .box.box-primary {
        border-top-color: #3498db;
      }
      .box.box-success {
        border-top-color: #27ae60;
      }
      .box.box-warning {
        border-top-color: #f39c12;
      }
      .box.box-danger {
        border-top-color: #e74c3c;
      }
      .box.box-info {
        border-top-color: #9b59b6;
      }
      
      /* Value box colors */
      .small-box.bg-blue {
        background-color: #3498db !important;
      }
      .small-box.bg-green {
        background-color: #27ae60 !important;
      }
      .small-box.bg-yellow {
        background-color: #f39c12 !important;
      }
      .small-box.bg-purple {
        background-color: #9b59b6 !important;
      }
      .small-box.bg-red {
        background-color: #e74c3c !important;
      }
      
      /* Button styling */
      .btn-primary {
        background-color: #3498db;
        border-color: #2980b9;
      }
      .btn-success {
        background-color: #27ae60;
        border-color: #229954;
      }
      .btn-warning {
        background-color: #f39c12;
        border-color: #d68910;
      }
      .btn-danger {
        background-color: #e74c3c;
        border-color: #c0392b;
      }
      .btn-info {
        background-color: #9b59b6;
        border-color: #8e44ad;
      }
      
      /* Alert styling */
      .alert-success {
        background-color: #d4edda;
        border-color: #c3e6cb;
        color: #155724;
      }
      .alert-info {
        background-color: #d1ecf1;
        border-color: #bee5eb;
        color: #0c5460;
      }
      .alert-warning {
        background-color: #fff3cd;
        border-color: #ffeeba;
        color: #856404;
      }
      .alert-danger {
        background-color: #f8d7da;
        border-color: #f5c6cb;
        color: #721c24;
      }
      
      /* Welcome banner */
      .welcome-banner {
        background: linear-gradient(135deg, #3498db 0%, #9b59b6 100%);
        color: white;
        padding: 30px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
      }
      
      /* Step cards */
      .step-card {
        background: white;
        border-radius: 10px;
        padding: 20px;
        margin: 10px 0;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        transition: transform 0.3s ease;
      }
      .step-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 5px 20px rgba(0,0,0,0.15);
      }
      .step-number {
        width: 40px;
        height: 40px;
        border-radius: 50%;
        background: linear-gradient(135deg, #3498db, #9b59b6);
        color: white;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        font-size: 18px;
      }
      
      /* Content area padding */
      .content-wrapper {
        background-color: #f4f6f9;
      }
      
      /* Table styling */
      .dataTables_wrapper {
        padding: 10px;
      }
      
      /* Plot containers */
      .plotly {
        border-radius: 5px;
      }
    "))
  ),
  
  # Use shinyjs
  useShinyjs(),
  
  # Tab Items
  tabItems(
    # =========================================================================
    # MODULE 1: Data Upload & EDA
    # =========================================================================
    
    # Data Upload Tab
    tabItem(
      tabName = "data_upload",
      h2(icon("upload"), " Data Upload", style = "color: #2c3e50;"),
      p("Upload your CSV file or use a sample dataset to get started."),
      hr(),
      dataUploadUI("data_module")
    ),
    
    # Dataset Overview Tab
    tabItem(
      tabName = "data_summary",
      h2(icon("info-circle"), " Dataset Overview", style = "color: #2c3e50;"),
      p("Explore your dataset's structure and basic statistics."),
      hr(),
      dataSummaryUI("data_module")
    ),
    
    # Summary Statistics Tab
    tabItem(
      tabName = "summary_stats",
      h2(icon("calculator"), " Summary Statistics", style = "color: #2c3e50;"),
      p("Detailed statistical summaries for numeric and categorical variables."),
      hr(),
      summaryStatsUI("data_module")
    ),
    
    # EDA Visualizations Tab
    tabItem(
      tabName = "eda_viz",
      h2(icon("chart-bar"), " Exploratory Visualizations", style = "color: #2c3e50;"),
      p("Interactive charts and plots for data exploration."),
      hr(),
      edaVisualizationsUI("data_module")
    ),
    
    # Preprocessing Tab
    tabItem(
      tabName = "preprocessing",
      h2(icon("cogs"), " Data Preprocessing", style = "color: #2c3e50;"),
      p("Clean and prepare your data for machine learning."),
      hr(),
      preprocessingUI("data_module")
    ),
    
    # =========================================================================
    # MODULE 2: ML Model Training
    # =========================================================================
    
    # Model Configuration Tab
    tabItem(
      tabName = "model_config",
      h2(icon("sliders-h"), " Model Configuration & Training", style = "color: #2c3e50;"),
      p("Select target variable, choose model type, and train your model."),
      hr(),
      modelConfigUI("ml_module")
    ),
    
    # Model Metrics Tab
    tabItem(
      tabName = "model_metrics",
      h2(icon("chart-line"), " Model Performance", style = "color: #2c3e50;"),
      p("Evaluate your model's performance with comprehensive metrics."),
      hr(),
      modelMetricsUI("ml_module")
    ),
    
    # =========================================================================
    # MODULE 3: Explainable AI
    # =========================================================================
    
    # Global Explanations Tab
    tabItem(
      tabName = "xai_global",
      h2(icon("globe"), " Global Model Explanations", style = "color: #2c3e50;"),
      p("Understand overall model behavior through feature importance and partial dependence plots."),
      hr(),
      xaiGlobalUI("xai_module")
    ),
    
    # Local Explanations Tab
    tabItem(
      tabName = "xai_local",
      h2(icon("user"), " Local Explanations", style = "color: #2c3e50;"),
      p("Explain individual predictions using LIME and SHAP values."),
      hr(),
      xaiLocalUI("xai_module")
    ),
    
    # =========================================================================
    # HELP & ABOUT
    # =========================================================================
    
    # Help Tab
    tabItem(
      tabName = "help",
      h2(icon("question-circle"), " Help & User Guide", style = "color: #2c3e50;"),
      hr(),
      
      fluidRow(
        column(12,
          div(class = "welcome-banner",
            h3(icon("book-open"), " Getting Started with EXPLAINR-AI"),
            p("Follow these steps to analyze your data with explainable machine learning.")
          )
        )
      ),
      
      fluidRow(
        column(4,
          div(class = "step-card",
            fluidRow(
              column(2, div(class = "step-number", "1")),
              column(10, 
                h4("Upload Data"),
                p("Start by uploading a CSV file or use one of our sample datasets. 
                   The system will automatically detect column types and data structure.")
              )
            )
          )
        ),
        column(4,
          div(class = "step-card",
            fluidRow(
              column(2, div(class = "step-number", "2")),
              column(10,
                h4("Explore & Preprocess"),
                p("Use interactive visualizations to understand your data. 
                   Apply preprocessing steps like missing value imputation and normalization.")
              )
            )
          )
        ),
        column(4,
          div(class = "step-card",
            fluidRow(
              column(2, div(class = "step-number", "3")),
              column(10,
                h4("Train Model"),
                p("Select your target variable and choose from interpretable ML algorithms. 
                   The system auto-detects classification vs regression problems.")
              )
            )
          )
        )
      ),
      
      fluidRow(
        column(4,
          div(class = "step-card",
            fluidRow(
              column(2, div(class = "step-number", "4")),
              column(10,
                h4("Evaluate Performance"),
                p("Review comprehensive metrics including accuracy, precision, recall, F1-score 
                   for classification or RMSE, MAE, R² for regression.")
              )
            )
          )
        ),
        column(4,
          div(class = "step-card",
            fluidRow(
              column(2, div(class = "step-number", "5")),
              column(10,
                h4("Global Explanations"),
                p("Understand overall model behavior through feature importance rankings 
                   and partial dependence plots showing feature effects.")
              )
            )
          )
        ),
        column(4,
          div(class = "step-card",
            fluidRow(
              column(2, div(class = "step-number", "6")),
              column(10,
                h4("Local Explanations"),
                p("Explain individual predictions using LIME and SHAP values. 
                   Understand why the model made specific decisions.")
              )
            )
          )
        )
      ),
      
      br(),
      
      box(
        title = "Supported Models",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        
        h5(icon("tags"), " Classification Models:"),
        tags$ul(
          tags$li("Logistic Regression - Linear decision boundary"),
          tags$li("Decision Tree - Rule-based interpretable model"),
          tags$li("Random Forest - Ensemble of decision trees"),
          tags$li("Naive Bayes - Probabilistic classifier")
        ),
        
        h5(icon("chart-line"), " Regression Models:"),
        tags$ul(
          tags$li("Linear Regression - Linear relationship modeling"),
          tags$li("Decision Tree - Rule-based predictions"),
          tags$li("Random Forest - Ensemble predictions"),
          tags$li("SVM Regression - Support Vector Machine")
        )
      ),
      
      box(
        title = "Explanation Methods",
        status = "warning",
        solidHeader = TRUE,
        width = 6,
        
        h5(icon("globe"), " Global Explanations:"),
        tags$ul(
          tags$li(strong("Feature Importance"), " - Ranks features by their predictive power"),
          tags$li(strong("Partial Dependence Plots"), " - Shows marginal effect of features")
        ),
        
        h5(icon("user"), " Local Explanations:"),
        tags$ul(
          tags$li(strong("LIME"), " - Local Interpretable Model-agnostic Explanations"),
          tags$li(strong("SHAP"), " - SHapley Additive exPlanations for contribution analysis")
        )
      )
    ),
    
    # About Tab
    tabItem(
      tabName = "about",
      h2(icon("info"), " About EXPLAINR-AI", style = "color: #2c3e50;"),
      hr(),
      
      fluidRow(
        column(8,
          box(
            title = "Project Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            p("EXPLAINR-AI is an academic capstone project demonstrating proficiency in R programming, 
               machine learning, and explainable AI techniques. The application bridges the gap between 
               complex ML models and human interpretability."),
            
            h5(icon("bullseye"), " Key Objectives:"),
            tags$ul(
              tags$li("Demonstrate end-to-end data science workflow in R"),
              tags$li("Implement interpretable machine learning models"),
              tags$li("Apply explainable AI techniques for model transparency"),
              tags$li("Create a user-friendly web application for data analysis")
            ),
            
            h5(icon("code"), " Technology Stack:"),
            tags$ul(
              tags$li(strong("Shiny + shinydashboard"), " - Web application framework"),
              tags$li(strong("tidyverse + dplyr"), " - Data manipulation"),
              tags$li(strong("ggplot2 + plotly"), " - Visualization"),
              tags$li(strong("caret"), " - Machine learning framework"),
              tags$li(strong("DALEX + iml + lime"), " - Explainable AI")
            )
          )
        ),
        
        column(4,
          box(
            title = "Version Info",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            p(icon("tag"), strong(" Version: "), "1.0.0"),
            p(icon("calendar"), strong(" Year: "), "2026"),
            p(icon("graduation-cap"), strong(" Type: "), "Academic Capstone"),
            p(icon("code-branch"), strong(" R Version: "), as.character(getRversion())),
            
            hr(),
            
            p(icon("balance-scale"), strong(" License: "), "MIT"),
            p(icon("github"), strong(" Repository: "), "GitHub (Academic)")
          ),
          
          box(
            title = "Academic Value",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            tags$ul(
              tags$li("R programming proficiency"),
              tags$li("ML model implementation"),
              tags$li("XAI concept application"),
              tags$li("Web development skills"),
              tags$li("Data analysis workflow")
            )
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Combine UI Components
# -----------------------------------------------------------------------------

ui <- dashboardPage(
  title = "EXPLAINR-AI",
  header,
  sidebar,
  body,
  skin = "blue"
)
