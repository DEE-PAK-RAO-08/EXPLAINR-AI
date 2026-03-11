# =============================================================================
# EXPLAINR-AI: Server Logic (server.R)
# =============================================================================
# Central server orchestrating all module interactions
# =============================================================================

# Source module server files
source("modules/mod_data_server.R")
source("modules/mod_eda_viz_server.R")
source("modules/mod_ml_model.R")
source("modules/mod_ml_metrics.R")
source("modules/mod_xai.R")
source("modules/mod_xai_local.R")

# -----------------------------------------------------------------------------
# Server Function
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # ===========================================================================
  # MODULE 1: Data Preparation & EDA
  # ===========================================================================
  
  # Initialize Data Module - returns reactive data
  data_reactive <- dataModuleServer("data_module", session)
  
  # Add EDA visualization server
  edaVisualizationServer("data_module", data_reactive)
  
  # ===========================================================================
  # MODULE 2: ML Model Development
  # ===========================================================================
  
  # Initialize ML Module - returns trained model and metrics
  model_reactive <- mlModuleServer("ml_module", data_reactive)
  
  # Add ML Metrics server
  mlMetricsServer("ml_module", model_reactive)
  
  # ===========================================================================
  # MODULE 3: Explainable AI
  # ===========================================================================
  
  # Initialize XAI Global Module
  xai_reactive <- xaiModuleServer("xai_module", model_reactive)
  
  # Initialize XAI Local Module (LIME & SHAP)
  xaiLocalServer("xai_module", model_reactive, xai_reactive)
  
  # PDP Server
  pdpServer("xai_module", model_reactive, xai_reactive)
  
  # ===========================================================================
  # GLOBAL SESSION MANAGEMENT
  # ===========================================================================
  
  # Session initialization message
  observeEvent(session, {
    showNotification(
      "Welcome to EXPLAINR-AI! Start by uploading your data.",
      type = "message",
      duration = 5
    )
  }, once = TRUE)
  
  # Handle sidebar navigation with data validation
  observeEvent(input$sidebar_menu, {
    current_tab <- input$sidebar_menu
    
    # Check if data is loaded for relevant tabs
    data_required_tabs <- c("data_summary", "summary_stats", "eda_viz", 
                           "preprocessing", "model_config", "model_metrics",
                           "xai_global", "xai_local")
    
    if (current_tab %in% data_required_tabs) {
      if (is.null(data_reactive()$data)) {
        showNotification(
          "Please upload data first!",
          type = "warning",
          duration = 3
        )
        updateTabItems(session, "sidebar_menu", "data_upload")
      }
    }
    
    # Check if model is trained for XAI tabs
    model_required_tabs <- c("model_metrics", "xai_global", "xai_local")
    
    if (current_tab %in% model_required_tabs) {
      if (is.null(model_reactive()$model)) {
        showNotification(
          "Please train a model first!",
          type = "warning",
          duration = 3
        )
        if (is.null(data_reactive()$data)) {
          updateTabItems(session, "sidebar_menu", "data_upload")
        } else {
          updateTabItems(session, "sidebar_menu", "model_config")
        }
      }
    }
  })
  
  # ===========================================================================
  # DOWNLOAD HANDLERS FOR REPORTS
  # ===========================================================================
  
  # Full Report Download (optional enhancement)
  output$download_full_report <- downloadHandler(
    filename = function() {
      paste("EXPLAINR_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html", sep = "")
    },
    content = function(file) {
      # Placeholder for report generation
      # Could use rmarkdown::render() for full HTML reports
      writeLines("EXPLAINR-AI Report - Full implementation coming soon", file)
    }
  )
  
  # ===========================================================================
  # SESSION CLEANUP
  # ===========================================================================
  
  session$onSessionEnded(function() {
    # Cleanup operations when session ends
    cat("EXPLAINR-AI session ended\n")
  })
  
}
