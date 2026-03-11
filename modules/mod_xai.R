# =============================================================================
# EXPLAINR-AI: Module 3 - Explainable AI
# =============================================================================
# This module handles:
# - Global explanations: Feature Importance, Partial Dependence Plots
# - Local explanations: LIME, SHAP values
# - Interactive visualization of model decisions
# =============================================================================

# -----------------------------------------------------------------------------
# UI Components for XAI Module
# -----------------------------------------------------------------------------

#' XAI Global Explanations UI
xaiGlobalUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = span(icon("globe"), " Global Model Explanations"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      fluidRow(
        column(12,
          actionButton(ns("generate_global"), "Generate Global Explanations",
                      icon = icon("cogs"), class = "btn-success btn-lg"),
          br(), br(),
          uiOutput(ns("global_status"))
        )
      )
    ),
    
    box(
      title = span(icon("sort-amount-down"), " Feature Importance"),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(4,
          sliderInput(ns("n_features"), "Number of Features to Display:",
                     min = 5, max = 20, value = 10)
        ),
        column(8,
          radioButtons(ns("importance_type"), "Importance Method:",
                      choices = c("Permutation" = "permutation", "Model-based" = "model"),
                      inline = TRUE)
        )
      ),
      plotlyOutput(ns("feature_importance_plot"), height = "450px"),
      downloadButton(ns("download_importance"), "Download Plot", class = "btn-sm btn-info")
    ),
    
    box(
      title = span(icon("chart-area"), " Partial Dependence Plots (PDP)"),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(6,
          selectInput(ns("pdp_variable"), "Select Feature for PDP:",
                     choices = NULL)
        ),
        column(6,
          sliderInput(ns("pdp_resolution"), "Resolution (Grid Points):",
                     min = 10, max = 50, value = 20)
        )
      ),
      plotlyOutput(ns("pdp_plot"), height = "400px"),
      downloadButton(ns("download_pdp"), "Download Plot", class = "btn-sm btn-success")
    )
  )
}

#' XAI Local Explanations UI
xaiLocalUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = span(icon("user"), " Local Explanations - Single Observation"),
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      
      fluidRow(
        column(4,
          numericInput(ns("obs_index"), "Select Observation Index:",
                      min = 1, max = 100, value = 1, step = 1),
          helpText("Select a row from the test set to explain")
        ),
        column(4,
          actionButton(ns("generate_local"), "Generate Local Explanation",
                      icon = icon("microscope"), class = "btn-warning btn-lg")
        ),
        column(4,
          uiOutput(ns("obs_prediction"))
        )
      ),
      
      hr(),
      
      h5("Selected Observation Data:"),
      DTOutput(ns("selected_obs_table"))
    ),
    
    box(
      title = span(icon("lemon"), " LIME Explanation"),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(4,
          sliderInput(ns("lime_n_features"), "Number of Features:",
                     min = 3, max = 15, value = 6)
        ),
        column(4,
          sliderInput(ns("lime_n_permutations"), "Number of Permutations:",
                     min = 500, max = 2000, value = 1000, step = 100)
        ),
        column(4,
          br(),
          actionButton(ns("run_lime"), "Run LIME",
                      icon = icon("play"), class = "btn-info")
        )
      ),
      plotlyOutput(ns("lime_plot"), height = "400px"),
      downloadButton(ns("download_lime"), "Download Plot", class = "btn-sm btn-info")
    ),
    
    box(
      title = span(icon("chart-bar"), " SHAP Values"),
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(6,
          actionButton(ns("run_shap"), "Calculate SHAP Values",
                      icon = icon("calculator"), class = "btn-danger")
        ),
        column(6,
          helpText("SHAP values show how each feature contributes to the prediction")
        )
      ),
      plotlyOutput(ns("shap_plot"), height = "400px"),
      downloadButton(ns("download_shap"), "Download Plot", class = "btn-sm btn-danger")
    )
  )
}

# -----------------------------------------------------------------------------
# Server Logic for XAI Module
# -----------------------------------------------------------------------------

#' XAI Module Server
xaiModuleServer <- function(id, model_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Values
    rv <- reactiveValues(
      explainer = NULL,
      feature_importance = NULL,
      pdp_data = NULL,
      lime_explanation = NULL,
      shap_values = NULL,
      global_ready = FALSE,
      local_ready = FALSE
    )
    
    # Update observation range based on test data
    observe({
      req(model_reactive()$test_data)
      n_obs <- nrow(model_reactive()$test_data)
      updateNumericInput(session, "obs_index", max = n_obs)
    })
    
    # Update PDP variable choices
    observe({
      req(model_reactive()$train_data, model_reactive()$target_var)
      features <- setdiff(names(model_reactive()$train_data), model_reactive()$target_var)
      numeric_features <- features[sapply(model_reactive()$train_data[features], is.numeric)]
      updateSelectInput(session, "pdp_variable", choices = numeric_features)
    })
    
    # Generate Global Explanations
    observeEvent(input$generate_global, {
      req(model_reactive()$model, model_reactive()$train_data, model_reactive()$target_var)
      
      withProgress(message = 'Generating global explanations...', value = 0, {
        tryCatch({
          model <- model_reactive()$model
          train_data <- model_reactive()$train_data
          target_var <- model_reactive()$target_var
          problem_type <- model_reactive()$problem_type
          
          # Prepare data for DALEX
          X <- train_data[, setdiff(names(train_data), target_var), drop = FALSE]
          y <- train_data[[target_var]]
          
          # Convert factors to numeric for DALEX if needed
          X_numeric <- X
          for (col in names(X_numeric)) {
            if (is.factor(X_numeric[[col]]) || is.character(X_numeric[[col]])) {
              X_numeric[[col]] <- as.numeric(as.factor(X_numeric[[col]]))
            }
          }
          
          incProgress(0.2, detail = "Creating explainer...")
          
          # Create custom predict function for caret models
          # DALEX needs a function that returns numeric values
          predict_fn <- function(model, newdata) {
            if (problem_type == "classification") {
              # For classification, try to get probabilities
              p <- try(predict(model, newdata = newdata, type = "prob"), silent = TRUE)
              if (inherits(p, "try-error")) {
                # Fallback to class conversion if prob fails
                return(as.numeric(predict(model, newdata = newdata)))
              }
              # If 2 classes, return the probability of the first class
              if (ncol(p) == 2) return(p[, 1])
              # For multiclass, DALEX can handle the matrix
              return(as.matrix(p))
            } else {
              return(predict(model, newdata = newdata))
            }
          }
          
          # Create DALEX explainer
          explainer <- DALEX::explain(
            model = model,
            data = X,
            y = if(is.factor(y)) as.numeric(y) else y,
            predict_function = predict_fn,
            label = paste("EXPLAINR-AI:", model$method),
            verbose = FALSE,
            type = problem_type
          )
          
          rv$explainer <- explainer
          
          incProgress(0.5, detail = "Calculating feature importance...")
          
          # Calculate feature importance using model's variable importance
          if (input$importance_type == "model") {
            vi <- varImp(model, scale = TRUE)
            rv$feature_importance <- vi
          } else {
            # Permutation importance using DALEX
            fi <- DALEX::model_parts(explainer, type = "variable_importance", N = 500)
            rv$feature_importance <- fi
          }
          
          incProgress(1, detail = "Complete!")
          rv$global_ready <- TRUE
          
          showNotification("Global explanations generated!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 10)
        })
      })
    })
    
    # Global Status
    output$global_status <- renderUI({
      if (rv$global_ready) {
        div(class = "alert alert-success",
           icon("check-circle"), " Global explanations generated successfully!")
      } else if (!is.null(model_reactive()$model)) {
        div(class = "alert alert-info",
           icon("info-circle"), " Click 'Generate Global Explanations' to analyze model behavior.")
      } else {
        div(class = "alert alert-warning",
           icon("exclamation-triangle"), " Please train a model first!")
      }
    })
    
    # Feature Importance Plot
    output$feature_importance_plot <- renderPlotly({
      req(rv$feature_importance)
      
      if (input$importance_type == "model") {
        # Model-based importance from caret
        vi <- rv$feature_importance
        imp_df <- data.frame(
          Feature = rownames(vi$importance),
          Importance = vi$importance[, 1]
        )
      } else {
        # Permutation importance from DALEX
        fi <- rv$feature_importance
        imp_df <- data.frame(
          Feature = fi$variable,
          Importance = fi$dropout_loss - fi$dropout_loss[fi$variable == "_full_model_"]
        )
        imp_df <- imp_df[imp_df$Feature != "_baseline_" & imp_df$Feature != "_full_model_", ]
      }
      
      # Sort and limit
      imp_df <- imp_df[order(-imp_df$Importance), ]
      imp_df <- head(imp_df, input$n_features)
      
      plot_ly(data = imp_df,
             x = ~Importance,
             y = ~reorder(Feature, Importance),
             type = "bar",
             orientation = "h",
             marker = list(
               color = ~Importance,
               colorscale = list(c(0, '#3498db'), c(1, '#e74c3c'))
             ),
             hovertemplate = "<b>%{y}</b><br>Importance: %{x:.4f}<extra></extra>") %>%
        layout(
          title = "Feature Importance Ranking",
          xaxis = list(title = "Importance Score"),
          yaxis = list(title = ""),
          showlegend = FALSE
        )
    })
    
    # Download Handler for Feature Importance
    output$download_importance <- downloadHandler(
      filename = function() {
        paste("feature_importance_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
      },
      content = function(file) {
        req(rv$feature_importance)
        
        if (input$importance_type == "model") {
          vi <- rv$feature_importance
          imp_df <- data.frame(
            Feature = rownames(vi$importance),
            Importance = vi$importance[, 1]
          )
        } else {
          fi <- rv$feature_importance
          imp_df <- data.frame(
            Feature = fi$variable,
            Importance = fi$dropout_loss - fi$dropout_loss[fi$variable == "_full_model_"]
          )
          imp_df <- imp_df[imp_df$Feature != "_baseline_" & imp_df$Feature != "_full_model_", ]
        }
        
        imp_df <- imp_df[order(-imp_df$Importance), ]
        imp_df <- head(imp_df, input$n_features)
        
        p <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = "#3498db") +
          coord_flip() +
          labs(title = "Feature Importance Ranking", x = "", y = "Importance Score") +
          theme_minimal()
        
        ggsave(file, p, width = 10, height = 8, dpi = 300)
      }
    )
    
    # Return for potential use elsewhere
    return(reactive({
      list(
        explainer = rv$explainer,
        global_ready = rv$global_ready
      )
    }))
  })
}
