# =============================================================================
# EXPLAINR-AI: Module 2 - ML Model Development
# =============================================================================
# This module handles:
# - Target variable selection
# - Automatic problem type detection (classification vs regression)
# - Model training using caret framework
# - Performance evaluation with appropriate metrics
# - Prediction display
# =============================================================================

# -----------------------------------------------------------------------------
# UI Components for ML Module
# -----------------------------------------------------------------------------

#' Model Configuration UI
modelConfigUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = span(icon("cog"), " Model Configuration"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      fluidRow(
        column(4,
          selectInput(ns("target_var"), "Select Target Variable:",
                     choices = NULL, selected = NULL),
          uiOutput(ns("problem_type_display"))
        ),
        column(4,
          sliderInput(ns("train_split"), "Training Set Ratio:",
                     min = 0.5, max = 0.9, value = 0.7, step = 0.05),
          helpText("Remaining data will be used for testing")
        ),
        column(4,
          selectInput(ns("model_type"), "Select Model:",
                     choices = NULL, selected = NULL),
          helpText("Available models depend on problem type")
        )
      ),
      
      hr(),
      
      fluidRow(
        column(12,
          actionButton(ns("train_model"), "Train Model",
                      icon = icon("play"), class = "btn-success btn-lg"),
          actionButton(ns("reset_model"), "Reset",
                      icon = icon("undo"), class = "btn-warning btn-lg"),
          br(), br(),
          uiOutput(ns("training_status"))
        )
      )
    )
  )
}

#' Model Metrics UI
modelMetricsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = span(icon("chart-line"), " Model Performance"),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      
      fluidRow(
        valueBoxOutput(ns("metric1"), width = 3),
        valueBoxOutput(ns("metric2"), width = 3),
        valueBoxOutput(ns("metric3"), width = 3),
        valueBoxOutput(ns("metric4"), width = 3)
      ),
      
      conditionalPanel(
        condition = "output.is_classification == true",
        ns = ns,
        h4("Confusion Matrix"),
        plotlyOutput(ns("confusion_matrix"), height = "400px")
      ),
      
      conditionalPanel(
        condition = "output.is_classification == false",
        ns = ns,
        h4("Actual vs Predicted"),
        plotlyOutput(ns("regression_plot"), height = "400px")
      )
    ),
    
    box(
      title = span(icon("table"), " Prediction Results"),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      DTOutput(ns("predictions_table")),
      br(),
      downloadButton(ns("download_predictions"), "Download Predictions", class = "btn-primary")
    ),
    
    box(
      title = span(icon("info-circle"), " Model Summary"),
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      verbatimTextOutput(ns("model_summary"))
    )
  )
}

# -----------------------------------------------------------------------------
# Server Logic for ML Module
# -----------------------------------------------------------------------------

#' ML Module Server
mlModuleServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Values
    rv <- reactiveValues(
      trained_model = NULL,
      train_data = NULL,
      test_data = NULL,
      predictions = NULL,
      problem_type = NULL,
      target_var = NULL,
      model_name = NULL,
      metrics = list()
    )
    
    # Update target variable choices when data changes
    observe({
      req(data_reactive()$data)
      data <- data_reactive()$data
      updateSelectInput(session, "target_var", choices = names(data))
    })
    
    # Detect problem type and update model choices
    observeEvent(input$target_var, {
      req(data_reactive()$data, input$target_var)
      data <- data_reactive()$data
      target <- data[[input$target_var]]
      
      problem_type <- detect_problem_type(target)
      rv$problem_type <- problem_type
      rv$target_var <- input$target_var
      
      if (problem_type == "classification") {
        choices <- c("Logistic Regression" = "glm",
                    "Decision Tree" = "rpart",
                    "Random Forest" = "rf",
                    "Naive Bayes" = "naive_bayes")
      } else {
        choices <- c("Linear Regression" = "lm",
                    "Decision Tree" = "rpart",
                    "Random Forest" = "rf",
                    "SVM Regression" = "svmRadial")
      }
      
      updateSelectInput(session, "model_type", choices = choices)
    })
    
    # Display problem type
    output$problem_type_display <- renderUI({
      req(rv$problem_type)
      type_color <- if(rv$problem_type == "classification") "success" else "info"
      type_icon <- if(rv$problem_type == "classification") "tags" else "chart-line"
      
      div(
        class = paste("alert alert-", type_color, sep = ""),
        icon(type_icon),
        strong(" Problem Type: "),
        toupper(rv$problem_type)
      )
    })
    
    # Output for conditional panel
    output$is_classification <- reactive({
      !is.null(rv$problem_type) && rv$problem_type == "classification"
    })
    outputOptions(output, "is_classification", suspendWhenHidden = FALSE)
    
    # Train Model
    observeEvent(input$train_model, {
      req(data_reactive()$data, input$target_var, input$model_type)
      
      withProgress(message = 'Training model...', value = 0, {
        tryCatch({
          data <- data_reactive()$data
          
          # Remove rows with NA in target
          data <- data[!is.na(data[[input$target_var]]), ]
          
          # Prepare formula
          target_var <- input$target_var
          feature_vars <- setdiff(names(data), target_var)
          
          # Remove non-numeric/factor columns that can't be used
          valid_cols <- sapply(data[feature_vars], function(x) {
            is.numeric(x) || is.factor(x) || is.character(x)
          })
          feature_vars <- feature_vars[valid_cols]
          
          # Convert character to factor
          for (col in feature_vars) {
            if (is.character(data[[col]])) {
              data[[col]] <- as.factor(data[[col]])
            }
          }
          
          # Ensure target is factor for classification
          if (rv$problem_type == "classification") {
            data[[target_var]] <- as.factor(data[[target_var]])
          }
          
          incProgress(0.2, detail = "Splitting data...")
          
          # Train-test split
          set.seed(42)
          train_idx <- createDataPartition(data[[target_var]], p = input$train_split, list = FALSE)
          
          # Ensure no NAs in training/test data
          train_data <- na.omit(data[train_idx, c(feature_vars, target_var)])
          test_data <- na.omit(data[-train_idx, c(feature_vars, target_var)])
          
          rv$train_data <- train_data
          rv$test_data <- test_data
          
          # Create formula
          formula <- as.formula(paste(target_var, "~ ."))
          
          # Train model using caret
          ctrl <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
          
          model <- train(
            formula,
            data = train_data,
            method = input$model_type,
            trControl = ctrl
          )
          
          rv$trained_model <- model
          rv$model_name <- input$model_type
          
          incProgress(0.6, detail = "Making predictions...")
          
          # Predictions
          predictions <- predict(model, newdata = test_data)
          rv$predictions <- predictions
          
          incProgress(0.8, detail = "Calculating metrics...")
          
          # Calculate metrics
          actual <- test_data[[target_var]]
          
          if (rv$problem_type == "classification") {
            cm <- confusionMatrix(predictions, actual)
            
            # Extract metrics - handle vector (2-class) vs matrix (>2-class)
            if (is.null(dim(cm$byClass))) {
              # 2-class problem (vector)
              metric_precision <- cm$byClass["Precision"]
              metric_recall <- cm$byClass["Recall"]
              metric_f1 <- cm$byClass["F1"]
            } else {
              # Multi-class problem (matrix)
              metric_precision <- mean(cm$byClass[, "Precision"], na.rm = TRUE)
              metric_recall <- mean(cm$byClass[, "Recall"], na.rm = TRUE)
              metric_f1 <- mean(cm$byClass[, "F1"], na.rm = TRUE)
            }
            
            rv$metrics <- list(
              accuracy = cm$overall["Accuracy"],
              precision = metric_precision,
              recall = metric_recall,
              f1 = metric_f1,
              confusion_matrix = cm$table
            )
          } else {
            actual_num <- as.numeric(actual)
            pred_num <- as.numeric(predictions)
            rv$metrics <- list(
              rmse = sqrt(mean((pred_num - actual_num)^2)),
              mae = mean(abs(pred_num - actual_num)),
              r2 = (cor(pred_num, actual_num, use = "complete.obs"))^2,
              mape = mean(abs((actual_num - pred_num) / actual_num)) * 100
            )
          }
          
          incProgress(1, detail = "Complete!")
          showNotification("Model trained successfully!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Training error:", e$message), type = "error", duration = 10)
        })
      })
    })
    
    # Reset Model
    observeEvent(input$reset_model, {
      rv$trained_model <- NULL
      rv$train_data <- NULL
      rv$test_data <- NULL
      rv$predictions <- NULL
      rv$metrics <- list()
      showNotification("Model reset!", type = "warning")
    })
    
    # Training Status
    output$training_status <- renderUI({
      if (!is.null(rv$trained_model)) {
        div(class = "alert alert-success",
           icon("check-circle"), " Model trained successfully using ", strong(rv$model_name),
           " on ", nrow(rv$train_data), " training samples.")
      } else {
        div(class = "alert alert-info",
           icon("info-circle"), " Configure parameters and click 'Train Model' to begin.")
      }
    })
    
    # Return trained model and data for XAI module
    return(reactive({
      list(
        model = rv$trained_model,
        train_data = rv$train_data,
        test_data = rv$test_data,
        predictions = rv$predictions,
        problem_type = rv$problem_type,
        target_var = rv$target_var,
        metrics = rv$metrics
      )
    }))
  })
}
