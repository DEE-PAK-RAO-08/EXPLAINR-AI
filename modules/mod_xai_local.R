# =============================================================================
# EXPLAINR-AI: Module 3 Part 2 - Local Explanations (LIME & SHAP)
# =============================================================================

#' XAI Local Explanations Server
xaiLocalServer <- function(id, model_reactive, xai_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Values
    rv <- reactiveValues(
      lime_explanation = NULL,
      shap_values = NULL,
      selected_obs = NULL
    )
    
    # Selected Observation
    observe({
      req(model_reactive()$test_data, input$obs_index)
      idx <- input$obs_index
      if (idx >= 1 && idx <= nrow(model_reactive()$test_data)) {
        rv$selected_obs <- model_reactive()$test_data[idx, , drop = FALSE]
      }
    })
    
    # Display selected observation
    output$selected_obs_table <- renderDT({
      req(rv$selected_obs)
      datatable(
        rv$selected_obs,
        options = list(scrollX = TRUE, paging = FALSE, searching = FALSE),
        rownames = FALSE
      )
    })
    
    # Display prediction for selected observation
    output$obs_prediction <- renderUI({
      req(model_reactive()$model, rv$selected_obs)
      
      tryCatch({
        pred <- predict(model_reactive()$model, newdata = rv$selected_obs)
        actual <- rv$selected_obs[[model_reactive()$target_var]]
        
        pred_color <- if(as.character(pred) == as.character(actual)) "success" else "danger"
        
        div(
          class = paste("alert alert-", pred_color, sep = ""),
          h5(icon("robot"), " Model Prediction"),
          p(strong("Predicted: "), as.character(pred)),
          p(strong("Actual: "), as.character(actual))
        )
      }, error = function(e) {
        div(class = "alert alert-warning", "Unable to generate prediction")
      })
    })
    
    # Generate Local Explanation (general)
    observeEvent(input$generate_local, {
      req(model_reactive()$model, rv$selected_obs)
      showNotification("Use LIME or SHAP buttons below to generate specific explanations", type = "message")
    })
    
    # LIME Explanation
    observeEvent(input$run_lime, {
      req(model_reactive()$model, model_reactive()$train_data, 
          rv$selected_obs, model_reactive()$target_var)
      
      withProgress(message = 'Running LIME...', value = 0, {
        tryCatch({
          model <- model_reactive()$model
          train_data <- model_reactive()$train_data
          target_var <- model_reactive()$target_var
          problem_type <- model_reactive()$problem_type
          
          # Prepare training data for LIME
          X_train <- train_data[, setdiff(names(train_data), target_var), drop = FALSE]
          X_explain <- rv$selected_obs[, setdiff(names(rv$selected_obs), target_var), drop = FALSE]
          
          incProgress(0.3, detail = "Creating LIME explainer...")
          
          # Create LIME explainer with proper model type
          if (problem_type == "classification") {
            lime_explainer <- lime::lime(
              x = X_train,
              model = model,
              bin_continuous = TRUE,
              n_bins = 5
            )
          } else {
            lime_explainer <- lime::lime(
              x = X_train,
              model = model,
              bin_continuous = TRUE,
              n_bins = 5
            )
          }
          
          incProgress(0.6, detail = "Generating explanation...")
          
          # Generate explanation
          explanation <- lime::explain(
            x = X_explain,
            explainer = lime_explainer,
            n_labels = if(problem_type == "classification") 1 else NULL,
            n_features = input$lime_n_features,
            n_permutations = input$lime_n_permutations
          )
          
          rv$lime_explanation <- explanation
          
          incProgress(1, detail = "Complete!")
          showNotification("LIME explanation generated!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("LIME Error:", e$message), type = "error", duration = 10)
        })
      })
    })
    
    # LIME Plot
    output$lime_plot <- renderPlotly({
      req(rv$lime_explanation)
      
      explanation <- rv$lime_explanation
      
      # Create plot data
      plot_data <- data.frame(
        Feature = explanation$feature_desc,
        Weight = explanation$feature_weight,
        stringsAsFactors = FALSE
      )
      
      # Color by positive/negative
      plot_data$Color <- ifelse(plot_data$Weight >= 0, "#27ae60", "#e74c3c")
      plot_data$Direction <- ifelse(plot_data$Weight >= 0, "Positive", "Negative")
      
      # Sort by absolute weight
      plot_data <- plot_data[order(abs(plot_data$Weight), decreasing = TRUE), ]
      
      plot_ly(data = plot_data,
             x = ~Weight,
             y = ~reorder(Feature, abs(Weight)),
             type = "bar",
             orientation = "h",
             marker = list(color = ~Color),
             text = ~paste(Direction, ": ", round(Weight, 4)),
             hovertemplate = "<b>%{y}</b><br>%{text}<extra></extra>") %>%
        layout(
          title = paste("LIME Explanation for Observation", input$obs_index),
          xaxis = list(title = "Feature Weight"),
          yaxis = list(title = ""),
          showlegend = FALSE
        ) %>%
        add_trace(
          x = 0, y = plot_data$Feature,
          type = "scatter", mode = "lines",
          line = list(color = "black", dash = "dash", width = 1),
          showlegend = FALSE,
          hoverinfo = "none"
        )
    })
    
    # SHAP Values
    observeEvent(input$run_shap, {
      req(model_reactive()$model, model_reactive()$train_data,
          rv$selected_obs, model_reactive()$target_var)
      
      withProgress(message = 'Calculating SHAP values...', value = 0, {
        tryCatch({
          model <- model_reactive()$model
          train_data <- model_reactive()$train_data
          target_var <- model_reactive()$target_var
          
          # Prepare data
          X_train <- train_data[, setdiff(names(train_data), target_var), drop = FALSE]
          X_explain <- rv$selected_obs[, setdiff(names(rv$selected_obs), target_var), drop = FALSE]
          
          incProgress(0.3, detail = "Creating predictor...")
          
          # Use iml for SHAP approximation
          # Create predictor object
          predictor <- iml::Predictor$new(
            model = model,
            data = X_train,
            y = train_data[[target_var]]
          )
          
          incProgress(0.5, detail = "Computing Shapley values...")
          
          # Calculate Shapley values for the observation
          shapley <- iml::Shapley$new(
            predictor = predictor,
            x.interest = X_explain,
            sample.size = 100
          )
          
          rv$shap_values <- shapley
          
          incProgress(1, detail = "Complete!")
          showNotification("SHAP values calculated!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("SHAP Error:", e$message), type = "error", duration = 10)
        })
      })
    })
    
    # SHAP Plot
    output$shap_plot <- renderPlotly({
      req(rv$shap_values)
      
      shap_results <- rv$shap_values$results
      
      # Create plot data
      plot_data <- data.frame(
        Feature = shap_results$feature,
        SHAP = shap_results$phi,
        stringsAsFactors = FALSE
      )
      
      # Aggregate if multiple classes
      plot_data <- aggregate(SHAP ~ Feature, data = plot_data, FUN = mean)
      
      # Color by positive/negative
      plot_data$Color <- ifelse(plot_data$SHAP >= 0, "#9b59b6", "#1abc9c")
      
      # Sort by absolute SHAP value
      plot_data <- plot_data[order(abs(plot_data$SHAP), decreasing = TRUE), ]
      
      plot_ly(data = plot_data,
             x = ~SHAP,
             y = ~reorder(Feature, abs(SHAP)),
             type = "bar",
             orientation = "h",
             marker = list(color = ~Color),
             hovertemplate = "<b>%{y}</b><br>SHAP Value: %{x:.4f}<extra></extra>") %>%
        layout(
          title = paste("SHAP Values for Observation", input$obs_index),
          xaxis = list(title = "SHAP Value (contribution to prediction)"),
          yaxis = list(title = ""),
          showlegend = FALSE
        ) %>%
        add_trace(
          x = 0, y = plot_data$Feature,
          type = "scatter", mode = "lines",
          line = list(color = "black", dash = "dash", width = 1),
          showlegend = FALSE,
          hoverinfo = "none"
        )
    })
    
    # Download handlers
    output$download_lime <- downloadHandler(
      filename = function() paste("lime_explanation_obs", input$obs_index, ".png", sep = ""),
      content = function(file) {
        req(rv$lime_explanation)
        p <- lime::plot_features(rv$lime_explanation)
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
    
    output$download_shap <- downloadHandler(
      filename = function() paste("shap_values_obs", input$obs_index, ".png", sep = ""),
      content = function(file) {
        req(rv$shap_values)
        p <- plot(rv$shap_values)
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
  })
}

#' PDP Server Function
pdpServer <- function(id, model_reactive, xai_reactive) {
  moduleServer(id, function(input, output, session) {
    
    output$pdp_plot <- renderPlotly({
      req(xai_reactive()$explainer, input$pdp_variable)
      
      tryCatch({
        explainer <- xai_reactive()$explainer
        
        # Calculate PDP
        pdp <- DALEX::model_profile(
          explainer,
          variables = input$pdp_variable,
          N = min(500, nrow(explainer$data)),
          grid_points = input$pdp_resolution
        )
        
        # Extract data for plotting
        pdp_data <- pdp$agr_profiles
        
        plot_ly(data = pdp_data,
               x = ~`_x_`,
               y = ~`_yhat_`,
               type = "scatter",
               mode = "lines+markers",
               line = list(color = "#3498db", width = 3),
               marker = list(size = 8, color = "#2980b9"),
               hovertemplate = paste(input$pdp_variable, ": %{x:.2f}<br>",
                                    "Avg Prediction: %{y:.4f}<extra></extra>")) %>%
          layout(
            title = paste("Partial Dependence Plot:", input$pdp_variable),
            xaxis = list(title = input$pdp_variable),
            yaxis = list(title = "Average Prediction")
          )
        
      }, error = function(e) {
        plot_ly() %>%
          add_annotations(
            text = paste("Error generating PDP:", e$message),
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 14, color = "red")
          )
      })
    })
    
    output$download_pdp <- downloadHandler(
      filename = function() paste("pdp_", input$pdp_variable, ".png", sep = ""),
      content = function(file) {
        req(xai_reactive()$explainer, input$pdp_variable)
        pdp <- DALEX::model_profile(xai_reactive()$explainer, variables = input$pdp_variable)
        p <- plot(pdp) + theme_minimal()
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
  })
}
