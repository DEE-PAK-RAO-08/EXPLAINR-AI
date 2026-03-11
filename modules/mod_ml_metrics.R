# =============================================================================
# EXPLAINR-AI: Module 2 Part 2 - ML Metrics and Outputs Server
# =============================================================================

#' ML Metrics Server Functions
mlMetricsServer <- function(id, model_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Metric Value Boxes for Classification
    output$metric1 <- renderValueBox({
      req(model_reactive()$metrics)
      metrics <- model_reactive()$metrics
      
      if (model_reactive()$problem_type == "classification") {
        valueBox(
          format_metric(metrics$accuracy, 3),
          "Accuracy",
          icon = icon("bullseye"),
          color = "green"
        )
      } else {
        valueBox(
          format_metric(metrics$rmse, 3),
          "RMSE",
          icon = icon("ruler"),
          color = "red"
        )
      }
    })
    
    output$metric2 <- renderValueBox({
      req(model_reactive()$metrics)
      metrics <- model_reactive()$metrics
      
      if (model_reactive()$problem_type == "classification") {
        valueBox(
          format_metric(metrics$precision, 3),
          "Precision",
          icon = icon("crosshairs"),
          color = "blue"
        )
      } else {
        valueBox(
          format_metric(metrics$mae, 3),
          "MAE",
          icon = icon("chart-bar"),
          color = "yellow"
        )
      }
    })
    
    output$metric3 <- renderValueBox({
      req(model_reactive()$metrics)
      metrics <- model_reactive()$metrics
      
      if (model_reactive()$problem_type == "classification") {
        valueBox(
          format_metric(metrics$recall, 3),
          "Recall",
          icon = icon("search"),
          color = "yellow"
        )
      } else {
        valueBox(
          format_metric(metrics$r2, 3),
          "R²",
          icon = icon("chart-line"),
          color = "green"
        )
      }
    })
    
    output$metric4 <- renderValueBox({
      req(model_reactive()$metrics)
      metrics <- model_reactive()$metrics
      
      if (model_reactive()$problem_type == "classification") {
        valueBox(
          format_metric(metrics$f1, 3),
          "F1-Score",
          icon = icon("balance-scale"),
          color = "purple"
        )
      } else {
        valueBox(
          paste0(format_metric(metrics$mape, 2), "%"),
          "MAPE",
          icon = icon("percent"),
          color = "blue"
        )
      }
    })
    
    # Confusion Matrix Plot
    output$confusion_matrix <- renderPlotly({
      req(model_reactive()$metrics$confusion_matrix)
      cm <- model_reactive()$metrics$confusion_matrix
      
      cm_df <- as.data.frame(cm)
      names(cm_df) <- c("Predicted", "Actual", "Count")
      
      plot_ly(data = cm_df, x = ~Actual, y = ~Predicted, z = ~Count,
             type = "heatmap",
             colorscale = list(c(0, '#ecf0f1'), c(1, '#27ae60')),
             text = ~Count,
             texttemplate = "%{text}",
             hovertemplate = "Actual: %{x}<br>Predicted: %{y}<br>Count: %{z}<extra></extra>") %>%
        layout(
          title = "Confusion Matrix",
          xaxis = list(title = "Actual"),
          yaxis = list(title = "Predicted")
        )
    })
    
    # Regression Plot (Actual vs Predicted)
    output$regression_plot <- renderPlotly({
      req(model_reactive()$test_data, model_reactive()$predictions, model_reactive()$target_var)
      
      actual <- model_reactive()$test_data[[model_reactive()$target_var]]
      predicted <- model_reactive()$predictions
      
      df <- data.frame(Actual = as.numeric(actual), Predicted = as.numeric(predicted))
      
      plot_ly(data = df, x = ~Actual, y = ~Predicted, type = "scatter", mode = "markers",
             marker = list(color = "#3498db", size = 8, opacity = 0.7),
             hovertemplate = "Actual: %{x:.2f}<br>Predicted: %{y:.2f}<extra></extra>") %>%
        add_trace(x = c(min(df$Actual), max(df$Actual)),
                 y = c(min(df$Actual), max(df$Actual)),
                 type = "scatter", mode = "lines",
                 line = list(color = "#e74c3c", dash = "dash"),
                 name = "Perfect Prediction",
                 hoverinfo = "none") %>%
        layout(
          title = "Actual vs Predicted Values",
          xaxis = list(title = "Actual"),
          yaxis = list(title = "Predicted"),
          showlegend = TRUE
        )
    })
    
    # Predictions Table
    output$predictions_table <- renderDT({
      req(model_reactive()$test_data, model_reactive()$predictions, model_reactive()$target_var)
      
      result_df <- model_reactive()$test_data
      result_df$Predicted <- model_reactive()$predictions
      
      # Move target and predicted to front
      target_col <- model_reactive()$target_var
      col_order <- c(target_col, "Predicted", setdiff(names(result_df), c(target_col, "Predicted")))
      result_df <- result_df[, col_order]
      
      # Add match indicator
      if (model_reactive()$problem_type == "classification") {
        result_df$Match <- ifelse(result_df[[target_col]] == result_df$Predicted, "✓", "✗")
        col_order <- c(target_col, "Predicted", "Match", setdiff(names(result_df), c(target_col, "Predicted", "Match")))
        result_df <- result_df[, col_order]
      }
      
      datatable(
        head(result_df, 100),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })
    
    # Model Summary
    output$model_summary <- renderPrint({
      req(model_reactive()$model)
      print(model_reactive()$model)
    })
    
    # Download Predictions
    output$download_predictions <- downloadHandler(
      filename = function() {
        paste("predictions_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
      },
      content = function(file) {
        result_df <- model_reactive()$test_data
        result_df$Predicted <- model_reactive()$predictions
        write.csv(result_df, file, row.names = FALSE)
      }
    )
  })
}
