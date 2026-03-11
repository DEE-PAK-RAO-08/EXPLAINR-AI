# =============================================================================
# EXPLAINR-AI: Module 1 Part 4 - EDA Server Functions (Visualizations)
# =============================================================================

#' EDA Visualization Server Functions
edaVisualizationServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Missing Values Analysis
    output$missing_table <- renderDT({
      req(data_reactive()$data)
      missing_stats <- calculate_missing_stats(data_reactive()$data)
      datatable(missing_stats, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE) %>%
        formatStyle('Missing_Percent', background = styleColorBar(c(0, 100), '#e74c3c'),
                   backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
    })
    
    output$missing_plot <- renderPlotly({
      req(data_reactive()$data)
      missing_stats <- calculate_missing_stats(data_reactive()$data)
      missing_stats <- missing_stats[missing_stats$Missing_Count > 0, ]
      
      if (nrow(missing_stats) == 0) {
        plot_ly() %>%
          add_annotations(text = "No missing values found!", x = 0.5, y = 0.5,
                         xref = "paper", yref = "paper", showarrow = FALSE,
                         font = list(size = 18, color = "#2ecc71")) %>%
          layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
      } else {
        plot_ly(data = missing_stats, x = ~reorder(Variable, -Missing_Percent),
               y = ~Missing_Percent, type = "bar",
               marker = list(color = ~Missing_Percent, colorscale = list(c(0, '#f39c12'), c(1, '#e74c3c'))),
               text = ~paste(Missing_Count, "missing"),
               hovertemplate = "<b>%{x}</b><br>Missing: %{y:.1f}%<br>%{text}<extra></extra>") %>%
          layout(title = "Missing Values by Variable", xaxis = list(title = "", tickangle = -45),
                yaxis = list(title = "Missing Percentage (%)"))
      }
    })
    
    # Summary Statistics
    output$numeric_stats <- renderDT({
      req(data_reactive()$data)
      data <- data_reactive()$data
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      
      if (length(numeric_vars) == 0) {
        return(datatable(data.frame(Message = "No numeric variables found")))
      }
      
      stats <- data.frame(
        Variable = numeric_vars,
        Min = sapply(data[numeric_vars], function(x) round(min(x, na.rm = TRUE), 2)),
        Max = sapply(data[numeric_vars], function(x) round(max(x, na.rm = TRUE), 2)),
        Mean = sapply(data[numeric_vars], function(x) round(mean(x, na.rm = TRUE), 2)),
        Median = sapply(data[numeric_vars], function(x) round(median(x, na.rm = TRUE), 2)),
        Std_Dev = sapply(data[numeric_vars], function(x) round(sd(x, na.rm = TRUE), 2)),
        Q1 = sapply(data[numeric_vars], function(x) round(quantile(x, 0.25, na.rm = TRUE), 2)),
        Q3 = sapply(data[numeric_vars], function(x) round(quantile(x, 0.75, na.rm = TRUE), 2)),
        stringsAsFactors = FALSE
      )
      datatable(stats, options = list(scrollX = TRUE), rownames = FALSE)
    })
    
    output$categorical_stats <- renderDT({
      req(data_reactive()$data)
      data <- data_reactive()$data
      cat_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
      
      if (length(cat_vars) == 0) {
        return(datatable(data.frame(Message = "No categorical variables found")))
      }
      
      stats <- data.frame(
        Variable = cat_vars,
        Unique_Values = sapply(data[cat_vars], function(x) length(unique(x))),
        Mode = sapply(data[cat_vars], function(x) { tbl <- table(x); names(tbl)[which.max(tbl)] }),
        Mode_Freq = sapply(data[cat_vars], function(x) { tbl <- table(x); max(tbl) }),
        Mode_Pct = sapply(data[cat_vars], function(x) { tbl <- table(x); round(max(tbl) / sum(tbl) * 100, 2) }),
        stringsAsFactors = FALSE
      )
      datatable(stats, options = list(scrollX = TRUE), rownames = FALSE)
    })
    
    # Histogram
    output$histogram_plot <- renderPlotly({
      req(data_reactive()$data, input$hist_var)
      data <- data_reactive()$data
      validate(need(input$hist_var %in% names(data), "Please select a valid variable"))
      
      p <- ggplot(data, aes_string(x = input$hist_var)) +
        geom_histogram(bins = input$hist_bins, fill = "#3498db", color = "white", alpha = 0.8) +
        labs(title = paste("Distribution of", input$hist_var), x = input$hist_var, y = "Frequency") +
        theme_minimal()
      
      if (input$hist_density) {
        p <- p + geom_density(aes(y = after_stat(count)), color = "#e74c3c", linewidth = 1)
      }
      ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
    })
    
    # Boxplot
    output$boxplot_plot <- renderPlotly({
      req(data_reactive()$data, input$box_var)
      data <- data_reactive()$data
      validate(need(input$box_var %in% names(data), "Please select a valid variable"))
      
      if (is.null(input$box_group) || input$box_group == "") {
        p <- ggplot(data, aes_string(y = input$box_var)) +
          geom_boxplot(fill = "#f39c12", color = "#2c3e50", alpha = 0.8)
      } else {
        p <- ggplot(data, aes_string(x = input$box_group, y = input$box_var, fill = input$box_group)) +
          geom_boxplot(alpha = 0.8) + scale_fill_brewer(palette = "Set2") +
          theme(legend.position = "none")
      }
      
      p <- p + labs(title = paste("Boxplot of", input$box_var), y = input$box_var) + theme_minimal()
      ggplotly(p)
    })
    
    # Correlation Heatmap
    output$correlation_heatmap <- renderPlotly({
      req(data_reactive()$data)
      data <- data_reactive()$data
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      validate(need(length(numeric_vars) >= 2, "Need at least 2 numeric variables for correlation"))
      
      cor_matrix <- cor(data[numeric_vars], use = "pairwise.complete.obs", method = input$corr_method)
      
      plot_ly(x = colnames(cor_matrix), y = rownames(cor_matrix), z = cor_matrix, type = "heatmap",
             colorscale = list(c(0, '#3498db'), c(0.5, '#ecf0f1'), c(1, '#e74c3c')),
             zmin = -1, zmax = 1,
             text = if(input$corr_values) round(cor_matrix, 2) else NULL,
             texttemplate = if(input$corr_values) "%{text}" else NULL,
             hovertemplate = "<b>%{y}</b> vs <b>%{x}</b><br>Correlation: %{z:.3f}<extra></extra>") %>%
        layout(title = paste(input$corr_method, "Correlation Heatmap"),
              xaxis = list(title = "", tickangle = -45), yaxis = list(title = ""))
    })
    
    # Bar Chart
    output$bar_chart <- renderPlotly({
      req(data_reactive()$data, input$bar_var)
      data <- data_reactive()$data
      validate(need(input$bar_var %in% names(data), "Please select a valid variable"))
      
      freq_table <- as.data.frame(table(data[[input$bar_var]]))
      names(freq_table) <- c("Category", "Frequency")
      freq_table <- freq_table[order(-freq_table$Frequency), ]
      
      if (input$bar_type == "bar") {
        plot_ly(data = freq_table, x = ~reorder(Category, -Frequency), y = ~Frequency,
               type = "bar", marker = list(color = "#2ecc71"),
               text = ~paste(round(Frequency / sum(Frequency) * 100, 1), "%"),
               textposition = "outside",
               hovertemplate = "<b>%{x}</b><br>Count: %{y}<br>%{text}<extra></extra>") %>%
          layout(title = paste("Distribution of", input$bar_var),
                xaxis = list(title = input$bar_var, tickangle = -45), yaxis = list(title = "Frequency"))
      } else {
        plot_ly(data = freq_table, labels = ~Category, values = ~Frequency, type = "pie",
               textinfo = "label+percent",
               hovertemplate = "<b>%{label}</b><br>Count: %{value}<br>Percentage: %{percent}<extra></extra>") %>%
          layout(title = paste("Distribution of", input$bar_var))
      }
    })
    
    # Download Handlers
    output$download_hist <- downloadHandler(
      filename = function() paste("histogram_", input$hist_var, ".png", sep = ""),
      content = function(file) {
        req(data_reactive()$data, input$hist_var)
        p <- ggplot(data_reactive()$data, aes_string(x = input$hist_var)) +
          geom_histogram(bins = input$hist_bins, fill = "#3498db", color = "white") +
          labs(title = paste("Distribution of", input$hist_var)) +
          theme_minimal()
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
    
    output$download_box <- downloadHandler(
      filename = function() paste("boxplot_", input$box_var, ".png", sep = ""),
      content = function(file) {
        req(data_reactive()$data, input$box_var)
        p <- ggplot(data_reactive()$data, aes_string(y = input$box_var)) +
          geom_boxplot(fill = "#f39c12") +
          labs(title = paste("Boxplot of", input$box_var)) +
          theme_minimal()
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
    
    output$download_corr <- downloadHandler(
      filename = function() "correlation_heatmap.png",
      content = function(file) {
        req(data_reactive()$data)
        numeric_vars <- names(data_reactive()$data)[sapply(data_reactive()$data, is.numeric)]
        cor_matrix <- cor(data_reactive()$data[numeric_vars], use = "complete.obs")
        png(file, width = 800, height = 800)
        corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                          addCoef.col = "black", tl.col = "black")
        dev.off()
      }
    )
    
    output$download_bar <- downloadHandler(
      filename = function() paste("barchart_", input$bar_var, ".png", sep = ""),
      content = function(file) {
        req(data_reactive()$data, input$bar_var)
        freq_table <- as.data.frame(table(data_reactive()$data[[input$bar_var]]))
        names(freq_table) <- c("Category", "Frequency")
        p <- ggplot(freq_table, aes(x = reorder(Category, -Frequency), y = Frequency)) +
          geom_bar(stat = "identity", fill = "#2ecc71") +
          labs(title = paste("Distribution of", input$bar_var), x = input$bar_var) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
  })
}
