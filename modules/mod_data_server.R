# =============================================================================
# EXPLAINR-AI: Module 1 Part 3 - Data Module Server Logic
# =============================================================================

#' Data Module Server
dataModuleServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Values
    rv <- reactiveValues(
      original_data = NULL,
      current_data = NULL,
      preprocess_log = character(0),
      data_loaded = FALSE
    )
    
    # File Upload Handler
    observeEvent(input$file_upload, {
      req(input$file_upload)
      cat("DEBUG: File upload triggered. File:", input$file_upload$name, "\n")
      
      tryCatch({
        if (input$file_upload$size > MAX_FILE_SIZE) {
          showNotification("File size exceeds 10MB limit!", type = "error", duration = 5)
          return()
        }
        
        data <- read.csv(input$file_upload$datapath, stringsAsFactors = TRUE,
                        na.strings = c("", "NA", "N/A", "null", "NULL"))
        
        cat("DEBUG: File read. Rows:", nrow(data), "Cols:", ncol(data), "\n")
        
        if (nrow(data) == 0) {
          showNotification("The uploaded file is empty!", type = "error")
          return()
        }
        
        if (nrow(data) > MAX_ROWS) {
          data <- data[1:MAX_ROWS, ]
          showNotification(paste("Dataset truncated to", MAX_ROWS, "rows for performance."),
                          type = "warning", duration = 5)
        }
        
        rv$original_data <- data
        rv$current_data <- data
        rv$preprocess_log <- paste(Sys.time(), "- Data loaded:", input$file_upload$name)
        rv$data_loaded <- TRUE
        
        cat("DEBUG: rv$current_data updated\n")
        update_select_inputs(data)
        
        showNotification(paste("Successfully loaded", nrow(data), "rows and", ncol(data), "columns!"),
                        type = "message", duration = 3)
        
      }, error = function(e) {
        cat("DEBUG: Error in file upload:", e$message, "\n")
        showNotification(paste("Error loading file:", e$message), type = "error", duration = 5)
      })
    })
    
    # Sample Data Loaders
    observeEvent(input$load_classification, {
      cat("DEBUG: Loading classification sample\n")
      data <- generate_sample_classification(500)
      rv$original_data <- data
      rv$current_data <- data
      rv$preprocess_log <- paste(Sys.time(), "- Loaded classification sample dataset")
      rv$data_loaded <- TRUE
      update_select_inputs(data)
      cat("DEBUG: Sample classification loaded. Rows:", nrow(data), "\n")
      showNotification("Classification sample dataset loaded!", type = "message")
    })
    
    observeEvent(input$load_regression, {
      cat("DEBUG: Loading regression sample\n")
      data <- generate_sample_regression(500)
      rv$original_data <- data
      rv$current_data <- data
      rv$preprocess_log <- paste(Sys.time(), "- Loaded regression sample dataset")
      rv$data_loaded <- TRUE
      update_select_inputs(data)
      cat("DEBUG: Sample regression loaded. Rows:", nrow(data), "\n")
      showNotification("Regression sample dataset loaded!", type = "message")
    })
    
    # Helper Function to Update Select Inputs
    update_select_inputs <- function(data) {
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
      all_vars <- names(data)
      
      updateSelectInput(session, "hist_var", choices = numeric_vars, selected = numeric_vars[1])
      updateSelectInput(session, "box_var", choices = numeric_vars, selected = numeric_vars[1])
      updateSelectInput(session, "box_group", choices = c("None" = "", categorical_vars))
      updateSelectInput(session, "bar_var", choices = categorical_vars, selected = categorical_vars[1])
      updateSelectInput(session, "norm_vars", choices = numeric_vars)
      updateCheckboxGroupInput(session, "exclude_vars", choices = all_vars)
    }
    
    # Data Preview Outputs
    output$data_info <- renderUI({
      req(rv$current_data)
      data <- rv$current_data
      
      tagList(
        fluidRow(
          column(4, tags$b("Dimensions: "), paste(nrow(data), "rows ×", ncol(data), "columns")),
          column(4, tags$b("Memory: "), format(object.size(data), units = "MB")),
          column(4, tags$b("Complete Cases: "), sum(complete.cases(data)), "of", nrow(data))
        )
      )
    })
    
    output$data_preview <- renderDT({
      req(rv$current_data)
      datatable(head(rv$current_data, 100),
               options = list(scrollX = TRUE, pageLength = 10, dom = 'lfrtip', lengthMenu = c(5, 10, 25, 50)),
               class = 'cell-border stripe hover', rownames = FALSE)
    })
    
    # Value Boxes
    output$vb_rows <- renderValueBox({
      req(rv$current_data)
      valueBox(formatC(nrow(rv$current_data), format = "d", big.mark = ","),
              "Total Rows", icon = icon("table"), color = "blue")
    })
    
    output$vb_cols <- renderValueBox({
      req(rv$current_data)
      valueBox(ncol(rv$current_data), "Total Columns", icon = icon("columns"), color = "green")
    })
    
    output$vb_numeric <- renderValueBox({
      req(rv$current_data)
      n_numeric <- sum(sapply(rv$current_data, is.numeric))
      valueBox(n_numeric, "Numeric Variables", icon = icon("calculator"), color = "yellow")
    })
    
    output$vb_categorical <- renderValueBox({
      req(rv$current_data)
      n_cat <- sum(sapply(rv$current_data, function(x) is.factor(x) | is.character(x)))
      valueBox(n_cat, "Categorical Variables", icon = icon("tags"), color = "purple")
    })
    
    # Column Information Table
    output$column_info <- renderDT({
      req(rv$current_data)
      data <- rv$current_data
      
      info <- data.frame(
        Column = names(data),
        Type = sapply(data, function(x) class(x)[1]),
        Unique_Values = sapply(data, function(x) length(unique(x))),
        Missing = sapply(data, function(x) sum(is.na(x))),
        Missing_Pct = sapply(data, function(x) round(sum(is.na(x)) / length(x) * 100, 2)),
        Sample_Values = sapply(data, function(x) paste(head(unique(na.omit(x)), 3), collapse = ", ")),
        stringsAsFactors = FALSE
      )
      
      datatable(info, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE) %>%
        formatStyle('Missing_Pct', backgroundColor = styleInterval(c(5, 20), c('#2ecc71', '#f39c12', '#e74c3c')))
    })
    
    # -------------------------------------------------------------------------
    # Preprocessing Logic
    # -------------------------------------------------------------------------
    
    # Missing Value Treatment
    observeEvent(input$apply_missing, {
      req(rv$current_data)
      data <- rv$current_data
      
      tryCatch({
        if (input$missing_method == "remove") {
          new_data <- na.omit(data)
          msg <- paste("Removed", nrow(data) - nrow(new_data), "rows with missing values")
        } else if (input$missing_method == "mean") {
          new_data <- data
          numeric_cols <- sapply(new_data, is.numeric)
          new_data[numeric_cols] <- lapply(new_data[numeric_cols], function(x) {
            x[is.na(x)] <- mean(x, na.rm = TRUE)
            x
          })
          msg <- "Applied mean imputation to numeric variables"
        } else if (input$missing_method == "median") {
          new_data <- data
          numeric_cols <- sapply(new_data, is.numeric)
          new_data[numeric_cols] <- lapply(new_data[numeric_cols], function(x) {
            x[is.na(x)] <- median(x, na.rm = TRUE)
            x
          })
          msg <- "Applied median imputation to numeric variables"
        } else if (input$missing_method == "mode") {
          new_data <- data
          for (col in names(new_data)) {
            if (any(is.na(new_data[[col]]))) {
              mode_val <- names(sort(table(new_data[[col]]), decreasing = TRUE))[1]
              if (is.numeric(new_data[[col]])) {
                new_data[[col]][is.na(new_data[[col]])] <- as.numeric(mode_val)
              } else {
                new_data[[col]][is.na(new_data[[col]])] <- mode_val
              }
            }
          }
          msg <- "Applied mode imputation to all variables"
        }
        
        rv$current_data <- new_data
        rv$preprocess_log <- c(rv$preprocess_log, paste(Sys.time(), "-", msg))
        showNotification(msg, type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Normalization
    observeEvent(input$apply_norm, {
      req(rv$current_data, input$norm_vars, input$norm_method)
      
      if (input$norm_method == "none" || length(input$norm_vars) == 0) return()
      
      tryCatch({
        data <- rv$current_data
        vars <- input$norm_vars
        
        for (var in vars) {
          if (input$norm_method == "minmax") {
            min_val <- min(data[[var]], na.rm = TRUE)
            max_val <- max(data[[var]], na.rm = TRUE)
            if (max_val > min_val) {
              data[[var]] <- (data[[var]] - min_val) / (max_val - min_val)
            }
          } else if (input$norm_method == "zscore") {
            data[[var]] <- scale(data[[var]])[, 1]
          } else if (input$norm_method == "log") {
            if (min(data[[var]], na.rm = TRUE) > 0) {
              data[[var]] <- log(data[[var]])
            } else {
              showNotification(paste("Cannot apply log to", var, "- contains non-positive values"), type = "warning")
            }
          }
        }
        
        rv$current_data <- data
        msg <- paste("Applied", input$norm_method, "normalization to:", paste(vars, collapse = ", "))
        rv$preprocess_log <- c(rv$preprocess_log, paste(Sys.time(), "-", msg))
        showNotification("Normalization applied!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Exclude Variables
    observeEvent(input$apply_exclude, {
      req(rv$current_data, input$exclude_vars)
      
      if (length(input$exclude_vars) == 0) return()
      
      tryCatch({
        data <- rv$current_data
        vars_to_remove <- input$exclude_vars
        data <- data[, !(names(data) %in% vars_to_remove), drop = FALSE]
        
        rv$current_data <- data
        update_select_inputs(data)
        
        msg <- paste("Removed variables:", paste(vars_to_remove, collapse = ", "))
        rv$preprocess_log <- c(rv$preprocess_log, paste(Sys.time(), "-", msg))
        showNotification(msg, type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Reset Data
    observeEvent(input$reset_data, {
      req(rv$original_data)
      rv$current_data <- rv$original_data
      update_select_inputs(rv$original_data)
      rv$preprocess_log <- c(rv$preprocess_log, paste(Sys.time(), "- Data reset to original"))
      showNotification("Data reset to original!", type = "message")
    })
    
    # Preprocessing Log Output
    output$preprocess_log <- renderText({
      if (length(rv$preprocess_log) == 0) {
        "No preprocessing operations performed yet."
      } else {
        paste(rv$preprocess_log, collapse = "\n")
      }
    })
    
    # Download Processed Data
    output$download_processed <- downloadHandler(
      filename = function() {
        paste("processed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(rv$current_data, file, row.names = FALSE)
      }
    )
    
    # Return reactive values for use in other modules
    return(reactive({
      list(data = rv$current_data, original = rv$original_data, loaded = rv$data_loaded)
    }))
  })
}
