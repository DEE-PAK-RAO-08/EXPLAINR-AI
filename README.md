# EXPLAINR-AI

## Explainable AI Web Application for Machine Learning Interpretation

![R](https://img.shields.io/badge/R-4.0+-blue.svg)
![Shiny](https://img.shields.io/badge/Shiny-1.7+-green.svg)
![License](https://img.shields.io/badge/License-MIT-yellow.svg)
![Academic](https://img.shields.io/badge/Type-Academic%20Capstone-purple.svg)

---

## 📋 Overview

**EXPLAINR-AI** is a comprehensive R Shiny web application designed for academic capstone projects. It demonstrates proficiency in R programming, machine learning, and explainable AI techniques. The application enables users to:

- Upload and explore datasets through interactive EDA
- Train interpretable machine learning models
- Understand model decisions through global and local explanations

---

## 🎯 Features

### Module 1: Data Preparation & EDA
- ✅ CSV file upload with automatic type detection
- ✅ Interactive data preview with DT tables
- ✅ Missing value analysis and visualization
- ✅ Summary statistics for numeric and categorical variables
- ✅ Interactive visualizations (histograms, boxplots, correlation heatmaps, bar charts)
- ✅ Data preprocessing (missing value treatment, normalization, feature selection)

### Module 2: ML Model Development
- ✅ Target variable selection with automatic problem type detection
- ✅ Multiple interpretable models:
  - **Classification**: Logistic Regression, Decision Tree, Random Forest, Naive Bayes
  - **Regression**: Linear Regression, Decision Tree, Random Forest, SVM
- ✅ Configurable train-test split
- ✅ Comprehensive performance metrics:
  - **Classification**: Accuracy, Precision, Recall, F1-Score, Confusion Matrix
  - **Regression**: RMSE, MAE, R², MAPE

### Module 3: Explainable AI
- ✅ **Global Explanations**:
  - Feature Importance (model-based and permutation)
  - Partial Dependence Plots (PDP)
- ✅ **Local Explanations**:
  - LIME (Local Interpretable Model-agnostic Explanations)
  - SHAP Values (Shapley Additive exPlanations)

---

## 🛠️ Technology Stack

| Category | Packages |
|----------|----------|
| **Web Framework** | shiny, shinydashboard, shinyWidgets, shinyjs |
| **Data Manipulation** | tidyverse, dplyr, tidyr |
| **Visualization** | ggplot2, plotly, corrplot, DT |
| **Machine Learning** | caret, rpart, randomForest, e1071 |
| **Explainable AI** | DALEX, iml, lime |
| **Metrics** | pROC, Metrics |

---

## 📁 Project Structure

```
EXPLAINR-AI/
├── app.R                      # Main application entry point
├── global.R                   # Global configuration and utilities
├── ui.R                       # User interface components
├── server.R                   # Server logic orchestration
├── setup.R                    # Package installation script
├── README.md                  # This file
│
└── modules/
    ├── mod_data_eda.R         # Data upload UI components
    ├── mod_data_eda_ui2.R     # EDA visualizations UI
    ├── mod_data_server.R      # Data module server logic
    ├── mod_eda_viz_server.R   # EDA visualization server
    ├── mod_ml_model.R         # ML model configuration & training
    ├── mod_ml_metrics.R       # ML metrics display
    ├── mod_xai.R              # XAI global explanations
    └── mod_xai_local.R        # XAI local explanations (LIME, SHAP)
```

---

## 🚀 Quick Start

### Prerequisites
- R version 4.0 or higher
- RStudio (recommended)

### Installation

1. **Clone or Download** the repository to your local machine

2. **Open RStudio** and set the working directory:
   ```r
   setwd("path/to/EXPLAINR-AI")
   ```

3. **Install Required Packages** by running the setup script:
   ```r
   source("setup.R")
   ```
   This will automatically install all required packages.

4. **Launch the Application**:
   ```r
   source("app.R")
   ```
   OR
   ```r
   shiny::runApp()
   ```

5. **Access the Application** in your web browser at the displayed URL (typically `http://127.0.0.1:XXXX`)

---

## 📖 User Guide

### Step 1: Upload Data
- Navigate to **"Data Upload & EDA" → "Upload Data"**
- Upload a CSV file or click a sample dataset button
- Maximum file size: 10MB
- Maximum rows: 10,000 (for performance)

### Step 2: Explore Data
- View dataset overview in **"Dataset Overview"**
- Check summary statistics in **"Summary Statistics"**
- Create visualizations in **"Visualizations"**
- Apply preprocessing in **"Preprocessing"**

### Step 3: Train Model
- Go to **"Model Training" → "Configure & Train"**
- Select your target variable
- The system auto-detects classification vs regression
- Choose a model type and training ratio
- Click **"Train Model"**

### Step 4: Evaluate Performance
- View metrics in **"Performance Metrics"**
- For classification: Accuracy, Precision, Recall, F1, Confusion Matrix
- For regression: RMSE, MAE, R², predictions plot

### Step 5: Generate Explanations
- **Global Explanations**: Feature importance and PDP
- **Local Explanations**: Select an observation, run LIME and SHAP

---

## 📊 Supported Data Formats

- **File Format**: CSV only
- **Encoding**: UTF-8 recommended
- **Missing Values**: Recognized patterns: "", "NA", "N/A", "null", "NULL"
- **Data Types**: Numeric, Factor, Character automatically detected

---

## 🎓 Academic Value

This project demonstrates:

1. **R Programming Proficiency**
   - Modular code organization
   - Reactive programming patterns
   - Package integration

2. **Machine Learning Implementation**
   - caret unified training interface
   - Multiple algorithm support
   - Cross-validation

3. **Explainable AI Application**
   - DALEX framework integration
   - Model-agnostic explanations
   - Visual interpretation methods

4. **Web Application Development**
   - Shiny framework expertise
   - Dashboard design
   - Interactive visualizations

5. **Data Science Workflow**
   - End-to-end pipeline
   - Preprocessing automation
   - Performance evaluation

---

## 🔧 Troubleshooting

### Common Issues

1. **Package Installation Errors**
   ```r
   # Try installing problematic packages individually
   install.packages("package_name", dependencies = TRUE)
   ```

2. **Memory Issues with Large Datasets**
   - Reduce dataset size before upload
   - Close other applications
   - Increase R memory limit

3. **Model Training Errors**
   - Ensure no missing values in target variable
   - Check that categorical variables are factors
   - Reduce complexity for small datasets

4. **XAI Errors**
   - Train a model first
   - Ensure sufficient test data exists
   - Try with fewer features selected

---

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 👥 Authors

**Academic Capstone Project - 2026**

---

## 🙏 Acknowledgments

- [Shiny](https://shiny.rstudio.com/) - Web application framework
- [DALEX](https://github.com/ModelOriented/DALEX) - Explainable AI
- [caret](https://topepo.github.io/caret/) - Machine learning framework
- [plotly](https://plotly.com/r/) - Interactive visualizations

---

## 📞 Support

For questions or issues related to this academic project, please refer to the **Help & Guide** section within the application.

---

*Built with ❤️ using R and Shiny*
