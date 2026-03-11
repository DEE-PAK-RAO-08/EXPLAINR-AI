options(repos=c(CRAN='https://cloud.r-project.org/'))
lib_path <- 'c:/Users/LENOVO/Downloads/EXPLAINR-AI/r_library'
if (!dir.exists(lib_path)) dir.create(lib_path)

required_packages <- c(
  'shiny', 'shinydashboard', 'shinyWidgets', 'shinyjs', 'shinyBS',
  'tidyverse', 'dplyr', 'tidyr', 'readr', 'ggplot2', 'plotly',
  'corrplot', 'scales', 'reshape2', 'RColorBrewer', 'DT',
  'caret', 'rpart', 'rpart.plot', 'randomForest', 'e1071',
  'DALEX', 'iml', 'lime', 'pROC', 'Metrics', 'shapviz', 'kernelshap'
)

missing_packages <- required_packages[!(required_packages %in% installed.packages(lib.loc=lib_path)[,'Package'])]

if (length(missing_packages) > 0) {
  cat('Installing missing packages:', paste(missing_packages, collapse=', '), '\n')
  install.packages(missing_packages, lib=lib_path, dependencies=TRUE)
} else {
  cat('All packages are already installed.\n')
}
