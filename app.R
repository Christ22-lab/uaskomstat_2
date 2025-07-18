# ===================================================================
# DASHBOARD STATISTIK TERPADU - ANALISIS DATA KOMPREHENSIF (MODERN UI)
# ===================================================================
# 
# Dashboard R Shiny untuk analisis statistik lengkap
# Data: SOVI (Social Vulnerability Index) 
# Sumber: https://www.sciencedirect.com/science/article/pii/S2352340921010180
#
# Dikembangkan dengan R Shiny - VERSI MODERN UI
# ===================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(corrplot)
library(leaflet)
library(htmlwidgets)
library(knitr)
library(rmarkdown)
library(openxlsx)
library(VIM)
library(mice)
library(nortest)
library(car)
library(broom)
library(dplyr)
library(gridExtra)
library(sf)
library(maps)
library(moments)
library(officer)
library(haven)  # For reading SPSS files

# Global variables
sovi_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
distance_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
metadata_url <- "https://www.sciencedirect.com/science/article/pii/S2352340921010180"

# Load data function
load_data <- function() {
  tryCatch({
    sovi_data <- read.csv(sovi_url, stringsAsFactors = FALSE)
    distance_data <- read.csv(distance_url, stringsAsFactors = FALSE)
    list(sovi = sovi_data, distance = distance_data)
  }, error = function(e) {
    # Fallback: create sample data if URL fails
    warning("Could not load data from URL, using sample data")
    n <- 100
    sovi_sample <- data.frame(
      ID = 1:n,
      State = sample(c("CA", "TX", "FL", "NY", "PA"), n, replace = TRUE),
      County = paste("County", 1:n),
      Population = rnorm(n, 50000, 15000),
      Income = rnorm(n, 45000, 12000),
      Education = rnorm(n, 85, 10),
      Age_65_Over = rnorm(n, 15, 5),
      Disability = rnorm(n, 12, 3),
      SOVI_Score = rnorm(n, 0, 1)
    )
    distance_sample <- matrix(runif(n*n), nrow = n)
    list(sovi = sovi_sample, distance = distance_sample)
  })
}

# Load data at startup
data_list <- load_data()
original_data <- data_list$sovi
distance_matrix <- data_list$distance

# Helper functions
create_interpretation <- function(test_result, test_type) {
  switch(test_type,
         "normality" = {
           if (test_result$p.value > 0.05) {
             "Interpretasi: Data mengikuti distribusi normal (p > 0.05). Asumsi normalitas terpenuhi untuk analisis parametrik."
           } else {
             "Interpretasi: Data tidak mengikuti distribusi normal (p ≤ 0.05). Pertimbangkan transformasi data atau gunakan uji non-parametrik."
           }
         },
         "homogeneity" = {
           if (test_result$p.value > 0.05) {
             "Interpretasi: Varians antar kelompok homogen (p > 0.05). Asumsi homogenitas varians terpenuhi."
           } else {
             "Interpretasi: Varians antar kelompok tidak homogen (p ≤ 0.05). Pertimbangkan transformasi data atau gunakan uji yang robust terhadap heteroskedastisitas."
           }
         },
         "t_test" = {
           if (test_result$p.value < 0.05) {
             paste0("Interpretasi: Terdapat perbedaan signifikan (p = ", round(test_result$p.value, 4), 
                    "). Tolak H₀, terima H₁. Rata-rata kedua kelompok berbeda secara statistik.")
           } else {
             paste0("Interpretasi: Tidak terdapat perbedaan signifikan (p = ", round(test_result$p.value, 4), 
                    "). Gagal tolak H₀. Rata-rata kedua kelompok tidak berbeda secara statistik.")
           }
         },
         "anova" = {
           if (test_result$`Pr(>F)`[1] < 0.05) {
             paste0("Interpretasi: Terdapat perbedaan signifikan antar kelompok (p = ", round(test_result$`Pr(>F)`[1], 4), 
                    "). Minimal ada satu kelompok yang berbeda.")
           } else {
             paste0("Interpretasi: Tidak terdapat perbedaan signifikan antar kelompok (p = ", round(test_result$`Pr(>F)`[1], 4), 
                    "). Semua kelompok memiliki rata-rata yang sama.")
           }
         }
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Statistik Terpadu - SOVI Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "data_management", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "exploration", icon = icon("chart-line"),
               menuSubItem("Statistik Deskriptif", tabName = "descriptive"),
               menuSubItem("Visualisasi", tabName = "visualization"),
               menuSubItem("Peta", tabName = "mapping")
      ),
      menuItem("Uji Asumsi", tabName = "assumptions", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inference", icon = icon("calculator"),
               menuSubItem("Uji Rata-rata", tabName = "mean_tests"),
               menuSubItem("Uji Proporsi & Varians", tabName = "prop_var_tests"),
               menuSubItem("ANOVA", tabName = "anova_tests")
      ),
      menuItem("Regresi Linear", tabName = "regression", icon = icon("line-chart")),
      menuItem("Metadata", tabName = "metadata", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* ===== MODERN CSS STYLING ===== */
        
        /* Import Google Fonts */
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap');
        
        /* Root Variables for Consistent Theming */
        :root {
          --primary-color: #667eea;
          --primary-dark: #5a67d8;
          --secondary-color: #764ba2;
          --accent-color: #f093fb;
          --success-color: #48bb78;
          --warning-color: #ed8936;
          --error-color: #f56565;
          --info-color: #4299e1;
          --dark-color: #2d3748;
          --light-color: #f7fafc;
          --gray-50: #f9fafb;
          --gray-100: #f3f4f6;
          --gray-200: #e5e7eb;
          --gray-300: #d1d5db;
          --gray-400: #9ca3af;
          --gray-500: #6b7280;
          --gray-600: #4b5563;
          --gray-700: #374151;
          --gray-800: #1f2937;
          --gray-900: #111827;
          --shadow-sm: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
          --shadow-md: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
          --shadow-lg: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05);
          --shadow-xl: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04);
          --border-radius: 12px;
          --border-radius-sm: 8px;
          --border-radius-lg: 16px;
          --transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        }
        
        /* Global Styles */
        * {
          box-sizing: border-box;
        }
        
        body {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          font-weight: 400;
          line-height: 1.6;
          color: var(--gray-700);
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          min-height: 100vh;
        }
        
        /* Dashboard Layout */
        .content-wrapper, .right-side {
          background: #f8fafc !important;
          min-height: 100vh;
        }
        
        /* Force consistent background */
        .main-content {
          background: #f8fafc !important;
        }
        
        /* Header Styling */
        .main-header {
          background: linear-gradient(135deg, var(--primary-color) 0%, var(--secondary-color) 100%) !important;
          border: none !important;
          box-shadow: var(--shadow-lg);
        }
        
        .main-header .navbar {
          background: transparent !important;
        }
        
        .main-header .navbar-brand {
          color: white !important;
          font-weight: 600;
          font-size: 1.25rem;
        }
        
        /* Sidebar Styling */
        .main-sidebar {
          background: white !important;
          box-shadow: var(--shadow-xl);
          border-right: 1px solid var(--gray-200);
        }
        
        .sidebar-menu > li > a {
          color: var(--gray-700) !important;
          font-weight: 500;
          padding: 10px 16px;
          border-radius: var(--border-radius-sm);
          margin: 2px 6px;
          transition: var(--transition);
          font-size: 14px; /* Standardized font size */
        }
        
        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
          background: linear-gradient(135deg, var(--primary-color), var(--secondary-color)) !important;
          color: white !important;
          transform: translateX(4px);
          box-shadow: var(--shadow-md);
        }
        
        .sidebar-menu > li > a > .fa,
        .sidebar-menu > li > a > .glyphicon,
        .sidebar-menu > li > a > .ion {
          margin-right: 12px;
          font-size: 14px;
        }
        
        /* Fix for sub-menu background and font size */
        .treeview-menu {
          background: white !important; /* Ensure sub-menu background is white */
          padding-left: 0; /* Remove default padding */
        }

        .treeview-menu > li > a {
          color: var(--gray-600) !important;
          padding: 8px 20px 8px 40px;
          font-size: 13px; /* Standardized font size for sub-menu items */
          transition: var(--transition);
          border-radius: 0 !important; /* Remove border-radius for sub-menu items */
          margin: 0; /* Remove margin for sub-menu items */
        }
        
        .treeview-menu > li > a:hover,
        .treeview-menu > li.active > a {
          background: var(--gray-100) !important;
          color: var(--primary-color) !important;
          border-left: 3px solid var(--primary-color);
          transform: none; /* Remove transform for sub-menu items */
          box-shadow: none; /* Remove shadow for sub-menu items */
        }
        
        /* Box/Card Styling - Simplified for browser consistency */
        .box {
          background: white !important;
          border: 1px solid #e5e7eb !important;
          border-radius: 12px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06) !important;
          margin-bottom: 20px;
          overflow: hidden;
          transition: all 0.3s ease;
        }
        
        .box:hover {
          box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05) !important;
          transform: translateY(-2px);
        }
        
        .box-header {
          border-bottom: 1px solid #e5e7eb !important;
          padding: 16px 20px;
          position: relative;
        }
        
        /* Default header for boxes without status */
        .box .box-header:not(.box-primary .box-header):not(.box-info .box-header):not(.box-success .box-header):not(.box-warning .box-header) {
          background: linear-gradient(135deg, #f9fafb 0%, white 100%) !important;
        }
        
        .box-header.with-border {
          border-bottom: 2px solid var(--primary-color);
        }
        
        .box-title {
          font-size: 1.25rem;
          font-weight: 600;
          color: var(--gray-800);
          margin: 0;
        }
        
        .box-body {
          padding: 16px;
        }
        
        /* Status Colors for Boxes */
        .box-primary .box-header {
          background: linear-gradient(135deg, var(--primary-color), var(--primary-dark)) !important;
          color: white !important;
          border-bottom: 1px solid var(--primary-dark) !important;
        }
        
        .box-primary .box-title {
          color: white !important;
        }
        
        .box-info .box-header {
          background: linear-gradient(135deg, var(--info-color), #3182ce) !important;
          color: white !important;
          border-bottom: 1px solid #3182ce !important;
        }
        
        .box-info .box-title {
          color: white !important;
        }
        
        .box-success .box-header {
          background: linear-gradient(135deg, var(--success-color), #38a169) !important;
          color: white !important;
          border-bottom: 1px solid #38a169 !important;
        }
        
        .box-success .box-title {
          color: white !important;
        }
        
        .box-warning .box-header {
          background: linear-gradient(135deg, var(--warning-color), #dd6b20) !important;
          color: white !important;
          border-bottom: 1px solid #dd6b20 !important;
        }
        
        .box-warning .box-title {
          color: white !important;
        }
        
        /* Button Styling */
        .btn {
          font-weight: 500;
          border-radius: var(--border-radius-sm);
          padding: 10px 20px;
          font-size: 0.95rem; /* Standardized smaller font size */
          border: none;
          cursor: pointer;
          transition: var(--transition);
          text-transform: none;
          letter-spacing: 0.025em;
        }
        
        .btn:hover {
          transform: translateY(-2px);
          box-shadow: var(--shadow-lg);
        }
        
        .btn:active {
          transform: translateY(0);
        }
        
        .btn-primary {
          background: linear-gradient(135deg, var(--primary-color), var(--primary-dark));
          color: white;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, var(--primary-dark), var(--secondary-color));
          color: white;
        }
        
        .btn-success {
          background: linear-gradient(135deg, var(--success-color), #38a169);
          color: white;
        }
        
        .btn-success:hover {
          background: linear-gradient(135deg, #38a169, #2f855a);
          color: white;
        }
        
        .btn-warning {
          background: linear-gradient(135deg, var(--warning-color), #dd6b20);
          color: white;
        }
        
        .btn-warning:hover {
          background: linear-gradient(135deg, #dd6b20, #c05621);
          color: white;
        }
        
        .btn-info {
          background: linear-gradient(135deg, var(--info-color), #3182ce);
          color: white;
        }
        
        .btn-info:hover {
          background: linear-gradient(135deg, #3182ce, #2c5aa0);
          color: white;
        }
        
        .btn-lg {
          padding: 16px 32px;
          font-size: 1.15rem; /* Adjusted large button font size */
          border-radius: var(--border-radius);
        }
        
        /* Form Controls */
        .form-control {
          border: 2px solid var(--gray-200);
          border-radius: var(--border-radius-sm);
          padding: 10px 14px;
          font-size: 0.95rem; /* Standardized smaller font size */
          transition: var(--transition);
          background: white;
        }
        
        .form-control:focus {
          border-color: var(--primary-color);
          box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
          outline: none;
        }
        
        .form-group label {
          font-weight: 500;
          color: var(--gray-700);
          margin-bottom: 6px;
          font-size: 0.95rem; /* Standardized smaller font size */
        }
        
        /* Select2 Styling */
        .select2-container--default .select2-selection--single {
          border: 2px solid var(--gray-200) !important;
          border-radius: var(--border-radius-sm) !important;
          height: 44px !important;
          padding: 8px 12px !important;
          font-size: 1.15rem; /* Adjusted select2 font size */
        }
        
        .select2-container--default .select2-selection--single:focus {
          border-color: var(--primary-color) !important;
        }
        
        .select2-container--default .select2-selection--multiple {
          border: 2px solid var(--gray-200) !important;
          border-radius: var(--border-radius-sm) !important;
          min-height: 44px !important;
          font-size: 1.15rem; /* Adjusted select2 font size */
        }
        
        /* DataTable Styling */
        .dataTables_wrapper {
          font-family: inherit;
        }
        
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          color: var(--gray-600);
          font-size: 1.15rem; /* Adjusted data table info/pagination font size */
        }
        
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          border-radius: var(--border-radius-sm) !important;
          margin: 0 2px;
          transition: var(--transition);
        }
        
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background: var(--primary-color) !important;
          color: white !important;
          border-color: var(--primary-color) !important;
        }
        
        table.dataTable thead th {
          background: var(--gray-50);
          color: var(--gray-700);
          font-weight: 600;
          border-bottom: 2px solid var(--gray-200);
          padding: 16px 12px;
          font-size: 1.15rem; /* Adjusted data table header font size */
        }
        
        table.dataTable tbody tr {
          transition: var(--transition);
        }
        
        table.dataTable tbody tr:hover {
          background: var(--gray-50);
        }
        
        table.dataTable tbody td {
          padding: 12px;
          border-bottom: 1px solid var(--gray-100);
          font-size: 1.15rem; /* Adjusted data table cell font size */
        }
        
        /* Interpretation Box */
        .interpretation-box {
          background: linear-gradient(135deg, #e6fffa 0%, #f0fff4 100%);
          border: 1px solid var(--success-color);
          border-left: 4px solid var(--success-color);
          border-radius: var(--border-radius);
          padding: 20px;
          margin: 16px 0;
          box-shadow: var(--shadow-sm);
        }
        
        .interpretation-box h5,
        .interpretation-box h6 {
          color: var(--gray-800);
          font-weight: 600;
          margin-bottom: 12px;
          font-size: 1.1.15rem; /* Adjusted interpretation box title font size */
        }
        
        .interpretation-box p,
        .interpretation-box ul,
        .interpretation-box li {
          color: var(--gray-700);
          font-size: 1.15rem; /* Adjusted font size for interpretation text */
          line-height: 1.6;
        }
        .interpretation-box ul {
          padding-left: 20px;
          margin-top: 8px;
        }
        .interpretation-box li {
          margin-bottom: 4px;
        }
        
        /* Feature Cards on Home Page */
        .feature-card {
          background: white !important;
          border-radius: 12px !important;
          padding: 20px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06) !important;
          transition: all 0.3s ease !important;
          border-left: 4px solid #667eea !important;
          height: 100%;
          border: 1px solid #e5e7eb !important;
        }
        
        .feature-card:hover {
          transform: translateY(-4px);
          box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04) !important;
        }
        
        .feature-card h5 {
          color: var(--gray-800);
          font-weight: 600;
          margin-bottom: 12px;
          font-size: 1.15rem; /* Adjusted feature card title font size */
        }
        
        .feature-card p {
          color: var(--gray-600);
          font-size: 1.15rem; /* Adjusted feature card text font size */
          line-height: 1.6;
          margin: 0;
        }
        
        /* Info Box Styling */
        .info-box {
          background: white;
          border-radius: var(--border-radius);
          padding: 20px;
          margin-bottom: 16px;
          border: 1px solid var(--gray-200);
          box-shadow: var(--shadow-sm);
        }
        
        .info-box h4,
        .info-box h5 {
          color: var(--gray-800);
          font-weight: 600;
          margin-bottom: 12px;
          font-size: 1.15rem; /* Adjusted info box title font size */
        }
        
        .info-box p,
        .info-box ul,
        .info-box li {
          color: var(--gray-600);
          font-size: 1.15rem; /* Adjusted font size for info text */
          margin-bottom: 8px;
          line-height: 1.6;
        }
        
        .info-box ul {
          padding-left: 20px;
        }
        
        .info-box ul li {
          margin-bottom: 4px;
        }
        
        /* Code and Verbatim Output */
        pre, code, .shiny-text-output { /* Added .shiny-text-output for verbatim text */
          font-family: 'JetBrains Mono', 'Fira Code', Consolas, monospace;
          background: var(--gray-50);
          border: 1px solid var(--gray-200);
          border-radius: var(--border-radius-sm);
          padding: 16px;
          font-size: 0.95rem; /* Adjusted code/output font size */
          line-height: 1.5;
          color: var(--gray-800);
          overflow-x: auto;
          white-space: pre-wrap; /* Allow text to wrap */
          word-wrap: break-word; /* Break long words */
        }
        
        /* Progress Bars */
        .progress {
          background: var(--gray-200);
          border-radius: var(--border-radius-sm);
          overflow: hidden;
        }
        
        .progress-bar {
          background: linear-gradient(135deg, var(--primary-color), var(--secondary-color));
          transition: var(--transition);
        }
        
        /* Alerts */
        .alert {
          border-radius: var(--border-radius);
          border: none;
          padding: 16px 20px;
          margin-bottom: 16px;
          font-weight: 500;
          font-size: 1.15rem; /* Adjusted alert font size */
        }
        
        .alert-info {
          background: linear-gradient(135deg, #e6f7ff 0%, #f0f9ff 100%);
          color: var(--info-color);
          border-left: 4px solid var(--info-color);
        }
        
        .alert-success {
          background: linear-gradient(135deg, #f0fff4 0%, #e6fffa 100%);
          color: var(--success-color);
          border-left: 4px solid var(--success-color);
        }
        
        .alert-warning {
          background: linear-gradient(135deg, #fffbeb 0%, #fef3c7 100%);
          color: var(--warning-color);
          border-left: 4px solid var(--warning-color);
        }
        
        /* Tabs */
        .nav-tabs {
          border-bottom: 2px solid var(--gray-200);
          margin-bottom: 20px;
        }
        
        .nav-tabs > li > a {
          border-radius: var(--border-radius-sm) var(--border-radius-sm) 0 0;
          color: var(--gray-600);
          font-weight: 500;
          transition: var(--transition);
          font-size: 1.15rem; /* Adjusted tab font size */
        }
        
        .nav-tabs > li.active > a,
        .nav-tabs > li > a:hover {
          background: var(--primary-color);
          color: white;
          border-color: var(--primary-color);
        }
        
        /* Responsive Design */
        @media (max-width: 768px) {
          .box-body {
            padding: 16px;
          }
          
          .btn {
            padding: 10px 20px;
            font-size: 0.95rem; /* Adjusted responsive button font size */
          }
          
          .btn-lg {
            padding: 14px 28px;
            font-size: 1.05rem; /* Adjusted responsive large button font size */
          }
          
          .feature-card {
            padding: 20px;
            margin-bottom: 16px;
          }
        }
        
        /* Loading Spinner */
        .shiny-spinner-output-container {
          display: flex;
          align-items: center;
          justify-content: center;
          min-height: 200px;
        }
        
        /* Custom Scrollbar */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        
        ::-webkit-scrollbar-track {
          background: var(--gray-100);
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: var(--gray-400);
          border-radius: 4px;
          transition: var(--transition);
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: var(--gray-500);
        }
        
        /* Plotly Container */
        .plotly {
          border-radius: var(--border-radius);
          overflow: hidden;
          box-shadow: var(--shadow-sm);
        }
        
        /* Leaflet Map */
        .leaflet-container {
          border-radius: var(--border-radius);
          box-shadow: var(--shadow-md);
        }
        
        /* Animation Classes */
        .fade-in {
          animation: fadeIn 0.5s ease-in;
        }
        
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        .slide-in {
          animation: slideIn 0.3s ease-out;
        }
        
        @keyframes slideIn {
          from { transform: translateX(-20px); opacity: 0; }
          to { transform: translateX(0); opacity: 1; }
        }
        
        /* Utility Classes */
        .text-gradient {
          background: linear-gradient(135deg, var(--primary-color), var(--secondary-color));
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
          font-weight: 600;
        }
        
        .bg-gradient {
          background: linear-gradient(135deg, var(--primary-color), var(--secondary-color));
        }
        
        .shadow-soft {
          box-shadow: var(--shadow-md);
        }
        
        .rounded-modern {
          border-radius: var(--border-radius);
        }
        
        /* Browser Compatibility Fixes */
        * {
          -webkit-font-smoothing: antialiased;
          -moz-osx-font-smoothing: grayscale;
        }
        
        /* Force consistent styling across browsers */
        .content-wrapper {
          background: #f8fafc !important;
        }
        
        .main-sidebar {
          background: white !important;
        }
        
        /* Ensure all boxes look consistent */
        .box, .info-box, .feature-card {
          border: 1px solid #e5e7eb !important;
          background: white !important;
        }
        
        /* Fix any dark theme overrides */
        body.dark-mode .content-wrapper,
        body.dark-mode .right-side {
          background: #f8fafc !important;
        }
        
        /* Dark mode support - Override to maintain light theme */
        @media (prefers-color-scheme: dark) {
          .content-wrapper, .right-side {
            background: #f8fafc !important;
            color: #374151 !important;
          }
          
          .box {
            background: white !important;
            border-color: #e5e7eb !important;
            color: #374151 !important;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06) !important;
          }
          
          /* Override all dark mode styles to maintain light theme */
          .form-control {
            background: white !important;
            border-color: #e5e7eb !important;
            color: #374151 !important;
          }
          .sidebar-menu > li > a {
            color: #374151 !important;
          }
          .sidebar-menu > li > a:hover,
          .sidebar-menu > li.active > a {
            background: linear-gradient(135deg, #667eea, #764ba2) !important;
            color: white !important;
          }
          .treeview-menu {
            background: white !important;
          }
          .treeview-menu > li > a {
            color: #6b7280 !important;
          }
          .treeview-menu > li > a:hover,
          .treeview-menu > li.active > a {
            background: #f3f4f6 !important;
            color: #667eea !important;
          }
          table.dataTable thead th {
            background: #f9fafb !important;
            color: #374151 !important;
            border-bottom-color: #e5e7eb !important;
          }
          table.dataTable tbody tr:hover {
            background: #f9fafb !important;
          }
          table.dataTable tbody td {
            border-bottom-color: #f3f4f6 !important;
          }
          .interpretation-box {
            background: linear-gradient(135deg, #e6fffa 0%, #f0fff4 100%) !important;
            border-color: #48bb78 !important;
            color: #374151 !important;
          }
          .interpretation-box h5, .interpretation-box h6 {
            color: #1f2937 !important;
          }
          .interpretation-box p, .interpretation-box ul, .interpretation-box li {
            color: #374151 !important;
          }
          .info-box {
            background: white !important;
            border-color: #e5e7eb !important;
            color: #374151 !important;
          }
          .info-box p, .info-box ul, .info-box li {
            color: #6b7280 !important;
          }
          pre, code, .shiny-text-output {
            background: #f9fafb !important;
            border-color: #e5e7eb !important;
            color: #1f2937 !important;
          }
        }
      "))
    ),
    
    tabItems(
      # =================== BERANDA ===================
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, title = "Selamat Datang di Dashboard Statistik Terpadu", status = "primary", solidHeader = TRUE,
                    div(class = "fade-in",
                        h3("Tentang Dashboard", class = "text-gradient"),
                        p("Dashboard Statistik Terpadu adalah aplikasi web interaktif yang dikembangkan untuk analisis data SOVI (Social Vulnerability Index) secara komprehensif. Dashboard ini menyediakan berbagai fitur analisis statistik mulai dari eksplorasi data dasar hingga analisis regresi yang kompleks."),
                        
                        h4("Data yang Digunakan"),
                        tags$ul(
                          tags$li(strong("Dataset Utama:"), " SOVI (Social Vulnerability Index) Data"),
                          tags$li(strong("Sumber:"), " Scientific Data Journal - Nature"),
                          tags$li(strong("URL Data:"), tags$a(href = sovi_url, "SOVI Dataset", target = "_blank")),
                          tags$li(strong("URL Metadata:"), tags$a(href = metadata_url, "Artikel Ilmiah", target = "_blank"))
                        ),
                        
                        h4("Fitur Dashboard"),
                        tags$div(
                          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin: 20px 0;",
                          tags$div(class = "feature-card slide-in",
                                   h5("Manajemen Data"),
                                   p("Transformasi variabel kontinyu ke kategorik, penanganan missing values, dan preprocessing data.")
                          ),
                          tags$div(class = "feature-card slide-in",
                                   h5("Eksplorasi Data"),
                                   p("Statistik deskriptif lengkap, visualisasi interaktif, dan pemetaan geografis data.")
                          ),
                          tags$div(class = "feature-card slide-in",
                                   h5("Uji Asumsi"),
                                   p("Uji normalitas dan homogenitas data untuk memastikan validitas analisis statistik.")
                          ),
                          tags$div(class = "feature-card slide-in",
                                   h5("Statistik Inferensia"),
                                   p("Uji hipotesis lengkap: uji rata-rata, proporsi, varians, dan ANOVA.")
                          ),
                          tags$div(class = "feature-card slide-in",
                                   h5("Regresi Linear"),
                                   p("Analisis regresi berganda dengan uji asumsi dan diagnostik model.")
                          )
                        ),
                        
                        br(),
                        h4("Informasi Teknis"),
                        tags$div(class = "info-box",
                                 tags$ul(
                                   tags$li(strong("Platform:"), " R Shiny"),
                                   tags$li(strong("Versi R:"), R.version.string),
                                   tags$li(strong("Package Utama:"), " shiny, ggplot2, plotly, DT, leaflet"),
                                   tags$li(strong("Format Output:"), " PDF, Word, Excel, JPG"),
                                   tags$li(strong("Responsif:"), " Ya, dapat diakses di desktop dan mobile")
                                 )
                        ),
                        
                        br(),
                        div(style = "text-align: center;",
                            actionButton("start_analysis", "Mulai Analisis", class = "btn-primary btn-lg"),
                        ),
                        
                        br(), br(),
                        h4("Dukungan dan Bantuan"),
                        p("Jika Anda mengalami kendala atau membutuhkan bantuan, silakan merujuk ke dokumentasi atau hubungi tim pengembang."),
                        
                        downloadButton("download_manual", "Download Manual Pengguna", class = "btn-info")
                    )
                )
              )
      ),
      
      # =================== MANAJEMEN DATA ===================
      tabItem(tabName = "data_management",
              fluidRow(
                box(width = 12, title = "Manajemen Data - Pengelolaan dan Transformasi Dataset", status = "info", solidHeader = TRUE,
                    div(class = "info-box",
                        p(strong("Tujuan Menu:"), "Menu ini digunakan untuk mengelola data, melakukan upload file, preview data, dan transformasi variabel kontinyu menjadi kategorik."),
                        p(strong("Fitur Utama:"), "Upload file (CSV/Excel), load data default SOVI, preview data, transformasi variabel (kategorisasi, normalisasi, standardisasi), dan download hasil transformasi."),
                        p(strong("Cara Penggunaan:"), "1) Pilih sumber data (default/custom), 2) Pilih variabel untuk ditransformasi, 3) Tentukan metode transformasi, 4) Terapkan transformasi, 5) Download hasil jika diperlukan.")
                    )
                )
              ),
              
              fluidRow(
                box(width = 4, title = "Pilih Sumber Data", status = "primary", solidHeader = TRUE,
                    radioButtons("data_source", "Pilih Sumber Data:",
                                 choices = list(
                                   "Data Default (SOVI)" = "default",
                                   "Upload Data Custom" = "custom"
                                 ),
                                 selected = "default"),
                    
                    conditionalPanel(
                      condition = "input.data_source == 'custom'",
                      fileInput("file_upload", "Upload File (CSV/Excel/SPSS)",
                                accept = c(".csv", ".xlsx", ".xls", ".sav")),
                      helpText("Format yang didukung: CSV (.csv), Excel (.xlsx, .xls), SPSS (.sav)")
                    ),
                    
                    conditionalPanel(
                      condition = "input.data_source == 'default'",
                      actionButton("load_default", "Load Default SOVI Data", class = "btn-success")
                    ),
                    
                    br(),
                    h5("Data Summary:"),
                    verbatimTextOutput("data_summary")
                ),
                
                box(width = 8, title = "Preview Data", status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("data_preview")
                )
              ),
              
              fluidRow(
                box(width = 6, title = "Transformasi Variabel", status = "warning", solidHeader = TRUE,
                    selectInput("var_to_transform", "Pilih Variabel untuk Ditransformasi:",
                                choices = NULL),
                    selectInput("transform_method", "Metode Transformasi:",
                                choices = list(
                                  "Kategorisasi (Quantile)" = "quantile",
                                  "Kategorisasi (Custom)" = "custom",
                                  "Log Transformation" = "log",
                                  "Square Root" = "sqrt",
                                  "Standardization" = "scale"
                                )),
                    conditionalPanel(
                      condition = "input.transform_method == 'custom'",
                      div(class = "info-box",
                          h5("Kategorisasi Custom - Mengubah Data Kontinyu ke Kategorik:"),
                          p(strong("Penjelasan:"), "Masukkan nilai batas (breakpoint) dan nama kategori untuk membagi data kontinyu."),
                          p("Contoh: Jika data Income berkisar 20,000-80,000, Anda bisa membuat kategori:"),
                          tags$ul(
                            tags$li("Rendah: 20,000 - 35,000"),
                            tags$li("Sedang: 35,000 - 55,000"), 
                            tags$li("Tinggi: 55,000 - 80,000")
                          )
                      ),
                      
                      numericInput("n_custom_breaks", "Jumlah Kategori:", value = 3, min = 2, max = 8),
                      
                      # Dynamic UI untuk breakpoints dan labels
                      div(id = "custom_breaks_container",
                          h6("Masukkan Nilai Batas (Breakpoints):"),
                          uiOutput("custom_breaks_ui")
                      ),
                      
                      div(id = "custom_labels_container", 
                          h6("Masukkan Nama Kategori:"),
                          uiOutput("custom_labels_ui")
                      ),
                      
                      div(class = "interpretation-box", style = "margin-top: 15px;",
                          h6("Preview Kategorisasi:"),
                          tableOutput("category_preview")
                      ),
                      
                      helpText("Nilai breakpoint harus urut dari kecil ke besar. Nama kategori akan diterapkan pada rentang yang sesuai.")
                    ),
                    conditionalPanel(
                      condition = "input.transform_method == 'quantile'",
                      numericInput("n_quantiles", "Jumlah Kategori:", value = 4, min = 2, max = 10)
                    ),
                    actionButton("apply_transform", "Terapkan Transformasi", class = "btn-warning"),
                    br(), br(),
                    div(class = "interpretation-box",
                        h5("Interpretasi Transformasi:"),
                        textOutput("transform_interpretation")
                    )
                ),
                
                box(width = 6, title = "Hasil Transformasi", status = "success", solidHeader = TRUE,
                    DT::dataTableOutput("transformed_preview"),
                    br(),
                    downloadButton("download_transformed", "Download Data Transformed", class = "btn-success")
                )
              )
      ),
      
      # =================== STATISTIK DESKRIPTIF ===================
      tabItem(tabName = "descriptive",
              fluidRow(
                box(width = 12, title = "Statistik Deskriptif - Analisis Ringkasan Data", status = "info", solidHeader = TRUE,
                    div(class = "info-box",
                        p(strong("🎯 Tujuan Menu:"), "Menu ini digunakan untuk menganalisis karakteristik dasar data melalui ukuran pemusatan, penyebaran, dan bentuk distribusi."),
                        p(strong("⚡ Fitur Utama:"), "Perhitungan mean, median, standar deviasi, min, max, skewness, kurtosis, analisis berdasarkan kelompok, dan visualisasi distribusi data."),
                        p(strong("📋 Cara Penggunaan:"), "1) Pilih variabel yang akan dianalisis, 2) Tentukan pengelompokan (opsional), 3) Jalankan analisis, 4) Interpretasi hasil dan download laporan.")
                    )
                )
              ),
              
              fluidRow(
                box(width = 4, title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE,
                    selectInput("desc_variables", "Pilih Variabel:",
                                choices = NULL, multiple = TRUE),
                    selectInput("group_by_var", "Group By (Opsional):",
                                choices = c("None" = "none"), selected = "none"),
                    checkboxInput("include_plots", "Sertakan Plot", value = TRUE),
                    actionButton("run_descriptive", "Jalankan Analisis", class = "btn-primary")
                ),
                
                box(width = 8, title = "Statistik Deskriptif", status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("descriptive_table"),
                    br(),
                    div(class = "interpretation-box",
                        h5("Interpretasi Statistik Deskriptif:"),
                        textOutput("descriptive_interpretation")
                    )
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Visualisasi Distribusi", status = "success", solidHeader = TRUE,
                    plotlyOutput("descriptive_plots", height = "600px"),
                    br(),
                    div(class = "interpretation-box",
                        h5("Interpretasi Visualisasi:"),
                        textOutput("plot_interpretation")
                    ),
                    br(),
                    downloadButton("download_desc_report", "Download Laporan Lengkap (Word)", class = "btn-info")
                )
              )
      ),
      
      # =================== VISUALISASI ===================
      tabItem(tabName = "visualization",
              fluidRow(
                box(width = 12, title = "Visualisasi Data - Representasi Grafis", status = "info", solidHeader = TRUE,
                    div(class = "info-box",
                        p(strong("🎯 Tujuan Menu:"), "Menu ini digunakan untuk membuat berbagai jenis visualisasi data yang interaktif untuk memahami pola, hubungan, dan distribusi data."),
                        p(strong("⚡ Fitur Utama:"), "Scatter plot, box plot, histogram, correlation matrix, bar chart, density plot dengan interaktivitas plotly dan opsi pewarnaan berdasarkan kategori."),
                        p(strong("📋 Cara Penggunaan:"), "1) Pilih jenis plot, 2) Tentukan variabel X dan Y (jika diperlukan), 3) Pilih variabel untuk pewarnaan (opsional), 4) Buat visualisasi dan download hasil.")
                    )
                )
              ),
              
              fluidRow(
                box(width = 3, title = "Pengaturan Visualisasi", status = "primary", solidHeader = TRUE,
                    selectInput("plot_type", "Jenis Plot:",
                                choices = list(
                                  "Scatter Plot" = "scatter",
                                  "Box Plot" = "boxplot",
                                  "Histogram" = "histogram",
                                  "Correlation Plot" = "correlation",
                                  "Bar Chart" = "barplot",
                                  "Density Plot" = "density"
                                )),
                    selectInput("x_var", "Variabel X:", choices = NULL),
                    conditionalPanel(
                      condition = "input.plot_type == 'scatter' || input.plot_type == 'boxplot'",
                      selectInput("y_var", "Variabel Y:", choices = NULL)
                    ),
                    conditionalPanel(
                      condition = "input.plot_type != 'correlation'",
                      selectInput("color_var", "Color By (Opsional):", 
                                  choices = c("None" = "none"), selected = "none")
                    ),
                    actionButton("create_plot", "Buat Visualisasi", class = "btn-primary")
                ),
                
                box(width = 9, title = "Visualisasi Data", status = "info", solidHeader = TRUE,
                    plotlyOutput("main_plot", height = "500px"),
                    br(),
                    div(class = "interpretation-box",
                        h5("Interpretasi Visualisasi:"),
                        textOutput("visual_interpretation")
                    ),
                    br(),
                    fluidRow(
                      column(6, downloadButton("download_plot_jpg", "Download JPG", class = "btn-success")),
                      column(6, downloadButton("download_plot_word", "Download Word", class = "btn-success"))
                    )
                )
              )
      ),
      
      # =================== PEMETAAN ===================
      tabItem(tabName = "mapping",
              fluidRow(
                box(width = 12, title = "Pemetaan Data - Visualisasi Spasial", status = "info", solidHeader = TRUE,
                    div(class = "info-box",
                        p(strong("🎯 Tujuan Menu:"), "Menu ini digunakan untuk membuat visualisasi data dalam bentuk peta interaktif untuk menganalisis pola geografis dan distribusi spasial."),
                        p(strong("⚡ Fitur Utama:"), "Heat map, choropleth map, point map dengan koordinat geografis, legenda interaktif, dan fitur zoom/pan pada peta Leaflet."),
                        p(strong("📋 Cara Penggunaan:"), "1) Pilih variabel untuk dipetakan, 2) Tentukan jenis peta, 3) Buat peta interaktif, 4) Analisis pola spasial dan download peta.")
                    )
                )
              ),
              
              fluidRow(
                box(width = 4, title = "Pengaturan Peta", status = "primary", solidHeader = TRUE,
                    p("Fitur pemetaan untuk data geografis SOVI"),
                    selectInput("map_variable", "Variabel untuk Dipetakan:", choices = NULL),
                    selectInput("map_type", "Jenis Peta:",
                                choices = list(
                                  "Heat Map" = "heatmap",
                                  "Choropleth" = "choropleth", 
                                  "Point Map" = "points"
                                )),
                    actionButton("create_map", "Buat Peta", class = "btn-primary"),
                    br(), br(),
                    div(class = "interpretation-box",
                        h5("Interpretasi Peta:"),
                        textOutput("map_interpretation")
                    )
                ),
                
                box(width = 8, title = "Peta Interaktif", status = "info", solidHeader = TRUE,
                    leafletOutput("interactive_map", height = "500px"),
                    br(),
                    downloadButton("download_map_jpg", "Download Peta (JPG)", class = "btn-success")
                )
              )
      ),
      
      # =================== UJI ASUMSI ===================
      tabItem(tabName = "assumptions",
              fluidRow(
                box(width = 12, title = "Uji Asumsi Data - Verifikasi Prasyarat Statistik", status = "info", solidHeader = TRUE,
                    div(class = "info-box",
                        p(strong("🎯 Tujuan Menu:"), "Menu ini digunakan untuk menguji asumsi-asumsi dasar yang diperlukan sebelum melakukan analisis statistik parametrik."),
                        p(strong("⚡ Fitur Utama:"), "Uji normalitas (Shapiro-Wilk/Anderson-Darling), uji homogenitas varians (Levene's test), visualisasi Q-Q plot dan histogram untuk validasi asumsi."),
                        p(strong("📋 Cara Penggunaan:"), "1) Pilih variabel untuk diuji, 2) Tentukan variabel kelompok untuk uji homogenitas, 3) Jalankan uji asumsi, 4) Interpretasi hasil untuk menentukan metode analisis yang sesuai.")
                    )
                )
              ),
              
              fluidRow(
                box(width = 4, title = "Pengaturan Uji Asumsi", status = "primary", solidHeader = TRUE,
                                    selectInput("assumption_var", "Pilih Variabel:", choices = NULL),
                selectInput("assumption_group", "Group By (untuk homogenitas):", 
                            choices = c("None" = "none"), selected = "none"),
                h5("Uji yang Akan Dilakukan:"),
                checkboxInput("test_normality", "Uji Normalitas", value = TRUE),
                checkboxInput("test_homogeneity", "Uji Homogenitas (Levene)", value = TRUE),
                checkboxInput("test_bartlett", "Uji Bartlett", value = FALSE),
                actionButton("run_assumptions", "Jalankan Uji", class = "btn-primary")
                ),
                
                                  box(width = 8, title = "Hasil Uji Asumsi", status = "info", solidHeader = TRUE,
                      h4("Uji Normalitas"),
                      verbatimTextOutput("normality_result"),
                      div(class = "interpretation-box",
                          textOutput("normality_interpretation")
                      ),
                      
                      br(),
                      h4("Uji Homogenitas (Levene)"),
                      verbatimTextOutput("homogeneity_result"),
                      div(class = "interpretation-box",
                          textOutput("homogeneity_interpretation")
                      ),
                      
                      br(),
                      h4("Uji Bartlett"),
                      verbatimTextOutput("bartlett_result"),
                      div(class = "interpretation-box",
                          textOutput("bartlett_interpretation")
                      ),
                      
                      br(),
                      downloadButton("download_assumption_report", "Download Laporan Uji Asumsi (Word)", class = "btn-info")
                  )
              ),
              
              fluidRow(
                box(width = 12, title = "Visualisasi Uji Asumsi", status = "success", solidHeader = TRUE,
                    plotlyOutput("assumption_plots", height = "400px")
                )
              )
      ),
      
      # =================== UJI RATA-RATA ===================
      tabItem(tabName = "mean_tests",
              fluidRow(
                box(width = 12, title = "Uji Rata-rata - Pengujian Hipotesis Mean", status = "info", solidHeader = TRUE,
                    div(class = "info-box",
                        p(strong("🎯 Tujuan Menu:"), "Menu ini digunakan untuk menguji hipotesis tentang rata-rata populasi menggunakan uji t (satu sampel, dua sampel independen, atau berpasangan)."),
                        p(strong("⚡ Fitur Utama:"), "One sample t-test, two sample t-test, paired t-test dengan confidence interval, visualisasi distribusi, dan interpretasi statistik lengkap."),
                        p(strong("📋 Cara Penggunaan:"), "1) Pilih jenis uji t-test, 2) Tentukan variabel dan parameter uji, 3) Set confidence level, 4) Jalankan uji dan interpretasi hasil keputusan H₀/H₁.")
                    )
                )
              ),
              
              fluidRow(
                box(width = 4, title = "Pengaturan Uji Rata-rata", status = "primary", solidHeader = TRUE,
                    selectInput("mean_test_type", "Jenis Uji:",
                                choices = list(
                                  "One Sample t-test" = "one_sample",
                                  "Two Sample t-test" = "two_sample",
                                  "Paired t-test" = "paired"
                                )),
                    selectInput("mean_test_var", "Variabel:", choices = NULL),
                    conditionalPanel(
                      condition = "input.mean_test_type == 'one_sample'",
                      numericInput("test_value", "Nilai yang Diuji:", value = 0)
                    ),
                    conditionalPanel(
                      condition = "input.mean_test_type != 'one_sample'",
                      selectInput("group_var_mean", "Variabel Kelompok:", choices = NULL)
                    ),
                    numericInput("confidence_level", "Confidence Level:", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
                    actionButton("run_mean_test", "Jalankan Uji", class = "btn-primary")
                ),
                
                box(width = 8, title = "Hasil Uji Rata-rata", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("mean_test_result"),
                    br(),
                    div(class = "interpretation-box",
                        h5("Interpretasi Hasil:"),
                        textOutput("mean_test_interpretation")
                    ),
                    br(),
                    plotlyOutput("mean_test_plot"),
                    br(),
                    downloadButton("download_mean_test", "Download Hasil Uji (Word)", class = "btn-success")
                )
              )
      ),
      
        # =================== UJI PROPORSI & VARIANS ===================
  tabItem(tabName = "prop_var_tests",
          fluidRow(
            box(width = 12, title = "Uji Proporsi dan Varians - Pengujian Hipotesis Parameter", status = "info", solidHeader = TRUE,
                div(class = "info-box",
                    p(strong("Tujuan Menu:"), "Menu ini digunakan untuk menguji hipotesis tentang proporsi dan varians populasi menggunakan berbagai jenis uji statistik."),
                    p(strong("Fitur Utama:"), "Uji proporsi satu sampel, uji proporsi dua sampel, uji varians satu sampel, uji varians dua sampel dengan confidence interval dan interpretasi statistik."),
                    p(strong("Cara Penggunaan:"), "1) Pilih jenis uji, 2) Tentukan variabel dan parameter uji, 3) Set nilai uji, 4) Jalankan uji dan interpretasi hasil.")
                )
            )
          ),
          fluidRow(
            box(width = 4, title = "Pengaturan Uji", status = "primary", solidHeader = TRUE,
                selectInput("prop_var_test_type", "Jenis Uji:",
                            choices = list(
                              "Uji Proporsi 1 Sampel" = "prop_one",
                              "Uji Proporsi 2 Sampel" = "prop_two",
                              "Uji Varians 1 Sampel" = "var_one",
                              "Uji Varians 2 Sampel" = "var_two"
                            )),
                selectInput("prop_var_variable", "Variabel:", choices = NULL),
                conditionalPanel(
                  condition = "input.prop_var_test_type == 'prop_one'",
                  numericInput("prop_test_value", "Proporsi yang Diuji:", value = 0.5, min = 0, max = 1)
                ),
                conditionalPanel(
                  condition = "input.prop_var_test_type == 'var_one'",
                  numericInput("var_test_value", "Varians yang Diuji:", value = 1, min = 0)
                ),
                conditionalPanel(
                  condition = "input.prop_var_test_type == 'prop_two' || input.prop_var_test_type == 'var_two'",
                  selectInput("group_var_prop", "Variabel Kelompok:", choices = NULL)
                ),
                actionButton("run_prop_var_test", "Jalankan Uji", class = "btn-primary")
            ),
            
            box(width = 8, title = "Hasil Uji Proporsi/Varians", status = "info", solidHeader = TRUE,
                verbatimTextOutput("prop_var_result"),
                br(),
                div(class = "interpretation-box",
                    h5("Interpretasi Hasil:"),
                    textOutput("prop_var_interpretation")
                ),
                br(),
                plotlyOutput("prop_var_plot"),
                br(),
                downloadButton("download_prop_var_test", "Download Hasil Uji (Word)", class = "btn-success")
            )
          )
  ),
      
      # =================== ANOVA ===================
      tabItem(tabName = "anova_tests",
              fluidRow(
                box(width = 12, title = "ANOVA - Analisis Varians untuk Perbandingan Multiple Group", status = "info", solidHeader = TRUE,
                    div(class = "info-box",
                        p(strong("🎯 Tujuan Menu:"), "Menu ini digunakan untuk menguji perbedaan rata-rata antar multiple kelompok menggunakan Analysis of Variance (ANOVA) satu arah atau dua arah."),
                        p(strong("⚡ Fitur Utama:"), "One-way ANOVA, two-way ANOVA dengan/tanpa interaksi, post-hoc test (Tukey HSD), visualisasi perbandingan kelompok, dan plot diagnostik residual."),
                        p(strong("📋 Cara Penggunaan:"), "1) Pilih jenis ANOVA, 2) Tentukan variabel dependen dan faktor, 3) Set opsi interaksi dan post-hoc, 4) Jalankan analisis dan interpretasi hasil F-test.")
                    )
                )
              ),
              
              fluidRow(
                box(width = 4, title = "Pengaturan ANOVA", status = "primary", solidHeader = TRUE,
                    selectInput("anova_type", "Jenis ANOVA:",
                                choices = list(
                                  "One-Way ANOVA" = "oneway",
                                  "Two-Way ANOVA" = "twoway"
                                )),
                    selectInput("anova_dependent", "Variabel Dependen:", choices = NULL),
                    selectInput("anova_factor1", "Faktor 1:", choices = NULL),
                    conditionalPanel(
                      condition = "input.anova_type == 'twoway'",
                      selectInput("anova_factor2", "Faktor 2:", choices = NULL),
                      checkboxInput("anova_interaction", "Sertakan Interaksi", value = TRUE)
                    ),
                    checkboxInput("post_hoc", "Post-hoc Test (Tukey HSD)", value = TRUE),
                    actionButton("run_anova", "Jalankan ANOVA", class = "btn-primary")
                ),
                
                box(width = 8, title = "Hasil ANOVA", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("anova_result"),
                    br(),
                    div(class = "interpretation-box",
                        h5("Interpretasi ANOVA:"),
                        textOutput("anova_interpretation")
                    ),
                    conditionalPanel(
                      condition = "input.post_hoc == true",
                      br(),
                      h4("Post-hoc Test (Tukey HSD):"),
                      verbatimTextOutput("posthoc_result")
                    ),
                    br(),
                    downloadButton("download_anova_test", "Download Hasil ANOVA (Word)", class = "btn-success")
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Visualisasi ANOVA", status = "success", solidHeader = TRUE,
                    plotlyOutput("anova_plots", height = "500px")
                )
              )
      ),
      
      # =================== REGRESI LINEAR ===================
      tabItem(tabName = "regression",
              fluidRow(
                box(width = 12, title = "Regresi Linear Berganda - Analisis Hubungan dan Prediksi", status = "info", solidHeader = TRUE,
                    div(class = "info-box",
                        p(strong("🎯 Tujuan Menu:"), "Menu ini digunakan untuk menganalisis hubungan linear antara variabel dependen dengan satu atau lebih variabel independen menggunakan regresi linear berganda."),
                        p(strong("⚡ Fitur Utama:"), "Model regresi berganda, uji asumsi (normalitas, homoskedastisitas, multikolinearitas), diagnostik model (Cook's distance, leverage), dan plot diagnostik komprehensif."),
                        p(strong("📋 Cara Penggunaan:"), "1) Pilih variabel dependen dan independen, 2) Set opsi uji asumsi dan diagnostik, 3) Jalankan regresi, 4) Evaluasi model dan interpretasi koefisien serta R-squared.")
                    )
                )
              ),
              
              fluidRow(
                box(width = 4, title = "Pengaturan Regresi", status = "primary", solidHeader = TRUE,
                    selectInput("reg_dependent", "Variabel Dependen:", choices = NULL),
                    selectInput("reg_independent", "Variabel Independen:", choices = NULL, multiple = TRUE),
                    checkboxInput("reg_diagnostics", "Uji Diagnostik Model", value = TRUE),
                    checkboxInput("reg_assumptions", "Uji Asumsi Regresi", value = TRUE),
                    actionButton("run_regression", "Jalankan Regresi", class = "btn-primary")
                ),
                
                box(width = 8, title = "Hasil Regresi Linear Berganda", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("regression_summary"),
                    br(),
                    div(class = "interpretation-box",
                        h5("Interpretasi Model:"),
                        textOutput("regression_interpretation")
                    )
                )
              ),
              
              conditionalPanel(
                condition = "input.reg_assumptions == true",
                fluidRow(
                  box(width = 6, title = "Uji Asumsi Regresi", status = "warning", solidHeader = TRUE,
                      verbatimTextOutput("regression_assumptions"),
                      br(),
                      div(class = "interpretation-box",
                          textOutput("assumptions_interpretation")
                      )
                  ),
                  
                  box(width = 6, title = "Diagnostik Model", status = "success", solidHeader = TRUE,
                      verbatimTextOutput("regression_diagnostics"),
                      br(),
                      div(class = "interpretation-box",
                          textOutput("diagnostics_interpretation")
                      )
                  )
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Plot Diagnostik Regresi", status = "info", solidHeader = TRUE,
                    plotlyOutput("regression_plots", height = "600px"),
                    br(),
                    downloadButton("download_regression_report", "Download Laporan Regresi (Word)", class = "btn-info")
                )
              )
      ),
      
      # =================== METADATA ===================
      tabItem(tabName = "metadata",
              fluidRow(
                box(width = 12, title = "Metadata Dataset SOVI", status = "primary", solidHeader = TRUE,
                    div(class = "fade-in",
                        h3("Social Vulnerability Index (SOVI) Dataset", class = "text-gradient"),
                        
                        h4("Sumber Data"),
                        tags$ul(
                          tags$li(strong("Artikel:"), tags$a(href = metadata_url, "A county-level dataset for informing the United States social vulnerability to environmental hazards", target = "_blank")),
                          tags$li(strong("Journal:"), "Scientific Data - Nature"),
                          tags$li(strong("DOI:"), "10.1038/s41597-021-01080-w"),
                          tags$li(strong("Tahun Publikasi:"), "2021")
                        ),
                        
                        h4("Deskripsi Dataset"),
                        p("Dataset Social Vulnerability Index (SOVI) menyediakan data tingkat county di Amerika Serikat yang mengukur kerentanan sosial terhadap bahaya lingkungan. Dataset ini dikembangkan untuk membantu dalam perencanaan mitigasi bencana dan manajemen risiko."),
                        
                        h4("Struktur Data"),
                        tags$div(class = "info-box",
                                 tags$ul(
                                   tags$li(strong("Unit Analisis:"), " County-level (tingkat kabupaten)"),
                                   tags$li(strong("Cakupan Geografis:"), " Amerika Serikat"),
                                   tags$li(strong("Jumlah Variabel:"), " 30+ indikator sosial-ekonomi"),
                                   tags$li(strong("Format:"), " CSV (Comma Separated Values)")
                                 )
                        ),
                        
                        h4("Variabel Utama"),
                        DT::dataTableOutput("metadata_variables"),
                        
                        br(),
                        h4("Metodologi SOVI"),
                        tags$div(class = "info-box", style = "border-left-color: var(--warning-color); background: linear-gradient(135deg, #fffbeb 0%, #fef3c7 100%);",
                                 p("Social Vulnerability Index (SOVI) dikembangkan menggunakan Principal Component Analysis (PCA) untuk mengintegrasikan multiple indikator sosial-ekonomi menjadi satu indeks komposit. Indeks ini mengidentifikasi area dengan tingkat kerentanan sosial tinggi yang membutuhkan perhatian khusus dalam perencanaan mitigasi bencana."),
                                 
                                 h5("Komponen Utama SOVI:"),
                                 tags$ul(
                                   tags$li("Demographics (demografi)"),
                                   tags$li("Socioeconomic Status (status sosial-ekonomi)"),
                                   tags$li("Housing and Transportation (perumahan dan transportasi)"),
                                   tags$li("Community and Environment (komunitas dan lingkungan)")
                                 )
                        ),
                        
                        h4("Aplikasi dan Kegunaan"),
                        tags$div(class = "info-box", style = "border-left-color: var(--info-color); background: linear-gradient(135deg, #e6f7ff 0%, #f0f9ff 100%);",
                                 tags$ul(
                                   tags$li("Perencanaan mitigasi bencana"),
                                   tags$li("Alokasi sumber daya emergency management"),
                                   tags$li("Identifikasi komunitas berisiko tinggi"),
                                   tags$li("Penelitian kerentanan sosial"),
                                   tags$li("Analisis disparitas sosial-ekonomi")
                                 )
                        ),
                        
                        h4("Sitasi"),
                        tags$div(class = "info-box", style = "background: var(--gray-50); border-left-color: var(--gray-400); font-family: 'JetBrains Mono', monospace;",
                                 p("Flanagan, B.E., Hallisey, E.J., Adams, E. et al. A county-level dataset for informing the United States social vulnerability to environmental hazards. Sci Data 8, 290 (2021). https://doi.org/10.1038/s41597-021-01080-w")
                        ),
                        
                        br(),
                        downloadButton("download_metadata_report", "Download Metadata Lengkap (Word)", class = "btn-primary")
                    )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    current_data = original_data,
    transformed_data = NULL
  )
  
  # Dynamic UI untuk custom breaks
  output$custom_breaks_ui <- renderUI({
    n_breaks <- input$n_custom_breaks
    if(is.null(n_breaks)) n_breaks <- 3
    
    # Generate n_breaks + 1 input fields (untuk batas awal dan akhir setiap kategori)
    break_inputs <- lapply(1:(n_breaks + 1), function(i) {
      numericInput(paste0("break_", i), 
                   paste("Breakpoint", i, ":"),
                   value = (i-1) * 20)
    })
    
    div(
      fluidRow(
        lapply(break_inputs, function(x) column(width = ceiling(12/(n_breaks+1)), x))
      )
    )
  })
  
  # Dynamic UI untuk custom labels
  output$custom_labels_ui <- renderUI({
    n_breaks <- input$n_custom_breaks
    if(is.null(n_breaks)) n_breaks <- 3
    
    # Generate n_breaks input fields untuk nama kategori
    label_inputs <- lapply(1:n_breaks, function(i) {
      textInput(paste0("label_", i), 
                paste("Kategori", i, ":"),
                value = paste("Kategori", i))
    })
    
    div(
      fluidRow(
        lapply(label_inputs, function(x) column(width = ceiling(12/n_breaks), x))
      )
    )
  })
  
  # Preview kategorisasi custom
  output$category_preview <- renderTable({
    n_breaks <- input$n_custom_breaks
    if(is.null(n_breaks)) return(NULL)
    
    # Ambil nilai breakpoints
    breaks <- sapply(1:(n_breaks + 1), function(i) {
      val <- input[[paste0("break_", i)]]
      if(is.null(val)) return((i-1) * 20)
      return(val)
    })
    
    # Ambil nama kategori
    labels <- sapply(1:n_breaks, function(i) {
      val <- input[[paste0("label_", i)]]
      if(is.null(val)) return(paste("Kategori", i))
      return(val)
    })
    
    # Buat preview table
    preview_data <- data.frame(
      "No" = 1:n_breaks,
      "Nama Kategori" = labels,
      "Rentang Nilai" = paste(breaks[1:n_breaks], "-", breaks[2:(n_breaks+1)]),
      stringsAsFactors = FALSE
    )
    
    return(preview_data)
  }, striped = TRUE, hover = TRUE)
  
  # Update choices when data changes (including transformed variables)
  observe({
    # Use transformed data if available, otherwise use current data
    data_to_use <- if (!is.null(values$transformed_data)) values$transformed_data else values$current_data
    
    if (!is.null(data_to_use)) {
      numeric_vars <- names(data_to_use)[sapply(data_to_use, is.numeric)]
      all_vars <- names(data_to_use)
      factor_vars <- names(data_to_use)[sapply(data_to_use, function(x) is.factor(x) || is.character(x) || length(unique(x)) <= 10)]
      char_vars <- names(data_to_use)[sapply(data_to_use, function(x) is.character(x) || is.factor(x))]
      
      # Original numeric variables for transformation
      original_numeric <- names(values$current_data)[sapply(values$current_data, is.numeric)]
      
      updateSelectInput(session, "var_to_transform", choices = original_numeric)
      updateSelectInput(session, "desc_variables", choices = numeric_vars)
      updateSelectInput(session, "x_var", choices = all_vars)
      updateSelectInput(session, "y_var", choices = numeric_vars)
      updateSelectInput(session, "assumption_var", choices = numeric_vars)
      updateSelectInput(session, "mean_test_var", choices = numeric_vars)
      updateSelectInput(session, "prop_var_variable", choices = all_vars)
      updateSelectInput(session, "anova_dependent", choices = numeric_vars)
      updateSelectInput(session, "anova_factor1", choices = factor_vars)
      updateSelectInput(session, "anova_factor2", choices = factor_vars)
      updateSelectInput(session, "reg_dependent", choices = numeric_vars)
      updateSelectInput(session, "reg_independent", choices = numeric_vars)
      updateSelectInput(session, "map_variable", choices = numeric_vars)
      
      # Group by options
      group_choices <- c("None" = "none", setNames(char_vars, char_vars))
      color_choices <- c("None" = "none", setNames(c(char_vars, factor_vars), c(char_vars, factor_vars)))
      updateSelectInput(session, "group_by_var", choices = group_choices)
      updateSelectInput(session, "assumption_group", choices = group_choices)
      updateSelectInput(session, "group_var_mean", choices = char_vars)
      updateSelectInput(session, "group_var_prop", choices = char_vars)
      updateSelectInput(session, "color_var", choices = color_choices)
    }
  })
  
  # Auto load default data on startup or when data source changes
  observe({
    if (input$data_source == "default") {
      values$current_data <- original_data
    }
  })
  
  observeEvent(input$load_default, {
    values$current_data <- original_data
    showNotification("Data SOVI berhasil dimuat ulang!", type = "message")
  })
  
  observeEvent(input$file_upload, {
    if (!is.null(input$file_upload) && input$data_source == "custom") {
      tryCatch({
        file_ext <- tools::file_ext(input$file_upload$name)
        if (file_ext %in% c("csv")) {
          values$current_data <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          values$current_data <- openxlsx::read.xlsx(input$file_upload$datapath)
        } else if (file_ext %in% c("sav")) {
          values$current_data <- haven::read_sav(input$file_upload$datapath)
          # Convert to data.frame and handle labels
          values$current_data <- as.data.frame(values$current_data)
        } else {
          showNotification("Format file tidak didukung!", type = "error")
          return()
        }
        showNotification("File berhasil diupload!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
    }
  })
  
  # =================== DATA MANAGEMENT ===================
  output$data_summary <- renderText({
    if (!is.null(values$current_data)) {
      paste0("Jumlah baris: ", nrow(values$current_data), "
",
"Jumlah kolom: ", ncol(values$current_data), "
",
"Missing values: ", sum(is.na(values$current_data)))
    }
  })
  
  output$data_preview <- DT::renderDataTable({
    if (!is.null(values$current_data)) {
      DT::datatable(values$current_data, options = list(scrollX = TRUE, pageLength = 10))
    }
  })
  
  # Transform data
  observeEvent(input$apply_transform, {
    req(input$var_to_transform, input$transform_method)
    
    var_name <- input$var_to_transform
    data_copy <- values$current_data
    
    if (input$transform_method == "quantile") {
      data_copy[[paste0(var_name, "_cat")]] <- cut(data_copy[[var_name]], 
                                                   breaks = quantile(data_copy[[var_name]], na.rm = TRUE,
                                                                     probs = seq(0, 1, length.out = input$n_quantiles + 1)),
                                                   include.lowest = TRUE,
                                                   labels = paste0("Q", 1:input$n_quantiles))
    } else if (input$transform_method == "custom") {
      # Ambil nilai breaks dari input dinamis
      n_breaks <- input$n_custom_breaks
      if(is.null(n_breaks)) n_breaks <- 3
      
      # Ambil breakpoints
      breaks <- sapply(1:(n_breaks + 1), function(i) {
        val <- input[[paste0("break_", i)]]
        if(is.null(val)) return((i-1) * 20)
        return(val)
      })
      breaks <- sort(unique(breaks))
      
      # Ambil labels
      labels <- sapply(1:n_breaks, function(i) {
        val <- input[[paste0("label_", i)]]
        if(is.null(val)) return(paste("Kategori", i))
        return(val)
      })
      
      data_copy[[paste0(var_name, "_cat")]] <- cut(data_copy[[var_name]], 
                                                   breaks = breaks,
                                                   include.lowest = TRUE,
                                                   labels = labels[1:(length(breaks)-1)])
    } else if (input$transform_method == "log") {
      data_copy[[paste0(var_name, "_log")]] <- log(data_copy[[var_name]] + 1)
    } else if (input$transform_method == "sqrt") {
      data_copy[[paste0(var_name, "_sqrt")]] <- sqrt(abs(data_copy[[var_name]]))
    } else if (input$transform_method == "scale") {
      data_copy[[paste0(var_name, "_scaled")]] <- scale(data_copy[[var_name]])[,1]
    }
    
    values$transformed_data <- data_copy
    showNotification("Transformasi berhasil diterapkan!", type = "message")
  })
  
  output$transform_interpretation <- renderText({
    if (!is.null(values$transformed_data)) {
      method <- input$transform_method
      if (method == "quantile") {
                                "Interpretasi Transformasi:\n\nTransformasi kuantil membagi data menjadi kategori berdasarkan persentil, berguna untuk membuat kelompok dengan distribusi yang sama."
      } else if (method == "custom") {
        n_breaks <- input$n_custom_breaks
        if(is.null(n_breaks)) n_breaks <- 3
        
        # Ambil nama kategori dan breakpoints untuk interpretasi
        labels <- sapply(1:n_breaks, function(i) {
          val <- input[[paste0("label_", i)]]
          if(is.null(val)) return(paste("Kategori", i))
          return(val)
        })
        
        breaks <- sapply(1:(n_breaks + 1), function(i) {
          val <- input[[paste0("break_", i)]]
          if(is.null(val)) return((i-1) * 20)
          return(val)
        })
        
        interpretasi <- paste0(
          "TRANSFORMASI KATEGORISASI CUSTOM:\n\n",
          "Data kontinyu berhasil diubah menjadi ", n_breaks, " kategori:\n"
        )
        
        for(i in 1:n_breaks) {
          interpretasi <- paste0(interpretasi, 
                                 "• ", labels[i], ": ", breaks[i], " - ", breaks[i+1], "\n")
        }
        
        interpretasi <- paste0(interpretasi,
                               "\nMANFAAT KATEGORISASI:\n",
                               "• Mempermudah interpretasi data kontinyu\n",
                               "• Memungkinkan analisis berdasarkan kelompok\n",
                               "• Mengatasi outlier ekstrem\n",
                               "• Cocok untuk analisis non-parametrik\n\n",
                               "CATATAN: Pastikan breakpoint sesuai dengan distribusi data dan tujuan analisis Anda."
        )
        
        return(interpretasi)
              } else if (method == "log") {
          "Interpretasi Transformasi:\n\nTransformasi logaritma mengurangi skewness pada data dan menstabilkan varians."
        } else if (method == "scale") {
          "Interpretasi Transformasi:\n\nStandardisasi mengubah data menjadi z-score dengan mean=0 dan std=1, berguna untuk perbandingan variabel dengan skala berbeda."
        } else {
          "Interpretasi Transformasi:\n\nTransformasi telah diterapkan sesuai dengan metode yang dipilih."
        }
    }
  })
  
  output$transformed_preview <- DT::renderDataTable({
    if (!is.null(values$transformed_data)) {
      DT::datatable(values$transformed_data, options = list(scrollX = TRUE, pageLength = 10))
    }
  })
  
  # =================== DESCRIPTIVE STATISTICS (FIXED) ===================
  observeEvent(input$run_descriptive, {
    req(input$desc_variables)
    
    # Use transformed data if available
    data_to_use <- if (!is.null(values$transformed_data)) values$transformed_data else values$current_data
    data_subset <- data_to_use[, input$desc_variables, drop = FALSE]
    
    if (input$group_by_var != "none" && input$group_by_var %in% names(data_to_use)) {
      # Fixed group by issue
      group_data <- data_to_use[[input$group_by_var]]
      data_with_group <- data_subset
      data_with_group$group_var <- group_data
      
      desc_stats <- data_with_group %>%
        group_by(group_var) %>%
        summarise_all(list(
          Mean = ~mean(.x, na.rm = TRUE),
          Median = ~median(.x, na.rm = TRUE),
          SD = ~sd(.x, na.rm = TRUE),
          Min = ~min(.x, na.rm = TRUE),
          Max = ~max(.x, na.rm = TRUE)
        ), .groups = 'drop')
      
      # Rename the group column
      names(desc_stats)[1] <- input$group_by_var
    } else {
      desc_stats <- data_subset %>%
        summarise_all(list(
          Mean = ~mean(.x, na.rm = TRUE),
          Median = ~median(.x, na.rm = TRUE),
          SD = ~sd(.x, na.rm = TRUE),
          Min = ~min(.x, na.rm = TRUE),
          Max = ~max(.x, na.rm = TRUE),
          Skewness = ~ifelse(length(.x) > 3, moments::skewness(.x, na.rm = TRUE), NA),
          Kurtosis = ~ifelse(length(.x) > 3, moments::kurtosis(.x, na.rm = TRUE), NA)
        ))
    }
    
    output$descriptive_table <- DT::renderDataTable({
      DT::datatable(desc_stats, options = list(scrollX = TRUE)) %>%
        DT::formatRound(columns = 2:ncol(desc_stats), digits = 3)
    })
    
    output$descriptive_interpretation <- renderText({
      if (!is.null(desc_stats)) {
        interpretasi <- paste0(
          "INTERPRETASI STATISTIK DESKRIPTIF:\n\n",
          "Analisis melibatkan ", length(input$desc_variables), " variabel numerik.\n\n",
          "UKURAN PEMUSATAN:\n",
          "• Mean (rata-rata): Nilai rata-rata dari semua observasi\n",
          "• Median: Nilai tengah setelah data diurutkan (lebih robust terhadap outlier)\n",
          "• Mode: Nilai yang paling sering muncul\n\n",
          "UKURAN PENYEBARAN:\n",
          "• Standard Deviation (SD): Mengukur seberapa jauh data tersebar dari mean\n",
          "• Variance: Kuadrat dari standard deviation\n",
          "• Range: Selisih nilai maksimum dan minimum\n\n",
          "UKURAN BENTUK DISTRIBUSI:\n",
          "• Skewness: Mengukur kemiringan distribusi\n",
          "  • Nilai ≈ 0: Distribusi simetris\n",
          "  • Nilai > 1: Condong ke kanan (right-skewed)\n",
          "  • Nilai < -1: Condong ke kiri (left-skewed)\n",
          "• Kurtosis: Mengukur 'ketajaman' puncak distribusi\n",
          "  • Nilai ≈ 3: Distribusi normal\n",
          "  • Nilai > 3: Lebih tajam dari normal (leptokurtic)\n",
          "  • Nilai < 3: Lebih datar dari normal (platykurtic)\n\n",
          "TIPS INTERPRETASI:\n",
          "• Bandingkan mean vs median untuk deteksi skewness\n",
          "• CV (Coefficient of Variation) = SD/Mean * 100% untuk perbandingan variabilitas relatif\n",
          "• Gunakan plot untuk visualisasi yang lebih baik"
        )
        return(interpretasi)
      }
    })
    
    if (input$include_plots) {
      output$descriptive_plots <- renderPlotly({
        plots_list <- list()
        
        for (var in input$desc_variables) {
          p <- ggplot(values$current_data, aes_string(x = var)) +
            geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue") +
            geom_density(aes(y = ..density.. * nrow(values$current_data) * diff(range(values$current_data[[var]], na.rm = TRUE))/30), 
                         color = "red", size = 1) +
            labs(title = paste("Distribusi", var)) +
            theme_minimal()
          plots_list[[var]] <- ggplotly(p)
        }
        
        if (length(plots_list) == 1) {
          plots_list[[1]]
        } else {
          subplot(plots_list, nrows = ceiling(length(plots_list)/2))
        }
      })
      
      output$plot_interpretation <- renderText({
        "Interpretasi Visualisasi:\n\nHistogram menunjukkan distribusi frekuensi data, sementara garis density (merah) menunjukkan estimasi distribusi probabilitas. Bentuk distribusi dapat memberikan insight tentang normalitas data dan keberadaan outlier."
      })
    }
  })
  
  # =================== VISUALIZATION (FIXED) ===================
  observeEvent(input$create_plot, {
    req(input$plot_type, input$x_var)
    
    # Define color palette
    color_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
    
    tryCatch({
      if (input$plot_type == "scatter") {
        req(input$y_var)
        p <- ggplot(values$current_data, aes_string(x = input$x_var, y = input$y_var))
        if (input$color_var != "none") {
          p <- p + aes_string(color = input$color_var) +
            scale_color_manual(values = color_palette) +
            geom_point(alpha = 0.6)
        } else {
          p <- p + geom_point(alpha = 0.6, color = "#1f77b4")
        }
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
          labs(title = paste("Scatter Plot:", input$y_var, "vs", input$x_var)) +
          theme_minimal()
        
      } else if (input$plot_type == "boxplot") {
        req(input$y_var)
        p <- ggplot(values$current_data, aes_string(x = input$x_var, y = input$y_var))
        if (input$color_var != "none") {
          p <- p + aes_string(fill = input$color_var) +
            scale_fill_manual(values = color_palette) +
            geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2, 
                        size = 1.2, width = 0.6)
        } else {
          p <- p + geom_boxplot(fill = "#1f77b4", alpha = 0.7, outlier.shape = 16, 
                               outlier.size = 2, size = 1.2, width = 0.6)
        }
        p <- p + labs(title = paste("Box Plot:", input$y_var, "by", input$x_var)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major = element_line(color = "gray90", size = 0.5),
                panel.grid.minor = element_blank())
        
      } else if (input$plot_type == "histogram") {
        p <- ggplot(values$current_data, aes_string(x = input$x_var))
        if (input$color_var != "none") {
          p <- p + aes_string(fill = input$color_var) +
            scale_fill_manual(values = color_palette) +
            geom_histogram(bins = 30, alpha = 0.7)
        } else {
          p <- p + geom_histogram(bins = 30, alpha = 0.7, fill = "#1f77b4")
        }
        p <- p + labs(title = paste("Histogram:", input$x_var)) +
          theme_minimal()
        
      } else if (input$plot_type == "barplot") {
        # Fixed bar plot
        bar_data <- values$current_data %>%
          count(!!sym(input$x_var))
        
        p <- ggplot(bar_data, aes_string(x = input$x_var, y = "n"))
        if (input$color_var != "none" && input$color_var %in% names(values$current_data)) {
          color_data <- values$current_data[[input$color_var]][match(bar_data[[input$x_var]], values$current_data[[input$x_var]])]
          bar_data$color_var <- color_data
          p <- ggplot(bar_data, aes_string(x = input$x_var, y = "n", fill = "color_var")) +
            scale_fill_manual(values = color_palette)
        } else {
          p <- p + aes(fill = factor(1))
        }
        p <- p + geom_bar(stat = "identity", alpha = 0.7) +
          labs(title = paste("Bar Chart:", input$x_var), y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else if (input$plot_type == "density") {
        # Fixed density plot
        p <- ggplot(values$current_data, aes_string(x = input$x_var))
        if (input$color_var != "none") {
          p <- p + aes_string(fill = input$color_var, color = input$color_var) +
            scale_fill_manual(values = color_palette) +
            scale_color_manual(values = color_palette) +
            geom_density(alpha = 0.5)
        } else {
          p <- p + geom_density(fill = "#1f77b4", alpha = 0.5)
        }
        p <- p + labs(title = paste("Density Plot:", input$x_var)) +
          theme_minimal()
        
      } else if (input$plot_type == "correlation") {
        numeric_data <- values$current_data[sapply(values$current_data, is.numeric)]
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        
        # Convert correlation matrix to long format for ggplot
        cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
        cor_df$value <- as.vector(cor_matrix)
        
        p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient2(low = "#2ca02c", high = "#d62728", mid = "white", midpoint = 0) +
          labs(title = "Correlation Matrix") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      output$main_plot <- renderPlotly({
        if (exists("p") && inherits(p, "ggplot")) {
          ggplotly(p)
        } else {
          plotly::plot_ly() %>% plotly::add_text(text = "Error: Plot tidak dapat dibuat")
        }
      })
      
    }, error = function(e) {
      output$main_plot <- renderPlotly({
        plotly::plot_ly() %>% plotly::add_text(text = paste("Error:", e$message))
      })
    })
    
    output$visual_interpretation <- renderText({
      switch(input$plot_type,
             "scatter" = "Interpretasi Visualisasi:\n\nScatter plot menunjukkan hubungan antara dua variabel numerik. Garis regresi (merah) menunjukkan trend linear. Titik-titik yang tersebar di sekitar garis mengindikasikan kekuatan korelasi.",
             "boxplot" = "Interpretasi Visualisasi:\n\nBox plot menampilkan distribusi data melalui kuartil. Kotak menunjukkan IQR (Q1-Q3), garis tengah adalah median, dan whiskers menunjukkan range data. Outlier ditampilkan sebagai titik terpisah.",
             "histogram" = "Interpretasi Visualisasi:\n\nHistogram menunjukkan distribusi frekuensi variabel. Bentuk distribusi dapat mengindikasikan normalitas, skewness, atau multimodality data.",
             "correlation" = "Interpretasi Visualisasi:\n\nCorrelation matrix menunjukkan kekuatan hubungan linear antar variabel. Warna merah menunjukkan korelasi positif, hijau korelasi negatif, dan putih tidak ada korelasi.",
             "barplot" = "Interpretasi Visualisasi:\n\nBar chart menunjukkan frekuensi atau count dari setiap kategori dalam variabel. Tinggi bar mencerminkan jumlah observasi per kategori.",
             "density" = "Interpretasi Visualisasi:\n\nDensity plot menunjukkan estimasi distribusi probabilitas data. Kurva yang halus memberikan gambaran bentuk distribusi yang lebih smooth dibanding histogram.",
             "Visualisasi telah dibuat sesuai dengan jenis plot yang dipilih."
      )
    })
  })
  
  # =================== MAPPING (FIXED) ===================
  # Reactive values for map
  values$map_data <- reactive({
    if (input$map_variable != "" && !is.null(input$map_variable)) {
      n_points <- min(nrow(values$current_data), 200)  # Reduced for performance
      indices <- sample(nrow(values$current_data), n_points)
      
      # Generate more realistic US coordinates based on state data if available
      if ("State" %in% names(values$current_data)) {
        # Use state-based coordinates (simplified)
        state_coords <- data.frame(
          State = c("CA", "TX", "FL", "NY", "PA", "IL", "OH", "MI", "GA", "NC"),
          lat = c(36.7783, 31.9686, 27.7663, 42.1657, 40.2732, 40.3363, 40.3888, 43.3266, 33.7490, 35.7596),
          lng = c(-119.4179, -99.9018, -82.6404, -74.9481, -77.1017, -89.0022, -82.7649, -84.3426, -84.3426, -79.0193)
        )
        
        sample_data <- values$current_data[indices, ]
        map_coords <- merge(sample_data, state_coords, by = "State", all.x = TRUE)
        
        # Add random variation to coordinates
        map_coords$lat <- map_coords$lat + runif(nrow(map_coords), -2, 2)
        map_coords$lng <- map_coords$lng + runif(nrow(map_coords), -2, 2)
        
        # Fill missing coordinates with random US coordinates
        missing_coords <- is.na(map_coords$lat)
        map_coords$lat[missing_coords] <- runif(sum(missing_coords), 25, 49)
        map_coords$lng[missing_coords] <- runif(sum(missing_coords), -125, -65)
      } else {
        # Generate random US coordinates
        map_coords <- values$current_data[indices, ]
        map_coords$lat <- runif(n_points, 25, 49)
        map_coords$lng <- runif(n_points, -125, -65)
      }
      
      map_coords$value <- map_coords[[input$map_variable]]
      return(map_coords)
    }
    return(NULL)
  })
  
  observeEvent(input$create_map, {
    req(input$map_variable)
    
    map_data <- values$map_data()
    if (is.null(map_data)) return()
    
    # Create different map types
    if (input$map_type == "heatmap") {
      # Heat map style
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = map_data$value
      )
      
      map_widget <- leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -95, lat = 39, zoom = 4) %>%
        addCircleMarkers(
          ~lng, ~lat,
          radius = 8,
          color = "white",
          weight = 1,
          fillColor = ~pal(value),
          fillOpacity = 0.8,
          popup = ~paste("<strong>", input$map_variable, ":</strong>", round(value, 3))
        )
      
    } else if (input$map_type == "choropleth") {
      # Choropleth style with larger circles
      pal <- colorBin(
        palette = "Blues",
        domain = map_data$value,
        bins = 5
      )
      
      map_widget <- leaflet(map_data) %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        setView(lng = -95, lat = 39, zoom = 4) %>%
        addCircleMarkers(
          ~lng, ~lat,
          radius = ~pmax(5, pmin(20, (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)) * 15 + 5)),
          color = "darkblue",
          weight = 2,
          fillColor = ~pal(value),
          fillOpacity = 0.7,
          popup = ~paste("<strong>", input$map_variable, ":</strong>", round(value, 3))
        )
      
    } else { # points
      # Point map style
      pal <- colorNumeric(
        palette = c("#0066CC", "#3399FF", "#66B2FF", "#99CCFF", "#FFCC99", "#FF9966", "#FF6633", "#FF3300"),
        domain = map_data$value
      )
      
      map_widget <- leaflet(map_data) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        setView(lng = -95, lat = 39, zoom = 4) %>%
        addCircleMarkers(
          ~lng, ~lat,
          radius = 6,
          color = "white",
          weight = 1,
          fillColor = ~pal(value),
          fillOpacity = 0.9,
          popup = ~paste("<strong>", input$map_variable, ":</strong>", round(value, 3))
        )
    }
    
    # Add legend and scale
    map_widget <- map_widget %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~value,
        title = paste("Nilai", input$map_variable),
        opacity = 0.8
      ) %>%
      addScaleBar(position = "bottomleft")
    
    output$interactive_map <- renderLeaflet({
      map_widget
    })
    
    output$map_interpretation <- renderText({
      map_type_desc <- switch(input$map_type,
                              "heatmap" = "Heat map menggunakan gradasi warna untuk menunjukkan intensitas nilai variabel dengan latar belakang yang bersih.",
                              "choropleth" = "Choropleth map menggunakan ukuran dan warna lingkaran untuk menunjukkan variasi nilai dengan peta standar sebagai latar.",
                              "points" = "Point map menampilkan data sebagai titik-titik dengan warna yang mencerminkan nilai variabel di atas citra satelit."
      )
      
      paste0("Interpretasi Peta:\n\nPeta interaktif menunjukkan distribusi geografis dari variabel ", input$map_variable, 
             " menggunakan style ", input$map_type, ". ", map_type_desc,
             "\nKlik pada titik untuk melihat nilai detail. Pola spasial dapat mengungkap clustering geografis atau distribusi regional.")
    })
  })
  
  # =================== ASSUMPTION TESTS (FIXED) ===================
  observeEvent(input$run_assumptions, {
    req(input$assumption_var)
    
    var_data <- values$current_data[[input$assumption_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if (input$test_normality) {
      # Shapiro-Wilk test
      if (length(var_data) <= 5000) {
        norm_test <- shapiro.test(var_data)
      } else {
        # Use Anderson-Darling for large samples
        norm_test <- nortest::ad.test(var_data)
      }
      
      output$normality_result <- renderText({
        paste0(
          "**HIPOTESIS UJI NORMALITAS:**\n\n",
          "• H₀: Data berdistribusi normal\n",
          "• H₁: Data tidak berdistribusi normal\n\n",
          "**HASIL UJI NORMALITAS:**\n\n",
          "• Test: Shapiro-Wilk Test (atau Anderson-Darling untuk n>5000)\n",
          "• Statistik: ", round(norm_test$statistic, 4), "\n",
          "• p-value: ", format(norm_test$p.value, scientific = TRUE), "\n",
          "• Sampel size: ", length(var_data)
        )
      })
      
      output$normality_interpretation <- renderText({
        basic_interp <- create_interpretation(norm_test, "normality")
        
        detailed_interp <- paste0(
          "**INTERPRETASI UJI NORMALITAS LENGKAP:**\n\n",
          basic_interp, "\n\n",
          "**PENJELASAN STATISTIK:**\n",
          "• Test Statistic: ", round(norm_test$statistic, 4), "\n",
          "• p-value: ", format(norm_test$p.value, scientific = TRUE), "\n",
          "• Sampel size: ", length(var_data), "\n\n",
          "**KRITERIA KEPUTUSAN:**\n",
          "• α = 0.05 (tingkat signifikansi)\n",
          "• Jika p-value > 0.05: Gagal tolak H₀ (data normal)\n",
          "• Jika p-value ≤ 0.05: Tolak H₀ (data tidak normal)\n\n",
          "**IMPLIKASI UNTUK ANALISIS:**\n",
          if (norm_test$p.value > 0.05) {
            "• Data dapat digunakan untuk uji parametrik (t-test, ANOVA, regresi)\n• Asumsi normalitas terpenuhi\n• Hasil statistik inferensia akan valid"
          } else {
            "• Pertimbangkan transformasi data (log, sqrt, dll)\n• Gunakan uji non-parametrik sebagai alternatif\n• Periksa outlier yang mungkin mempengaruhi distribusi"
          }, "\n\n",
          "**CATATAN:** Untuk sampel besar (n>30), CLT berlaku sehingga normalitas kurang kritis."
        )
        
        return(detailed_interp)
      })
    }
    
    # Fixed homogeneity test
    if (input$test_homogeneity && input$assumption_group != "none" && input$assumption_group %in% names(values$current_data)) {
      group_data <- values$current_data[[input$assumption_group]]
      
      # Levene's test for homogeneity of variances
      test_data <- data.frame(value = var_data, group = group_data[1:length(var_data)])
      test_data <- test_data[complete.cases(test_data), ]
      
      if (length(unique(test_data$group)) > 1) {
        levene_test <- car::leveneTest(value ~ group, data = test_data)
        
        output$homogeneity_result <- renderText({
          paste0(
            "**HIPOTESIS UJI HOMOGENITAS:**\n\n",
            "• H₀: Varians antar kelompok homogen (σ₁² = σ₂² = ... = σₖ²)\n",
            "• H₁: Varians antar kelompok tidak homogen\n\n",
            "**HASIL UJI HOMOGENITAS (LEVENE'S TEST):**\n\n",
            "• F-statistic: ", round(levene_test$`F value`[1], 4), "\n",
            "• df1: ", levene_test$Df[1], "\n",
            "• df2: ", levene_test$Df[2], "\n",
            "• p-value: ", format(levene_test$`Pr(>F)`[1], scientific = TRUE), "\n",
            "• Jumlah grup: ", length(unique(test_data$group))
          )
        })
        
        output$homogeneity_interpretation <- renderText({
          basic_interp <- create_interpretation(list(p.value = levene_test$`Pr(>F)`[1]), "homogeneity")
          
          detailed_interp <- paste0(
            "**INTERPRETASI UJI HOMOGENITAS VARIANS:**\n\n",
            basic_interp, "\n\n",
            "**PENJELASAN STATISTIK:**\n",
            "• F-statistic: ", round(levene_test$`F value`[1], 4), "\n",
            "• df1: ", levene_test$Df[1], ", df2: ", levene_test$Df[2], "\n",
            "• p-value: ", format(levene_test$`Pr(>F)`[1], scientific = TRUE), "\n",
            "• Jumlah grup: ", length(unique(test_data$group)), "\n\n",
            "**KRITERIA KEPUTUSAN:**\n",
            "• H₀: σ₁² = σ₂² = ... = σₖ² (varians sama)\n",
            "• H₁: Minimal ada satu varians berbeda\n",
            "• α = 0.05 (tingkat signifikansi)\n\n",
            "**IMPLIKASI UNTUK ANALISIS:**\n",
            if (levene_test$`Pr(>F)`[1] > 0.05) {
              "• Dapat menggunakan ANOVA klasik\n• Pooled variance t-test valid\n• Asumsi homoskedastisitas terpenuhi"
            } else {
              "• Gunakan Welch's ANOVA (tidak asumsikan varians sama)\n• Separate variance t-test lebih tepat\n• Pertimbangkan transformasi data\n• Gunakan uji non-parametrik (Kruskal-Wallis)"
            }, "\n\n",
            "**CATATAN:** Levene's test robust terhadap non-normalitas dibanding Bartlett's test."
          )
          
          return(detailed_interp)
        })
      } else {
        output$homogeneity_result <- renderText({
          "Error: Variabel kelompok harus memiliki lebih dari satu kategori untuk uji homogenitas."
        })
        
        output$homogeneity_interpretation <- renderText({
          "Tidak dapat melakukan uji homogenitas karena hanya ada satu kelompok."
        })
      }
    } else if (input$test_homogeneity) {
      output$homogeneity_result <- renderText({
        "Pilih variabel kelompok untuk melakukan uji homogenitas."
      })
      
      output$homogeneity_interpretation <- renderText({
        "Uji homogenitas memerlukan variabel kelompok untuk membandingkan varians antar grup."
      })
    }
    
    # Bartlett test
    if (input$test_bartlett && input$assumption_group != "none" && input$assumption_group %in% names(values$current_data)) {
      group_data <- values$current_data[[input$assumption_group]]
      
      # Data validation
      if (length(var_data) < 10) {
        output$bartlett_result <- renderText({
          "PERINGATAN: Data kurang dari 10 observasi. Hasil uji Bartlett mungkin tidak reliabel."
        })
      } else {
        test_data <- data.frame(value = var_data, group = group_data[1:length(var_data)])
        test_data <- test_data[complete.cases(test_data), ]
        
        if (length(unique(test_data$group)) > 1) {
          bartlett_test <- bartlett.test(value ~ group, data = test_data)
          
          output$bartlett_result <- renderText({
            paste0(
              "HIPOTESIS UJI BARTLETT:\n\n",
              "H₀: Varians antar kelompok sama (σ₁² = σ₂² = ... = σₖ²)\n",
              "H₁: Varians antar kelompok tidak sama\n\n",
              "HASIL UJI BARTLETT:\n\n",
              "• Bartlett's K-squared: ", round(bartlett_test$statistic, 4), "\n",
              "• df: ", bartlett_test$parameter, "\n",
              "• p-value: ", format(bartlett_test$p.value, scientific = TRUE), "\n",
              "• Jumlah grup: ", length(unique(test_data$group))
            )
          })
          
          output$bartlett_interpretation <- renderText({
            paste0(
              "INTERPRETASI UJI BARTLETT:\n\n",
              if (bartlett_test$p.value > 0.05) {
                "Hasil menunjukkan varians antar kelompok homogen (p > 0.05).\n\nKESIMPULAN:\n• Gagal tolak H₀\n• Asumsi homogenitas varians terpenuhi\n• Dapat menggunakan uji parametrik klasik"
              } else {
                "Hasil menunjukkan varians antar kelompok tidak homogen (p ≤ 0.05).\n\nKESIMPULAN:\n• Tolak H₀\n• Asumsi homogenitas varians dilanggar\n• Gunakan uji yang robust terhadap heteroskedastisitas"
              }, "\n\n",
              "PERBEDAAN BARTLETT vs LEVENE:\n",
              "• Bartlett: Lebih sensitif terhadap non-normalitas\n",
              "• Levene: Lebih robust terhadap non-normalitas\n",
              "• Gunakan Levene jika data tidak normal\n",
              "• Gunakan Bartlett jika data normal dan perlu sensitivitas tinggi"
            )
          })
        } else {
          output$bartlett_result <- renderText({
            "Error: Variabel kelompok harus memiliki lebih dari satu kategori untuk uji Bartlett."
          })
          
          output$bartlett_interpretation <- renderText({
            "Tidak dapat melakukan uji Bartlett karena hanya ada satu kelompok."
          })
        }
      }
    } else if (input$test_bartlett) {
      output$bartlett_result <- renderText({
        "Pilih variabel kelompok untuk melakukan uji Bartlett."
      })
      
      output$bartlett_interpretation <- renderText({
        "Uji Bartlett memerlukan variabel kelompok untuk membandingkan varians antar grup."
      })
    }
    
    # Create assumption plots
    output$assumption_plots <- renderPlotly({
      p1 <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(bins = 30, alpha = 0.7, fill = "lightblue") +
        geom_density(color = "red", size = 1) +
        labs(title = "Histogram & Density Plot", x = input$assumption_var) +
        theme_minimal()
      
      p2 <- ggplot(data.frame(x = var_data), aes(sample = x)) +
        geom_qq() +
        geom_qq_line(color = "red") +
        labs(title = "Q-Q Plot") +
        theme_minimal()
      
      subplot(ggplotly(p1), ggplotly(p2), nrows = 1)
    })
  })
  
  # =================== MEAN TESTS ===================
  observeEvent(input$run_mean_test, {
    req(input$mean_test_var, input$mean_test_type)
    
    var_data <- values$current_data[[input$mean_test_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if (input$mean_test_type == "one_sample") {
      test_result <- t.test(var_data, mu = input$test_value, conf.level = input$confidence_level)
      
      output$mean_test_result <- renderText({
        paste0(
          "**HIPOTESIS UJI T SATU SAMPEL:**\n\n",
          "• H₀: μ = ", input$test_value, " (rata-rata populasi sama dengan nilai uji)\n",
          "• H₁: μ ≠ ", input$test_value, " (rata-rata populasi berbeda dari nilai uji)\n\n",
          "**HASIL UJI T SATU SAMPEL:**\n\n",
          "• t-statistic: ", round(test_result$statistic, 4), "\n",
          "• df: ", test_result$parameter, "\n",
          "• p-value: ", format(test_result$p.value, scientific = TRUE), "\n",
          "• Confidence Interval: [", paste(round(test_result$conf.int, 4), collapse = ", "), "]\n",
          "• Sample Mean: ", round(test_result$estimate, 4), "\n",
          "• Test Value: ", input$test_value
        )
      })
      
    } else if (input$mean_test_type == "two_sample" && !is.null(input$group_var_mean)) {
      group_data <- values$current_data[[input$group_var_mean]]
      test_data <- data.frame(value = var_data, group = group_data)
      test_data <- test_data[complete.cases(test_data), ]
      
      groups <- unique(test_data$group)
      if (length(groups) >= 2) {
        group1_data <- test_data$value[test_data$group == groups[1]]
        group2_data <- test_data$value[test_data$group == groups[2]]
        
        test_result <- t.test(group1_data, group2_data, conf.level = input$confidence_level)
        
        output$mean_test_result <- renderText({
          paste0(
            "**HIPOTESIS UJI T DUA SAMPEL:**\n\n",
            "• H₀: μ₁ = μ₂ (rata-rata kedua kelompok sama)\n",
            "• H₁: μ₁ ≠ μ₂ (rata-rata kedua kelompok berbeda)\n\n",
            "**HASIL UJI T DUA SAMPEL:**\n\n",
            "• t-statistic: ", round(test_result$statistic, 4), "\n",
            "• df: ", round(test_result$parameter, 2), "\n",
            "• p-value: ", format(test_result$p.value, scientific = TRUE), "\n",
            "• Confidence Interval: [", paste(round(test_result$conf.int, 4), collapse = ", "), "]\n",
            "• Mean Group 1 (", groups[1], "): ", round(test_result$estimate[1], 4), "\n",
            "• Mean Group 2 (", groups[2], "): ", round(test_result$estimate[2], 4)
          )
        })
      }
    }
    
    output$mean_test_interpretation <- renderText({
      if (exists("test_result")) {
        basic_interp <- create_interpretation(test_result, "t_test")
        
        detailed_interp <- paste0(
          "**INTERPRETASI UJI RATA-RATA LENGKAP:**\n\n",
          basic_interp, "\n\n",
          "**PENJELASAN STATISTIK:**\n",
          "• t-statistic: ", round(test_result$statistic, 4), "\n",
          "• df: ", round(test_result$parameter, 2), "\n",
          "• p-value: ", format(test_result$p.value, scientific = TRUE), "\n",
          "• Confidence Interval: [", paste(round(test_result$conf.int, 4), collapse = ", "), "]\n\n",
          "**EFFECT SIZE:** Cohen's d ≈ ", round(abs(test_result$statistic) / sqrt(test_result$parameter + 1), 3), "\n\n",
          "**KRITERIA KEPUTUSAN:**\n",
          "• α = 0.05 (tingkat signifikansi)\n",
          "• Jika p-value < 0.05: Tolak H₀\n",
          "• Jika p-value ≥ 0.05: Gagal tolak H₀\n\n",
          "**KESIMPULAN:**\n",
          if (test_result$p.value < 0.05) {
            "Terdapat perbedaan signifikan secara statistik.\n• Hasil mendukung H₁\n• Perbedaan tidak disebabkan oleh kebetulan"
          } else {
            "Tidak terdapat perbedaan signifikan secara statistik.\n• Hasil mendukung H₀\n• Perbedaan bisa disebabkan oleh kebetulan"
          }
        )
        
        return(detailed_interp)
      }
    })
    
    # Create plot
    output$mean_test_plot <- renderPlotly({
      if (input$mean_test_type == "one_sample") {
        p <- ggplot(data.frame(x = var_data), aes(x = x)) +
          geom_histogram(bins = 30, alpha = 0.7, fill = "lightblue") +
          geom_vline(xintercept = mean(var_data), color = "red", linetype = "dashed", size = 1) +
          geom_vline(xintercept = input$test_value, color = "blue", linetype = "solid", size = 1) +
          labs(title = "Distribution with Sample Mean (red) and Test Value (blue)") +
          theme_minimal()
      } else {
        group_data <- values$current_data[[input$group_var_mean]]
        plot_data <- data.frame(value = var_data, group = group_data)
        plot_data <- plot_data[complete.cases(plot_data), ]
        
        p <- ggplot(plot_data, aes(x = group, y = value, fill = group)) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          labs(title = "Comparison between Groups") +
          theme_minimal()
      }
      ggplotly(p)
    })
  })
  
  # =================== PROPORTION & VARIANCE TESTS ===================
  observeEvent(input$run_prop_var_test, {
    # Wrap entire test in try-catch to prevent crashes
    tryCatch({
      req(input$prop_var_variable, input$prop_var_test_type)
      
      # Use transformed data if available
      data_to_use <- if (!is.null(values$transformed_data)) values$transformed_data else values$current_data
      
      # Get variable data with proper validation
      if (!input$prop_var_variable %in% names(data_to_use)) {
        output$prop_var_result <- renderText({
          "ERROR: Variabel tidak ditemukan dalam dataset."
        })
        output$prop_var_interpretation <- renderText({
          "Silakan pilih variabel yang tersedia dalam dataset."
        })
        return()
      }
    
    var_data <- data_to_use[[input$prop_var_variable]]
    
    # Convert to numeric if needed for variance tests
    if (input$prop_var_test_type %in% c("var_one", "var_two")) {
      if (!is.numeric(var_data)) {
        # Try to convert to numeric
        var_data_numeric <- suppressWarnings(as.numeric(as.character(var_data)))
        if (all(is.na(var_data_numeric))) {
          output$prop_var_result <- renderText({
            "ERROR: Variabel harus numerik untuk uji varians. Silakan pilih variabel numerik."
          })
          return()
        }
        var_data <- var_data_numeric
      }
    }
    
    # Remove missing values
    var_data <- var_data[!is.na(var_data)]
    
    # Check minimum data requirements
    if (length(var_data) < 5) {
      output$prop_var_result <- renderText({
        paste0("ERROR: Data tidak cukup untuk analisis. Ditemukan ", length(var_data), 
               " observasi valid, minimum 5 diperlukan.")
      })
      return()
    }
    
    if (input$prop_var_test_type == "prop_one") {
      # One sample proportion test
      # Convert to binary if needed
      if (is.numeric(var_data)) {
        # Convert to binary based on median
        binary_data <- ifelse(var_data > median(var_data, na.rm = TRUE), 1, 0)
      } else {
        # Assume already categorical
        unique_vals <- unique(var_data)
        binary_data <- ifelse(var_data == unique_vals[1], 1, 0)
      }
      
      successes <- sum(binary_data)
      n <- length(binary_data)
      
      test_result <- prop.test(successes, n, p = input$prop_test_value)
      
      output$prop_var_result <- renderText({
        paste0(
          "HIPOTESIS UJI PROPORSI SATU SAMPEL:\n\n",
          "H₀: p = ", input$prop_test_value, " (proporsi populasi sama dengan nilai uji)\n",
          "H₁: p ≠ ", input$prop_test_value, " (proporsi populasi berbeda dari nilai uji)\n\n",
          "HASIL UJI PROPORSI SATU SAMPEL:\n\n",
          "• Chi-squared statistic: ", round(test_result$statistic, 4), "\n",
          "• df: ", test_result$parameter, "\n",
          "• p-value: ", format(test_result$p.value, scientific = TRUE), "\n",
          "• Confidence Interval: [", paste(round(test_result$conf.int, 4), collapse = ", "), "]\n",
          "• Sample proportion: ", round(test_result$estimate, 4), "\n",
          "• Test value: ", input$prop_test_value, "\n",
          "• Sample size: ", n
        )
      })
      
    } else if (input$prop_var_test_type == "var_one") {
      # One sample variance test (Chi-square test)
      sample_var <- var(var_data, na.rm = TRUE)
      n <- length(var_data)
      chi_stat <- (n - 1) * sample_var / input$var_test_value
      p_value <- 2 * pmin(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))
      
      # Create test result object for interpretation
      test_result <- list(statistic = chi_stat, p.value = p_value, df = n - 1)
      
      output$prop_var_result <- renderText({
        paste0(
          "HIPOTESIS UJI VARIANS SATU SAMPEL:\n\n",
          "H₀: σ² = ", input$var_test_value, " (varians populasi sama dengan nilai uji)\n",
          "H₁: σ² ≠ ", input$var_test_value, " (varians populasi berbeda dari nilai uji)\n\n",
          "HASIL UJI VARIANS SATU SAMPEL:\n\n",
          "• Chi-squared statistic: ", round(chi_stat, 4), "\n",
          "• df: ", n - 1, "\n",
          "• p-value: ", format(p_value, scientific = TRUE), "\n",
          "• Sample variance: ", round(sample_var, 4), "\n",
          "• Test value: ", input$var_test_value, "\n",
          "• Sample size: ", n
        )
      })
      
      # Force interpretation update for variance test
      output$prop_var_interpretation <- renderText({
        paste0(
          "INTERPRETASI HASIL UJI VARIANS 1 SAMPEL:\n\n",
          "HASIL UJI:\n",
          "• Chi-squared statistic: ", round(chi_stat, 4), "\n",
          "• df: ", n - 1, "\n",
          "• p-value: ", format(p_value, scientific = TRUE), "\n\n",
          "KEPUTUSAN STATISTIK:\n",
          if (p_value < 0.05) {
            paste0("• Tolak H₀ (p = ", round(p_value, 4), " < 0.05)\n",
                   "• Varians populasi BERBEDA secara signifikan dari nilai yang diuji\n",
                   "• Perbedaan tidak disebabkan oleh kebetulan")
          } else {
            paste0("• Gagal tolak H₀ (p = ", round(p_value, 4), " ≥ 0.05)\n",
                   "• Varians populasi TIDAK BERBEDA secara signifikan dari nilai yang diuji\n",
                   "• Data konsisten dengan hipotesis null")
          }, "\n\n",
          "IMPLIKASI PRAKTIS:\n",
          if (p_value < 0.05) {
            "• Variabilitas data berbeda dari ekspektasi\n• Perlu investigasi faktor penyebab variabilitas\n• Pertimbangkan transformasi data jika diperlukan"
          } else {
            "• Variabilitas data sesuai dengan ekspektasi\n• Model atau asumsi varians dapat diterima\n• Data memiliki konsistensi yang baik"
          }
        )
      })
      
    } else if (input$prop_var_test_type == "prop_two" && !is.null(input$group_var_prop)) {
      # Two sample proportion test
      if (!input$group_var_prop %in% names(data_to_use)) {
        output$prop_var_result <- renderText({
          "ERROR: Variabel kelompok tidak ditemukan dalam dataset."
        })
        output$prop_var_interpretation <- renderText({
          "Pilih variabel kelompok yang valid untuk melakukan uji proporsi dua sampel."
        })
        return()
      }
      
      group_data <- data_to_use[[input$group_var_prop]]
      
      # Ensure same length for data.frame
      min_length <- min(length(var_data), length(group_data))
      if (min_length == 0) {
        output$prop_var_result <- renderText({
          "ERROR: Tidak ada data yang valid untuk analisis."
        })
        output$prop_var_interpretation <- renderText({
          "Periksa data dan pastikan variabel memiliki nilai yang valid."
        })
        return()
      }
      
      test_data <- data.frame(
        value = var_data[1:min_length], 
        group = group_data[1:min_length]
      )
      test_data <- test_data[complete.cases(test_data), ]
      
      if (nrow(test_data) < 10) {
        output$prop_var_result <- renderText({
          paste0("ERROR: Data tidak cukup untuk uji dua sampel. Ditemukan ", nrow(test_data), 
                 " observasi valid, minimum 10 diperlukan.")
        })
        output$prop_var_interpretation <- renderText({
          "Uji proporsi dua sampel memerlukan minimal 10 observasi yang valid."
        })
        return()
      }
      
      groups <- unique(test_data$group)
      if (length(groups) >= 2) {
        group1_data <- test_data$value[test_data$group == groups[1]]
        group2_data <- test_data$value[test_data$group == groups[2]]
        
        # Convert to binary
        if (is.numeric(group1_data)) {
          median_val <- median(c(group1_data, group2_data), na.rm = TRUE)
          binary1 <- ifelse(group1_data > median_val, 1, 0)
          binary2 <- ifelse(group2_data > median_val, 1, 0)
        } else {
          unique_val <- unique(c(group1_data, group2_data))[1]
          binary1 <- ifelse(group1_data == unique_val, 1, 0)
          binary2 <- ifelse(group2_data == unique_val, 1, 0)
        }
        
        successes <- c(sum(binary1), sum(binary2))
        totals <- c(length(binary1), length(binary2))
        
        test_result <- prop.test(successes, totals)
        
        output$prop_var_result <- renderText({
          paste0(
            "HIPOTESIS UJI PROPORSI DUA SAMPEL:\n\n",
            "H₀: p₁ = p₂ (proporsi kedua kelompok sama)\n",
            "H₁: p₁ ≠ p₂ (proporsi kedua kelompok berbeda)\n\n",
            "HASIL UJI PROPORSI DUA SAMPEL:\n\n",
            "• Chi-squared statistic: ", round(test_result$statistic, 4), "\n",
            "• df: ", test_result$parameter, "\n",
            "• p-value: ", format(test_result$p.value, scientific = TRUE), "\n",
            "• Proportion Group 1 (", groups[1], "): ", round(test_result$estimate[1], 4), "\n",
            "• Proportion Group 2 (", groups[2], "): ", round(test_result$estimate[2], 4)
          )
        })
      }
      
    } else if (input$prop_var_test_type == "var_two" && !is.null(input$group_var_prop)) {
      # Two sample variance test (F-test)
      if (!input$group_var_prop %in% names(data_to_use)) {
        output$prop_var_result <- renderText({
          "ERROR: Variabel kelompok tidak ditemukan dalam dataset."
        })
        output$prop_var_interpretation <- renderText({
          "Pilih variabel kelompok yang valid untuk melakukan uji varians dua sampel."
        })
        return()
      }
      
      group_data <- data_to_use[[input$group_var_prop]]
      
      # Ensure same length for data.frame
      min_length <- min(length(var_data), length(group_data))
      if (min_length == 0) {
        output$prop_var_result <- renderText({
          "ERROR: Tidak ada data yang valid untuk analisis."
        })
        output$prop_var_interpretation <- renderText({
          "Periksa data dan pastikan variabel memiliki nilai yang valid."
        })
        return()
      }
      
      test_data <- data.frame(
        value = var_data[1:min_length], 
        group = group_data[1:min_length]
      )
      test_data <- test_data[complete.cases(test_data), ]
      
      if (nrow(test_data) < 10) {
        output$prop_var_result <- renderText({
          paste0("ERROR: Data tidak cukup untuk uji dua sampel. Ditemukan ", nrow(test_data), 
                 " observasi valid, minimum 10 diperlukan.")
        })
        output$prop_var_interpretation <- renderText({
          "Uji varians dua sampel memerlukan minimal 10 observasi yang valid."
        })
        return()
      }
      
      groups <- unique(test_data$group)
      if (length(groups) >= 2) {
        group1_data <- test_data$value[test_data$group == groups[1]]
        group2_data <- test_data$value[test_data$group == groups[2]]
        
        # Check minimum sample sizes for F-test
        if (length(group1_data) < 3 || length(group2_data) < 3) {
          output$prop_var_result <- renderText({
            paste0("ERROR: Ukuran sampel terlalu kecil untuk uji F.\n",
                   "Grup ", groups[1], ": ", length(group1_data), " observasi\n",
                   "Grup ", groups[2], ": ", length(group2_data), " observasi\n",
                   "Minimum 3 observasi per grup diperlukan.")
          })
          return()
        }
        
        test_result <- var.test(group1_data, group2_data)
        
        output$prop_var_result <- renderText({
          paste0(
            "HIPOTESIS UJI VARIANS DUA SAMPEL:\n\n",
            "H₀: σ₁² = σ₂² (varians kedua kelompok sama)\n",
            "H₁: σ₁² ≠ σ₂² (varians kedua kelompok berbeda)\n\n",
            "HASIL UJI VARIANS DUA SAMPEL (F-TEST):\n\n",
            "• F-statistic: ", round(test_result$statistic, 4), "\n",
            "• df num: ", test_result$parameter[1], "\n",
            "• df den: ", test_result$parameter[2], "\n",
            "• p-value: ", format(test_result$p.value, scientific = TRUE), "\n",
            "• Confidence Interval: [", paste(round(test_result$conf.int, 4), collapse = ", "), "]\n",
            "• Variance ratio: ", round(test_result$estimate, 4)
          )
        })
      }
    }
    
    # Interpretation with specific handling for each test type
    output$prop_var_interpretation <- renderText({
      if (exists("test_result")) {
        if (input$prop_var_test_type == "var_one") {
          # Specific interpretation for one-sample variance test
          interpretation <- paste0(
            "INTERPRETASI HASIL UJI VARIANS 1 SAMPEL:\n\n",
            "HASIL UJI:\n",
            "• Chi-squared statistic: ", round(test_result$statistic, 4), "\n",
            "• df: ", test_result$df, "\n",
            "• p-value: ", format(test_result$p.value, scientific = TRUE), "\n\n",
            "KEPUTUSAN STATISTIK:\n",
            if (test_result$p.value < 0.05) {
              paste0("• Tolak H₀ (p = ", round(test_result$p.value, 4), " < 0.05)\n",
                     "• Varians populasi BERBEDA secara signifikan dari nilai yang diuji\n",
                     "• Perbedaan tidak disebabkan oleh kebetulan")
            } else {
              paste0("• Gagal tolak H₀ (p = ", round(test_result$p.value, 4), " ≥ 0.05)\n",
                     "• Varians populasi TIDAK BERBEDA secara signifikan dari nilai yang diuji\n",
                     "• Data konsisten dengan hipotesis null")
            }, "\n\n",
            "IMPLIKASI PRAKTIS:\n",
            if (test_result$p.value < 0.05) {
              "• Variabilitas data berbeda dari ekspektasi\n• Perlu investigasi faktor penyebab variabilitas\n• Pertimbangkan transformasi data jika diperlukan"
            } else {
              "• Variabilitas data sesuai dengan ekspektasi\n• Model atau asumsi varians dapat diterima\n• Data memiliki konsistensi yang baik"
            }
          )
        } else {
          # General interpretation for other tests
          interpretation <- if (test_result$p.value < 0.05) {
            paste0("INTERPRETASI HASIL UJI:\n\n",
                   "Hasil menunjukkan perbedaan yang signifikan secara statistik (p = ", 
                   round(test_result$p.value, 4), ").\n\n",
                   "KESIMPULAN:\n",
                   "• Tolak H₀, terima H₁\n",
                   "• Terdapat perbedaan signifikan pada parameter yang diuji\n",
                   "• Hasil tidak disebabkan oleh kebetulan (α = 0.05)")
          } else {
            paste0("INTERPRETASI HASIL UJI:\n\n",
                   "Hasil menunjukkan tidak ada perbedaan yang signifikan secara statistik (p = ", 
                   round(test_result$p.value, 4), ").\n\n",
                   "KESIMPULAN:\n",
                   "• Gagal tolak H₀\n",
                   "• Tidak terdapat perbedaan signifikan pada parameter yang diuji\n",
                   "• Perbedaan yang diamati bisa disebabkan oleh kebetulan")
          }
        }
        return(interpretation)
      } else {
        return("Interpretasi akan muncul setelah uji dijalankan.")
      }
    })
    
    # Create plot (moved inside the observe event to access variables)
    output$prop_var_plot <- renderPlotly({
      # Recreate the data for plotting to avoid scope issues
      if (!exists("var_data") || is.null(var_data)) {
        # Fallback: recreate var_data for plotting
        data_to_use_plot <- if (!is.null(values$transformed_data)) values$transformed_data else values$current_data
        
        if (input$prop_var_variable %in% names(data_to_use_plot)) {
          var_data_plot <- data_to_use_plot[[input$prop_var_variable]]
          
          # Convert to numeric if needed for variance tests
          if (input$prop_var_test_type %in% c("var_one", "var_two")) {
            if (!is.numeric(var_data_plot)) {
              var_data_plot <- suppressWarnings(as.numeric(as.character(var_data_plot)))
            }
          }
          var_data_plot <- var_data_plot[!is.na(var_data_plot)]
        } else {
          var_data_plot <- NULL
        }
      } else {
        var_data_plot <- var_data
      }
      
      tryCatch({
        if (is.null(var_data_plot) || length(var_data_plot) == 0) {
          # Error plot when no data
          plotly::plot_ly() %>% 
            plotly::add_text(x = 0.5, y = 0.5, text = "Tidak ada data untuk ditampilkan", 
                           textfont = list(size = 16), showlegend = FALSE) %>%
            plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                         yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                         title = "Tidak Ada Data")
        } else if (input$prop_var_test_type == "var_one") {
          # Specific plot for one-sample variance test
          if (is.numeric(var_data_plot)) {
            plot_df <- data.frame(nilai = var_data_plot)
            p <- ggplot(plot_df, aes(x = nilai)) +
              geom_histogram(bins = 30, alpha = 0.7, fill = "lightblue", color = "white") +
              geom_vline(xintercept = mean(var_data_plot, na.rm = TRUE), color = "red", linetype = "dashed", size = 1) +
              geom_vline(xintercept = sqrt(input$var_test_value), color = "blue", linetype = "solid", size = 1) +
              labs(title = "Distribusi Data (Merah: Sample Mean, Biru: Test SD)", 
                   x = input$prop_var_variable, y = "Frekuensi") +
              theme_minimal()
            ggplotly(p)
          } else {
            plotly::plot_ly() %>% 
              plotly::add_text(x = 0.5, y = 0.5, text = "Data harus numerik untuk uji varians", 
                             textfont = list(size = 16), showlegend = FALSE) %>%
              plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                           yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                           title = "Error: Data Tidak Numerik")
          }
        } else if (input$prop_var_test_type == "var_two" && !is.null(input$group_var_prop)) {
          # Two-sample variance plot
          data_to_use_plot <- if (!is.null(values$transformed_data)) values$transformed_data else values$current_data
          
          if (input$group_var_prop %in% names(data_to_use_plot)) {
            group_data_plot <- data_to_use_plot[[input$group_var_prop]]
            test_data_plot <- data.frame(value = var_data_plot, group = group_data_plot[1:length(var_data_plot)])
            test_data_plot <- test_data_plot[complete.cases(test_data_plot), ]
            
            if (nrow(test_data_plot) > 0) {
              p <- ggplot(test_data_plot, aes(x = group, y = value, fill = group)) +
                geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
                labs(title = "Perbandingan Varians antar Kelompok", 
                     x = "Kelompok", y = input$prop_var_variable) +
                theme_minimal()
              ggplotly(p)
            } else {
              plotly::plot_ly() %>% 
                plotly::add_text(x = 0.5, y = 0.5, text = "Tidak ada data yang valid", 
                               textfont = list(size = 16), showlegend = FALSE)
            }
          } else {
            plotly::plot_ly() %>% 
              plotly::add_text(x = 0.5, y = 0.5, text = "Variabel kelompok tidak ditemukan", 
                             textfont = list(size = 16), showlegend = FALSE)
          }
        } else {
          # Fallback plot for other test types
          if (is.numeric(var_data_plot)) {
            plot_df <- data.frame(nilai = var_data_plot)
            p <- ggplot(plot_df, aes(x = nilai)) +
              geom_histogram(bins = 20, alpha = 0.7, fill = "lightgreen", color = "white") +
              labs(title = "Distribusi Data", x = "Nilai", y = "Frekuensi") +
              theme_minimal()
            ggplotly(p)
          } else {
            # For non-numeric data (proportion tests)
            if (length(unique(var_data_plot)) <= 10) {
              plot_df <- data.frame(kategori = as.factor(var_data_plot))
              p <- ggplot(plot_df, aes(x = kategori)) +
                geom_bar(alpha = 0.7, fill = "lightcoral") +
                labs(title = "Distribusi Kategori", x = "Kategori", y = "Frekuensi") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
              ggplotly(p)
            } else {
              plotly::plot_ly() %>% 
                plotly::add_text(x = 0.5, y = 0.5, text = "Terlalu banyak kategori untuk ditampilkan", 
                               textfont = list(size = 16), showlegend = FALSE)
            }
          }
        }
      }, error = function(e) {
        # Create error plot
        plotly::plot_ly() %>% 
          plotly::add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), 
                         textfont = list(size = 14), showlegend = FALSE) %>%
          plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                       yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                       title = "Plot Error")
      })
    })
    }, error = function(e) {
      # Global error handler to prevent crashes
      output$prop_var_result <- renderText({
        paste("ERROR: Terjadi kesalahan dalam analisis:", e$message, 
              "\nSilakan periksa data dan pengaturan uji.")
      })
      output$prop_var_interpretation <- renderText({
        "Terjadi kesalahan. Pastikan data dan variabel yang dipilih sesuai dengan jenis uji yang dipilih."
      })
      
      # Create safe error plot
      output$prop_var_plot <- renderPlotly({
        plotly::plot_ly() %>% 
          plotly::add_text(x = 0.5, y = 0.5, text = "Error dalam visualisasi", 
                         textfont = list(size = 16), showlegend = FALSE) %>%
          plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                       yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                       title = "Visualization Error")
      })
    })
  })
  
  # =================== ANOVA TESTS ===================
  observeEvent(input$run_anova, {
    req(input$anova_dependent, input$anova_factor1)
    
    # Clean and filter data
    anova_data <- values$current_data[c(input$anova_dependent, input$anova_factor1)]
    if (input$anova_type == "twoway" && !is.null(input$anova_factor2)) {
      anova_data <- values$current_data[c(input$anova_dependent, input$anova_factor1, input$anova_factor2)]
    }
    anova_data <- anova_data[complete.cases(anova_data), ]
    
    if (nrow(anova_data) < 5) {
      output$anova_result <- renderText({
        "Error: Data tidak cukup untuk melakukan analisis ANOVA. Minimum 5 observasi diperlukan."
      })
      return()
    }
    
    tryCatch({
      if (input$anova_type == "oneway") {
        formula_str <- paste(input$anova_dependent, "~", input$anova_factor1)
        anova_model <- aov(as.formula(formula_str), data = anova_data)
        anova_summary <- summary(anova_model)
        
        # Extract statistics safely
        f_stat <- anova_summary[[1]]$`F value`[1]
        p_val <- anova_summary[[1]]$`Pr(>F)`[1]
        df1 <- anova_summary[[1]]$Df[1]
        df2 <- anova_summary[[1]]$Df[2]
        sum_sq_between <- anova_summary[[1]]$`Sum Sq`[1]
        sum_sq_within <- anova_summary[[1]]$`Sum Sq`[2]
        mean_sq_between <- anova_summary[[1]]$`Mean Sq`[1]
        mean_sq_within <- anova_summary[[1]]$`Mean Sq`[2]
        
        output$anova_result <- renderText({
          paste0(
            "HIPOTESIS UJI ANOVA SATU ARAH:\n\n",
            "H₀: μ₁ = μ₂ = ... = μₖ (semua rata-rata grup sama)\n",
            "H₁: Minimal ada satu rata-rata grup yang berbeda\n\n",
            "HASIL UJI ANOVA SATU ARAH:\n\n",
            "Sumber Variasi: Antar Grup\n",
            "• Sum of Squares: ", round(sum_sq_between, 4), "\n",
            "• df: ", df1, "\n",
            "• Mean Square: ", round(mean_sq_between, 4), "\n\n",
            "Sumber Variasi: Dalam Grup (Error)\n",
            "• Sum of Squares: ", round(sum_sq_within, 4), "\n",
            "• df: ", df2, "\n",
            "• Mean Square: ", round(mean_sq_within, 4), "\n\n",
            "• F-statistic: ", round(f_stat, 4), "\n",
            "• p-value: ", format(p_val, scientific = TRUE)
          )
        })
      
        output$anova_interpretation <- renderText({
          basic_interp <- create_interpretation(anova_summary[[1]], "anova")
          
          detailed_interp <- paste0(
            "INTERPRETASI ANOVA LENGKAP:\n\n",
            basic_interp, "\n\n",
            "PENJELASAN STATISTIK:\n",
            "• F-statistic: ", round(f_stat, 4), "\n",
            "• df antara grup: ", df1, "\n",
            "• df dalam grup: ", df2, "\n",
            "• p-value: ", format(p_val, scientific = TRUE), "\n\n",
            "EFFECT SIZE:\n",
            "• Eta-squared (η²) ≈ ", round(anova_summary[[1]]$`Sum Sq`[1] / sum(anova_summary[[1]]$`Sum Sq`), 3), "\n",
            "  • 0.01: Small effect\n",
            "  • 0.06: Medium effect\n",
            "  • 0.14: Large effect\n\n",
            "KRITERIA KEPUTUSAN:\n",
            "• α = 0.05 (tingkat signifikansi)\n",
            "• Jika p-value < 0.05: Tolak H₀\n",
            "• Jika p-value ≥ 0.05: Gagal tolak H₀\n\n",
            "KESIMPULAN:\n",
            if (p_val < 0.05) {
              "Terdapat perbedaan signifikan antar kelompok.\n• Hasil mendukung H₁\n• Lanjutkan dengan uji post-hoc untuk mengetahui kelompok mana yang berbeda."
            } else {
              "Tidak terdapat perbedaan signifikan antar kelompok.\n• Hasil mendukung H₀\n• Semua kelompok memiliki rata-rata yang sama secara statistik."
            }
          )
          
          return(detailed_interp)
        })
        
        if (input$post_hoc) {
          tryCatch({
            tukey_result <- TukeyHSD(anova_model)
            
            # Extract significant comparisons
            tukey_df <- as.data.frame(tukey_result[[1]])
            sig_comparisons <- tukey_df[tukey_df$`p adj` < 0.05, ]
            
            output$posthoc_result <- renderText({
              result_text <- paste("POST-HOC TEST (TUKEY HSD):\n\n")
              result_text <- paste0(result_text, "INTERPRETASI POST-HOC:\n\n")
              
              if (nrow(sig_comparisons) > 0) {
                result_text <- paste0(result_text, "PERBANDINGAN YANG SIGNIFIKAN (p < 0.05):\n")
                for (i in 1:nrow(sig_comparisons)) {
                  comp_name <- rownames(sig_comparisons)[i]
                  p_val <- sig_comparisons$`p adj`[i]
                  diff_val <- sig_comparisons$diff[i]
                  result_text <- paste0(result_text, "• ", comp_name, ": perbedaan = ", 
                                      round(diff_val, 4), ", p = ", round(p_val, 4), "\n")
                }
                result_text <- paste0(result_text, "\nKESIMPULAN: Terdapat ", nrow(sig_comparisons), 
                                    " pasangan kelompok yang berbeda secara signifikan.\n\n")
              } else {
                result_text <- paste0(result_text, "TIDAK ADA PERBANDINGAN YANG SIGNIFIKAN (p ≥ 0.05)\n")
                result_text <- paste0(result_text, "KESIMPULAN: Meskipun ANOVA menunjukkan perbedaan, ")
                result_text <- paste0(result_text, "post-hoc test tidak menemukan pasangan spesifik yang berbeda signifikan.\n\n")
              }
              
              result_text <- paste0(result_text, "DETAIL HASIL:\n", capture.output(print(tukey_result)))
              return(result_text)
            })
          }, error = function(e) {
            output$posthoc_result <- renderText({
              paste("Error dalam post-hoc test:", e$message)
            })
          })
        }
      
      } else if (input$anova_type == "twoway" && !is.null(input$anova_factor2)) {
        if (input$anova_interaction) {
          formula_str <- paste(input$anova_dependent, "~", input$anova_factor1, "*", input$anova_factor2)
        } else {
          formula_str <- paste(input$anova_dependent, "~", input$anova_factor1, "+", input$anova_factor2)
        }
        
        anova_model <- aov(as.formula(formula_str), data = anova_data)
        anova_summary <- summary(anova_model)
        
        output$anova_result <- renderText({
          paste("HASIL UJI ANOVA DUA ARAH:\n\n", capture.output(print(anova_summary)))
        })
        
        output$anova_interpretation <- renderText({
          "INTERPRETASI ANOVA DUA ARAH:\n\nANOVA dua arah menguji efek utama dari dua faktor dan interaksi antar faktor terhadap variabel dependen."
        })
      }
      
      # Create ANOVA plots
      output$anova_plots <- renderPlotly({
        tryCatch({
          if (exists("anova_model") && input$anova_type == "oneway") {
            p1 <- ggplot(anova_data, aes_string(x = input$anova_factor1, y = input$anova_dependent)) +
              geom_boxplot(aes_string(fill = input$anova_factor1), alpha = 0.7, 
                          outlier.shape = 16, outlier.size = 2, size = 1.2, width = 0.6) +
              geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
              labs(title = "Perbandingan Kelompok (Box Plot)", 
                   x = input$anova_factor1, y = input$anova_dependent) +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "none")
            
            # Residuals plot
            residuals_data <- data.frame(
              fitted = fitted(anova_model),
              residuals = residuals(anova_model)
            )
            
            p2 <- ggplot(residuals_data, aes(x = fitted, y = residuals)) +
              geom_point(alpha = 0.6, size = 2) +
              geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
              geom_smooth(se = FALSE, color = "blue", size = 1) +
              labs(title = "Residuals vs Fitted Values", 
                   x = "Fitted Values", y = "Residuals") +
              theme_minimal()
            
            subplot(ggplotly(p1), ggplotly(p2), nrows = 1, 
                    subplot_titles = c("Group Comparisons", "Residual Analysis"))
          } else if (exists("anova_model") && input$anova_type == "twoway") {
            p <- ggplot(anova_data, aes_string(x = input$anova_factor1, y = input$anova_dependent, 
                                              fill = input$anova_factor2)) +
              geom_boxplot(alpha = 0.7) +
              labs(title = "Two-Way ANOVA Visualization", 
                   x = input$anova_factor1, y = input$anova_dependent,
                   fill = input$anova_factor2) +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
            ggplotly(p)
          } else {
            # Fallback plot if model doesn't exist
            p <- ggplot(anova_data, aes_string(x = input$anova_factor1, y = input$anova_dependent)) +
              geom_boxplot(alpha = 0.7, fill = "lightblue") +
              labs(title = "Data Overview", 
                   x = input$anova_factor1, y = input$anova_dependent) +
              theme_minimal()
            ggplotly(p)
          }
        }, error = function(e) {
          # Create a simple text plot for errors
          plotly::plot_ly() %>% 
            plotly::add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), 
                           textfont = list(size = 16), showlegend = FALSE) %>%
            plotly::layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                         yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                         title = "Visualization Error")
        })
      })
      
    }, error = function(e) {
      output$anova_result <- renderText({
        paste("Error dalam analisis ANOVA:", e$message)
      })
      
      output$anova_interpretation <- renderText({
        "Error: Tidak dapat melakukan analisis ANOVA. Periksa data dan variabel yang dipilih."
      })
    })
  })
  
  # =================== REGRESI LINEAR ===================
  observeEvent(input$run_regression, {
    req(input$reg_dependent, input$reg_independent)
    
    # Check if variables are numeric
    reg_data <- values$current_data[c(input$reg_dependent, input$reg_independent)]
    reg_data <- reg_data[complete.cases(reg_data), ]
    
    # Convert to numeric if needed and remove non-numeric values
    for (col in names(reg_data)) {
      if (!is.numeric(reg_data[[col]])) {
        reg_data[[col]] <- as.numeric(as.character(reg_data[[col]]))
      }
    }
    
    # Remove rows with NA values after conversion
    reg_data <- reg_data[complete.cases(reg_data), ]
    
    if (nrow(reg_data) < 5) {
      output$regression_summary <- renderText({
        "Error: Tidak cukup data numerik yang valid untuk analisis regresi."
      })
      return()
    }
    
    # Prepare formula
    formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
    
    # Fit regression model
    tryCatch({
      reg_model <- lm(as.formula(formula_str), data = reg_data)
      reg_summary <- summary(reg_model)
      
      # Extract key statistics
      r_squared <- reg_summary$r.squared
      adj_r_squared <- reg_summary$adj.r.squared
      f_stat <- reg_summary$fstatistic
      p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      
      output$regression_summary <- renderText({
        # Format coefficients table nicely
        coef_table <- reg_summary$coefficients
        
        # Create formatted coefficient table
        coef_text <- "TABEL KOEFISIEN:\n\n"
        coef_text <- paste0(coef_text, sprintf("%-15s %10s %10s %10s %10s\n", 
                                              "Variable", "Estimate", "Std.Error", "t value", "Pr(>|t|)"))
        coef_text <- paste0(coef_text, paste(rep("-", 65), collapse = ""), "\n")
        
        for(i in 1:nrow(coef_table)) {
          coef_text <- paste0(coef_text, sprintf("%-15s %10.4f %10.4f %10.3f %10.2e\n",
                                                rownames(coef_table)[i],
                                                coef_table[i,1], coef_table[i,2], 
                                                coef_table[i,3], coef_table[i,4]))
        }
        
        # Add significance indicators
        coef_text <- paste0(coef_text, "\nKode Signifikansi: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
        
        paste0(
          "HIPOTESIS UJI REGRESI LINEAR BERGANDA:\n\n",
          "H₀: β₁ = β₂ = ... = βₖ = 0 (semua koefisien regresi sama dengan nol)\n",
          "H₁: Minimal ada satu βᵢ ≠ 0 (minimal ada satu prediktor yang signifikan)\n\n",
          "RINGKASAN MODEL:\n\n",
          "• R-squared: ", round(r_squared, 4), " (", round(r_squared*100, 1), "% varians dijelaskan)\n",
          "• Adjusted R-squared: ", round(adj_r_squared, 4), "\n",
          "• F-statistic: ", round(f_stat[1], 4), " pada ", f_stat[2], " dan ", f_stat[3], " df\n",
          "• p-value: ", format(p_value, scientific = TRUE), "\n",
          "• Jumlah observasi: ", nrow(reg_data), "\n",
          "• Residual standard error: ", round(reg_summary$sigma, 4), "\n\n",
          coef_text
        )
      })
      
      output$regression_interpretation <- renderText({
        # Count significant predictors
        coef_summary <- reg_summary$coefficients
        sig_predictors <- sum(coef_summary[-1, 4] < 0.05, na.rm = TRUE)  # Exclude intercept
        total_predictors <- nrow(coef_summary) - 1
        
        paste0(
          "INTERPRETASI HASIL REGRESI:\n\n",
          "EVALUASI MODEL KESELURUHAN:\n",
          "• R-squared: ", round(r_squared, 4), " (menjelaskan ", round(r_squared*100, 1), "% variasi)\n",
          "• F-test p-value: ", format(p_value, scientific = TRUE), "\n",
          if (p_value < 0.05) {
            "• Model signifikan secara keseluruhan (p < 0.05)"
          } else {
            "• Model tidak signifikan secara keseluruhan (p ≥ 0.05)"
          }, "\n\n",
          "EVALUASI PREDIKTOR INDIVIDUAL:\n",
          "• Jumlah prediktor signifikan: ", sig_predictors, " dari ", total_predictors, "\n",
          "• Prediktor signifikan: variabel dengan p-value < 0.05\n",
          "• Arah hubungan: positif (+) atau negatif (-) dari tanda koefisien\n\n",
          "REKOMENDASI:\n",
          if (p_value < 0.05 && r_squared > 0.1) {
            "• Model dapat digunakan untuk prediksi\n• Fokus pada prediktor yang signifikan\n• Periksa asumsi regresi"
          } else if (p_value < 0.05 && r_squared <= 0.1) {
            "• Model signifikan tapi daya prediksi rendah\n• Pertimbangkan tambahan variabel prediktor\n• Evaluasi outliers"
          } else {
            "• Model perlu diperbaiki\n• Pertimbangkan transformasi variabel\n• Evaluasi ulang pemilihan prediktor"
          }
        )
      })
      
      if (input$reg_assumptions) {
        # Test assumptions with error handling
        tryCatch({
          # Normality test
          residuals_data <- residuals(reg_model)
          if (length(residuals_data) <= 5000 && length(residuals_data) >= 3) {
            norm_test <- shapiro.test(residuals_data)
          } else {
            norm_test <- list(statistic = NA, p.value = NA)
          }
          
          # Homoscedasticity test
          tryCatch({
            homo_test <- car::ncvTest(reg_model)
          }, error = function(e) {
            homo_test <<- list(ChiSquare = NA, p = NA)
          })
          
          # VIF calculation
          vif_text <- if (length(input$reg_independent) > 1) {
            tryCatch({
              vif_values <- car::vif(reg_model)
              if (is.matrix(vif_values)) {
                # Handle matrix output from vif
                vif_values <- vif_values[, 1]
              }
              paste(paste("•", names(vif_values), ":", round(vif_values, 3)), collapse = "\n")
            }, error = function(e) {
              "• Error calculating VIF - možda postoji savršena kolinearnost"
            })
          } else {
            "• VIF tidak dapat dihitung untuk satu prediktor"
          }
        
          output$regression_assumptions <- renderText({
            paste0(
              "UJI ASUMSI REGRESI:\n\n",
              "1. Normalitas Residual (Shapiro-Wilk):\n",
              "• Statistik: ", ifelse(is.na(norm_test$statistic), "N/A", round(norm_test$statistic, 4)), "\n",
              "• p-value: ", ifelse(is.na(norm_test$p.value), "N/A", format(norm_test$p.value, scientific = TRUE)), "\n\n",
              "2. Homoskedastisitas (Breusch-Pagan):\n",
              "• Statistik: ", ifelse(is.na(homo_test$ChiSquare), "N/A", round(homo_test$ChiSquare, 4)), "\n",
              "• p-value: ", ifelse(is.na(homo_test$p), "N/A", format(homo_test$p, scientific = TRUE)), "\n\n",
              "3. Multikolinearitas (VIF):\n",
              vif_text
            )
          })
        }, error = function(e) {
          output$regression_assumptions <- renderText({
            paste("Error dalam uji asumsi:", e$message)
          })
        })
        
        output$assumptions_interpretation <- renderText({
          paste0(
            "**INTERPRETASI UJI ASUMSI:**\n\n",
            "**Normalitas Residual:**\n",
            if (norm_test$p.value > 0.05) {
              "• Asumsi normalitas terpenuhi (p > 0.05)\n• Residual berdistribusi normal"
            } else {
              "• Asumsi normalitas dilanggar (p ≤ 0.05)\n• Residual tidak berdistribusi normal"
            }, "\n\n",
            "**Homoskedastisitas:**\n",
            if (homo_test$p > 0.05) {
              "• Asumsi homoskedastisitas terpenuhi (p > 0.05)\n• Varians residual konstan"
            } else {
              "• Asumsi homoskedastisitas dilanggar (p ≤ 0.05)\n• Terjadi heteroskedastisitas"
            }, "\n\n",
            "**Multikolinearitas:**\n",
            if (length(input$reg_independent) > 1) {
              vif_values <- car::vif(reg_model)
              if (all(vif_values < 5)) {
                "• Tidak ada masalah multikolinearitas (semua VIF < 5)"
              } else if (any(vif_values >= 10)) {
                "• Masalah multikolinearitas serius (ada VIF ≥ 10)"
              } else {
                "• Multikolinearitas sedang (ada VIF 5-10)"
              }
            } else {
              "• Hanya satu prediktor, tidak perlu uji multikolinearitas"
            }
          )
        })
      }
      
      if (input$reg_diagnostics) {
        # Model diagnostics
        cooks_d <- cooks.distance(reg_model)
        leverage <- hatvalues(reg_model)
        influential_points <- which(cooks_d > 4/nrow(reg_data))
        leverage_points <- which(leverage > 2*length(coef(reg_model))/nrow(reg_data))
        
        output$regression_diagnostics <- renderText({
          paste0(
            "**DIAGNOSTIK MODEL:**\n\n",
            "**Influential Points (Cook's Distance > 4/n):**\n",
            if (length(influential_points) > 0) {
              paste("• Observasi berpengaruh:", paste(influential_points, collapse = ", "), "\n",
                    "• Jumlah: ", length(influential_points), " dari ", nrow(reg_data), " observasi")
            } else {
              "• Tidak ada observasi yang sangat berpengaruh"
            }, "\n\n",
            "**Leverage Points (Hat values > 2p/n):**\n",
            if (length(leverage_points) > 0) {
              paste("• Observasi dengan leverage tinggi:", paste(leverage_points, collapse = ", "), "\n",
                    "• Jumlah: ", length(leverage_points), " dari ", nrow(reg_data), " observasi")
            } else {
              "• Tidak ada observasi dengan leverage tinggi"
            }
          )
        })
        
        output$diagnostics_interpretation <- renderText({
          paste0(
            "**INTERPRETASI DIAGNOSTIK:**\n\n",
            "**Cook's Distance:**\n",
            "• Mengidentifikasi observasi yang sangat berpengaruh terhadap model\n",
            "• Nilai > 4/n menunjukkan observasi berpengaruh\n",
            "• Pertimbangkan untuk menginvestigasi atau menghapus observasi ini\n\n",
            "**Leverage:**\n",
            "• Mengidentifikasi observasi dengan nilai prediktor yang ekstrem\n",
            "• Nilai > 2p/n menunjukkan leverage tinggi\n",
            "• Observasi ini dapat mempengaruhi hasil regresi secara signifikan"
          )
        })
      }
      
      # Create diagnostic plots
      output$regression_plots <- renderPlotly({
        # Prepare diagnostic data
        diag_data <- data.frame(
          fitted = fitted(reg_model),
          residuals = residuals(reg_model),
          standardized_residuals = rstandard(reg_model),
          leverage = hatvalues(reg_model),
          cooks_distance = cooks.distance(reg_model)
        )
        
        # Residuals vs Fitted
        p1 <- ggplot(diag_data, aes(x = fitted, y = residuals)) +
          geom_point(alpha = 0.6) +
          geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
          geom_smooth(se = FALSE, color = "blue") +
          labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
          theme_minimal()
        
        # Q-Q plot
        p2 <- ggplot(diag_data, aes(sample = standardized_residuals)) +
          geom_qq() +
          geom_qq_line(color = "red") +
          labs(title = "Normal Q-Q Plot") +
          theme_minimal()
        
        # Scale-Location plot
        p3 <- ggplot(diag_data, aes(x = fitted, y = sqrt(abs(standardized_residuals)))) +
          geom_point(alpha = 0.6) +
          geom_smooth(se = FALSE, color = "red") +
          labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|") +
          theme_minimal()
        
        # Cook's Distance
        p4 <- ggplot(diag_data, aes(x = 1:nrow(diag_data), y = cooks_distance)) +
          geom_col(alpha = 0.7) +
          geom_hline(yintercept = 4/nrow(values$current_data), color = "red", linetype = "dashed") +
          labs(title = "Cook's Distance", x = "Observation", y = "Cook's Distance") +
          theme_minimal()
        
        subplot(
          ggplotly(p1), ggplotly(p2),
          ggplotly(p3), ggplotly(p4),
          nrows = 2
        )
      })
      
    }, error = function(e) {
      output$regression_summary <- renderText({
        paste("Error dalam analisis regresi:", e$message, "
Pastikan variabel yang dipilih adalah numerik.")
      })
      
      output$regression_interpretation <- renderText({
        "Error: Tidak dapat melakukan analisis regresi. Periksa kembali data dan variabel yang dipilih."
      })
    })
  })
  
  # =================== METADATA ===================
  output$metadata_variables <- DT::renderDataTable({
    metadata_vars <- data.frame(
      Variable = c("FIPS", "State", "County", "Population", "Income", "Education", "Age_65_Over", 
                   "Disability", "Single_Parent", "Minority", "Mobile_Home", "Crowding", "No_Vehicle", 
                   "Unemployment", "Poverty", "SOVI_Score"),
      Description = c("Federal Information Processing Standard code",
                      "State name",
                      "County name", 
                      "Total population",
                      "Median household income",
                      "Percentage with high school education or higher",
                      "Percentage of population aged 65 and over",
                      "Percentage with disability",
                      "Percentage of single-parent households",
                      "Percentage of minority population",
                      "Percentage living in mobile homes",
                      "Percentage living in crowded conditions",
                      "Percentage of households with no vehicle",
                      "Unemployment rate",
                      "Poverty rate",
                      "Social Vulnerability Index Score"),
      Type = c("Categorical", "Categorical", "Categorical", "Numerical", "Numerical", 
               "Numerical", "Numerical", "Numerical", "Numerical", "Numerical", 
               "Numerical", "Numerical", "Numerical", "Numerical", "Numerical", "Numerical")
    )
    DT::datatable(metadata_vars, options = list(pageLength = 20))
  })
  
  # =================== DOWNLOAD HANDLERS ===================
  output$download_manual <- downloadHandler(
    filename = "Manual_Dashboard_Statistik_Terpadu.docx",
    content = function(file) {
      # Create Word manual
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "MANUAL PENGGUNA DASHBOARD STATISTIK TERPADU", style = "heading 1")
      doc <- officer::body_add_par(doc, "Dashboard ini menyediakan analisis statistik komprehensif untuk data SOVI.")
      doc <- officer::body_add_par(doc, "Fitur utama meliputi manajemen data, eksplorasi data, uji asumsi, statistik inferensia, dan regresi linear.")
      print(doc, target = file)
    }
  )
  
  output$download_transformed <- downloadHandler(
    filename = function() {
      paste0("transformed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$transformed_data)) {
        write.csv(values$transformed_data, file, row.names = FALSE)
      }
    }
  )
  
  # Word report for descriptive statistics
  output$download_desc_report <- downloadHandler(
    filename = function() {
      paste0("laporan_statistik_deskriptif_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "LAPORAN STATISTIK DESKRIPTIF", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Tanggal:", Sys.Date()))
      doc <- officer::body_add_par(doc, "")
      
      if (!is.null(input$desc_variables)) {
        doc <- officer::body_add_par(doc, "VARIABEL YANG DIANALISIS:", style = "heading 2")
        doc <- officer::body_add_par(doc, paste(input$desc_variables, collapse = ", "))
        doc <- officer::body_add_par(doc, "")
        
        doc <- officer::body_add_par(doc, "INTERPRETASI:", style = "heading 2")
        doc <- officer::body_add_par(doc, "Analisis statistik deskriptif memberikan gambaran karakteristik data melalui ukuran pemusatan dan penyebaran.")
      }
      
      print(doc, target = file)
    }
  )
  
  # JPG download for plots
  output$download_plot_jpg <- downloadHandler(
    filename = function() {
      paste0("plot_", input$plot_type, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      # Save the current plot as JPG
      if (exists("p")) {
        ggsave(file, plot = p, device = "jpeg", width = 10, height = 6, dpi = 300)
      }
    }
  )
  
  # JPG download for map (using webshot)
  output$download_map_jpg <- downloadHandler(
    filename = function() {
      paste0("peta_", input$map_variable, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      map_data <- values$map_data()
      if (!is.null(map_data)) {
        # Create a simple ggplot map for JPG export
        p_map <- ggplot(map_data, aes(x = lng, y = lat, color = value)) +
          geom_point(size = 3, alpha = 0.7) +
          scale_color_gradient2(low = "blue", high = "red", mid = "yellow", midpoint = median(map_data$value, na.rm = TRUE)) +
          borders("state", colour = "black", fill = NA) +
          coord_quickmap() +
          labs(title = paste("Peta", input$map_variable), 
               x = "Longitude", y = "Latitude", color = input$map_variable) +
          theme_minimal()
        
        ggsave(file, plot = p_map, device = "jpeg", width = 12, height = 8, dpi = 300)
      }
    }
  )
  
  # Download handler for assumption tests report
  output$download_assumption_report <- downloadHandler(
    filename = function() {
      paste0("laporan_uji_asumsi_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "LAPORAN UJI ASUMSI", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Tanggal:", Sys.Date()))
      doc <- officer::body_add_par(doc, "")
      
      if (!is.null(input$assumption_var)) {
        doc <- officer::body_add_par(doc, "VARIABEL YANG DIUJI:", style = "heading 2")
        doc <- officer::body_add_par(doc, input$assumption_var)
        doc <- officer::body_add_par(doc, "")
        
        if (input$test_normality) {
          doc <- officer::body_add_par(doc, "UJI NORMALITAS:", style = "heading 2")
          doc <- officer::body_add_par(doc, "Hasil uji normalitas menunjukkan apakah data mengikuti distribusi normal.")
          doc <- officer::body_add_par(doc, "")
        }
        
        if (input$test_homogeneity) {
          doc <- officer::body_add_par(doc, "UJI HOMOGENITAS:", style = "heading 2")
          doc <- officer::body_add_par(doc, "Hasil uji homogenitas menunjukkan apakah varians antar kelompok sama.")
          doc <- officer::body_add_par(doc, "")
        }
      }
      
      print(doc, target = file)
    }
  )
  
  # Download handler for mean test report
  output$download_mean_test <- downloadHandler(
    filename = function() {
      paste0("laporan_uji_rata_rata_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "LAPORAN UJI RATA-RATA", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Tanggal:", Sys.Date()))
      doc <- officer::body_add_par(doc, "")
      
      if (!is.null(input$mean_test_var)) {
        doc <- officer::body_add_par(doc, "VARIABEL YANG DIUJI:", style = "heading 2")
        doc <- officer::body_add_par(doc, input$mean_test_var)
        doc <- officer::body_add_par(doc, "")
        
        doc <- officer::body_add_par(doc, "JENIS UJI:", style = "heading 2")
        if (input$mean_test_type == "one_sample") {
          doc <- officer::body_add_par(doc, "Uji t satu sampel")
        } else {
          doc <- officer::body_add_par(doc, "Uji t dua sampel")
        }
        doc <- officer::body_add_par(doc, "")
        
        doc <- officer::body_add_par(doc, "INTERPRETASI:", style = "heading 2")
        doc <- officer::body_add_par(doc, "Hasil uji menunjukkan apakah terdapat perbedaan rata-rata yang signifikan secara statistik.")
      }
      
      print(doc, target = file)
    }
  )
  
  # Download handler for ANOVA report
  output$download_anova_test <- downloadHandler(
    filename = function() {
      paste0("laporan_anova_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "LAPORAN ANALISIS VARIANS (ANOVA)", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Tanggal:", Sys.Date()))
      doc <- officer::body_add_par(doc, "")
      
      if (!is.null(input$anova_dependent)) {
        doc <- officer::body_add_par(doc, "VARIABEL DEPENDEN:", style = "heading 2")
        doc <- officer::body_add_par(doc, input$anova_dependent)
        doc <- officer::body_add_par(doc, "")
        
        doc <- officer::body_add_par(doc, "FAKTOR:", style = "heading 2")
        doc <- officer::body_add_par(doc, input$anova_factor1)
        if (!is.null(input$anova_factor2)) {
          doc <- officer::body_add_par(doc, paste("Faktor 2:", input$anova_factor2))
        }
        doc <- officer::body_add_par(doc, "")
        
        doc <- officer::body_add_par(doc, "INTERPRETASI:", style = "heading 2")
        doc <- officer::body_add_par(doc, "ANOVA menguji apakah terdapat perbedaan rata-rata antar kelompok/grup.")
      }
      
      print(doc, target = file)
    }
  )
  
  # Download handler for regression report
  output$download_regression_report <- downloadHandler(
    filename = function() {
      paste0("laporan_regresi_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "LAPORAN REGRESI LINEAR BERGANDA", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Tanggal:", Sys.Date()))
      doc <- officer::body_add_par(doc, "")
      
      if (!is.null(input$reg_dependent)) {
        doc <- officer::body_add_par(doc, "VARIABEL DEPENDEN:", style = "heading 2")
        doc <- officer::body_add_par(doc, input$reg_dependent)
        doc <- officer::body_add_par(doc, "")
        
        if (!is.null(input$reg_independent)) {
          doc <- officer::body_add_par(doc, "VARIABEL INDEPENDEN:", style = "heading 2")
          doc <- officer::body_add_par(doc, paste(input$reg_independent, collapse = ", "))
        }
        doc <- officer::body_add_par(doc, "")
        
        doc <- officer::body_add_par(doc, "INTERPRETASI:", style = "heading 2")
        doc <- officer::body_add_par(doc, "Analisis regresi linear berganda menunjukkan hubungan antara variabel dependen dengan variabel independen.")
      }
      
      print(doc, target = file)
    }
  )
  
  # Download handler for metadata report
  output$download_metadata_report <- downloadHandler(
    filename = function() {
      paste0("metadata_lengkap_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "METADATA LENGKAP DASHBOARD STATISTIK TERPADU", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Tanggal:", Sys.Date()))
      doc <- officer::body_add_par(doc, "")
      
      doc <- officer::body_add_par(doc, "INFORMASI UMUM:", style = "heading 2")
      doc <- officer::body_add_par(doc, "Dashboard: Dashboard Statistik Terpadu")
      doc <- officer::body_add_par(doc, "Versi: 1.0")
      doc <- officer::body_add_par(doc, "Tanggal Pembuatan: 2024")
      doc <- officer::body_add_par(doc, "")
      
      doc <- officer::body_add_par(doc, "FITUR UTAMA:", style = "heading 2")
      doc <- officer::body_add_par(doc, "• Manajemen Data (Upload, Transform)")
      doc <- officer::body_add_par(doc, "• Eksplorasi Data (Statistik Deskriptif, Visualisasi, Peta)")
      doc <- officer::body_add_par(doc, "• Uji Asumsi (Normalitas, Homogenitas)")
      doc <- officer::body_add_par(doc, "• Statistik Inferensia (t-test, ANOVA)")
      doc <- officer::body_add_par(doc, "• Regresi Linear Berganda")
      doc <- officer::body_add_par(doc, "• Download Multi-format (Word, Excel, JPG)")
      doc <- officer::body_add_par(doc, "")
      
      doc <- officer::body_add_par(doc, "SUMBER DATA:", style = "heading 2")
      doc <- officer::body_add_par(doc, "Data SOVI (Social Vulnerability Index) dari GitHub")
      doc <- officer::body_add_par(doc, "URL: https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State/data/distribution/va_geo_ffxco_2020_2020_sovi.csv")
      
      print(doc, target = file)
    }
  )
  
  # Start analysis button
  observeEvent(input$start_analysis, {
    updateTabItems(session, "sidebar", "data_management")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
