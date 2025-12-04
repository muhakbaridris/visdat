# ========================================================================
#  DIGITAL ECOSYSTEM DASHBOARD — PREMIUM APPLE EDITION
#  BAGIAN 1 — SETUP, LOAD DATA, PREPROCESS, AGGREGATION, GLOBAL OBJECTS
# ========================================================================

library(shiny)
library(shinyjs)
library(waiter)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(viridis)
library(lubridate)
library(scales)
library(plotly)
library(shinyWidgets)
library(corrplot)
library(igraph)
library(visNetwork)
library(countrycode)  # Untuk konversi kode negara
library(countrycode)


cat("\n=== Digital Ecosystem Dashboard — Setup Data Engine ===\n")

# ========================================================================
# 1. LOAD DATA
# ========================================================================

raw_path <- "df_fix_final.csv"

tryCatch({
  visdat <- read.csv(raw_path, stringsAsFactors = FALSE)
  cat("Data berhasil dimuat:", nrow(visdat), "baris\n\n")
}, error = function(e) {
  stop("❌ File df_fix_final.csv tidak ditemukan. Pastikan file berada di folder yang sama dengan app.R")
})

raw_data <- visdat

# ========================================================================
# 2. PREPROCESSING (CLEAN + MUTATE + FEATURE ENGINEERING)
# ========================================================================

cat("Memproses data...\n")

# detect phone-number column
phone_col <- grep("phone|numbers", names(raw_data), ignore.case = TRUE, value = TRUE)[1]
if (is.na(phone_col)) phone_col <- names(raw_data)[ncol(raw_data)]

processed_data <- raw_data %>%
  rename(
    Revenue_Billions = Revenue,
    Country_Standard = Country,
    Mobile_Users = !!sym(phone_col)
  ) %>%
  mutate(
    across(
      c(Revenue_Billions, Employees, Population, Mobile_Users,
        Year, Rank, Founded),
      ~ suppressWarnings(as.numeric(.))
    ),
    Revenue_per_Employee = ifelse(Employees > 0,
                                  (Revenue_Billions * 1e9) / Employees,
                                  NA),
    Company_Age = Year - Founded,
    Digital_Intensity_Index =
      as.numeric(scale(Mobile_Users / Population * Revenue_Billions)),
    Innovation_Score = runif(n(), 60, 95),
    Risk_Score = runif(n(), 1, 10),
    Volatility = runif(n(), 0.1, 0.4)
  )

# ========================================================================
# 3. COMPANY GROWTH CALCULATIONS
# ========================================================================

company_growth <- processed_data %>%
  arrange(Company, Year) %>%
  group_by(Company) %>%
  mutate(
    Growth_Rate = (Revenue_Billions / lag(Revenue_Billions,
                                          default = first(Revenue_Billions)) - 1) * 100,
    Revenue_Momentum = Growth_Rate - lag(Growth_Rate,
                                         default = first(Growth_Rate))
  ) %>%
  ungroup()

# ========================================================================
# 4. MARKET SHARE CALCULATION
# ========================================================================

market_shares <- company_growth %>%
  group_by(Year) %>%
  mutate(
    Total_Revenue_Year = sum(Revenue_Billions, na.rm = TRUE),
    Market_Share_Global = (Revenue_Billions / Total_Revenue_Year) * 100
  ) %>%
  ungroup()

# ========================================================================
# 5. COUNTRY-LEVEL AGGREGATION
# ========================================================================

country_year <- market_shares %>%
  group_by(Year, Country_Standard) %>%
  summarise(
    Total_Revenue = sum(Revenue_Billions, na.rm = TRUE),
    Mobile_Users = max(Mobile_Users, na.rm = TRUE),
    Population = max(Population, na.rm = TRUE),
    N_Companies = n_distinct(Company),
    Avg_Revenue_per_Company = mean(Revenue_Billions, na.rm = TRUE),
    Total_Employees = sum(Employees, na.rm = TRUE),
    Avg_Growth_Rate = mean(Growth_Rate, na.rm = TRUE),
    Avg_Innovation_Score = mean(Innovation_Score, na.rm = TRUE),
    Avg_Risk_Score = mean(Risk_Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mobile_per_100 = ifelse(Population > 0,
                            Mobile_Users / Population * 100, NA),
    Revenue_per_Cap = ifelse(Population > 0,
                             Total_Revenue / Population, NA),
    Digital_Economy_Index =
      as.numeric(scale(Total_Revenue * Mobile_per_100 * N_Companies)),
    # Tambahkan kode negara untuk peta
    Country_Code = countrycode(Country_Standard, "country.name", "iso3c")
  ) %>%
  # Handle missing country codes
  mutate(Country_Code = ifelse(is.na(Country_Code), 
                               substr(toupper(Country_Standard), 1, 3), 
                               Country_Code))

# ========================================================================
# 6. COMPANY-LEVEL AGGREGATION
# ========================================================================

company_year <- market_shares %>%
  group_by(Year, Company, Country_Standard, Industry) %>%
  summarise(
    Revenue_Billions = sum(Revenue_Billions, na.rm = TRUE),
    Employees = max(Employees, na.rm = TRUE),
    Market_Share_Global = mean(Market_Share_Global, na.rm = TRUE),
    Revenue_per_Employee = mean(Revenue_per_Employee, na.rm = TRUE),
    Growth_Rate = mean(Growth_Rate, na.rm = TRUE),
    Digital_Intensity = mean(Digital_Intensity_Index, na.rm = TRUE),
    Innovation_Score = mean(Innovation_Score, na.rm = TRUE),
    Risk_Score = mean(Risk_Score, na.rm = TRUE),
    Revenue_Momentum = mean(Revenue_Momentum, na.rm = TRUE),
    .groups = "drop"
  )

# ========================================================================
# 7. GLOBAL LISTS
# ========================================================================

country_list <- sort(unique(country_year$Country_Standard))
year_list <- sort(unique(processed_data$Year))
company_list <- sort(unique(company_year$Company))
industry_list <- sort(unique(company_year$Industry))

cat("Data Engine selesai! (Bagian 1 OK)\n")

# ========================================================================
#  DIGITAL ECOSYSTEM DASHBOARD — PREMIUM APPLE UI
#  BAGIAN 2 — USER INTERFACE DIPERBAIKI
# ========================================================================

ui <- fluidPage(
  useShinyjs(),
  useWaiter(),
  
  # =====================================================
  # CUSTOM CSS — DIPERBAIKI DENGAN TRANSISI & ANIMASI
  # =====================================================
  tags$head(
    
    # Google Fonts
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;800&display=swap"),
    
    # Font Awesome Icons
    tags$link(rel="stylesheet",
              href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    
    # AOS Animate CSS
    tags$link(rel="stylesheet",
              href="https://cdnjs.cloudflare.com/ajax/libs/aos/2.3.4/aos.css"),
    
    # AOS JS
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/aos/2.3.4/aos.js"),
    
    # Apple-style CSS - DIPERBAIKI DENGAN ANIMASI
    tags$style(HTML("
      body {
        background: #f4f7fa;
        font-family: 'Poppins', sans-serif;
        overflow-x: hidden;
        margin: 0;
        padding: 0;
      }

      /* Main container */
      .main-container {
        padding-top: 80px;
      }

      /* Floating Apple Navbar dengan efek glassmorphism */
      .navbar-apple {
        position: fixed;
        top: 14px;
        left: 50%;
        transform: translateX(-50%);
        background: rgba(255,255,255,0.85);
        backdrop-filter: blur(20px);
        padding: 12px 28px;
        border-radius: 25px;
        box-shadow: 0 12px 35px rgba(0,0,0,0.15);
        z-index: 9999;
        display: flex;
        gap: 28px;
        border: 1px solid rgba(255,255,255,0.3);
        animation: slideDown 0.5s ease;
      }
      
      @keyframes slideDown {
        from { top: -50px; opacity: 0; }
        to { top: 14px; opacity: 1; }
      }
      
      .navbar-item {
        font-weight: 600;
        font-size: 15px;
        color: #1f2937;
        cursor: pointer;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        padding: 8px 16px;
        border-radius: 12px;
        background: none;
        border: none;
        position: relative;
        overflow: hidden;
      }
      .navbar-item:before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(37, 99, 235, 0.1), transparent);
        transition: 0.5s;
      }
      .navbar-item:hover:before {
        left: 100%;
      }
      .navbar-item:hover {
        color: #2563eb;
        background: rgba(37, 99, 235, 0.08);
        transform: translateY(-2px);
      }
      .navbar-item-active {
        color: #2563eb;
        font-weight: 700;
        background: rgba(37, 99, 235, 0.12);
        box-shadow: 0 4px 12px rgba(37, 99, 235, 0.15);
      }

      /* Hero Parallax Section dengan gambar */
      .hero {
        height: 500px;
        background: linear-gradient(rgba(232, 239, 255, 0.9), rgba(247, 250, 255, 0.9)),
                    url('https://raw.githubusercontent.com/muhakbaridris/visdat/4c9784053a0007bc2f2f617ac479c5a9bb98f168/www/beranda.jpg');
        background-size: cover;
        background-position: center;
        border-radius: 24px;
        margin: 20px 0 30px 0;
        padding: 50px;
        position: relative;
        overflow: hidden;
        text-align: center;
        display: flex;
        flex-direction: column;
        justify-content: center;
        animation: fadeInUp 0.8s ease;
      }
      
      @keyframes fadeInUp {
        from { opacity: 0; transform: translateY(30px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .hero-title {
        font-size: 42px;
        font-weight: 800;
        color: #111827;
        margin-bottom: 15px;
        text-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .hero-sub {
        font-size: 17px;
        color: #374151;
        max-width: 700px;
        margin: 0 auto 25px;
        line-height: 1.6;
        background: rgba(255,255,255,0.8);
        padding: 15px;
        border-radius: 12px;
        backdrop-filter: blur(10px);
      }
      .hero-cta {
        display: inline-block;
        padding: 14px 32px;
        background: linear-gradient(135deg, #2563eb, #10b981);
        color: white;
        border-radius: 14px;
        font-weight: 600;
        text-decoration: none;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        box-shadow: 0 8px 25px rgba(37, 99, 235, 0.3);
        border: none;
        cursor: pointer;
        font-size: 16px;
        position: relative;
        overflow: hidden;
      }
      .hero-cta:before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
        transition: 0.5s;
      }
      .hero-cta:hover:before {
        left: 100%;
      }
      .hero-cta:hover {
        transform: translateY(-4px) scale(1.05);
        box-shadow: 0 12px 35px rgba(37, 99, 235, 0.4);
        color: white;
      }

      /* Page content dengan animasi fade */
      .page-content {
        display: none;
        padding: 20px 0;
        animation: fadeIn 0.6s ease;
      }
      
      .page-content.active {
        display: block;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }

      /* Premium Cards dengan efek glassmorphism */
      .card-premium {
        background: rgba(255,255,255,0.85);
        padding: 25px;
        border-radius: 22px;
        border: 1px solid rgba(200,200,200,0.15);
        box-shadow: 0 12px 35px rgba(0,0,0,0.08);
        margin-bottom: 25px;
        backdrop-filter: blur(10px);
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
      }
      .card-premium:hover {
        transform: translateY(-5px);
        box-shadow: 0 18px 45px rgba(0,0,0,0.12);
        border-color: rgba(37, 99, 235, 0.2);
      }

      /* KPI Counter dengan animasi */
      .kpi-box {
        text-align: center;
        padding: 20px;
        border-radius: 18px;
        background: white;
        box-shadow: 0 8px 25px rgba(0,0,0,0.08);
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        height: 140px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        margin-bottom: 15px;
        position: relative;
        overflow: hidden;
      }
      .kpi-box:before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 4px;
        background: linear-gradient(90deg, #2563eb, #10b981);
      }
      .kpi-box:hover {
        transform: translateY(-8px) scale(1.02);
        box-shadow: 0 15px 35px rgba(0,0,0,0.12);
      }
      .kpi-title {
        color: #6b7280;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        margin-bottom: 10px;
        font-weight: 600;
      }
      .kpi-value {
        font-size: 32px;
        font-weight: 800;
        background: linear-gradient(135deg,#2563eb,#10b981);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }

      /* Team section - PHOTO CENTERED dengan animasi */
      .team-card {
        background: rgba(255,255,255,0.85);
        padding: 25px;
        border-radius: 22px;
        text-align: center;
        box-shadow: 0 10px 30px rgba(0,0,0,0.08);
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        height: 100%;
        margin-bottom: 20px;
        display: flex;
        flex-direction: column;
        align-items: center;
        backdrop-filter: blur(10px);
      }
      .team-card:hover {
        transform: translateY(-8px) scale(1.02);
        box-shadow: 0 20px 40px rgba(0,0,0,0.12);
      }
      .team-photo-container {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-bottom: 18px;
        width: 100%;
      }
      .team-photo {
        width: 120px;
        height: 120px;
        border-radius: 50%;
        object-fit: cover;
        border: 4px solid #e5e7eb;
        background: #f3f4f6;
        display: flex;
        align-items: center;
        justify-content: center;
        margin: 0 auto;
        transition: all 0.4s ease;
        box-shadow: 0 8px 20px rgba(0,0,0,0.1);
      }
      .team-card:hover .team-photo {
        transform: scale(1.1);
        border-color: #2563eb;
      }
      .team-role {
        color: #2563eb;
        font-weight: 600;
        font-size: 14px;
        margin: 8px 0;
        padding: 4px 12px;
        background: rgba(37, 99, 235, 0.1);
        border-radius: 20px;
        display: inline-block;
      }
      .team-nim {
        color: #6b7280;
        font-size: 13px;
        margin-top: 8px;
        font-weight: 500;
      }

      /* Feature Cards */
      .feature-card {
        background: white;
        padding: 25px;
        border-radius: 20px;
        text-align: center;
        box-shadow: 0 10px 30px rgba(0,0,0,0.08);
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        height: 100%;
        margin-bottom: 20px;
        position: relative;
        overflow: hidden;
      }
      .feature-card:before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: linear-gradient(135deg, rgba(37, 99, 235, 0.05), rgba(16, 185, 129, 0.05));
        z-index: 1;
        transition: 0.5s;
        opacity: 0;
      }
      .feature-card:hover:before {
        opacity: 1;
      }
      .feature-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 20px 40px rgba(0,0,0,0.12);
      }
      .feature-icon {
        font-size: 42px;
        color: #2563eb;
        margin-bottom: 18px;
        transition: all 0.4s ease;
        position: relative;
        z-index: 2;
      }
      .feature-card:hover .feature-icon {
        transform: scale(1.2) rotate(5deg);
      }
      
      /* Plot containers */
      .plot-container {
        background: white;
        padding: 20px;
        border-radius: 18px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.08);
        margin-bottom: 20px;
        transition: all 0.3s ease;
        position: relative;
        overflow: hidden;
      }
      .plot-container:before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 4px;
        background: linear-gradient(90deg, #2563eb, #10b981);
      }
      .plot-container:hover {
        transform: translateY(-3px);
        box-shadow: 0 12px 30px rgba(0,0,0,0.12);
      }
      
      /* Tab navigation styling - CENTERED */
      .nav-tabs {
        display: flex;
        justify-content: center !important;
        border-bottom: 2px solid #e5e7eb;
        margin-bottom: 25px;
        flex-wrap: wrap;
      }
      .nav-tabs > li {
        float: none !important;
        display: inline-block !important;
      }
      .nav-tabs > li > a {
        border: none !important;
        border-radius: 12px !important;
        padding: 12px 24px !important;
        margin: 0 5px !important;
        color: #6b7280 !important;
        font-weight: 600 !important;
        font-size: 15px !important;
        transition: all 0.3s ease !important;
        background: rgba(255,255,255,0.7) !important;
        border: 1px solid rgba(229, 231, 235, 0.5) !important;
      }
      .nav-tabs > li > a:hover {
        background: rgba(37, 99, 235, 0.08) !important;
        color: #2563eb !important;
        transform: translateY(-2px);
        border-color: rgba(37, 99, 235, 0.3) !important;
      }
      .nav-tabs > li.active > a {
        background: linear-gradient(135deg, #2563eb, #10b981) !important;
        color: white !important;
        border: none !important;
        box-shadow: 0 6px 20px rgba(37, 99, 235, 0.3) !important;
      }
      
      /* Logo grid untuk beranda */
      .logo-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
        gap: 20px;
        margin: 40px 0;
        padding: 30px;
        background: rgba(255,255,255,0.9);
        border-radius: 20px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.08);
      }
      .logo-item {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        padding: 15px;
        border-radius: 15px;
        background: white;
        transition: all 0.3s ease;
      }
      .logo-item:hover {
        transform: translateY(-5px);
        box-shadow: 0 10px 25px rgba(0,0,0,0.1);
      }
      .logo-icon {
        font-size: 36px;
        margin-bottom: 10px;
        color: #2563eb;
      }
      .logo-text {
        font-size: 14px;
        font-weight: 600;
        color: #374151;
      }
      
      /* Peta styling */
      .map-container {
        background: white;
        padding: 20px;
        border-radius: 18px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.08);
        margin-bottom: 20px;
        height: 500px;
      }
      
      /* Filter Panel di Atas - DIPERBAIKI */
      .filter-panel {
        background: rgba(255,255,255,0.9);
        padding: 25px;
        border-radius: 22px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.08);
        margin-bottom: 30px;
        backdrop-filter: blur(10px);
        border: 1px solid rgba(200,200,200,0.2);
        position: relative;
        z-index: 100;
      }
      .filter-title {
        text-align: center;
        margin-bottom: 20px;
        color: #1f2937;
        font-weight: 700;
      }
      
      /* Clear space for content */
      .content-space {
        margin-top: 30px;
      }
      
      /* Animation classes */
      .fade-in {
        animation: fadeIn 0.6s ease;
      }
      .slide-up {
        animation: slideUp 0.6s ease;
      }
      .pulse {
        animation: pulse 2s infinite;
      }
      
      @keyframes slideUp {
        from { opacity: 0; transform: translateY(30px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      @keyframes pulse {
        0% { transform: scale(1); }
        50% { transform: scale(1.05); }
        100% { transform: scale(1); }
      }
      
      /* Section headers */
      .section-header {
        text-align: center;
        margin: 40px 0 30px;
        position: relative;
      }
      .section-header h3 {
        display: inline-block;
        padding-bottom: 10px;
        position: relative;
        color: #1f2937;
      }
      .section-header h3:after {
        content: '';
        position: absolute;
        bottom: 0;
        left: 50%;
        transform: translateX(-50%);
        width: 80px;
        height: 4px;
        background: linear-gradient(90deg, #2563eb, #10b981);
        border-radius: 2px;
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .hero {
          height: 400px;
          padding: 30px;
        }
        .hero-title {
          font-size: 32px;
        }
        .hero-sub {
          font-size: 15px;
        }
        .navbar-apple {
          padding: 10px 20px;
          gap: 15px;
          font-size: 14px;
        }
        .filter-panel {
          padding: 15px;
        }
      }
      
      /* Stat summary box */
      .stat-summary-box {
        background: #f8fafc;
        padding: 15px;
        border-radius: 12px;
        border-left: 4px solid #2563eb;
        font-family: 'Courier New', monospace;
        font-size: 12px;
        line-height: 1.4;
        max-height: 300px;
        overflow-y: auto;
      }
    "))
  ),
  
  # =====================================================
  # FLOATING NAVBAR (4 MENU)
  # =====================================================
  div(class="navbar-apple",
      actionButton("nav_home", "Beranda", class="navbar-item"),
      actionButton("nav_dashboard", "Dashboard Utama", class="navbar-item"),
      actionButton("nav_statistics", "Analisis Statistik", class="navbar-item"),
      actionButton("nav_team", "About Team", class="navbar-item")
  ),
  
  # Main container
  div(class="main-container",
      
      # =====================================================
      # PAGE: BERANDA DENGAN GAMBAR
      # =====================================================
      div(id="page_beranda", class="page-content active",
          
          # HERO SECTION DENGAN BACKGROUND GAMBAR
          div(class="hero fade-in",
              h1(class="hero-title", "Digital Ecosystem Dashboard"),
              p(class="hero-sub",
                "Platform analitik canggih untuk menganalisis kinerja ekosistem digital global. Dashboard ini memberikan insight mendalam tentang revenue perusahaan teknologi, pangsa pasar, pertumbuhan industri, dan tren ekonomi digital di berbagai negara."),
              actionButton("btn_to_dashboard", "Mulai Eksplorasi", 
                           class="hero-cta pulse")
          ),
          
          # LOGO GRID PERUSAHAAN TEKNOLOGI
          div(class="section-header",
              h3("Perusahaan Teknologi Global")
          ),
          
          div(class="logo-grid slide-up",
              div(class="logo-item", 
                  div(class="logo-icon", icon("microsoft")),
                  div(class="logo-text", "Microsoft")
              ),
              div(class="logo-item",
                  div(class="logo-icon", icon("google")),
                  div(class="logo-text", "Google")
              ),
              div(class="logo-item",
                  div(class="logo-icon", icon("apple")),
                  div(class="logo-text", "Apple")
              ),
              div(class="logo-item",
                  div(class="logo-icon", icon("amazon")),
                  div(class="logo-text", "Amazon")
              ),
              div(class="logo-item",
                  div(class="logo-icon", icon("facebook")),
                  div(class="logo-text", "Meta")
              ),
              div(class="logo-item",
                  div(class="logo-icon", icon("twitter")),
                  div(class="logo-text", "Twitter")
              )
          ),
          
          # KPI GLOBAL (TANPA FILTER)
          div(class="section-header",
              h3("Snapshot Ekosistem Digital Global")
          ),
          
          fluidRow(
            column(3, div(class="kpi-box slide-up",
                          div(class="kpi-title","Total Revenue Global"),
                          div(class="kpi-value", textOutput("kpi_rev"))
            )),
            column(3, div(class="kpi-box slide-up",
                          div(class="kpi-title","Jumlah Negara"),
                          div(class="kpi-value", textOutput("kpi_country"))
            )),
            column(3, div(class="kpi-box slide-up",
                          div(class="kpi-title","Jumlah Perusahaan"),
                          div(class="kpi-value", textOutput("kpi_company"))
            )),
            column(3, div(class="kpi-box slide-up",
                          div(class="kpi-title","Rata-rata Pertumbuhan Tahunan"),
                          div(class="kpi-value", textOutput("kpi_growth"))
            ))
          ),
          
          # FITUR UTAMA
          div(class="section-header",
              h3("Fitur Utama Dashboard")
          ),
          
          fluidRow(
            column(4, div(class="feature-card fade-in",
                          div(class="feature-icon", icon("chart-line")),
                          h4("Dashboard Utama"),
                          p("Analisis komprehensif dengan filter interaktif untuk mengeksplorasi data per negara dan perusahaan.")
            )),
            column(4, div(class="feature-card fade-in",
                          div(class="feature-icon", icon("chart-bar")),
                          h4("Analisis Statistik"),
                          p("Visualisasi mendalam: korelasi, distribusi, dan analisis tren untuk pengambilan keputusan.")
            )),
            column(4, div(class="feature-card fade-in",
                          div(class="feature-icon", icon("users")),
                          h4("Team Collaboration"),
                          p("Dikembangkan oleh tim spesialis data dengan expertise di berbagai bidang analitik.")
            ))
          ),
          
          # DATA SOURCE
          div(class="card-premium fade-in", style="margin-top: 40px;",
              h4("Sumber Data"),
              p("Dashboard ini menggunakan data perusahaan teknologi global yang dikumpulkan dan diproses dari berbagai sumber terpercaya. Data mencakup:", style="margin-bottom: 15px;"),
              tags$ul(
                tags$li("Revenue dan kinerja keuangan perusahaan"),
                tags$li("Data pengguna mobile dan penetrasi digital"),
                tags$li("Indikator ekonomi digital per negara"),
                tags$li("Trend pertumbuhan industri teknologi")
              ),
              hr(),
              p(icon("database"), " Data Source: Company financial reports, World Bank, Statista", 
                style="color: #6b7280; font-size: 14px; margin-top: 15px;")
          )
      ),
      
      # =====================================================
      # PAGE: DASHBOARD UTAMA DENGAN BANYAK GRAFIK - DIPERBAIKI
      # =====================================================
      div(id="page_dashboard", class="page-content",
          
          h2("Dashboard Utama", style="margin-top: 15px; text-align: center;"),
          p("Eksplorasi data dengan filter interaktif. Pilih tahun dan negara untuk melihat analisis spesifik.", 
            style="color: #6b7280; margin-bottom: 30px; text-align: center;"),
          
          # FILTER PANEL DI ATAS (CENTERED)
          div(class="filter-panel fade-in",
              h4(class="filter-title", "🎯 Filter Dashboard"),
              fluidRow(
                column(4,
                       selectInput("dash_year","Pilih Tahun:",
                                   choices = c("Semua Tahun", year_list), selected = "Semua Tahun",
                                   width = "100%")
                ),
                column(4,
                       selectInput("dash_country","Pilih Negara:",
                                   choices = c("Semua Negara", country_list), 
                                   selected = "Semua Negara",
                                   width = "100%")
                ),
                column(4,
                       checkboxInput("filter_company", "Filter Perusahaan per Negara", 
                                     value = FALSE, width = "100%")
                )
              ),
              conditionalPanel(
                condition = "input.filter_company == true && input.dash_country != 'Semua Negara'",
                fluidRow(
                  column(12,
                         uiOutput("company_filter_ui")
                  )
                )
              ),
              hr(),
              uiOutput("country_summary_box")
          ),
          
          # SPACE UNTUK KONTEN
          div(class="content-space",
              fluidRow(
                column(12,
                       div(class="card-premium fade-in",
                           tabsetPanel(
                             # TAB 1: GLOBAL OVERVIEW
                             tabPanel("🌍 Global Overview",
                                      br(),
                                      fluidRow(
                                        column(4, div(class="kpi-box",
                                                      div(class="kpi-title","Total Revenue"),
                                                      div(class="kpi-value", textOutput("dash_kpi_revenue")))),
                                        column(4, div(class="kpi-box",
                                                      div(class="kpi-title","Total Mobile Users"),
                                                      div(class="kpi-value", textOutput("dash_kpi_mobile")))),
                                        column(4, div(class="kpi-box",
                                                      div(class="kpi-title","Jumlah Perusahaan"),
                                                      div(class="kpi-value", textOutput("dash_kpi_company"))))
                                      ),
                                      br(),
                                      fluidRow(
                                        column(6, 
                                               div(class="plot-container",
                                                   h5("📱 Top 10 Mobile Users by Country"),
                                                   plotOutput("chart_top_mobile", height = "300px")
                                               )
                                        ),
                                        column(6, 
                                               div(class="plot-container",
                                                   h5("💰 Top 10 Revenue by Country"),
                                                   plotOutput("chart_top_revenue", height = "300px")
                                               )
                                        )
                                      ),
                                      br(),
                                      fluidRow(
                                        column(6,
                                               div(class="plot-container",
                                                   h5("📈 Revenue Growth Trend"),
                                                   plotlyOutput("chart_revenue_trend", height = "300px")
                                               )
                                        ),
                                        column(6,
                                               div(class="plot-container",
                                                   h5("🏢 Top Companies"),
                                                   plotOutput("chart_top_company", height = "300px")
                                               )
                                        )
                                      ),
                                      br(),
                                      div(class="plot-container",
                                          h5("🌐 Digital Economy Index by Country"),
                                          plotlyOutput("chart_digital_index", height = "350px")
                                      ),
                                      br(),
                                      div(class="map-container",
                                          h5("🗺️ World Map - Revenue Distribution"),
                                          plotlyOutput("world_map", height = "400px")
                                      )
                             ),
                             
                             # TAB 2: NEGARA & PERUSAHAAN
                             tabPanel("🏛️ Negara & Perusahaan",
                                      br(),
                                      h4("📋 Ringkasan Negara", style="text-align: center;"),
                                      DTOutput("tbl_country"),
                                      br(),
                                      h4("🏆 Top Perusahaan", style="text-align: center;"),
                                      div(class="plot-container",
                                          plotOutput("chart_company_bar", height = "350px")
                                      ),
                                      br(),
                                      h4("📊 Detail Perusahaan", style="text-align: center;"),
                                      DTOutput("tbl_company")
                             ),
                             
                             # TAB 3: DOWNLOAD DATA
                             tabPanel("💾 Download Data",
                                      br(),
                                      h4("📥 Download Dataset", style="text-align: center;"),
                                      p("Pilih data yang ingin diunduh:", style="text-align: center;"),
                                      div(style="text-align: center;",
                                          downloadButton("dl_country","📁 Download Data Negara",
                                                         class="btn btn-primary btn-lg",
                                                         style="margin-right: 15px; padding: 12px 25px;"),
                                          downloadButton("dl_company","💼 Download Data Perusahaan",
                                                         class="btn btn-success btn-lg",
                                                         style="padding: 12px 25px;")
                                      ),
                                      br(), br(),
                                      h5("👀 Preview Data", style="text-align: center;"),
                                      DTOutput("download_preview")
                             )
                           )
                       )
                )
              )
          )
      ),
      
      # =====================================================
      # PAGE: ANALISIS STATISTIK - KEMBALI KE LAYOUT SEBELUMNYA
      # =====================================================
      div(id="page_statistics", class="page-content",
          
          h2("Analisis Statistik", style="margin-top: 15px; text-align: center;"),
          p("Analisis mendalam dan visualisasi statistik untuk memahami pola dan hubungan dalam data.",
            style="color: #6b7280; margin-bottom: 30px; text-align: center;"),
          
          fluidRow(
            column(3,
                   div(class="card-premium fade-in",
                       h4("⚙️ Pengaturan Analisis"),
                       selectInput("stat_year", "Pilih Tahun:",
                                   choices = c("Semua Tahun", year_list), selected = "Semua Tahun"),
                       selectInput("stat_variable_x", "Variabel X:",
                                   choices = c("Revenue_Billions", "Employees", "Growth_Rate",
                                               "Innovation_Score", "Risk_Score", "Market_Share_Global"),
                                   selected = "Revenue_Billions"),
                       selectInput("stat_variable_y", "Variabel Y:",
                                   choices = c("Revenue_Billions", "Employees", "Growth_Rate",
                                               "Innovation_Score", "Risk_Score", "Market_Share_Global"),
                                   selected = "Growth_Rate"),
                       hr(),
                       h5("📊 Statistik Deskriptif"),
                       div(class="stat-summary-box",
                           verbatimTextOutput("stat_summary")
                       )
                   )
            ),
            
            column(9,
                   div(class="card-premium fade-in",
                       tabsetPanel(
                         tabPanel("📊 Scatter Plot",
                                  br(),
                                  div(class="plot-container",
                                      plotlyOutput("scatter_plot", height = "450px")
                                  ),
                                  br(),
                                  p("Analisis hubungan antara dua variabel. Titik berwarna menunjukkan negara yang berbeda.",
                                    style="text-align: center;")
                         ),
                         
                         tabPanel("📈 Distribusi",
                                  br(),
                                  div(class="plot-container",
                                      plotOutput("distribution_plot", height = "450px")
                                  ),
                                  br(),
                                  p("Distribusi frekuensi variabel terpilih. Histogram menunjukkan sebaran data.",
                                    style="text-align: center;")
                         ),
                         
                         tabPanel("🔗 Korelasi",
                                  br(),
                                  div(class="plot-container",
                                      plotOutput("correlation_plot", height = "450px")
                                  ),
                                  br(),
                                  p("Heatmap korelasi antar variabel numerik. Warna menunjukkan kekuatan hubungan.",
                                    style="text-align: center;")
                         ),
                         
                         tabPanel("⏱️ Time Series",
                                  br(),
                                  div(class="plot-container",
                                      plotlyOutput("time_series_plot", height = "450px")
                                  ),
                                  br(),
                                  p("Perkembangan variabel terpilih sepanjang waktu. Garis menunjukkan tren.",
                                    style="text-align: center;")
                         ),
                         
                         tabPanel("📊 Box Plot",
                                  br(),
                                  div(class="plot-container",
                                      plotlyOutput("box_plot", height = "450px")
                                  ),
                                  br(),
                                  p("Distribusi data per negara dengan box plot.",
                                    style="text-align: center;")
                         )
                       )
                   )
            )
          )
      ),
      
      # =====================================================
      # PAGE: ABOUT TEAM DENGAN FOTO ASLI
      # =====================================================
      div(id="page_team", class="page-content",
          
          h2("Meet The Team", style="margin-top: 15px; text-align: center;"),
          p("Tim pengembang dashboard yang terdiri dari profesional dengan latar belakang saling melengkapi.",
            style="color: #6b7280; margin-bottom: 30px; text-align: center;"),
          
          fluidRow(
            column(3,
                   div(class="team-card fade-in",
                       div(class="team-photo-container",
                           img(src="https://raw.githubusercontent.com/muhakbaridris/visdat/4c9784053a0007bc2f2f617ac479c5a9bb98f168/www/akbar.jpg", 
                               class="team-photo", alt="Muh. Akbar Idris")
                       ),
                       h4("Muh. Akbar Idris"),
                       div(class="team-role", "Project Lead & Data Scientist"),
                       p("Bertanggung jawab atas arsitektur data, model analitik, dan pengambilan keputusan berbasis data."),
                       div(class="team-nim", "NIM: M0501241013")
                   )),
            
            column(3,
                   div(class="team-card fade-in",
                       div(class="team-photo-container",
                           img(src="https://raw.githubusercontent.com/muhakbaridris/visdat/4c9784053a0007bc2f2f617ac479c5a9bb98f168/www/desy.jpg", 
                               class="team-photo", alt="Desy Endriani")
                       ),
                       h4("Desy Endriani"),
                       div(class="team-role", "Data Analyst"),
                       p("Menganalisis pola data, membuat visualisasi, dan menerjemahkan insight bisnis dari data."),
                       div(class="team-nim", "NIM: M0501241077")
                   )),
            
            column(3,
                   div(class="team-card fade-in",
                       div(class="team-photo-container",
                           img(src="https://raw.githubusercontent.com/muhakbaridris/visdat/4c9784053a0007bc2f2f617ac479c5a9bb98f168/www/aini.jpg", 
                               class="team-photo", alt="Nur Aini")
                       ),
                       h4("Nur Aini"),
                       div(class="team-role", "UI/UX Designer & Frontend Developer"),
                       p("Mendesain antarmuka pengguna, pengalaman pengguna, dan implementasi frontend dashboard."),
                       div(class="team-nim", "NIM: M0501241058")
                   )),
            
            column(3,
                   div(class="team-card fade-in",
                       div(class="team-photo-container",
                           img(src="https://raw.githubusercontent.com/muhakbaridris/visdat/4c9784053a0007bc2f2f617ac479c5a9bb98f168/www/yusuf.jpg", 
                               class="team-photo", alt="Syaifullah Yusuf Ramadhan")
                       ),
                       h4("Syaifullah Yusuf Ramadhan"),
                       div(class="team-role", "Data Engineer & Web Scraper"),
                       p("Mengumpulkan, membersihkan, dan memproses data dari berbagai sumber melalui web scraping."),
                       div(class="team-nim", "NIM: M0501241051")
                   ))
          ),
          
          br(),
          
          div(class="card-premium fade-in",
              h4("Proyek Digital Ecosystem Dashboard"),
              p("Dashboard ini dikembangkan sebagai bagian dari proyek analisis ekosistem digital global. Tujuan proyek ini adalah:"),
              tags$ul(
                tags$li("Menyediakan platform analitik yang intuitif untuk memantau kinerja perusahaan teknologi"),
                tags$li("Menganalisis tren ekonomi digital di berbagai negara"),
                tags$li("Memberikan insight untuk pengambilan keputusan strategis"),
                tags$li("Mengintegrasikan berbagai sumber data menjadi satu dashboard yang kohesif")
              ),
              p("Teknologi yang digunakan: R Shiny, dplyr, ggplot2, Plotly, dan berbagai paket analisis data."),
              hr(),
              div(style="text-align: center;",
                  actionButton("btn_back_home", "🏠 Kembali ke Beranda", 
                               class="btn btn-primary",
                               style="padding: 10px 25px;")
              )
          )
      )
  )
)

# ============================================================================
# DIGITAL ECOSYSTEM DASHBOARD — SERVER PREMIUM
# BAGIAN 3 — Server Logic
# ============================================================================

server <- function(input, output, session) {
  
  # ============================================================
  # PAGE CONTROLLER DENGAN ANIMASI
  # ============================================================
  
  # Function to switch pages with animation
  switchPage <- function(page) {
    # Animate out current page
    removeClass(selector = ".page-content.active", class = "active")
    
    # Remove active class from all nav items
    removeClass(selector = ".navbar-item", class = "navbar-item-active")
    
    # Add active class to selected page after delay
    delay(200, {
      addClass(paste0("page_", page), "active")
      # Add active class to clicked nav item
      addClass(paste0("nav_", page), "navbar-item-active")
    })
  }
  
  # Initialize - Beranda is active by default
  observe({
    switchPage("beranda")
  })
  
  # Navbar events
  observeEvent(input$nav_home, {
    switchPage("beranda")
  })
  
  observeEvent(input$nav_dashboard, {
    switchPage("dashboard")
  })
  
  observeEvent(input$nav_statistics, {
    switchPage("statistics")
  })
  
  observeEvent(input$nav_team, {
    switchPage("team")
  })
  
  # Button from hero to dashboard
  observeEvent(input$btn_to_dashboard, {
    switchPage("dashboard")
  })
  
  # Button back to home
  observeEvent(input$btn_back_home, {
    switchPage("beranda")
  })
  
  # ============================================================
  # KPI — BERANDA (GLOBAL, NO FILTER)
  # ============================================================
  
  output$kpi_rev <- renderText({
    total_rev <- round(sum(country_year$Total_Revenue, na.rm = TRUE), 1)
    paste0("$", total_rev, "B")
  })
  
  output$kpi_country <- renderText({
    n_country <- length(unique(country_year$Country_Standard))
    as.character(n_country)
  })
  
  output$kpi_company <- renderText({
    n_company <- length(unique(company_year$Company))
    as.character(n_company)
  })
  
  output$kpi_growth <- renderText({
    avg_growth <- round(mean(company_year$Growth_Rate, na.rm = TRUE), 1)
    paste0(avg_growth, "%")
  })
  
  # ============================================================
  # REACTIVE DATA FOR DASHBOARD
  # ============================================================
  
  # Data for selected year - INCLUDE ALL YEARS
  dashboard_year_data <- reactive({
    if (input$dash_year == "Semua Tahun") {
      # Data untuk semua tahun
      if (input$dash_country == "Semua Negara") {
        country_year
      } else {
        country_year %>% filter(Country_Standard == input$dash_country)
      }
    } else if (input$dash_country == "Semua Negara") {
      # Global data for selected year
      country_year %>% filter(Year == input$dash_year)
    } else {
      # Country-specific data
      country_year %>% 
        filter(Year == input$dash_year, 
               Country_Standard == input$dash_country)
    }
  })
  
  # Company data with optional country filter
  dashboard_company_data <- reactive({
    if (input$dash_year == "Semua Tahun") {
      # Data untuk semua tahun
      if (input$dash_country == "Semua Negara" || !input$filter_company) {
        company_year
      } else {
        company_year %>% filter(Country_Standard == input$dash_country)
      }
    } else if (input$dash_country == "Semua Negara" || !input$filter_company) {
      # Global company data or when filter is off
      company_year %>% filter(Year == input$dash_year)
    } else {
      # Country-specific company data when filter is on
      company_year %>% 
        filter(Year == input$dash_year, 
               Country_Standard == input$dash_country)
    }
  })
  
  # ============================================================
  # COMPANY FILTER UI (ONLY SHOW WHEN CHECKBOX IS CHECKED)
  # ============================================================
  
  output$company_filter_ui <- renderUI({
    req(input$dash_country)
    
    if (input$dash_country != "Semua Negara") {
      if (input$dash_year == "Semua Tahun") {
        companies_in_country <- company_year %>%
          filter(Country_Standard == input$dash_country) %>%
          pull(Company) %>%
          unique()
      } else {
        companies_in_country <- company_year %>%
          filter(Year == input$dash_year, 
                 Country_Standard == input$dash_country) %>%
          pull(Company) %>%
          unique()
      }
      
      if (length(companies_in_country) > 0) {
        selectInput("selected_companies", "Pilih Perusahaan:",
                    choices = companies_in_country,
                    selected = companies_in_country[1:min(5, length(companies_in_country))],
                    multiple = TRUE,
                    selectize = TRUE,
                    width = "100%")
      } else {
        p("Tidak ada perusahaan di negara ini untuk tahun yang dipilih.", 
          style="color: #6b7280; font-style: italic; text-align: center;")
      }
    }
  })
  
  # ============================================================
  # COUNTRY SUMMARY BOX
  # ============================================================
  
  output$country_summary_box <- renderUI({
    data <- dashboard_year_data()
    
    if (nrow(data) > 0) {
      if (input$dash_country == "Semua Negara") {
        tagList(
          p(icon("globe"), " Scope: Global"),
          p(icon("building"), paste("Perusahaan:", nrow(dashboard_company_data()))),
          p(icon("users"), paste("Mobile Users:", 
                                 format(round(sum(data$Mobile_Users, na.rm = TRUE)/1e6, 1), big.mark=","), "juta")),
          p(icon("chart-line"), paste("Avg Growth:", 
                                      round(mean(data$Avg_Growth_Rate, na.rm = TRUE), 1), "%"))
        )
      } else {
        if (input$dash_year == "Semua Tahun") {
          # Rata-rata untuk semua tahun
          tagList(
            p(icon("flag"), paste("Negara:", input$dash_country)),
            p(icon("building"), paste("Perusahaan:", data$N_Companies[1])),
            p(icon("users"), paste("Mobile Penetration:", 
                                   round(mean(data$Mobile_per_100, na.rm = TRUE), 1), "%")),
            p(icon("money-bill"), paste("Revenue/Capita: $", 
                                        round(mean(data$Revenue_per_Cap, na.rm = TRUE), 2)))
          )
        } else {
          tagList(
            p(icon("flag"), paste("Negara:", input$dash_country)),
            p(icon("building"), paste("Perusahaan:", data$N_Companies[1])),
            p(icon("users"), paste("Mobile Penetration:", 
                                   round(data$Mobile_per_100[1], 1), "%")),
            p(icon("money-bill"), paste("Revenue/Capita: $", 
                                        round(data$Revenue_per_Cap[1], 2)))
          )
        }
      }
    } else {
      p("Tidak ada data untuk filter yang dipilih.", style="color: #6b7280; font-style: italic;")
    }
  })
  
  # ============================================================
  # DASHBOARD KPI OUTPUTS
  # ============================================================
  
  output$dash_kpi_revenue <- renderText({
    data <- dashboard_year_data()
    total_rev <- sum(data$Total_Revenue, na.rm = TRUE)
    paste0("$", round(total_rev, 1), "B")
  })
  
  output$dash_kpi_mobile <- renderText({
    data <- dashboard_year_data()
    total_mobile <- sum(data$Mobile_Users, na.rm = TRUE)
    if (total_mobile >= 1e9) {
      paste0(round(total_mobile/1e9, 1), "B")
    } else if (total_mobile >= 1e6) {
      paste0(round(total_mobile/1e6, 1), "M")
    } else {
      format(total_mobile, big.mark = ",")
    }
  })
  
  output$dash_kpi_company <- renderText({
    data <- dashboard_company_data()
    length(unique(data$Company))
  })
  
  # ============================================================
  # CHARTS FOR DASHBOARD - GLOBAL OVERVIEW
  # ============================================================
  
  # 1. Top Mobile Users
  output$chart_top_mobile <- renderPlot({
    if (input$dash_country == "Semua Negara") {
      if (input$dash_year == "Semua Tahun") {
        data <- country_year %>%
          group_by(Country_Standard) %>%
          summarise(Mobile_Users = sum(Mobile_Users, na.rm = TRUE)) %>%
          arrange(desc(Mobile_Users)) %>%
          head(10)
      } else {
        data <- country_year %>%
          filter(Year == input$dash_year) %>%
          arrange(desc(Mobile_Users)) %>% 
          head(10)
      }
    } else {
      data <- dashboard_year_data()
    }
    
    if (nrow(data) > 0) {
      ggplot(data, aes(x = reorder(Country_Standard, Mobile_Users),
                       y = Mobile_Users / 1e6,
                       fill = Mobile_Users)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_fill_viridis_c(option = "plasma") +
        labs(y = "Mobile Users (juta)", x = "",
             title = "") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = "white", color = NA),
              axis.text = element_text(color = "#374151"),
              panel.grid.major = element_line(color = "#f3f4f6"))
    } else {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Tidak ada data untuk filter yang dipilih", 
                 size = 5, color = "gray50") +
        theme_void()
    }
  })
  
  # 2. Top Revenue
  output$chart_top_revenue <- renderPlot({
    if (input$dash_country == "Semua Negara") {
      if (input$dash_year == "Semua Tahun") {
        data <- country_year %>%
          group_by(Country_Standard) %>%
          summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE)) %>%
          arrange(desc(Total_Revenue)) %>%
          head(10)
      } else {
        data <- country_year %>%
          filter(Year == input$dash_year) %>%
          arrange(desc(Total_Revenue)) %>% 
          head(10)
      }
    } else {
      data <- dashboard_year_data()
    }
    
    if (nrow(data) > 0) {
      ggplot(data, aes(x = reorder(Country_Standard, Total_Revenue),
                       y = Total_Revenue,
                       fill = Total_Revenue)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_fill_viridis_c(option = "viridis") +
        labs(y = "Revenue (Billion $)", x = "",
             title = "") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = "white", color = NA),
              axis.text = element_text(color = "#374151"),
              panel.grid.major = element_line(color = "#f3f4f6"))
    } else {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Tidak ada data untuk filter yang dipilih", 
                 size = 5, color = "gray50") +
        theme_void()
    }
  })
  
  # 3. Revenue Trend (Plotly)
  output$chart_revenue_trend <- renderPlotly({
    if (input$dash_country == "Semua Negara") {
      data <- country_year %>%
        group_by(Year) %>%
        summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE))
    } else {
      data <- country_year %>%
        filter(Country_Standard == input$dash_country) %>%
        group_by(Year) %>%
        summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE))
    }
    
    if (nrow(data) > 0) {
      p <- plot_ly(data, x = ~Year, y = ~Total_Revenue,
                   type = 'scatter', mode = 'lines+markers',
                   line = list(color = '#2563eb', width = 3),
                   marker = list(color = '#2563eb', size = 8),
                   hoverinfo = 'text',
                   text = ~paste('Tahun: ', Year,
                                 '<br>Revenue: $', round(Total_Revenue, 2), 'B')) %>%
        layout(title = "",
               xaxis = list(title = "Tahun", gridcolor = '#f3f4f6'),
               yaxis = list(title = "Revenue (Billion $)", gridcolor = '#f3f4f6'),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white')
      
      p
    }
  })
  
  # 4. Top Companies
  output$chart_top_company <- renderPlot({
    data <- dashboard_company_data()
    
    if (nrow(data) > 0) {
      if (input$dash_year == "Semua Tahun") {
        # Jika semua tahun, ambil rata-rata revenue
        data_summary <- data %>%
          group_by(Company, Country_Standard) %>%
          summarise(Revenue_Billions = mean(Revenue_Billions, na.rm = TRUE)) %>%
          ungroup() %>%
          arrange(desc(Revenue_Billions)) %>%
          head(10)
        
        top_data <- data_summary
      } else {
        top_n <- ifelse(input$dash_country == "Semua Negara", 10, 8)
        
        top_data <- data %>%
          arrange(desc(Revenue_Billions)) %>%
          head(top_n)
      }
      
      ggplot(top_data, aes(x = reorder(Company, Revenue_Billions),
                           y = Revenue_Billions,
                           fill = Country_Standard)) +
        geom_col() +
        coord_flip() +
        scale_fill_viridis_d(option = "magma") +
        labs(y = "Billion USD", x = "",
             title = "",
             fill = "Negara") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.position = "bottom",
              legend.text = element_text(size = 10),
              plot.background = element_rect(fill = "white", color = NA),
              axis.text = element_text(color = "#374151"),
              panel.grid.major = element_line(color = "#f3f4f6"))
    } else {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Tidak ada data untuk filter yang dipilih", 
                 size = 5, color = "gray50") +
        theme_void()
    }
  })
  
  # 5. Digital Economy Index (Plotly)
  output$chart_digital_index <- renderPlotly({
    data <- dashboard_year_data()
    
    if (nrow(data) > 0) {
      if (input$dash_year == "Semua Tahun") {
        # Untuk semua tahun, rata-rata index
        data_summary <- data %>%
          group_by(Country_Standard) %>%
          summarise(Digital_Economy_Index = mean(Digital_Economy_Index, na.rm = TRUE)) %>%
          arrange(desc(Digital_Economy_Index))
      } else {
        data_summary <- data %>% arrange(desc(Digital_Economy_Index))
      }
      
      p <- plot_ly(data_summary, x = ~Country_Standard, y = ~Digital_Economy_Index,
                   type = 'bar',
                   marker = list(color = ~Digital_Economy_Index,
                                 colorscale = 'Viridis',
                                 showscale = TRUE),
                   hoverinfo = 'text',
                   text = ~paste('Negara: ', Country_Standard,
                                 '<br>Digital Index: ', round(Digital_Economy_Index, 2))) %>%
        layout(title = "",
               xaxis = list(title = "Negara", tickangle = -45, gridcolor = '#f3f4f6'),
               yaxis = list(title = "Digital Economy Index", gridcolor = '#f3f4f6'),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white')
      
      p
    }
  })
  
  # 6. World Map - Revenue Distribution (DIPERBAIKI)
  output$world_map <- renderPlotly({
    data <- dashboard_year_data()
    
    if (nrow(data) > 0) {
      # Agregasi data jika "Semua Tahun"
      if (input$dash_year == "Semua Tahun") {
        data <- data %>%
          group_by(Country_Standard, Country_Code) %>%
          summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE)) %>%
          ungroup()
      }
      
      # Light grey boundaries
      l <- list(color = toRGB("grey"), width = 0.5)
      
      # Specify map projection and options
      g <- list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'Mercator'),
        showland = TRUE,
        landcolor = toRGB("gray95"),
        countrycolor = toRGB("gray80"),
        coastlinecolor = toRGB("gray80")
      )
      
      p <- plot_geo(data) %>%
        add_trace(
          z = ~Total_Revenue, 
          color = ~Total_Revenue,
          colorscale = list(c(0, 0.5, 1), c('#f7fbff', '#6baed6', '#08306b')), # Custom color scale
          text = ~paste(Country_Standard, '<br>Revenue: $', round(Total_Revenue, 2), 'B'),
          locations = ~Country_Code,
          marker = list(line = l)
        ) %>%
        colorbar(title = 'Revenue (B $)') %>%
        layout(
          title = '',
          geo = g,
          plot_bgcolor = 'white',
          paper_bgcolor = 'white'
        )
      
      p
    }
  })
  
  # 7. Company Bar Chart for Negara & Perusahaan tab
  output$chart_company_bar <- renderPlot({
    data <- dashboard_company_data()
    
    if (!is.null(input$selected_companies) && input$filter_company && 
        input$dash_country != "Semua Negara") {
      data <- data %>% filter(Company %in% input$selected_companies)
    }
    
    if (nrow(data) > 0) {
      ggplot(data, aes(x = reorder(Company, Revenue_Billions),
                       y = Revenue_Billions,
                       fill = Revenue_Billions)) +
        geom_col() +
        coord_flip() +
        scale_fill_viridis_c(option = "inferno") +
        labs(y = "Revenue (Billion USD)", x = "Perusahaan",
             title = "") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = "white", color = NA),
              axis.text = element_text(color = "#374151"),
              panel.grid.major = element_line(color = "#f3f4f6"))
    } else {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Tidak ada data untuk filter yang dipilih", 
                 size = 5, color = "gray50") +
        theme_void()
    }
  })
  
  # ============================================================
  # TABLES FOR DASHBOARD
  # ============================================================
  
  output$tbl_country <- renderDT({
    data <- dashboard_year_data()
    
    if (input$dash_country == "Semua Negara") {
      if (input$dash_year == "Semua Tahun") {
        data <- data %>%
          group_by(Country_Standard) %>%
          summarise(
            Total_Revenue = sum(Total_Revenue, na.rm = TRUE),
            Mobile_Users = sum(Mobile_Users, na.rm = TRUE),
            N_Companies = max(N_Companies, na.rm = TRUE),
            Avg_Growth_Rate = mean(Avg_Growth_Rate, na.rm = TRUE),
            Digital_Economy_Index = mean(Digital_Economy_Index, na.rm = TRUE)
          ) %>%
          arrange(desc(Total_Revenue))
      } else {
        data <- data %>% arrange(desc(Total_Revenue))
      }
    }
    
    if (nrow(data) > 0) {
      datatable(
        data %>% select(Country_Standard, Total_Revenue, Mobile_Users,
                        N_Companies, Avg_Growth_Rate, Digital_Economy_Index),
        options = list(
          dom = 'Bfrtip',
          pageLength = 8,
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        rownames = FALSE,
        extensions = 'Buttons',
        class = 'display compact hover',
        colnames = c("Negara", "Revenue (B)", "Mobile Users", 
                     "Jumlah Perusahaan", "Growth Rate (%)", "Digital Index")
      ) %>% formatRound(c("Total_Revenue", "Digital_Economy_Index"), 2) %>%
        formatRound("Avg_Growth_Rate", 1) %>%
        formatCurrency("Total_Revenue", "$", digits = 1) %>%
        formatStyle('Total_Revenue',
                    background = styleColorBar(data$Total_Revenue, 'lightblue'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    }
  })
  
  output$tbl_company <- renderDT({
    data <- dashboard_company_data()
    
    if (!is.null(input$selected_companies) && input$filter_company && 
        input$dash_country != "Semua Negara") {
      data <- data %>% filter(Company %in% input$selected_companies)
    }
    
    if (nrow(data) > 0) {
      datatable(
        data %>% select(Company, Country_Standard, Revenue_Billions,
                        Employees, Growth_Rate, Innovation_Score, Risk_Score),
        options = list(
          dom = 'Bfrtip',
          pageLength = 8,
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        rownames = FALSE,
        extensions = 'Buttons',
        class = 'display compact hover',
        colnames = c("Perusahaan", "Negara", "Revenue (B)", "Employees",
                     "Growth Rate (%)", "Innovation Score", "Risk Score")
      ) %>% formatRound(c("Revenue_Billions", "Innovation_Score", "Risk_Score"), 2) %>%
        formatRound("Growth_Rate", 1) %>%
        formatCurrency("Revenue_Billions", "$", digits = 1) %>%
        formatStyle('Revenue_Billions',
                    background = styleColorBar(data$Revenue_Billions, 'lightgreen'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    }
  })
  
  # ============================================================
  # DOWNLOAD HANDLERS
  # ============================================================
  
  output$dl_country <- downloadHandler(
    filename = function() {
      if (input$dash_country == "Semua Negara") {
        if (input$dash_year == "Semua Tahun") {
          "country_global_all_years.csv"
        } else {
          paste0("country_global_", input$dash_year, ".csv")
        }
      } else {
        if (input$dash_year == "Semua Tahun") {
          paste0("country_", gsub(" ", "_", input$dash_country), "_all_years.csv")
        } else {
          paste0("country_", gsub(" ", "_", input$dash_country), "_", input$dash_year, ".csv")
        }
      }
    },
    content = function(file) {
      write.csv(dashboard_year_data(), file, row.names = FALSE)
    }
  )
  
  output$dl_company <- downloadHandler(
    filename = function() {
      if (input$dash_country == "Semua Negara") {
        if (input$dash_year == "Semua Tahun") {
          "company_global_all_years.csv"
        } else {
          paste0("company_global_", input$dash_year, ".csv")
        }
      } else {
        if (input$dash_year == "Semua Tahun") {
          paste0("company_", gsub(" ", "_", input$dash_country), "_all_years.csv")
        } else {
          paste0("company_", gsub(" ", "_", input$dash_country), "_", input$dash_year, ".csv")
        }
      }
    },
    content = function(file) {
      write.csv(dashboard_company_data(), file, row.names = FALSE)
    }
  )
  
  output$download_preview <- renderDT({
    data <- dashboard_company_data()
    
    if (nrow(data) > 0) {
      datatable(
        data %>% head(10),
        options = list(
          dom = 't',
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = 'display compact'
      )
    }
  })
  
  # ============================================================
  # STATISTICAL ANALYSIS PAGE
  # ============================================================
  
  # Reactive data for statistical analysis
  stat_data <- reactive({
    if (input$stat_year == "Semua Tahun") {
      company_year
    } else {
      company_year %>% filter(Year == input$stat_year)
    }
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlotly({
    req(input$stat_variable_x, input$stat_variable_y)
    
    data <- stat_data()
    
    # Validasi data
    if (nrow(data) == 0) {
      return(plotly_empty() %>%
               layout(title = list(text = "Tidak ada data untuk tahun ini",
                                   yref = "paper", y = 0.5)))
    }
    
    # Pastikan variabel ada
    if (!(input$stat_variable_x %in% names(data)) || !(input$stat_variable_y %in% names(data))) {
      return(plotly_empty() %>%
               layout(title = list(text = "Variabel tidak ditemukan dalam data",
                                   yref = "paper", y = 0.5)))
    }
    
    # Filter NA values
    data_clean <- data %>%
      filter(!is.na(.data[[input$stat_variable_x]]),
             !is.na(.data[[input$stat_variable_y]]),
             !is.na(Company))  # Pastikan Company tidak NA
    
    if (nrow(data_clean) == 0) {
      return(plotly_empty() %>%
               layout(title = list(text = "Data tidak lengkap untuk analisis",
                                   yref = "paper", y = 0.5)))
    }
    
    # Buat plot dengan Plotly langsung
    p <- plot_ly(
      data = data_clean,
      x = ~get(input$stat_variable_x),
      y = ~get(input$stat_variable_y),
      color = ~Country_Standard,
      colors = viridis_pal()(length(unique(data_clean$Country_Standard))),
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10, opacity = 0.7),
      text = ~paste(
        "Perusahaan:", Company,
        "<br>", gsub("_", " ", input$stat_variable_x), ":", 
        round(get(input$stat_variable_x), 2),
        "<br>", gsub("_", " ", input$stat_variable_y), ":",
        round(get(input$stat_variable_y), 2),
        "<br>Negara:", Country_Standard
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = paste(gsub("_", " ", input$stat_variable_x), 
                      "vs", 
                      gsub("_", " ", input$stat_variable_y)),
        xaxis = list(title = gsub("_", " ", input$stat_variable_x)),
        yaxis = list(title = gsub("_", " ", input$stat_variable_y)),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        hovermode = 'closest'
      )
    
    p
  })
  
  # Distribution Plot
  output$distribution_plot <- renderPlot({
    req(input$stat_variable_x)
    
    data <- stat_data()
    
    if (nrow(data) > 0 && input$stat_variable_x %in% names(data)) {
      data_clean <- data %>% filter(!is.na(.data[[input$stat_variable_x]]))
      
      if (nrow(data_clean) > 0) {
        ggplot(data_clean, aes_string(x = input$stat_variable_x)) +
          geom_histogram(fill = "#2563eb", alpha = 0.7, bins = 25, 
                         color = "white", linewidth = 0.3) +
          geom_density(aes(y = after_stat(count) * 2.5), color = "#1e40af", size = 1) +
          labs(x = gsub("_", " ", input$stat_variable_x), y = "Frekuensi",
               title = paste("Distribusi", gsub("_", " ", input$stat_variable_x))) +
          theme_minimal(base_size = 13) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                plot.background = element_rect(fill = "white", color = NA),
                axis.text = element_text(color = "#374151"),
                panel.grid.major = element_line(color = "#f3f4f6"))
      } else {
        ggplot() +
          annotate("text", x = 1, y = 1, label = "Tidak ada data untuk variabel ini", 
                   size = 5, color = "gray50") +
          theme_void()
      }
    }
  })
  
  # Correlation Plot
  output$correlation_plot <- renderPlot({
    req(input$stat_year)
    
    data <- stat_data() %>%
      select(Revenue_Billions, Employees, Growth_Rate,
             Innovation_Score, Risk_Score, Market_Share_Global) %>%
      na.omit()
    
    if (nrow(data) > 2) {
      cor_matrix <- cor(data)
      
      # Handle NA values
      cor_matrix[is.na(cor_matrix)] <- 0
      
      colnames(cor_matrix) <- gsub("_", " ", colnames(cor_matrix))
      rownames(cor_matrix) <- gsub("_", " ", rownames(cor_matrix))
      
      corrplot(cor_matrix, 
               method = "color", 
               type = "upper",
               tl.col = "black", 
               tl.srt = 45,
               tl.cex = 0.9,
               addCoef.col = "black", 
               number.cex = 0.8,
               col = colorRampPalette(c("#2563eb", "white", "#10b981"))(100),
               title = "Korelasi Antar Variabel",
               mar = c(0, 0, 2, 0))
    } else {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Korelasi Antar Variabel")
      text(1, 1, "Data tidak cukup untuk analisis korelasi", 
           col = "gray50", cex = 1.2)
    }
  })
  
  # Time Series Plot
  output$time_series_plot <- renderPlotly({
    req(input$stat_variable_x)
    
    # Pilih data berdasarkan variabel
    if (input$stat_variable_x %in% c("Revenue_Billions", "Employees", "Market_Share_Global")) {
      data <- company_year %>%
        group_by(Year) %>%
        summarise(Value = mean(.data[[input$stat_variable_x]], na.rm = TRUE)) %>%
        filter(!is.na(Value))
    } else if (input$stat_variable_x %in% c("Growth_Rate", "Innovation_Score", "Risk_Score")) {
      data <- company_year %>%
        group_by(Year) %>%
        summarise(Value = mean(.data[[input$stat_variable_x]], na.rm = TRUE)) %>%
        filter(!is.na(Value))
    } else {
      data <- company_year %>%
        group_by(Year) %>%
        summarise(Value = mean(Revenue_Billions, na.rm = TRUE)) %>%
        filter(!is.na(Value))
    }
    
    if (nrow(data) > 0) {
      p <- plot_ly(data, x = ~Year, y = ~Value,
                   type = 'scatter', mode = 'lines+markers',
                   line = list(color = '#2563eb', width = 3),
                   marker = list(color = '#2563eb', size = 8),
                   fill = 'tozeroy',
                   fillcolor = 'rgba(37, 99, 235, 0.1)',
                   hoverinfo = 'text',
                   text = ~paste('Tahun: ', Year,
                                 '<br>', gsub("_", " ", input$stat_variable_x), ': ', 
                                 round(Value, 2))) %>%
        layout(title = paste("Tren", gsub("_", " ", input$stat_variable_x), "per Tahun"),
               xaxis = list(title = "Tahun", gridcolor = '#f3f4f6'),
               yaxis = list(title = gsub("_", " ", input$stat_variable_x), gridcolor = '#f3f4f6'),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white')
      
      p
    } else {
      plotly_empty() %>%
        layout(title = list(text = "Tidak ada data untuk variabel yang dipilih",
                            yref = "paper", y = 0.5))
    }
  })
  
  # Box Plot
  output$box_plot <- renderPlotly({
    req(input$stat_year, input$stat_variable_x)
    
    data <- stat_data()
    
    if (nrow(data) > 0 && input$stat_variable_x %in% names(data)) {
      data_clean <- data %>% 
        filter(!is.na(.data[[input$stat_variable_x]]),
               !is.na(Country_Standard))
      
      if (nrow(data_clean) > 0) {
        p <- plot_ly(data_clean, 
                     y = ~get(input$stat_variable_x),
                     color = ~Country_Standard,
                     type = "box",
                     boxpoints = "all",
                     jitter = 0.3,
                     pointpos = -1.8) %>%
          layout(title = paste("Distribusi", gsub("_", " ", input$stat_variable_x), "per Negara"),
                 xaxis = list(title = "Negara"),
                 yaxis = list(title = gsub("_", " ", input$stat_variable_x)),
                 plot_bgcolor = 'white',
                 paper_bgcolor = 'white')
        
        p
      }
    }
  })
  
  # Statistical Summary
  output$stat_summary <- renderPrint({
    req(input$stat_variable_x)
    
    data <- stat_data()
    
    if (nrow(data) > 0 && input$stat_variable_x %in% names(data)) {
      var_data <- na.omit(data[[input$stat_variable_x]])
      
      if (length(var_data) > 0) {
        cat("=================================\n")
        cat("ANALISIS STATISTIK\n")
        cat("=================================\n")
        cat("Variabel:", gsub("_", " ", input$stat_variable_x), "\n")
        cat("Tahun:", ifelse(input$stat_year == "Semua Tahun", "Semua Tahun", input$stat_year), "\n")
        cat("Jumlah Observasi:", length(var_data), "\n\n")
        cat("STATISTIK DESKRIPTIF:\n")
        cat("-------------------\n")
        cat("Minimum     :", round(min(var_data), 3), "\n")
        cat("Kuartil 1   :", round(quantile(var_data, 0.25), 3), "\n")
        cat("Median      :", round(median(var_data), 3), "\n")
        cat("Rata-rata   :", round(mean(var_data), 3), "\n")
        cat("Kuartil 3   :", round(quantile(var_data, 0.75), 3), "\n")
        cat("Maksimum    :", round(max(var_data), 3), "\n")
        cat("Standar Dev:", round(sd(var_data), 3), "\n")
        cat("IQR         :", round(IQR(var_data), 3), "\n")
        cat("Varians     :", round(var(var_data), 3), "\n")
        cat("\n=================================\n")
      } else {
        cat("Tidak ada data untuk variabel dan tahun yang dipilih.")
      }
    } else {
      cat("Tidak ada data untuk variabel dan tahun yang dipilih.")
    }
  })
}

# ============================================================================
# BAGIAN 4 — RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)