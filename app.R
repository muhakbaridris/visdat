# ============================================================
#  DIGITAL ECOSYSTEM DASHBOARD - PREMIUM ENTERPRISE EDITION
#  UI Ultra Modern + Performance Optimized
# ============================================================

# ==== 0. LIBRARIES ====
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(tidyr)
library(leaflet)
library(forecast)
library(cluster)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(igraph)
library(visNetwork)
library(wordcloud2)
library(lubridate)
library(shinyWidgets)
library(purrr)
library(scales)
library(viridis)
library(RColorBrewer)
library(waiter)
library(shinyjs)
library(rlang)
library(plotly)

cat("\n=== Digital Ecosystem Dashboard — Premium Enterprise Edition ===\n")

# ============================================================
#  BAGIAN 1 — LOAD & PREPROCESS DATA
# ============================================================

# ==== 1. LOAD DATA ====
tryCatch({
  visdat <- read.csv("df_fix_final.csv", stringsAsFactors = FALSE)
  cat("Data berhasil dimuat:", nrow(visdat), "baris\n")
}, error = function(e) {
  cat("Gagal memuat data, membuat dummy data...\n")
  set.seed(123)
  visdat <- data.frame(
    Rank = 1:30,
    Company = rep(c("Apple", "Google", "Microsoft", "Amazon", "Facebook",
                    "Samsung", "Tencent", "Alibaba", "Netflix", "Uber"), 3),
    Revenue = runif(30, 50, 500),
    Employees = runif(30, 10000, 500000),
    Headquarters = rep(c("Cupertino", "Mountain View", "Redmond",
                         "Seattle", "Menlo Park",
                         "Suwon", "Shenzhen", "Hangzhou",
                         "Los Gatos", "San Francisco"), 3),
    Country = rep(c("United States", "United States", "United States",
                    "United States", "United States",
                    "South Korea", "China", "China",
                    "United States", "United States"), 3),
    Year = rep(2021:2023, each = 10),
    Founded = rep(c(1976, 1998, 1975, 1994, 2004, 1969,
                    1998, 1999, 1997, 2009), 3),
    Industry = rep(c("Consumer Electronics","Internet","Software",
                     "E-commerce","Social Media",
                     "Electronics","Technology","E-commerce",
                     "Entertainment","Technology"), 3),
    Population = runif(30, 1e7, 1e9),
    X.of.phone.numbers = runif(30, 1e7, 1e9)
  )
})

raw_data <- visdat

# ==== 2. PREPROCESSING ====
cat("Memproses data...\n")

phone_col <- grep("phone", names(raw_data), ignore.case = TRUE, value = TRUE)[1]
if (is.na(phone_col)) phone_col <- "X.of.phone.numbers"

processed_data <- raw_data %>%
  rename(
    Revenue_Billions = Revenue,
    Country_Standard = Country,
    Mobile_Users = !!sym(phone_col)
  ) %>%
  mutate(
    across(
      c(Revenue_Billions, Employees, Population,
        Mobile_Users, Year, Rank, Founded),
      ~ suppressWarnings(as.numeric(.))
    ),
    Revenue_per_Employee =
      ifelse(Employees > 0, (Revenue_Billions * 1e9) / Employees, NA),
    Company_Age = Year - Founded,
    Digital_Intensity_Index =
      as.numeric(scale(Mobile_Users / Population * Revenue_Billions)),
    Market_Share_Global = NA_real_,
    Growth_Rate = NA_real_,
    Volatility = runif(n(), 0.1, 0.5),
    Innovation_Score = runif(n(), 1, 100),
    Risk_Score = runif(n(), 1, 10)
  )

# ==== 3. GROWTH + MARKET SHARE ====
company_growth <- processed_data %>%
  arrange(Company, Year) %>%
  group_by(Company) %>%
  mutate(
    Growth_Rate =
      (Revenue_Billions / lag(Revenue_Billions,
                              default = first(Revenue_Billions)) - 1) * 100,
    Revenue_Momentum = Growth_Rate -
      lag(Growth_Rate, default = first(Growth_Rate))
  ) %>%
  ungroup()

market_shares <- company_growth %>%
  group_by(Year) %>%
  mutate(
    Total_Revenue_Year = sum(Revenue_Billions, na.rm = TRUE),
    Market_Share_Global =
      Revenue_Billions / Total_Revenue_Year * 100
  ) %>%
  ungroup()

# ==== 4. AGGREGASI NEGARA ====
country_year <- market_shares %>%
  group_by(Year, Country_Standard) %>%
  summarise(
    Total_Revenue = sum(Revenue_Billions, na.rm = TRUE),
    Mobile_Users  = max(Mobile_Users, na.rm = TRUE),
    Population    = max(Population, na.rm = TRUE),
    N_Companies   = n_distinct(Company),
    Avg_Revenue_per_Company = mean(Revenue_Billions, na.rm = TRUE),
    Total_Employees         = sum(Employees, na.rm = TRUE),
    Avg_Growth_Rate         = mean(Growth_Rate, na.rm = TRUE),
    HHI = sum((Revenue_Billions / Total_Revenue * 100)^2,
              na.rm = TRUE),
    Avg_Innovation_Score = mean(Innovation_Score, na.rm = TRUE),
    Total_Risk_Score     = mean(Risk_Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mobile_per_100 =
      ifelse(Population > 0,
             Mobile_Users / Population * 100, NA_real_),
    Revenue_per_Cap =
      ifelse(Population > 0,
             Total_Revenue / Population, NA_real_),
    Digital_Economy_Index =
      as.numeric(scale(Total_Revenue * Mobile_per_100 * N_Companies)),
    Ecosystem_Health =
      as.numeric(scale(Total_Revenue * Avg_Innovation_Score /
                         (1 + Total_Risk_Score))),
    Country_Tier = case_when(
      Digital_Economy_Index > 1.5 ~ "Digital Leader",
      Digital_Economy_Index > 0   ~ "Digital Advancer",
      Digital_Economy_Index > -1  ~ "Digital Emerging",
      TRUE                        ~ "Digital Laggard"
    )
  )

# ==== 5. AGGREGASI PERUSAHAAN ====
company_year <- market_shares %>%
  group_by(Year, Company, Country_Standard, Industry) %>%
  summarise(
    Revenue_Billions = sum(Revenue_Billions, na.rm = TRUE),
    Employees        = max(Employees, na.rm = TRUE),
    Market_Share_Global = mean(Market_Share_Global, na.rm = TRUE),
    Revenue_per_Employee = mean(Revenue_per_Employee, na.rm = TRUE),
    Growth_Rate = mean(Growth_Rate, na.rm = TRUE),
    Digital_Intensity = mean(Digital_Intensity_Index, na.rm = TRUE),
    Innovation_Score = mean(Innovation_Score, na.rm = TRUE),
    Risk_Score = mean(Risk_Score, na.rm = TRUE),
    Revenue_Momentum = mean(Revenue_Momentum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Company_Performance =
      as.numeric(scale(Revenue_Billions *
                         Growth_Rate *
                         Innovation_Score)),
    Competitive_Position = case_when(
      Company_Performance > 1.5 ~ "Market Leader",
      Company_Performance > 0.5 ~ "Strong Contender",
      Company_Performance > -0.5 ~ "Challenger",
      TRUE ~ "Niche Player"
    )
  )

# ==== 6. TREND: HHI ====
hhi_global <- company_year %>%
  group_by(Year) %>%
  mutate(share = Revenue_Billions / sum(Revenue_Billions,
                                        na.rm = TRUE)) %>%
  summarise(HHI = sum(share^2, na.rm = TRUE),
            .groups = "drop")

hhi_country <- company_year %>%
  group_by(Year, Country_Standard) %>%
  mutate(share = Revenue_Billions / sum(Revenue_Billions,
                                        na.rm = TRUE)) %>%
  summarise(HHI = sum(share^2, na.rm = TRUE),
            .groups = "drop")

# ==== 7. NETWORK SETUP (FIX) ====
all_companies <- unique(company_year$Company)
n_companies <- length(all_companies)

network_nodes <- data.frame(
  id = 1:n_companies,
  label = all_companies,
  group = sample(c("Tech Giant", "Growth", "Stable", "Volatile"),
                 n_companies, replace = TRUE),
  value = runif(n_companies, 5, 20)
)

set.seed(1234)
network_edges <- data.frame(
  from = sample(1:n_companies, 80, replace = TRUE),
  to   = sample(1:n_companies, 80, replace = TRUE),
  value = runif(80, 0.1, 1)
) %>%
  filter(from != to) %>%
  distinct()

# ==== 8. SENTIMENT SETUP ====
sentiment_data <- data.frame(
  Company = all_companies,
  Sentiment_Score = rnorm(n_companies, 0, 1),
  News_Volume = sample(100:10000, n_companies, replace = TRUE),
  Social_Mentions = sample(500:50000, n_companies, replace = TRUE)
)

# ==== 9. LISTS ====
country_list <- sort(unique(country_year$Country_Standard))
year_list <- sort(unique(processed_data$Year))

cat("Data preparation selesai!\n\n")

# ============================================================
#  BAGIAN 2 — UI PREMIUM
# ============================================================

ui <- fluidPage(
  useShinyjs(),
  useWaiter(),
  
  # ==== HEAD & THEME ====
  tags$head(
    tags$title("Digital Ecosystem Dashboard"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700;800;900&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      
      :root {
        --primary: #2563eb;
        --primary-dark: #1d4ed8;
        --primary-light: #60a5fa;
        --secondary: #10b981;
        --accent: #f59e0b;
        --dark: #1f2937;
        --light: #f8fafc;
        --gray: #6b7280;
        --success: #059669;
        --warning: #d97706;
        --danger: #dc2626;
      }
      
      body {
        font-family: 'Inter', 'Poppins', sans-serif;
        background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 50%, #f8fafc 100%);
        background-attachment: fixed;
        color: var(--dark);
        margin: 0;
        padding: 0;
        line-height: 1.6;
        font-weight: 400;
      }
      
      .app-shell {
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 15px;
      }
      
      /* HEADER STYLES */
      .top-bar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin: 20px 0 25px 0;
        padding: 0 10px;
      }
      
      .app-title {
        font-weight: 800;
        font-size: 28px;
        letter-spacing: -0.02em;
        background: linear-gradient(135deg, var(--primary), var(--secondary));
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin-bottom: 4px;
      }
      
      .app-subtitle {
        font-size: 14px;
        color: var(--gray);
        font-weight: 400;
        max-width: 600px;
      }
      
      .pill {
        background: linear-gradient(135deg, var(--primary), var(--secondary));
        color: white;
        padding: 6px 16px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 600;
        box-shadow: 0 4px 12px rgba(37, 99, 235, 0.25);
      }
      
      /* NAVBAR PREMIUM - CENTERED */
      .navbar {
        background: rgba(255, 255, 255, 0.85);
        backdrop-filter: blur(20px);
        border-radius: 16px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
        border: 1px solid rgba(255, 255, 255, 0.2);
        margin-bottom: 25px;
        padding: 0 15px;
      }
      
      /* CENTER NAVBAR FIX */
      .navbar-nav {
        display: flex !important;
        justify-content: center !important;
        width: 100% !important;
        flex-wrap: wrap;
      }
      
      .navbar-header {
        float: none !important;
        display: none !important;
      }
      
      .navbar-default .navbar-nav > li {
        float: none !important;
        display: inline-block !important;
      }
      
      .navbar-default .navbar-collapse {
        text-align: center !important;
        float: none !important;
      }
      
      .navbar > .container {
        display: block !important;
        text-align: center !important;
      }
      
      .navbar-default .navbar-nav > li > a {
        display: inline-block !important;
        float: none !important;
        font-weight: 600 !important;
        color: var(--dark) !important;
        text-transform: uppercase;
        letter-spacing: 0.03em;
        font-size: 13px;
        padding: 15px 20px !important;
        margin: 0 2px;
        border-radius: 12px;
        transition: all 0.3s ease;
      }
      
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > li > a:hover {
        background: linear-gradient(135deg, var(--primary), var(--secondary)) !important;
        color: white !important;
        box-shadow: 0 4px 15px rgba(37, 99, 235, 0.3);
        transform: translateY(-2px);
      }
      
      /* Ensure navbar remains centered on all screens */
      @media (max-width: 768px) {
        .navbar-nav {
          flex-direction: column !important;
        }
        
        .navbar-default .navbar-nav > li {
          display: block !important;
          width: 100% !important;
        }
      }
      
      /* CARD STYLES */
      .card-premium {
        background: rgba(255, 255, 255, 0.9);
        backdrop-filter: blur(15px);
        border-radius: 20px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.08);
        border: 1px solid rgba(255, 255, 255, 0.3);
        padding: 25px;
        margin-bottom: 25px;
        transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
        position: relative;
        overflow: hidden;
      }
      
      .card-premium::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, var(--primary), var(--secondary));
      }
      
      .card-premium:hover {
        transform: translateY(-8px);
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15);
      }
      
      .card-premium h4 {
        color: var(--dark);
        font-weight: 700;
        margin-bottom: 20px;
        font-size: 18px;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .card-premium h4 i {
        color: var(--primary);
      }
      
      /* SIDEBAR STYLES */
      .sidebar-premium {
        background: rgba(255, 255, 255, 0.85);
        backdrop-filter: blur(15px);
        border-radius: 20px;
        box-shadow: 0 8px 30px rgba(0, 0, 0, 0.08);
        border: 1px solid rgba(255, 255, 255, 0.3);
        padding: 25px;
        margin-bottom: 25px;
      }
      
      .sidebar-premium h4 {
        color: var(--dark);
        font-weight: 700;
        margin-bottom: 20px;
        font-size: 16px;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      /* KPI CARD STYLES */
      .kpi-card-premium {
        background: linear-gradient(135deg, #ffffff, #f8fafc);
        border-radius: 16px;
        padding: 20px;
        text-align: center;
        border: 1px solid rgba(255, 255, 255, 0.5);
        box-shadow: 0 6px 20px rgba(0, 0, 0, 0.07);
        transition: all 0.3s ease;
        position: relative;
        overflow: hidden;
      }
      
      .kpi-card-premium::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, var(--primary), var(--secondary));
      }
      
      .kpi-card-premium:hover {
        transform: translateY(-5px);
        box-shadow: 0 12px 25px rgba(0, 0, 0, 0.12);
      }
      
      .kpi-title-premium {
        font-size: 12px;
        color: var(--gray);
        font-weight: 600;
        margin-bottom: 8px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
      }
      
      .kpi-value-premium {
        font-size: 26px;
        font-weight: 800;
        color: var(--dark);
        margin-bottom: 5px;
        background: linear-gradient(135deg, var(--primary), var(--secondary));
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      
      .kpi-sub-premium {
        font-size: 11px;
        color: var(--gray);
        font-weight: 500;
      }
      
      /* BUTTON STYLES */
      .btn-premium {
        background: linear-gradient(135deg, var(--primary), var(--secondary));
        color: white;
        border: none;
        border-radius: 12px;
        padding: 10px 20px;
        font-weight: 600;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(37, 99, 235, 0.3);
      }
      
      .btn-premium:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 20px rgba(37, 99, 235, 0.4);
        color: white;
      }
      
      /* TABLE STYLES */
      .dataTables_wrapper {
        border-radius: 12px;
        overflow: hidden;
      }
      
      table.dataTable {
        border-radius: 12px !important;
        overflow: hidden !important;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.05) !important;
      }
      
      /* LOADING SPINNER */
      .shiny-spinner-output-container {
        display: flex;
        justify-content: center;
        align-items: center;
        min-height: 200px;
      }
      
      /* RESPONSIVE DESIGN */
      @media (max-width: 768px) {
        .top-bar {
          flex-direction: column;
          text-align: center;
          gap: 15px;
        }
        
        .app-title {
          font-size: 24px;
        }
        
        .card-premium, .sidebar-premium {
          padding: 20px;
        }
      }
      
      /* GLOW EFFECT */
      .glow-effect {
        box-shadow: 0 0 20px rgba(37, 99, 235, 0.15);
      }
      
      .glow-effect:hover {
        box-shadow: 0 0 30px rgba(37, 99, 235, 0.25);
      }
      
      /* GRADIENT TEXT */
      .gradient-text {
        background: linear-gradient(135deg, var(--primary), var(--secondary));
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        font-weight: 700;
      }
      
      /* BADGE STYLES */
      .badge-premium {
        background: linear-gradient(135deg, var(--primary), var(--secondary));
        color: white;
        padding: 4px 12px;
        border-radius: 20px;
        font-size: 11px;
        font-weight: 700;
      }
      
      /* TEAM CARD STYLES */
      .team-card-premium {
        text-align: center;
        padding: 25px 15px;
        background: rgba(255, 255, 255, 0.7);
        border-radius: 16px;
        box-shadow: 0 6px 20px rgba(0, 0, 0, 0.08);
        transition: all 0.3s ease;
        height: 100%;
        border: 1px solid rgba(255, 255, 255, 0.3);
      }
      
      .team-card-premium:hover {
        transform: translateY(-8px);
        box-shadow: 0 12px 30px rgba(0, 0, 0, 0.15);
      }
      
      .team-circle-premium {
        width: 70px;
        height: 70px;
        border-radius: 50%;
        background: linear-gradient(135deg, var(--primary), var(--secondary));
        color: white;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        margin: 0 auto 15px;
        font-size: 20px;
        box-shadow: 0 4px 15px rgba(37, 99, 235, 0.3);
      }
      
      .team-name-premium {
        font-weight: 700;
        margin-bottom: 8px;
        color: var(--dark);
        font-size: 16px;
      }
      
      .team-role-premium {
        font-size: 13px;
        color: var(--primary);
        margin-bottom: 12px;
        font-weight: 600;
      }
      
      /* BAR CHART LABEL STYLES */
      .bar-label {
        font-weight: bold;
        font-size: 12px;
        fill: black;
      }
    "))
  ),
  
  # ==== APP CONTAINER ====
  div(
    class = "app-shell",
    
    # ==== HEADER ====
    div(
      class = "top-bar",
      div(
        div(class = "app-title", "Digital Ecosystem Dashboard"),
        div(class = "app-subtitle",
            "Advanced analytics platform for global digital ecosystem monitoring, competitive intelligence, and strategic insights.")
      ),
      div(HTML("<span class='pill'><i class='fas fa-rocket'></i> Enterprise Analytics Platform</span>"))
    ),
    
    # ==== NAVBAR PREMIUM - CENTERED ====
    navbarPage(
      title = NULL,
      id = "main_menu",
      collapsible = TRUE,
      windowTitle = "Digital Ecosystem Dashboard",
      
      # ============================================================
      #  TAB 1 — BERANDA (STABIL & CLEAN)
      # ============================================================
      tabPanel(
        title = HTML("<i class='fas fa-home'></i> Beranda"),
        value = "beranda",
        br(),
        uiOutput("ui_beranda")
      ),
      
      # ============================================================
      #  TAB 2 — DASHBOARD UTAMA
      # ============================================================
      tabPanel(
        title = HTML("<i class='fas fa-tachometer-alt'></i> Dashboard Utama"),
        value = "dashboard",
        br(),
        uiOutput("ui_dashboard")
      ),
      
      # ============================================================
      #  TAB 3 — ANALISIS STATISTIK
      # ============================================================
      tabPanel(
        title = HTML("<i class='fas fa-chart-bar'></i> Analisis Statistik"),
        value = "statistik",
        br(),
        uiOutput("ui_statistik")
      ),
      
      # ============================================================
      #  TAB 4 — TREN WAKTU
      # ============================================================
      tabPanel(
        title = HTML("<i class='fas fa-chart-line'></i> Tren Waktu"),
        value = "tren",
        br(),
        uiOutput("ui_tren")
      ),
      
      # ============================================================
      #  TAB 5 — ADVANCED ANALYTICS
      # ============================================================
      tabPanel(
        title = HTML("<i class='fas fa-brain'></i> Advanced Analytics"),
        value = "advanced",
        br(),
        tabsetPanel(
          id = "adv_panel",
          type = "pills",
          
          tabPanel(
            title = HTML("<i class='fas fa-robot'></i> Machine Learning"),
            br(),
            uiOutput("ui_ml")
          ),
          
          tabPanel(
            title = HTML("<i class='fas fa-project-diagram'></i> Network Analysis"),
            br(),
            uiOutput("ui_network")
          ),
          
          tabPanel(
            title = HTML("<i class='fas fa-smile'></i> Sentiment Intelligence"),
            br(),
            uiOutput("ui_sentiment")
          )
        )
      ),
      
      # ============================================================
      #  TAB 6 — INNOVATION LAB
      # ============================================================
      tabPanel(
        title = HTML("<i class='fas fa-flask'></i> Innovation Lab"),
        value = "innovation",
        br(),
        uiOutput("ui_innovation")
      ),
      
      # ============================================================
      #  TAB 7 — COMPETITIVE INTELLIGENCE
      # ============================================================
      tabPanel(
        title = HTML("<i class='fas fa-chess-knight'></i> Competitive Intelligence"),
        value = "competitive",
        br(),
        uiOutput("ui_competitive")
      ),
      
      # ============================================================
      #  TAB 8 — ABOUT TEAM
      # ============================================================
      tabPanel(
        title = HTML("<i class='fas fa-users'></i> About Team"),
        value = "team",
        br(),
        uiOutput("ui_team")
      )
    )
  )
)

# ============================================================
#  BAGIAN 3 — SERVER OPTIMIZED
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # 3.1 — REACTIVE DATA
  # ----------------------------------------------------------
  
  # Data reactive untuk berbagai tab
  react_year_data <- reactive({
    req(input$home_year)
    country_year %>% filter(Year == input$home_year)
  })
  
  react_country_focus <- reactive({
    req(input$home_year, input$home_country)
    country_year %>%
      filter(Year == input$home_year, Country_Standard == input$home_country)
  })
  
  react_company_year <- reactive({
    req(input$home_year)
    company_year %>% filter(Year == input$home_year)
  })
  
  react_dash_year <- reactive({
    req(input$dash_year)
    country_year %>% filter(Year == input$dash_year)
  })
  
  react_dash_company <- reactive({
    req(input$dash_year)
    company_year %>% filter(Year == input$dash_year)
  })
  
  react_dash_country_focus <- reactive({
    req(input$dash_year, input$dash_country)
    country_year %>% filter(Year == input$dash_year,
                            Country_Standard == input$dash_country)
  })
  
  react_stat_data <- reactive({
    d <- company_year %>% filter(Year == input$stat_year)
    if (input$stat_country != "Semua Negara") {
      d <- d %>% filter(Country_Standard == input$stat_country)
    }
    
    if (input$stat_log_rev) {
      d$Revenue_Billions <- log1p(d$Revenue_Billions)
    }
    d
  })
  
  react_trend_data <- reactive({
    if (input$trend_country == "Global") {
      country_year
    } else {
      country_year %>% filter(Country_Standard == input$trend_country)
    }
  })
  
  react_network_filtered <- reactive({
    threshold <- input$network_threshold
    network_edges %>% filter(value >= threshold)
  })
  
  react_sentiment <- reactive({
    sentiment_data
  })
  
  react_competitive_data <- reactive({
    company_year %>%
      filter(Company == input$comp_company)
  })
  
  # ----------------------------------------------------------
  # 3.2 — UI OUTPUTS
  # ----------------------------------------------------------
  
  # 3.2.1 — BERANDA (STABIL TANPA ANIMASI BERLEBIHAN)
  output$ui_beranda <- renderUI({
    fluidRow(
      column(
        4,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-filter'></i> Filter Global")),
            selectInput("home_year", "Pilih Tahun:",
                        choices = year_list, selected = max(year_list),
                        width = "100%"),
            selectInput("home_country", "Negara Fokus:",
                        choices = country_list, selected = "United States",
                        width = "100%"),
            hr(),
            tags$small(HTML("<i class='fas fa-info-circle'></i> Beranda memberikan overview global mengenai revenue dan populasi digital."))
        ),
        
        # Quick Stats Card
        div(class = "card-premium",
            h4(HTML("<i class='fas fa-bolt'></i> Quick Stats")),
            uiOutput("quick_stats")
        )
      ),
      column(
        8,
        div(class = "card-premium",
            h4(HTML("<i class='fas fa-globe-americas'></i> Ringkasan Ekosistem Digital")),
            
            # KPI Row
            fluidRow(
              column(4, uiOutput("home_kpi_revenue_box")),
              column(4, uiOutput("home_kpi_mobile_box")),
              column(4, uiOutput("home_kpi_mobile100_box"))
            ),
            
            br(),
            
            # Charts Row
            fluidRow(
              column(6,
                     h5(HTML("<i class='fas fa-trophy'></i> Top Perusahaan")),
                     withSpinner(DTOutput("home_top_companies"), type = 4, color = "#2563eb")
              ),
              column(6,
                     h5(HTML("<i class='fas fa-flag'></i> Top Negara Mobile Users")),
                     withSpinner(plotOutput("home_top_countries", height = 300), type = 4, color = "#2563eb")
              )
            )
        )
      )
    )
  })
  
  # 3.2.2 — DASHBOARD UTAMA
  output$ui_dashboard <- renderUI({
    fluidRow(
      column(
        3,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-sliders-h'></i> Filter Dashboard")),
            selectInput("dash_year", "Pilih Tahun:",
                        choices = year_list, selected = max(year_list),
                        width = "100%"),
            selectInput("dash_country", "Drilldown Negara:",
                        choices = country_list, selected = "United States",
                        width = "100%"),
            hr(),
            downloadButton("dl_country_table", "Download Data Negara",
                           class = "btn-premium w-100 mb-2"),
            downloadButton("dl_company_table", "Download Data Perusahaan",
                           class = "btn-premium w-100")
        )
      ),
      column(
        9,
        div(class = "card-premium",
            tabsetPanel(
              type = "pills",
              
              tabPanel(
                title = HTML("<i class='fas fa-globe'></i> Global Overview"),
                br(),
                fluidRow(
                  column(4, uiOutput("dash_kpi_rev_box")),
                  column(4, uiOutput("dash_kpi_mob_box")),
                  column(4, uiOutput("dash_kpi_comp_box"))
                ),
                br(),
                fluidRow(
                  column(6, 
                         h5(HTML("<i class='fas fa-mobile-alt'></i> Top Mobile Countries")),
                         withSpinner(plotOutput("dash_top_mobile_countries", height = 300), type = 4)
                  ),
                  column(6,
                         h5(HTML("<i class='fas fa-dollar-sign'></i> Top Revenue Countries")),
                         withSpinner(plotOutput("dash_top_revenue_countries", height = 300), type = 4)
                  )
                )
              ),
              
              tabPanel(
                title = HTML("<i class='fas fa-building'></i> Negara & Perusahaan"),
                br(),
                h5(HTML("<i class='fas fa-flag'></i> Ringkasan Negara")),
                withSpinner(DTOutput("dash_country_table"), type = 4),
                br(),
                h5(HTML("<i class='fas fa-chart-bar'></i> Top Perusahaan (Revenue)")),
                withSpinner(plotOutput("dash_top_companies", height = 300), type = 4),
                br(),
                h5(HTML("<i class='fas fa-table'></i> Data Perusahaan")),
                withSpinner(DTOutput("dash_company_table"), type = 4)
              ),
              
              tabPanel(
                title = HTML("<i class='fas fa-search'></i> Country Drilldown"),
                br(),
                h4(textOutput("dash_country_title")),
                uiOutput("dash_country_summary"),
                br(),
                h5(HTML("<i class='fas fa-building'></i> Perusahaan di Negara Ini")),
                withSpinner(DTOutput("dash_country_companies"), type = 4)
              )
            )
        )
      )
    )
  })
  
  # 3.2.3 — ANALISIS STATISTIK
  output$ui_statistik <- renderUI({
    fluidRow(
      column(
        3,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-cog'></i> Pengaturan Analisis")),
            selectInput("stat_year", "Pilih Tahun:",
                        choices = year_list, selected = max(year_list)),
            selectInput("stat_country", "Filter Negara:",
                        choices = c("Semua Negara", country_list),
                        selected = "Semua Negara"),
            checkboxInput("stat_log_rev", "Log-transform Revenue", FALSE),
            hr(),
            tags$small(HTML("<i class='fas fa-info-circle'></i> Analisis statistik perusahaan tahun tertentu."))
        )
      ),
      column(
        9,
        div(class = "card-premium",
            h5(HTML("<i class='fas fa-scatter-chart'></i> Scatterplot Revenue vs Employees")),
            withSpinner(plotOutput("stat_scatter", height = 300), type = 4),
            br(),
            h5(HTML("<i class='fas fa-project-diagram'></i> Correlation Matrix")),
            withSpinner(plotOutput("stat_corr", height = 300), type = 4)
        )
      )
    )
  })
  
  # 3.2.4 — TREN WAKTU
  output$ui_tren <- renderUI({
    fluidRow(
      column(
        3,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-chart-line'></i> Pengaturan Tren")),
            selectInput("trend_country", "Negara:",
                        choices = c("Global", country_list),
                        selected = "Global"),
            selectInput("trend_company", "Perusahaan (opsional):",
                        choices = c("Tidak dipilih",
                                    sort(unique(company_year$Company))),
                        selected = "Tidak dipilih")
        )
      ),
      column(
        9,
        div(class = "card-premium",
            h5(HTML("<i class='fas fa-dollar-sign'></i> Trend Revenue")),
            withSpinner(plotOutput("trend_revenue", height = 300), type = 4),
            br(),
            h5(HTML("<i class='fas fa-chart-area'></i> Konsentrasi Pasar (HHI)")),
            withSpinner(plotOutput("trend_hhi", height = 300), type = 4)
        )
      )
    )
  })
  
  # 3.2.5 — MACHINE LEARNING
  output$ui_ml <- renderUI({
    fluidRow(
      column(
        3,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-robot'></i> ML Configuration")),
            selectInput("ml_algorithm", "Algorithm:",
                        choices = c("Random Forest", "XGBoost", "Neural Network", "Ensemble")),
            sliderInput("ml_training", "Training Split:",
                        min = 0.5, max = 0.9, value = 0.7),
            selectInput("ml_target", "Target Variable:",
                        choices = c("Revenue_Growth", "Market_Share", "Digital_Intensity")),
            actionButton("ml_train", "Train Model", class = "btn-premium w-100")
        )
      ),
      column(
        9,
        div(class = "card-premium",
            h5(HTML("<i class='fas fa-tachometer-alt'></i> Model Performance Metrics")),
            withSpinner(plotlyOutput("ml_performance"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-chart-bar'></i> Feature Importance")),
            withSpinner(plotOutput("ml_features"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-lightbulb'></i> Prediction Insights")),
            withSpinner(DTOutput("ml_predictions"), type = 4)
        )
      )
    )
  })
  
  # 3.2.6 — NETWORK ANALYSIS
  output$ui_network <- renderUI({
    fluidRow(
      column(
        3,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-project-diagram'></i> Network Settings")),
            sliderInput("network_threshold", "Edge Threshold:",
                        min = 0.1, max = 1, value = 0.5),
            selectInput("network_centrality", "Centrality:",
                        choices = c("Degree", "Betweenness", "Eigenvector", "Closeness")),
            checkboxInput("network_animate", "Animated Layout", TRUE),
            actionButton("network_analyze", "Analyze Network",
                         class = "btn-premium w-100")
        )
      ),
      column(
        9,
        div(class = "card-premium",
            h5(HTML("<i class='fas fa-network-wired'></i> Digital Ecosystem Network")),
            withSpinner(visNetworkOutput("network_plot", height = "500px"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-chart-bar'></i> Network Metrics")),
            withSpinner(DTOutput("network_metrics"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-users'></i> Community Detection")),
            withSpinner(plotOutput("community_analysis", height = 300), type = 4)
        )
      )
    )
  })
  
  # 3.2.7 — SENTIMENT INTELLIGENCE
  output$ui_sentiment <- renderUI({
    fluidRow(
      column(
        3,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-smile'></i> Sentiment Settings")),
            selectInput("sentiment_source", "Sumber:",
                        choices = c("News", "Social Media", "All")),
            sliderTextInput("sentiment_period", "Periode:",
                            choices = 2018:2023,
                            selected = c(2021, 2023)),
            checkboxInput("sentiment_trend", "Tampilkan Trend", TRUE),
            actionButton("sentiment_update", "Update Analysis",
                         class = "btn-premium w-100")
        )
      ),
      column(
        9,
        div(class = "card-premium",
            h5(HTML("<i class='fas fa-cloud'></i> Word Cloud")),
            withSpinner(wordcloud2Output("sentiment_wordcloud"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-chart-line'></i> Sentiment Timeline")),
            withSpinner(plotlyOutput("sentiment_timeline"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-project-diagram'></i> Correlation Matrix")),
            withSpinner(plotlyOutput("sentiment_correlation"), type = 4)
        )
      )
    )
  })
  
  # 3.2.8 — INNOVATION LAB
  output$ui_innovation <- renderUI({
    fluidRow(
      column(
        4,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-flask'></i> Simulation Parameters")),
            numericInput("sim_years", "Forecast Years:", 5, 1, 10),
            sliderInput("sim_growth", "Market Growth:", -10, 20, 8),
            sliderInput("sim_volatility", "Volatility:", 0.1, 2, 0.8),
            selectInput("sim_scenario", "Scenario:",
                        choices = c("Base Case", "Optimistic", "Pessimistic", "Disruption")),
            actionButton("sim_run", "Run Simulation",
                         class = "btn-premium w-100")
        )
      ),
      column(
        8,
        div(class = "card-premium",
            h5(HTML("<i class='fas fa-chart-line'></i> Monte Carlo Simulation")),
            withSpinner(plotlyOutput("monte_carlo"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-lightbulb'></i> Strategic Insights")),
            withSpinner(verbatimTextOutput("strategic_insights"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-chart-area'></i> Dynamic Scenario Analysis")),
            withSpinner(plotlyOutput("scenario_analysis"), type = 4)
        )
      )
    )
  })
  
  # 3.2.9 — COMPETITIVE INTELLIGENCE
  output$ui_competitive <- renderUI({
    fluidRow(
      column(
        3,
        div(class = "sidebar-premium",
            h4(HTML("<i class='fas fa-chess-knight'></i> Competitive Settings")),
            selectInput("comp_company", "Company:", choices = unique(company_year$Company)),
            pickerInput("comp_benchmarks", "Benchmarks:",
                        choices = unique(company_year$Company),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE)),
            sliderTextInput("comp_years", "Period:",
                            choices = sort(unique(company_year$Year)),
                            selected = range(company_year$Year)),
            actionButton("comp_analyze", "Analyze Competitors",
                         class = "btn-premium w-100")
        )
      ),
      column(
        9,
        div(class = "card-premium",
            h5(HTML("<i class='fas fa-crosshairs'></i> Competitive Positioning Matrix")),
            withSpinner(plotlyOutput("competitive_matrix"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-share-alt'></i> Market Share Dynamics")),
            withSpinner(plotlyOutput("market_share_flow"), type = 4),
            br(),
            h5(HTML("<i class='fas fa-clipboard-list'></i> SWOT Analysis")),
            withSpinner(DTOutput("swot_analysis"), type = 4)
        )
      )
    )
  })
  
  # 3.2.10 — ABOUT TEAM
  output$ui_team <- renderUI({
    fluidRow(
      column(
        12,
        div(class = "card-premium",
            h4(HTML("<i class='fas fa-users'></i> Meet The Team")),
            br(),
            fluidRow(
              column(3, 
                     div(class = "team-card-premium",
                         div(class = "team-circle-premium", "TL"),
                         div(class = "team-name-premium", "Nama Anda"),
                         div(class = "team-role-premium", "Project Lead & Data Scientist"),
                         tags$small("Integrasi data, desain analisis, dan implementasi dashboard.")
                     )
              ),
              column(3, 
                     div(class = "team-card-premium",
                         div(class = "team-circle-premium", "DA"),
                         div(class = "team-name-premium", "Anggota 2"),
                         div(class = "team-role-premium", "Data Analyst"),
                         tags$small("Cleaning data, agregasi negara & perusahaan, validasi data.")
                     )
              ),
              column(3, 
                     div(class = "team-card-premium",
                         div(class = "team-circle-premium", "UI"),
                         div(class = "team-name-premium", "Anggota 3"),
                         div(class = "team-role-premium", "UI/UX Designer"),
                         tags$small("Layout dashboard, design system, user experience.")
                     )
              ),
              column(3, 
                     div(class = "team-card-premium",
                         div(class = "team-circle-premium", "RS"),
                         div(class = "team-name-premium", "Anggota 4"),
                         div(class = "team-role-premium", "Research Specialist"),
                         tags$small("Narasi statistik, insight bisnis, competitive analysis.")
                     )
              )
            )
        )
      )
    )
  })
  
  # ----------------------------------------------------------
  # 3.3 — OUTPUT RENDER (BERANDA)
  # ----------------------------------------------------------
  
  # KPI Boxes untuk Beranda
  output$home_kpi_revenue_box <- renderUI({
    div(class = "kpi-card-premium",
        div(class = "kpi-title-premium", "Total Revenue Teknologi"),
        div(class = "kpi-value-premium", textOutput("home_kpi_revenue")),
        div(class = "kpi-sub-premium", "Akumulasi global")
    )
  })
  
  output$home_kpi_mobile_box <- renderUI({
    div(class = "kpi-card-premium",
        div(class = "kpi-title-premium", "Total Mobile Users"),
        div(class = "kpi-value-premium", textOutput("home_kpi_mobile")),
        div(class = "kpi-sub-premium", "Negara fokus")
    )
  })
  
  output$home_kpi_mobile100_box <- renderUI({
    div(class = "kpi-card-premium",
        div(class = "kpi-title-premium", "Mobile per 100 Penduduk"),
        div(class = "kpi-value-premium", textOutput("home_kpi_mobile100")),
        div(class = "kpi-sub-premium", "Penetrasi seluler")
    )
  })
  
  output$quick_stats <- renderUI({
    data <- react_year_data()
    tags$div(
      tags$p(HTML("<i class='fas fa-layer-group'></i> <strong>Total Negara:</strong> ", nrow(data))),
      tags$p(HTML("<i class='fas fa-industry'></i> <strong>Rata-rata Perusahaan/Negara:</strong> ", round(mean(data$N_Companies, na.rm = TRUE), 1))),
      tags$p(HTML("<i class='fas fa-chart-line'></i> <strong>Pertumbuhan Rata-rata:</strong> ", round(mean(data$Avg_Growth_Rate, na.rm = TRUE), 1), "%")),
      tags$p(HTML("<i class='fas fa-heartbeat'></i> <strong>Ekosistem Terbaik:</strong> ", data$Country_Standard[which.max(data$Ecosystem_Health)]))
    )
  })
  
  output$home_kpi_revenue <- renderText({
    data <- react_year_data()
    total <- sum(data$Total_Revenue, na.rm = TRUE)
    paste0("$", round(total, 1), "B")
  })
  
  output$home_kpi_mobile <- renderText({
    d <- react_country_focus()
    if (nrow(d) == 0) return("0")
    format(round(d$Mobile_Users[1], 0), big.mark = ",")
  })
  
  output$home_kpi_mobile100 <- renderText({
    d <- react_country_focus()
    if (nrow(d) == 0) return("0")
    paste0(round(d$Mobile_per_100[1], 1), "%")
  })
  
  output$home_top_companies <- renderDT({
    d <- react_company_year() %>%
      arrange(desc(Revenue_Billions)) %>%
      select(Company, Revenue_Billions, Growth_Rate, Market_Share_Global) %>%
      head(10) %>%
      mutate(Revenue_Billions = round(Revenue_Billions, 1),
             Growth_Rate = round(Growth_Rate, 1),
             Market_Share_Global = round(Market_Share_Global, 2))
    
    datatable(d, 
              options = list(
                pageLength = 10,
                dom = 't',
                scrollX = TRUE
              ),
              rownames = FALSE) %>%
      formatStyle(names(d), fontWeight = '500')
  })
  
  output$home_top_countries <- renderPlot({
    d <- react_year_data() %>%
      arrange(desc(Mobile_Users)) %>%
      head(10)
    
    ggplot(d, aes(x = reorder(Country_Standard, Mobile_Users),
                  y = Mobile_Users/1e6, fill = Country_Standard)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = round(Mobile_Users/1e6, 1)), 
                hjust = -0.2, size = 3.5, color = "black", fontface = "bold") +
      coord_flip() +
      scale_fill_viridis_d(option = "plasma", direction = -1) +
      labs(x = "", y = "Mobile Users (Juta)") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.background = element_rect(fill = "white", color = NA)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) # Memberi ruang untuk label
  })
  
  # ----------------------------------------------------------
  # 3.4 — OUTPUT RENDER (DASHBOARD UTAMA)
  # ----------------------------------------------------------
  
  output$dash_kpi_rev_box <- renderUI({
    div(class = "kpi-card-premium",
        div(class = "kpi-title-premium", "Total Revenue Global"),
        div(class = "kpi-value-premium", textOutput("dash_kpi_rev")),
        div(class = "kpi-sub-premium", "Tahun dipilih")
    )
  })
  
  output$dash_kpi_mob_box <- renderUI({
    div(class = "kpi-card-premium",
        div(class = "kpi-title-premium", "Total Mobile Users"),
        div(class = "kpi-value-premium", textOutput("dash_kpi_mob")),
        div(class = "kpi-sub-premium", "Akumulasi global")
    )
  })
  
  output$dash_kpi_comp_box <- renderUI({
    div(class = "kpi-card-premium",
        div(class = "kpi-title-premium", "Jumlah Perusahaan"),
        div(class = "kpi-value-premium", textOutput("dash_kpi_comp")),
        div(class = "kpi-sub-premium", "Dalam dataset")
    )
  })
  
  output$dash_kpi_rev <- renderText({
    d <- react_dash_year()
    paste0("$", round(sum(d$Total_Revenue), 1), "B")
  })
  
  output$dash_kpi_mob <- renderText({
    d <- react_dash_year()
    format(sum(d$Mobile_Users, na.rm = TRUE), big.mark = ",")
  })
  
  output$dash_kpi_comp <- renderText({
    d <- react_dash_company()
    length(unique(d$Company))
  })
  
  output$dash_top_mobile_countries <- renderPlot({
    d <- react_dash_year() %>%
      arrange(desc(Mobile_Users)) %>% 
      head(10)
    
    ggplot(d, aes(x = reorder(Country_Standard, Mobile_Users),
                  y = Mobile_Users/1e6, fill = Country_Standard)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = round(Mobile_Users/1e6, 1)), 
                hjust = -0.2, size = 3.5, color = "black", fontface = "bold") +
      coord_flip() +
      scale_fill_viridis_d(option = "viridis", direction = -1) +
      labs(x = "", y = "Mobile Users (Juta)") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$dash_top_revenue_countries <- renderPlot({
    d <- react_dash_year() %>%
      arrange(desc(Total_Revenue)) %>% 
      head(10)
    
    ggplot(d, aes(x = reorder(Country_Standard, Total_Revenue),
                  y = Total_Revenue, fill = Country_Standard)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = round(Total_Revenue, 1)), 
                hjust = -0.2, size = 3.5, color = "black", fontface = "bold") +
      coord_flip() +
      scale_fill_viridis_d(option = "magma", direction = -1) +
      labs(x = "", y = "Total Revenue (Billion $)") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$dash_country_table <- renderDT({
    d <- react_dash_year() %>%
      select(Country_Standard, Total_Revenue, Mobile_Users, N_Companies, Avg_Growth_Rate) %>%
      mutate(Total_Revenue = round(Total_Revenue, 1),
             Mobile_Users = round(Mobile_Users/1e6, 1),
             Avg_Growth_Rate = round(Avg_Growth_Rate, 1))
    
    datatable(d, 
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'ltipr'
              ),
              colnames = c('Negara', 'Revenue (B)', 'Mobile Users (Juta)', 'Jumlah Perusahaan', 'Pertumbuhan (%)'),
              rownames = FALSE) %>%
      formatStyle(names(d), fontWeight = '500')
  })
  
  output$dash_company_table <- renderDT({
    d <- react_dash_company() %>%
      select(Company, Country_Standard, Revenue_Billions, Growth_Rate, Innovation_Score) %>%
      mutate(Revenue_Billions = round(Revenue_Billions, 1),
             Growth_Rate = round(Growth_Rate, 1),
             Innovation_Score = round(Innovation_Score, 1))
    
    datatable(d, 
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'ltipr'
              ),
              rownames = FALSE) %>%
      formatStyle(names(d), fontWeight = '500')
  })
  
  output$dash_top_companies <- renderPlot({
    d <- react_dash_company() %>%
      arrange(desc(Revenue_Billions)) %>% 
      head(10)
    
    ggplot(d, aes(x = reorder(Company, Revenue_Billions),
                  y = Revenue_Billions, fill = Company)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = round(Revenue_Billions, 1)), 
                hjust = -0.2, size = 3.5, color = "black", fontface = "bold") +
      coord_flip() +
      scale_fill_viridis_d(option = "inferno", direction = -1) +
      labs(x = "", y = "Revenue (Billion $)") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$dash_country_title <- renderText({
    paste("Ringkasan —", input$dash_country, "(", input$dash_year, ")")
  })
  
  output$dash_country_summary <- renderUI({
    d <- react_dash_country_focus()
    if (nrow(d) == 0) {
      return(tags$div(class = "alert alert-warning", 
                      "Tidak ada data untuk kombinasi tahun & negara ini."))
    }
    
    d1 <- d[1, ]
    
    cards <- list(
      list(title = "Total Revenue", value = paste0("$", round(d1$Total_Revenue, 1), "B"), icon = "dollar-sign"),
      list(title = "Mobile Users", value = format(round(d1$Mobile_Users/1e6, 1), big.mark=","), icon = "users"),
      list(title = "Mobile Penetration", value = paste0(round(d1$Mobile_per_100, 1), "%"), icon = "percent"),
      list(title = "Jumlah Perusahaan", value = d1$N_Companies, icon = "building"),
      list(title = "Innovation Score", value = round(d1$Avg_Innovation_Score, 1), icon = "lightbulb"),
      list(title = "Risk Score", value = round(d1$Total_Risk_Score, 2), icon = "exclamation-triangle")
    )
    
    fluidRow(
      style = "margin-bottom: 20px;",
      lapply(cards, function(x){
        column(2,
               div(class = "kpi-card-premium",
                   div(class = "kpi-title-premium", x$title),
                   div(HTML(paste0("<i class='fas fa-", x$icon, "' style='font-size: 12px; color: #6b7280; margin-right: 5px;'></i>")),
                       class = "kpi-value-premium", x$value)
               )
        )
      })
    )
  })
  
  output$dash_country_companies <- renderDT({
    d <- react_dash_company() %>% 
      filter(Country_Standard == input$dash_country) %>%
      select(Company, Industry, Revenue_Billions, Growth_Rate, Innovation_Score) %>%
      mutate(Revenue_Billions = round(Revenue_Billions, 1),
             Growth_Rate = round(Growth_Rate, 1),
             Innovation_Score = round(Innovation_Score, 1))
    
    datatable(d, 
              options = list(
                pageLength = 8,
                scrollX = TRUE,
                dom = 'ltipr'
              ),
              colnames = c('Perusahaan', 'Industri', 'Revenue (B)', 'Pertumbuhan (%)', 'Skor Inovasi'),
              rownames = FALSE) %>%
      formatStyle(names(d), fontWeight = '500')
  })
  
  # ----------------------------------------------------------
  # 3.5 — OUTPUT RENDER (ANALISIS STATISTIK)
  # ----------------------------------------------------------
  
  output$stat_scatter <- renderPlot({
    d <- react_stat_data()
    ggplot(d, aes(Revenue_Billions, Employees, color = Industry, size = Revenue_Billions)) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_d(option = "plasma") +
      labs(x = "Revenue (Billions $)", y = "Employees") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "bottom")
  })
  
  output$stat_corr <- renderPlot({
    d <- react_stat_data() %>%
      select(Revenue_Billions, Employees, Growth_Rate,
             Digital_Intensity, Innovation_Score, Risk_Score) %>%
      na.omit()
    
    cor_matrix <- cor(d, use = "pairwise.complete.obs")
    
    corrplot(cor_matrix, 
             method = "color", 
             type = "upper",
             tl.cex = 0.8,
             tl.col = "black",
             addCoef.col = "black",
             number.cex = 0.7,
             diag = FALSE)
  })
  
  # ----------------------------------------------------------
  # 3.6 — OUTPUT RENDER (TREN WAKTU)
  # ----------------------------------------------------------
  
  output$trend_revenue <- renderPlot({
    d <- react_trend_data()
    
    p <- ggplot(d, aes(Year, Total_Revenue, color = Country_Standard,
                       group = Country_Standard)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 2, alpha = 0.8) +
      scale_color_viridis_d(option = "viridis") +
      labs(x = "Tahun", y = "Total Revenue (Billion $)", color = "Negara") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "bottom")
    
    if (input$trend_country == "Global") {
      p <- p + guides(color = guide_legend(nrow = 3))
    }
    
    p
  })
  
  output$trend_hhi <- renderPlot({
    d <- if (input$trend_country == "Global") {
      hhi_global
    } else {
      hhi_country %>% filter(Country_Standard == input$trend_country)
    }
    
    ggplot(d, aes(Year, HHI)) +
      geom_line(color = "#2563eb", size = 1.5, alpha = 0.8) +
      geom_point(color = "#2563eb", size = 3) +
      geom_area(fill = "#2563eb", alpha = 0.1) +
      labs(x = "Tahun", y = "HHI (Herfindahl Index)") +
      theme_minimal(base_family = "Inter")
  })
  
  # ============================================================
  # 4.0 — ADVANCED ANALYTICS — MACHINE LEARNING
  # ============================================================
  
  observeEvent(input$ml_train, {
    showNotification("Training machine learning model...", type = "message", duration = 3)
    
    # Simulasi training process
    Sys.sleep(2)
    
    output$ml_performance <- renderPlotly({
      metrics <- data.frame(
        Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "AUC-ROC"),
        Score = c(0.89, 0.85, 0.82, 0.83, 0.91)
      )
      
      plot_ly(metrics, x = ~Metric, y = ~Score, type = 'bar',
              marker = list(color = c('#2563eb', '#10b981', '#f59e0b', '#ef4444', '#8b5cf6'))) %>%
        layout(title = "Model Performance Metrics",
               yaxis = list(title = "Score", range = c(0, 1)))
    })
    
    output$ml_features <- renderPlot({
      features <- data.frame(
        Feature = c("Revenue", "Employees", "Innovation Score", "Growth Rate", "Digital Intensity"),
        Importance = c(0.25, 0.18, 0.22, 0.20, 0.15)
      )
      
      ggplot(features, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "#2563eb", alpha = 0.8) +
        coord_flip() +
        labs(x = "", y = "Feature Importance") +
        theme_minimal(base_family = "Inter")
    })
    
    output$ml_predictions <- renderDT({
      set.seed(123)
      predictions <- data.frame(
        Company = sample(unique(company_year$Company), 10),
        Actual_Growth = runif(10, -5, 20),
        Predicted_Growth = runif(10, -3, 18),
        Confidence = runif(10, 0.7, 0.95)
      ) %>%
        mutate(Error = abs(Actual_Growth - Predicted_Growth))
      
      datatable(predictions, options = list(pageLength = 5))
    })
  })
  
  # ============================================================
  # 5.0 — NETWORK ANALYSIS
  # ============================================================
  
  observeEvent(input$network_analyze, {
    showNotification("Analyzing network structure...", type = "message", duration = 2)
  })
  
  output$network_plot <- renderVisNetwork({
    edges <- react_network_filtered()
    
    visNetwork(network_nodes, edges) %>%
      visNodes(shape = "dot", 
               scaling = list(min = 10, max = 30),
               shadow = TRUE) %>%
      visEdges(arrows = "to", 
               smooth = list(enabled = TRUE, type = "dynamic"),
               shadow = TRUE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                 nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE,
                     keyboard = TRUE) %>%
      visLayout(randomSeed = 123)
  })
  
  output$network_metrics <- renderDT({
    set.seed(123)
    metrics <- data.frame(
      Company = network_nodes$label,
      Degree = sample(1:10, nrow(network_nodes), replace = TRUE),
      Betweenness = round(runif(nrow(network_nodes), 0, 1), 3),
      Closeness = round(runif(nrow(network_nodes), 0, 1), 3)
    )
    
    datatable(metrics, options = list(pageLength = 8))
  })
  
  output$community_analysis <- renderPlot({
    set.seed(123)
    communities <- data.frame(
      Community = factor(rep(1:4, each = 5)),
      Companies = sample(network_nodes$label, 20)
    )
    
    ggplot(communities, aes(x = Community, fill = Community)) +
      geom_bar(alpha = 0.8) +
      scale_fill_viridis_d(option = "plasma") +
      labs(x = "Community", y = "Number of Companies") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "none")
  })
  
  # ============================================================
  # 6.0 — SENTIMENT INTELLIGENCE
  # ============================================================
  
  observeEvent(input$sentiment_update, {
    showNotification("Updating sentiment analysis...", type = "message", duration = 2)
  })
  
  output$sentiment_wordcloud <- renderWordcloud2({
    words <- data.frame(
      word = c("innovation", "growth", "technology", "digital", "mobile", 
               "revenue", "market", "leadership", "strategy", "transformation",
               "ecosystem", "platform", "ai", "cloud", "data"),
      freq = c(25, 22, 20, 18, 16, 15, 14, 12, 10, 9, 8, 7, 6, 5, 4)
    )
    
    wordcloud2(words, size = 0.8, color = "random-dark", backgroundColor = "white")
  })
  
  output$sentiment_timeline <- renderPlotly({
    timeline_data <- data.frame(
      Date = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "month"),
      Sentiment = cumsum(rnorm(72, 0, 0.1)) + 5
    )
    
    plot_ly(timeline_data, x = ~Date, y = ~Sentiment, type = 'scatter', mode = 'lines',
            line = list(color = '#10b981', width = 3),
            fill = 'tozeroy', fillcolor = 'rgba(16, 185, 129, 0.1)') %>%
      layout(title = "Sentiment Timeline",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Sentiment Score"))
  })
  
  output$sentiment_correlation <- renderPlotly({
    corr_matrix <- matrix(c(
      1.0, 0.7, 0.5, 0.3,
      0.7, 1.0, 0.6, 0.4,
      0.5, 0.6, 1.0, 0.8,
      0.3, 0.4, 0.8, 1.0
    ), nrow = 4, byrow = TRUE)
    
    dimnames(corr_matrix) <- list(
      c("Sentiment", "Revenue", "Innovation", "Growth"),
      c("Sentiment", "Revenue", "Innovation", "Growth")
    )
    
    plot_ly(z = corr_matrix, type = "heatmap",
            colorscale = "Viridis",
            x = colnames(corr_matrix),
            y = rownames(corr_matrix)) %>%
      layout(title = "Sentiment Correlation Matrix")
  })
  
  # ============================================================
  # 7.0 — INNOVATION LAB
  # ============================================================
  
  observeEvent(input$sim_run, {
    showNotification("Running innovation simulation...", type = "warning", duration = 3)
  })
  
  output$monte_carlo <- renderPlotly({
    years <- input$sim_years
    n_sims <- 200
    
    # Generate simulation data
    set.seed(123)
    sims <- sapply(1:n_sims, function(i) {
      100 * cumprod(1 + rnorm(years, 
                              mean = input$sim_growth/100, 
                              sd = input$sim_volatility/10))
    })
    
    plot_data <- data.frame(
      Year = 1:years,
      Mean = rowMeans(sims),
      P5 = apply(sims, 1, quantile, probs = 0.05),
      P95 = apply(sims, 1, quantile, probs = 0.95)
    )
    
    plot_ly(plot_data) %>%
      add_ribbons(x = ~Year, ymin = ~P5, ymax = ~P95,
                  fillcolor = 'rgba(37, 99, 235, 0.2)',
                  line = list(color = 'transparent'),
                  name = "90% Confidence") %>%
      add_lines(x = ~Year, y = ~Mean,
                line = list(color = '#2563eb', width = 3),
                name = "Expected Path") %>%
      layout(title = "Monte Carlo Simulation - Market Value",
             xaxis = list(title = "Years"),
             yaxis = list(title = "Market Value (Indexed)"))
  })
  
  output$strategic_insights <- renderText({
    paste(
      "STRATEGIC INSIGHTS -", input$sim_scenario, "SCENARIO\n",
      "========================================\n",
      "• Forecast Period:", input$sim_years, "years\n",
      "• Market Growth Assumption:", input$sim_growth, "%\n",
      "• Volatility Setting:", input$sim_volatility, "\n",
      "• Key Recommendation: Focus on digital transformation initiatives\n",
      "• Risk Level: ", ifelse(input$sim_volatility > 1, "HIGH", "MODERATE"), "\n",
      "• Opportunity Window: Next", sample(2:5, 1), "years\n\n",
      "Strategic imperatives:\n",
      "1. Enhance digital capabilities\n", 
      "2. Diversify revenue streams\n",
      "3. Strengthen innovation pipeline\n",
      "4. Optimize operational efficiency"
    )
  })
  
  output$scenario_analysis <- renderPlotly({
    scenarios <- data.frame(
      Scenario = c("Base Case", "Optimistic", "Pessimistic", "Disruption"),
      Revenue_Impact = c(100, 145, 65, 40),
      Market_Share = c(25, 35, 15, 10),
      Innovation_Score = c(70, 90, 50, 30)
    )
    
    plot_ly(scenarios, x = ~Scenario, y = ~Revenue_Impact, type = 'bar',
            name = "Revenue Impact", marker = list(color = '#2563eb')) %>%
      add_bars(y = ~Market_Share, name = "Market Share", marker = list(color = '#10b981')) %>%
      add_bars(y = ~Innovation_Score, name = "Innovation Score", marker = list(color = '#f59e0b')) %>%
      layout(title = "Scenario Analysis Comparison",
             barmode = 'group',
             xaxis = list(title = "Scenario"),
             yaxis = list(title = "Score/Impact"))
  })
  
  # ============================================================
  # 8.0 — COMPETITIVE INTELLIGENCE
  # ============================================================
  
  observeEvent(input$comp_analyze, {
    showNotification("Analyzing competitive landscape...", type = "message", duration = 2)
  })
  
  output$competitive_matrix <- renderPlotly({
    d <- company_year %>%
      filter(Year == max(Year)) %>%
      head(15)
    
    plot_ly(d, x = ~Innovation_Score, y = ~Market_Share_Global,
            text = ~Company, type = 'scatter', mode = 'markers',
            marker = list(size = ~Revenue_Billions/10 + 10,
                          color = ~Growth_Rate,
                          colorscale = 'Viridis',
                          showscale = TRUE,
                          line = list(width = 2, color = 'white'))) %>%
      layout(title = "Competitive Positioning Matrix",
             xaxis = list(title = "Innovation Score"),
             yaxis = list(title = "Market Share (%)"),
             hoverlabel = list(bgcolor = 'white'))
  })
  
  output$market_share_flow <- renderPlotly({
    d <- react_competitive_data()
    
    plot_ly(d, x = ~Year, y = ~Market_Share_Global, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#2563eb', width = 3),
            marker = list(size = 8, color = '#2563eb')) %>%
      layout(title = paste("Market Share Dynamics -", input$comp_company),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Market Share (%)"))
  })
  
  output$swot_analysis <- renderDT({
    swot_data <- data.frame(
      Category = c("Strengths", "Strengths", "Strengths",
                   "Weaknesses", "Weaknesses", 
                   "Opportunities", "Opportunities",
                   "Threats", "Threats"),
      Item = c("Strong brand recognition", "High innovation capability", "Market leadership position",
               "Dependence on mature markets", "High R&D costs",
               "Digital transformation trends", "Emerging market growth",
               "Intense competition", "Regulatory changes"),
      Impact = c("High", "High", "High",
                 "Medium", "Medium",
                 "High", "Medium",
                 "High", "Medium")
    )
    
    datatable(swot_data, options = list(pageLength = 10)) %>%
      formatStyle('Category',
                  backgroundColor = styleEqual(
                    c("Strengths", "Weaknesses", "Opportunities", "Threats"),
                    c('#d1fae5', '#fee2e2', '#dbeafe', '#fef3c7')
                  ))
  })
}

# ============================================================
#  RUN APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)