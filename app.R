# ============================================================================
# DIGITAL ECOSYSTEM DASHBOARD — SERVER PREMIUM
# BAGIAN 3 — Server Logic - FIX LOADING BUG SEDERHANA
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
  # REACTIVE DATA FOR DASHBOARD - SIMPLE FIX
  # ============================================================
  
  # Data for selected year - SIMPLE FIX
  dashboard_year_data <- reactive({
    # Gunakan tryCatch untuk handle error
    tryCatch({
      if (is.null(input$dash_year) || is.null(input$dash_country)) {
        return(country_year)
      }
      
      if (input$dash_year == "Semua Tahun") {
        # Data untuk semua tahun
        if (input$dash_country == "Semua Negara") {
          country_year
        } else {
          country_year %>% filter(Country_Standard == input$dash_country)
        }
      } else if (input$dash_country == "Semua Negara") {
        # Global data for selected year
        country_year %>% filter(Year == as.numeric(input$dash_year))
      } else {
        # Country-specific data
        country_year %>% 
          filter(Year == as.numeric(input$dash_year), 
                 Country_Standard == input$dash_country)
      }
    }, error = function(e) {
      # Return empty data frame jika error
      data.frame()
    })
  })
  
  # Company data with optional country filter
  dashboard_company_data <- reactive({
    tryCatch({
      if (is.null(input$dash_year) || is.null(input$dash_country)) {
        return(company_year)
      }
      
      if (input$dash_year == "Semua Tahun") {
        # Data untuk semua tahun
        if (input$dash_country == "Semua Negara" || !input$filter_company) {
          company_year
        } else {
          company_year %>% filter(Country_Standard == input$dash_country)
        }
      } else if (input$dash_country == "Semua Negara" || !input$filter_company) {
        # Global company data or when filter is off
        company_year %>% filter(Year == as.numeric(input$dash_year))
      } else {
        # Country-specific company data when filter is on
        company_year %>% 
          filter(Year == as.numeric(input$dash_year), 
                 Country_Standard == input$dash_country)
      }
    }, error = function(e) {
      data.frame()
    })
  })
  
  # ============================================================
  # COMPANY FILTER UI (ONLY SHOW WHEN CHECKBOX IS CHECKED)
  # ============================================================
  
  output$company_filter_ui <- renderUI({
    if (input$dash_country != "Semua Negara") {
      if (input$dash_year == "Semua Tahun") {
        companies_in_country <- company_year %>%
          filter(Country_Standard == input$dash_country) %>%
          pull(Company) %>%
          unique()
      } else {
        companies_in_country <- company_year %>%
          filter(Year == as.numeric(input$dash_year), 
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
    # Pastikan data tersedia
    if (input$dash_country == "Semua Negara") {
      if (input$dash_year == "Semua Tahun") {
        data <- country_year %>%
          group_by(Country_Standard) %>%
          summarise(Mobile_Users = sum(Mobile_Users, na.rm = TRUE)) %>%
          arrange(desc(Mobile_Users)) %>%
          head(10)
      } else {
        data <- country_year %>%
          filter(Year == as.numeric(input$dash_year)) %>%
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
          filter(Year == as.numeric(input$dash_year)) %>%
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
      company_year %>% filter(Year == as.numeric(input$stat_year))
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
        cat("ANALISIS STATISTIK\n")
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
        cat("Standar Dev :", round(sd(var_data), 3), "\n")
        cat("IQR         :", round(IQR(var_data), 3), "\n")
        cat("Varians     :", round(var(var_data), 3), "\n")
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
