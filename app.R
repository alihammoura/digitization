install.packages('rsconnect')


library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plotly)
library(scales)
library(DT)

# Estimated Documents Remaining Data
sealed_estimates <- data.frame(
  Category = c("Manhattan", "Queens", "Bronx", "LL", "Y", "Z", "Total"),
  Envelopes_Low = c(48393, 26000, 45000, 3000, 19400, 322200, 463993),
  Envelopes_High = c(88393, 33000, 62000, 5000, 25010, 418180, 631583),
  Images_Low = c(241965, 130000, 225000, 15000, 97000, 1611000, 2319965),
  Images_High = c(441965, 165000, 310000, 25000, 125050, 2090900, 3157915),
  stringsAsFactors = FALSE
)

# Borough Data
borough_data <- read_excel("data/compiled_digitization.xlsx")
borough_totals <- borough_data %>%
  group_by(Borough) %>%
  summarise(
    Envelopes = sum(`#Envelopes`, na.rm = TRUE),
    Images = sum(`#Images`, na.rm = TRUE),
    .groups = "drop"
  )

# ---------- Load and process data ----------
load_phase_data <- function(sheet_name) {
  data <- read_excel("data/digitization_400k.xlsx", sheet = sheet_name) %>%
    drop_na() %>%
    mutate(
      Date = as.Date(Date, origin = "1899-12-30"),  # Convert from Excel serial
      Week = floor_date(Date, "week", week_start = 1)
    )
  return(data)
}

# Preparation Data Loaded
prep_data <- load_phase_data("prep_data")

# Scanning Data Loaded
scan_data <- load_phase_data("scan_data")

# Define CASO staff
caso_staff <- c("KE", "BH", "NW", "RA")

# Split scanning data
scan_caso_data <- scan_data %>% filter(`Staff_Initials` %in% caso_staff)
scan_inhouse_data <- scan_data %>% filter(!`Staff_Initials` %in% caso_staff)

#QA Data Loaded
qa_data   <- load_phase_data("qa_data")

# ---------- Modular UI ----------
phaseTabUI <- function(id, data, goal = 400000) {
  ns <- NS(id)
  
  staff_choices <- sort(unique(data$`Staff_Initials`))  # pull staff names for filter dropdown
  
  tagList(
    fluidRow(
      tabBox(
        id = ns("phase_tabs"),
        width = 12,
        
        tabPanel("Daily",
                 fluidRow(
                   column(4, dateRangeInput(ns("date_range"), "Filter Date Range:",
                                            start = min(data$Date), end = max(data$Date))),
                   column(4, selectInput(ns("staff_select"), "Filter by Staff:",
                                         choices = staff_choices, selected = staff_choices, multiple = TRUE))
                 ),
                 plotlyOutput(ns("daily_plot"))
        ),
        
        tabPanel("Weekly",
                 fluidRow(
                   column(4, dateRangeInput(ns("week_range"), "Filter Week Range:",
                                            start = min(data$Date), end = max(data$Date))),
                   column(4, selectInput(ns("staff_select_week"), "Filter by Staff:",
                                         choices = staff_choices, selected = staff_choices, multiple = TRUE))
                 ),
                 plotlyOutput(ns("weekly_plot"))
        ),
        
        tabPanel(
          title = paste("Projection (to", format(goal, big.mark = ",", scientific = FALSE), ")"),
          plotlyOutput(ns("projection_plot"))
        ),
        tabPanel("Staff Productivity",
                 fluidRow(
                   column(3, selectInput(ns("week1_select"), "Select Week 1:", choices = NULL)),
                   column(3, selectInput(ns("week2_select"), "Select Week 2:", choices = NULL))
                 ),
                 br(),
                 fluidRow(
                   column(12, uiOutput(ns("productivity_table")))
                 ),
                 br(),
                 fluidRow(
                   column(12, plotlyOutput(ns("productivity_line_chart")))
                 )
        )
      )
    )
  )
}

# ---------- UI ----------
ui <- dashboardPage(
  dashboardHeader(title = "Digitization Dashboard", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Digitization Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Prep", tabName = "prep", icon = icon("clipboard-list")),
      menuItem("Scan", icon = icon("camera"),
               menuSubItem("In-House", tabName = "scan_inhouse"),
               menuSubItem("CASO / LGRMIF", tabName = "scan_caso")
      ),
      menuItem("QA", tabName = "qa", icon = icon("check-circle")),
      menuItem("Borough Totals", tabName = "borough_totals", icon = icon("building")),
      menuItem("Sealed Estimates", tabName = "sealed_estimates", icon = icon("lock"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview",
              fluidPage(
                h2("FY25 Digitization Overview - Scanning 400k Sealed Documents", style = "margin-bottom: 20px;"),

                br(),
                
                fluidRow(
                  column(3, uiOutput("prep_status")),
                  column(3, uiOutput("inhouse_status")),
                  column(3, uiOutput("caso_status")),
                  column(3, uiOutput("qa_status"))
                ),
                
                br(),
                
                fluidRow(
                  box(
                    title = "Progress Breakdown by Phase",
                    width = 12,
                    plotlyOutput("overview_bar_chart")
                  )
                )
              )
      ),
      tabItem("prep",
              fluidPage(
                h2("Document Preparation Overview", style = "margin-top: 10px; margin-bottom: 20px;"),
                phaseTabUI("prep", prep_data)
              )
      ),
      tabItem("scan_inhouse",
              fluidPage(
                h2("In-House Scanning Progress", style = "margin-top: 10px; margin-bottom: 20px;"),
                phaseTabUI("scan_inhouse", scan_inhouse_data, goal = 150000)
              )
      ),
      tabItem("scan_caso",
              fluidPage(
                h2("CASO / LGRMIF Scanning Progress", style = "margin-top: 10px; margin-bottom: 20px;"),
                phaseTabUI("scan_caso", scan_caso_data, goal = 250000)
              )
      ),
      tabItem("qa",
              fluidPage(
                h2("Quality Assurance Summary", style = "margin-top: 10px; margin-bottom: 20px;"),
                phaseTabUI("qa", qa_data)
              )
      ),
      tabItem("borough_totals",
              fluidPage(
                h2("Borough-Level Digitization Summary", style = "margin-top: 10px; margin-bottom: 20px;"),
                
                # Total Counts
                fluidRow(
                  valueBox(
                    value = HTML(paste0("<span style='font-size: 28px;'>", comma(sum(borough_totals$Images)), "</span>")),
                    subtitle = HTML("<span style='font-size: 18px;'>Total Images Digitized</span>"),
                    icon = icon("database"),
                    color = "red",
                    width = 6
                  ),
                  valueBox(
                    value = HTML(paste0("<span style='font-size: 28px;'>", comma(sum(borough_totals$Envelopes)), "</span>")),
                    subtitle = HTML("<span style='font-size: 18px;'>Total Envelopes Scanned</span>"),
                    icon = icon("envelope-open-text"),
                    color = "blue",
                    width = 6
                  )
                ),
                
                # Borough Cards
                fluidRow(uiOutput("borough_summary_boxes")),
                
                # Donut Chart
                br(),
                fluidRow(
                  box(
                    title = "Total Images Digitized by Borough",
                    width = 12,
                    plotlyOutput("borough_donut_chart")
                  )
                )
              )
      ),
      tabItem("sealed_estimates",
              fluidPage(
                h2("Estimated Sealed Records for Digitization", style = "margin-bottom: 20px;"),
                p("This section provides low and high-end estimates for sealed records pending digitization, categorized by borough or document type."),
                
                br(),
                DTOutput("sealed_table"),
                br(),
                box(
                  title = "Low vs High Estimate: Image Counts by Category",
                  width = 12,
                  plotlyOutput("sealed_estimates_chart")
                )
              )
      )
      
      
      
    )
  )
)

# ---------- Modular Server ----------
phaseTabServer <- function(id, data, goal = 400000) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      available_weeks <- data %>%
        pull(Date) %>%
        unique() %>%
        floor_date("week", week_start = 1) %>%
        sort(decreasing = TRUE) %>%
        unique()
      
      updateSelectInput(session, "week1_select", choices = format(available_weeks, "%Y-%m-%d"))
      updateSelectInput(session, "week2_select", choices = format(available_weeks, "%Y-%m-%d"))
    })
    
    filtered_data <- reactive({
      req(input$date_range, input$staff_select)
      data %>%
        filter(
          Date >= input$date_range[1],
          Date <= input$date_range[2],
          `Staff_Initials` %in% input$staff_select
        )
    })
    
    weekly_filtered_data <- reactive({
      req(input$week_range, input$staff_select_week)
      data %>%
        filter(
          Date >= input$week_range[1],
          Date <= input$week_range[2],
          `Staff_Initials` %in% input$staff_select_week
        )
    })
    
    daily_totals <- reactive({
      filtered_data() %>%
        group_by(Date) %>%
        summarise(Daily_Images = sum(`#Images`, na.rm = TRUE), .groups = "drop")
    })
    
    weekly_totals <- reactive({
      weekly_filtered_data() %>%
        group_by(Week) %>%
        summarise(Weekly_Images = sum(`#Images`, na.rm = TRUE), .groups = "drop")
    })
    
    cumulative_data <- reactive({
      filtered_data() %>%
        arrange(Date) %>%
        mutate(Cumulative_Images = cumsum(`#Images`))
    })
    
    output$daily_plot <- renderPlotly({
      p <- ggplot(daily_totals(), aes(x = Date, y = Daily_Images, text = paste("Date:", Date, "<br>Images:", Daily_Images))) +
        geom_col(fill = "steelblue") +
        scale_y_continuous(labels = label_comma()) +
        labs(x = "Date", y = "Total Images Completed", title = "Total Daily Productivity") +
        scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text")
    })
    
    output$weekly_plot <- renderPlotly({
      p <- ggplot(weekly_totals(), aes(x = Week, y = Weekly_Images, text = paste("Week of:", Week, "<br>Images:", Weekly_Images))) +
        geom_col(fill = "darkorange") +
        scale_y_continuous(labels = label_comma()) +
        labs(x = "Week Starting", y = "Total Images Completed", title = "Total Weekly Productivity") +
        scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text")
    })
    
    output$projection_plot <- renderPlotly({
      df_full <- cumulative_data()
      
      # Early exit if not enough data
      if (nrow(df_full) < 2) return(NULL)
      
      # Use only the last 8 weeks of data
      df <- df_full %>%
        filter(Date >= Sys.Date() - weeks(8))
      
      if (nrow(df) < 2) {
        df <- df_full  # fallback to full dataset
      }
      
      df <- df %>%
        mutate(Day_Num = as.numeric(Date - min(Date)))
      
      
      if (nrow(df) < 2) return(NULL)
      
      # Calculate model
      model <- lm(Cumulative_Images ~ Day_Num, data = df)
      latest_total <- max(df_full$Cumulative_Images, na.rm = TRUE)
      
      annotations_list <- list()
      
      if (latest_total >= goal) {
        # If already completed, show a "Goal Completed" badge
        projected_date <- Sys.Date()
        
        annotations_list <- list(
          list(
            x = projected_date,
            y = goal,
            text = "🎯 Goal Completed!",
            xref = "x",
            yref = "y",
            showarrow = TRUE,
            arrowhead = 4,
            ax = -40,
            ay = -60,
            font = list(color = "green", size = 15)
          )
        )
        
        # No future projection needed
        future_dates <- NULL
      } else {
        # Still projecting to reach the goal
        projected_day <- ceiling((goal - coef(model)[1]) / coef(model)[2])
        projected_date <- min(df$Date) + days(projected_day)
        
        if (projected_day <= 0) {
          # Guard against invalid 'by' argument
          projected_date <- Sys.Date()
        }
        
        future_dates <- data.frame(
          Date = seq(min(df$Date), projected_date, by = "day")
        ) %>%
          mutate(Day_Num = as.numeric(Date - min(df$Date)))
        
        future_dates$Projected <- predict(model, newdata = future_dates)
        
        annotations_list <- list(
          list(
            x = projected_date,
            y = goal,
            text = paste("Est. Completion:", format(projected_date, "%b %d, %Y")),
            xref = "x",
            yref = "y",
            showarrow = TRUE,
            arrowhead = 4,
            ax = -40,
            ay = -60,
            font = list(color = "red", size = 15)
          )
        )
      }
      
      # Base plot
      p <- ggplot(df_full, aes(x = Date, y = Cumulative_Images)) +
        geom_line(color = "darkgreen", linewidth = 1.2) +
        geom_point()
      
      # Add future projections if not already completed
      if (!is.null(future_dates)) {
        p <- p +
          geom_line(data = future_dates, aes(x = Date, y = Projected),
                    linetype = "dashed", color = "gray40")
      }
      
      p <- p +
        geom_hline(yintercept = goal, linetype = "dotted", color = "red") +
        scale_y_continuous(labels = label_comma()) +
        labs(x = "Date", y = "Cumulative Images") +
        theme_minimal()
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(
          annotations = annotations_list,
          margin = list(t = 50, r = 50, b = 60)
        )
    })
    
    output$productivity_table <- renderUI({
      req(input$week1_select, input$week2_select)  # Ensure selections are made
      
      week1_start <- as.Date(input$week1_select)
      week2_start <- as.Date(input$week2_select)
      
      week1_end <- week1_start + days(6)
      week2_end <- week2_start + days(6)
      
      df_week1 <- data %>%
        filter(Date >= week1_start, Date <= week1_end) %>%
        group_by(Date, `Staff_Initials`) %>%
        summarise(Images = sum(`#Images`, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = `Staff_Initials`, values_from = Images, values_fill = 0) %>%
        arrange(Date)
      
      df_week2 <- data %>%
        filter(Date >= week2_start, Date <= week2_end) %>%
        group_by(Date, `Staff_Initials`) %>%
        summarise(Images = sum(`#Images`, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = `Staff_Initials`, values_from = Images, values_fill = 0) %>%
        arrange(Date)
      
      if (nrow(df_week1) == 0 && nrow(df_week2) == 0) {
        return(DT::datatable(data.frame(Message = "No data available for selected weeks.")))
      }
      
      tagList(
        fluidRow(
          column(6,
                 h4(paste("Week 1 (Week of", format(week1_start, "%b %d"), ")")),
                 DT::dataTableOutput(ns("productivity_week1"))
          ),
          column(6,
                 h4(paste("Week 2 (Week of", format(week2_start, "%b %d"), ")")),
                 DT::dataTableOutput(ns("productivity_week2"))
          )
        )
      )
    })
    
    output$productivity_week1 <- DT::renderDataTable({
      req(input$week1_select)
      
      week1_start <- as.Date(input$week1_select)
      week1_end <- week1_start + days(6)
      
      df <- data %>%
        filter(Date >= week1_start, Date <= week1_end)
      
      if (nrow(df) == 0) return(data.frame(Message = "No data"))
      
      df_wide <- df %>%
        group_by(Date, `Staff_Initials`) %>%
        summarise(Images = sum(`#Images`, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = `Staff_Initials`, values_from = Images, values_fill = 0) %>%
        arrange(Date)
      
      df_wide <- df_wide %>%
        mutate(Date = as.character(Date)) %>%
        mutate(Total = rowSums(select(., -Date)))
      
      total_row <- df_wide %>%
        select(-Date) %>%
        summarise(across(everything(), sum)) %>%
        mutate(Date = "TOTAL") %>%
        relocate(Date)
      
      df_final <- bind_rows(df_wide, total_row)
      
      DT::datatable(df_final, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
    })
    
    output$productivity_week2 <- DT::renderDataTable({
      req(input$week2_select)
      
      week2_start <- as.Date(input$week2_select)
      week2_end <- week2_start + days(6)
      
      df <- data %>%
        filter(Date >= week2_start, Date <= week2_end)
      
      if (nrow(df) == 0) return(data.frame(Message = "No data"))
      
      df_wide <- df %>%
        group_by(Date, `Staff_Initials`) %>%
        summarise(Images = sum(`#Images`, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = `Staff_Initials`, values_from = Images, values_fill = 0) %>%
        arrange(Date)
      
      df_wide <- df_wide %>%
        mutate(Date = as.character(Date)) %>%
        mutate(Total = rowSums(select(., -Date)))
      
      total_row <- df_wide %>%
        select(-Date) %>%
        summarise(across(everything(), sum)) %>%
        mutate(Date = "TOTAL") %>%
        relocate(Date)
      
      df_final <- bind_rows(df_wide, total_row)
      
      DT::datatable(df_final, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
    })
    
    output$productivity_line_chart <- renderPlotly({
      req(input$week1_select, input$week2_select)
      
      start_date <- min(as.Date(input$week1_select), as.Date(input$week2_select))
      end_date <- max(as.Date(input$week1_select), as.Date(input$week2_select)) + days(6)
      
      df <- data %>%
        filter(Date >= start_date, Date <= end_date)
      
      if (nrow(df) == 0) return(NULL)
      
      df <- df %>%
        group_by(Date, `Staff_Initials`) %>%
        summarise(Images = sum(`#Images`, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(df, aes(x = Date, y = Images, color = `Staff_Initials`)) +
        geom_line(linewidth = 1.2) +
        geom_point() +
        scale_y_continuous(labels = label_comma()) +
        labs(x = "Date", y = "# Images Completed", color = "Staff") +
        theme_minimal()
      
      ggplotly(p)
    })
    
  })
}

# ---------- Server ----------
server <- function(input, output, session) {
  phaseTabServer("prep", prep_data)
  phaseTabServer("scan_inhouse", scan_inhouse_data, goal = 150000)
  phaseTabServer("scan_caso", scan_caso_data, goal = 250000)
  phaseTabServer("qa", qa_data)
  # Completion goals
  prep_goal <- 400000
  inhouse_goal <- 150000
  caso_goal <- 250000
  qa_goal <- 400000
  
  # Compute totals
  prep_total <- sum(prep_data$`#Images`, na.rm = TRUE)
  inhouse_total <- sum(scan_inhouse_data$`#Images`, na.rm = TRUE)
  caso_total <- sum(scan_caso_data$`#Images`, na.rm = TRUE)
  qa_total <- sum(qa_data$`#Images`, na.rm = TRUE)
  
  # Percentages
  prep_pct <- 100 * prep_total / prep_goal
  inhouse_pct <- 100 * inhouse_total / inhouse_goal
  caso_pct <- 100 * caso_total / caso_goal
  qa_pct <- 100 * qa_total / qa_goal
  
  # ---- Overview Metrics ----
  output$prep_status <- renderUI({
    valueBox(
      value = HTML(paste0("<div style='font-size: 24px;'>", comma(prep_total), 
                          " <span style='font-size:18px;'>(", round(prep_pct), "%)</span></div>")),
      subtitle = HTML("<span style='font-size: 16px;'>Prep Completed</span>"),
      icon = icon("clipboard-check"),
      color = "green",
      width = 12
    )
  })
  
  output$inhouse_status <- renderUI({
    valueBox(
      value = HTML(paste0("<div style='font-size: 24px;'>", comma(inhouse_total), 
                          " <span style='font-size:18px;'>(", round(inhouse_pct), "%)</span></div>")),
      subtitle = HTML("<span style='font-size: 16px;'>In-House Scanning</span>"),
      icon = icon("camera"),
      color = "blue",
      width = 12
    )
  })
  
  output$caso_status <- renderUI({
    valueBox(
      value = HTML(paste0("<div style='font-size: 24px;'>", comma(caso_total), 
                          " <span style='font-size:18px;'>(", round(caso_pct), "%)</span></div>")),
      subtitle = HTML("<span style='font-size: 16px;'>CASO / LGRMIF Scanning</span>"),
      icon = icon("file-import"),
      color = "purple",
      width = 12
    )
  })
  
  output$qa_status <- renderUI({
    valueBox(
      value = HTML(paste0("<div style='font-size: 24px;'>", comma(qa_total), 
                          " <span style='font-size:18px;'>(", round(qa_pct), "%)</span></div>")),
      subtitle = HTML("<span style='font-size: 16px;'>QA Reviewed</span>"),
      icon = icon("check-circle"),
      color = "teal",
      width = 12
    )
  })
  
  output$overview_bar_chart <- renderPlotly({
    df <- data.frame(
      Phase = c("Prep", "Scan In-House", "Scan CASO / LGRMIF", "QA"),
      Completed = c(prep_total, inhouse_total, caso_total, qa_total),
      Goal = c(prep_goal, inhouse_goal, caso_goal, qa_goal)
    ) %>%
      mutate(Remaining = pmax(Goal - Completed, 0))
    
    plot_ly(df, y = ~Phase, x = ~Completed, type = 'bar', orientation = 'h',
            name = 'Completed', marker = list(color = '#2ECC71')) %>%
      add_trace(x = ~Remaining, name = 'Remaining', marker = list(color = '#E74C3C')) %>%
      layout(
        barmode = 'stack',
        title = list(text = "Progress by Phase", x = 0.5),
        xaxis = list(title = "Record Count"),
        yaxis = list(title = ""),
        legend = list(x = 0.8, y = 0.1)
      )
  })
  
  output$borough_summary_boxes <- renderUI({
    complete_boroughs <- c("Brooklyn", "Staten Island")
    
    boxes <- lapply(1:nrow(borough_totals), function(i) {
      borough <- borough_totals$Borough[i]
      envelopes <- borough_totals$Envelopes[i]
      images <- borough_totals$Images[i]
      
      is_complete <- borough %in% complete_boroughs
      icon_type <- if (is_complete) "check-circle" else "hourglass-half"
      badge <- if (is_complete) {
        tags$span("✅ Completed", style = "color: green; font-weight: bold;")
      } else {
        tags$span("In Progress", style = "color: gray; font-style: italic;")
      }
      
      box(
        title = tags$div(
          style = "font-size: 16px; font-weight: bold;",
          if (is_complete) tagList(icon("check-circle", style = "color: green;"), borough)
          else tagList(icon("hourglass-half", style = "color: gray;"), borough)
        ),
        solidHeader = TRUE,
        width = 4,
        style = paste0("background-color:", if (is_complete) "#d4edda" else "#dbe9f4", ";"),
        tags$p(style = "font-size: 18px;", tags$b("Images:"), comma(images)),
        tags$p(style = "font-size: 18px;", tags$b("Envelopes:"), comma(envelopes)),
        badge
      )
    })
    
    do.call(fluidRow, boxes)
  })
  
  output$borough_donut_chart <- renderPlotly({
    df <- borough_totals %>%
      mutate(
        Label = Borough,
        Completed = Borough %in% c("Brooklyn", "Staten Island"),
        FillColor = ifelse(Completed, "#2ECC71", "#3498DB")
      )
    
    plot_ly(
      data = df,
      labels = ~Label,
      values = ~Images,
      type = 'pie',
      hole = 0.5,
      marker = list(
        colors = df$FillColor,
        line = list(color = '#FFFFFF', width = 2)
      ),
      textinfo = 'label+percent',
      hoverinfo = 'label+value+percent',
      textfont = list(size = 14),
      showlegend = TRUE
    ) %>%
      layout(
        title = list(
          text = "Images Digitized by Borough",
          x = 0.5,
          font = list(size = 20, family = "Arial", color = "#2c3e50")
        ),
        margin = list(t = 70, b = 60),  # Top and bottom margin
        legend = list(orientation = 'h', x = 0.5, y = -0.2, xanchor = "center")
      )
  })
  output$sealed_table <- renderDT({
    datatable(
      sealed_estimates,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't',  # Only show the table (no pagination or length menu)
        ordering = FALSE  # Optional: disable column sorting if you want a clean static view
      ),
      extensions = 'Buttons',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(sealed_estimates),
        fontSize = '16px'
      ) %>%
      formatCurrency(
        columns = 2:5,          # Update to match your actual numeric columns
        currency = "",
        interval = 3,
        mark = ",",
        digits = 0
      )
  })
  
  output$sealed_estimates_chart <- renderPlotly({
    df_plot <- sealed_estimates %>% filter(Category != "Total")  # exclude total from bar chart
    
    plot_ly(df_plot,
            x = ~Category,
            y = ~Images_Low,
            name = 'Low Estimate',
            type = 'bar',
            marker = list(color = '#2980B9')) %>%
      add_trace(y = ~Images_High, name = 'High Estimate', marker = list(color = '#E67E22')) %>%
      layout(
        barmode = 'group',
        title = list(text = "Estimated Image Counts by Category", x = 0.5),
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Number of Images"),
        legend = list(x = 0.8, y = 1)
      )
  })
}

# ---------- Run App ----------
shinyApp(ui, server)
