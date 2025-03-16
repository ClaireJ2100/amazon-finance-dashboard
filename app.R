# Amazon Finance Shiny App

# load library
library(shiny)
library(tidyverse)
library(ggplot2)
library(thematic)
library(plotly)
library(dplyr)
library(bslib)
library(bs4Dash)
library(scales)
library(shinyWidgets)

# set options
options(shiny.autoreload = TRUE)


# Load Quarterly Data
quarterly_data <- read.csv("data/raw/quarterly_financials.csv")
quarterly_data$fiscalDateEnding <- as.Date(quarterly_data$fiscalDateEnding, format = "%Y-%m-%d")
quarterly_data <- quarterly_data %>%
  mutate(Year = lubridate::year(fiscalDateEnding),
         Quarter = paste0("Q", lubridate::quarter(fiscalDateEnding)))

# Load Annual Data
annual_data <- read.csv("data/raw/annual_financials.csv")
annual_data$fiscalDateEnding <- as.Date(annual_data$fiscalDateEnding, format = "%Y-%m-%d")
annual_data <- annual_data %>%
  mutate(Year = lubridate::year(fiscalDateEnding))


# Define UI
ui <- fluidPage(
  
  # Set Theme
  theme = bs_theme(bootswatch = "flatly"),
  
  # App Title
  titlePanel(
    HTML('<h2>&#128202; Amazon Financial Dashboard</h2>')
  ),
  
  # Sidebar Layout
  sidebarLayout(
    sidebarPanel(
      # Mode Selector
      radioButtons("mode", "Select Mode:", choices = c("Quarterly", "Annual"), selected = "Quarterly"),
      
      # Quarterly Mode: Year & Quarter Selection
      conditionalPanel(
        condition = "input.mode == 'Quarterly'",
        selectInput("quarterly_year", "Select Year:", choices = sort(unique(quarterly_data$Year), decreasing = TRUE)),
        selectInput("quarter", "Select Quarter:", choices = c("All", unique(quarterly_data$Quarter)), selected = "All")
      ),
      
      # Annual Mode: Multi-Year Selection
      conditionalPanel(
        condition = "input.mode == 'Annual'",
        pickerInput(
          inputId = "annual_years",
          label = "Select Year(s):",
          choices = sort(unique(annual_data$Year), decreasing = TRUE),
          selected = unique(annual_data$Year),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,  # Adds "Select All" and "Deselect All" buttons
            `live-search` = TRUE,  # Enables search within the dropdown
            `none-selected-text` = "Click to select years"  # Placeholder text
          )
        )
      )
    ),
    
    # Main Panel
    mainPanel(
      # Cards
      fluidRow(
        column(4, uiOutput("totalRevenue")),
        column(4, uiOutput("grossProfit")),
        column(4, uiOutput("operatingIncome"))
      ),
      
      # Line Chart
      plotlyOutput("line_chart"),
      
      # Bar Chart
      plotlyOutput("bar_chart"),
      
      # Growth Rate Chart
      plotlyOutput("growth_chart")
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive Data: Handle Both Modes
  filtered_data <- reactive({
    if (input$mode == "Quarterly") {
      df <- quarterly_data %>% filter(Year == input$quarterly_year)
      
      if (input$quarter != "All") {
        df <- df %>% filter(Quarter == input$quarter)
      }
      
    } else {
      # Ensure multiple years can be selected
      df <- annual_data %>% filter(Year %in% input$annual_years)
      
      # Keep `fiscalDateEnding` as a valid date
      df$fiscalDateEnding <- as.Date(df$fiscalDateEnding, format = "%Y-%m-%d")
    }
    
    df <- df %>% arrange(fiscalDateEnding)
    
    return(df)
  })
  
  # Cards
  output$totalRevenue <- renderUI({
    df <- filtered_data()
    value <- ifelse(nrow(df) > 0, paste0("$", comma(sum(df$totalRevenue, na.rm = TRUE))), "No Data")
    
    card(
      card_header("Total Revenue"),
      card_body(
        strong(value),
        p("Amazon's total revenue for the selected period.")
      )
    )
  })
  
  output$grossProfit <- renderUI({
    df <- filtered_data()
    value <- ifelse(nrow(df) > 0, paste0("$", comma(sum(df$grossProfit, na.rm = TRUE))), "No Data")
    
    card(
      card_header("Gross Profit"),
      card_body(
        strong(value),
        p("Revenue minus the cost of goods/services.")
      )
    )
  })
  
  output$operatingIncome <- renderUI({
    df <- filtered_data()
    value <- ifelse(nrow(df) > 0, paste0("$", comma(sum(df$operatingIncome, na.rm = TRUE))), "No Data")
    
    card(
      card_header("Operating Income"),
      card_body(
        strong(value),
        p("Profit from Amazon’s core business before tax/interest.")
      )
    )
  })
  
  # Line Chart
  output$line_chart <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) return(NULL)  # Prevents errors when data is empty
    
    df_long <- df %>%
      pivot_longer(cols = c(totalRevenue, netIncome), names_to = "variable", values_to = "value")
    
    p <- ggplot(df_long, aes(x = fiscalDateEnding, y = value, color = variable, group = variable)) +
      geom_line(linewidth = 1) +  
      geom_point(size = 3) +  
      scale_x_date(date_labels = "%Y") +  
      scale_y_continuous(labels = scales::comma) +  # Improve readability of large numbers
      labs(title = "Revenue vs Net Income Over Time", y = "Amount ($)", x = "Time") +
      scale_color_manual(values = c("totalRevenue" = "blue", "netIncome" = "green")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Bar Chart
  output$bar_chart <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) return(NULL)  # Prevents errors
    
    df_long <- df %>%
      select(costOfRevenue, sellingGeneralAndAdministrative, researchAndDevelopment) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      pivot_longer(cols = everything(), names_to = "Expense Category", values_to = "Amount")
    
    # Rename categories for better readability
    df_long$`Expense Category` <- recode(df_long$`Expense Category`,
                                         "costOfRevenue" = "Cost of Goods Sold",
                                         "sellingGeneralAndAdministrative" = "Sales & Admin Costs",
                                         "researchAndDevelopment" = "Research And Development Expenses"
    )
    
    p <- ggplot(df_long, aes(x = "Total Expenses", y = Amount, fill = `Expense Category`)) +
      geom_col() +
      scale_y_continuous(labels = scales::comma) +  
      labs(title = "Amazon's Expense Breakdown", y = "Amount ($)", x = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        legend.title = element_blank())
    
    ggplotly(p)
    
  })

  output$growth_chart <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) < 2) return(NULL)  # Prevent errors when only one period is selected
    
    df <- df %>%
      arrange(fiscalDateEnding) %>%
      mutate(growthRate = (totalRevenue - lag(totalRevenue)) / lag(totalRevenue) * 100) %>%
      drop_na()  
    
    p <- ggplot(df, aes(x = fiscalDateEnding, y = growthRate)) +
      geom_line(size = 1.5, color = "blue") +
      geom_point(size = 3) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert to percentage format
      labs(title = "Revenue Growth Over Time", y = "Growth Rate (%)", x = "Time") +
      theme_minimal() 
    
    ggplotly(p)
  })

}

shinyApp(ui, server)