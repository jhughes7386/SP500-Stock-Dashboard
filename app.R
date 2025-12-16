# ---- Libraries ----
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(zoo)
library(randomForest)
library(tidyr)

# ---- Load Data ----
sp500_prices <- read_csv("sp500_prices.csv", show_col_types = FALSE)
sp500_cum_returns <- read_csv("sp500_cum_returns.csv", show_col_types = FALSE)
sector_summary <- read_csv("sector_summary.csv", show_col_types = FALSE)
volatility_summary <- read_csv("volatility_summary.csv", show_col_types = FALSE)

# ---- Clean column names ----
if ("GICS.Sector" %in% names(sector_summary)) {
  sector_summary <- sector_summary %>% rename(sector = GICS.Sector)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("S&P 500 Stock Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ticker", "Select a Stock Symbol:",
                  choices = sort(unique(sp500_prices$ticker)), selected = "AAPL"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(sp500_prices$ref.date),
                     end = max(sp500_prices$ref.date)),
      selectInput("sector_input", "Select Sector for Sector Performance:",
                  choices = c("All", sort(unique(sector_summary$sector))), selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Prices", plotOutput("price_plot")),
        tabPanel("Cumulative Returns", plotOutput("return_plot")),
        tabPanel("Sector Performance", plotOutput("sector_plot")),
        tabPanel("Volatility", plotOutput("vol_plot")),
        tabPanel("Price Prediction (Next Day)", plotOutput("prediction_plot"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output) {
  
  # ---- Reactive filtered data ----
  filtered_data <- reactive({
    sp500_prices %>%
      filter(ticker == input$ticker,
             ref.date >= input$date_range[1],
             ref.date <= input$date_range[2])
  })
  
  filtered_cum_returns <- reactive({
    sp500_cum_returns %>%
      filter(ticker == input$ticker,
             ref.date >= input$date_range[1],
             ref.date <= input$date_range[2])
  })
  
  filtered_sector <- reactive({
    sector_summary %>%
      filter(ref.date >= input$date_range[1],
             ref.date <= input$date_range[2])
  })
  
  # ---- Stock Price Plot ----
  output$price_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = ref.date, y = price.adjusted)) +
      geom_line(color = "#0072B2", size = 1) +
      labs(title = paste("Adjusted Stock Price for", input$ticker),
           x = "Date", y = "Adjusted Price (USD)") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  # ---- Cumulative Return Plot ----
  output$return_plot <- renderPlot({
    ggplot(filtered_cum_returns(), aes(x = ref.date, y = cumulative_return)) +
      geom_line(color = "#009E73", size = 1) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      labs(title = paste("Cumulative Return for", input$ticker),
           x = "Date", y = "Cumulative Return (%)") +
      theme_minimal()
  })
  
  # ---- Sector Performance Plot ----
  output$sector_plot <- renderPlot({
    data <- filtered_sector() %>%
      mutate(year = format(ref.date, "%Y")) %>%
      group_by(year, sector) %>%
      summarise(avg_return = mean(avg_cum_return, na.rm = TRUE), .groups = "drop")
    
    base_colors <- c("#0072B2","#E69F00","#F0E442","#D55E00","#56B4E9",
                              "#009E73","#CC79A7","#999999","#882255","#44AA99","#DDCC77")
                              
    sectors_present <- sort(unique(data$sector))
    sector_colors <- setNames(base_colors[1:length(sectors_present)], sectors_present)
    
    if (input$sector_input != "All") {
      data <- data %>% filter(sector == input$sector_input)
      ggplot(data, aes(x = year, y = avg_return)) +
        geom_col(fill = sector_colors[input$sector_input], color = "black") +
        geom_text(aes(label = scales::percent(avg_return, accuracy = 0.1)),
                  vjust = ifelse(data$avg_return >= 0, -0.5, 1.5), size = 3) +
        labs(title = paste("Cumulative Performance of", input$sector_input, "Sector"),
             x = "Year", y = "Cumulative Return") +
        theme_minimal()
    } else {
      ggplot(data, aes(x = year, y = avg_return, group = sector, color = sector)) +
        geom_line(size = 1.2) + geom_point(size = 2) +
        scale_color_manual(values = sector_colors) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        labs(title = "Sector Performance Over Time",
             x = "Year", y = "Cumulative Return", color = "Sector") +
        theme_minimal()
    }
  })
  
  # ---- Volatility Plot ----
  output$vol_plot <- renderPlot({
    df <- filtered_data() %>%
      arrange(ref.date) %>%
      mutate(daily_return = (price.adjusted / lag(price.adjusted)) - 1,
             year = format(ref.date, "%Y"))
    
    volatility_by_year <- df %>%
      group_by(year) %>%
      summarise(volatility = sd(daily_return, na.rm = TRUE),
                n_obs = sum(!is.na(daily_return)), .groups = "drop") %>%
      filter(n_obs > 1)
    
    ggplot(volatility_by_year, aes(x = year, y = volatility, fill = volatility)) +
      geom_col() +
      scale_fill_gradient(low = "#FFC0C0", high = "#8B0000",
                          limits = c(0, max(volatility_by_year$volatility, na.rm = TRUE)),
                          oob = scales::squish) +
      labs(title = paste("Annual Volatility for", input$ticker),
           x = "Year", y = "Volatility (Std Dev of Daily Returns)", fill = "Volatility") +
      theme_minimal()
  })
  
  # ---- Random Forest Next-Day Price Prediction ----
  output$prediction_plot <- renderPlot({
    df <- filtered_data() %>%
      arrange(ref.date) %>%
      mutate(
        lag1 = lag(price.adjusted, 1),
        lag5 = lag(price.adjusted, 5),
        ma5 = zoo::rollmean(price.adjusted, 5, fill = NA, align = "right"),
        ma10 = zoo::rollmean(price.adjusted, 10, fill = NA, align = "right"),
        next_price = lead(price.adjusted, 1)
      ) %>%
      drop_na()
    
    if(nrow(df) < 10){
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Not enough data for prediction") +
        theme_void()
    } else {
      split_index <- floor(0.8 * nrow(df))
      train_data <- df[1:split_index, ]
      test_data <- df[(split_index + 1):nrow(df), ]
      rf_model <- randomForest(next_price ~ lag1 + lag5 + ma5 + ma10,
                               data = train_data, ntree = 300, mtry = 2)
      test_data$predicted <- predict(rf_model, test_data)
      
      ggplot(test_data, aes(x = ref.date)) +
        geom_line(aes(y = next_price, color = "Actual"), linewidth = 1) +
        geom_line(aes(y = predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
        scale_color_manual(values = c("Actual"="#0072B2","Predicted"="#D55E00")) +
        labs(title = paste("Random Forest Predicted vs Actual Prices for", input$ticker),
             x = "Date", y = "Adjusted Close Price (USD)", color = "") +
        theme_minimal()
    }
  })
  
}

# ---- Run App ----
shinyApp(ui, server)
