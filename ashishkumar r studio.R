
# RETAIL STORE SALES ANALYSIS DASHBOARD

# Install packages (run only if needed)
packages <- c("shiny", "ggplot2", "dplyr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Retail Store Sales Analysis Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Upload Retail Dataset"),
      
      fileInput(
        "file",
        "Choose CSV Dataset",
        accept = ".csv"
      ),
      
      hr(),
      
      h4("Dashboard Filters"),
      
      sliderInput(
        "price",
        "Product Price Range",
        min = 0, max = 100,
        value = c(0, 100)
      ),
      
      sliderInput(
        "discount",
        "Discount Range (%)",
        min = 0, max = 20,
        value = c(0, 20)
      ),
      
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      
      h2("Retail Store Performance"),
      
      fluidRow(
        column(4, wellPanel(
          h4("Estimated Sales Value"),
          textOutput("total_sales")
        )),
        
        column(4, wellPanel(
          h4("Average Product Price"),
          textOutput("avg_price")
        )),
        
        column(4, wellPanel(
          h4("Average Footfall"),
          textOutput("avg_footfall")
        ))
      ),
      
      fluidRow(
        column(6, wellPanel(
          h4("Average Discount"),
          textOutput("avg_discount")
        )),
        
        column(6, wellPanel(
          h4("Average Return Rate"),
          textOutput("avg_return")
        ))
      ),
      
      tabsetPanel(
        
        tabPanel(
          "Sales Analysis",
          plotOutput("sales_plot", height = "350px")
        ),
        
        tabPanel(
          "Customer Analysis",
          plotOutput("customer_plot", height = "350px")
        ),
        
        tabPanel(
          "Discount Analysis",
          plotOutput("discount_plot", height = "350px")
        ),
        
        tabPanel(
          "Stock Analysis",
          plotOutput("stock_plot", height = "350px")
        ),
        
        tabPanel(
          "Dataset",
          tableOutput("data_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Upload the CSV file extracted from your ZIP
  raw_data <- reactive({
    req(input$file)
    
    data <- read.csv(
      input$file$datapath,
      stringsAsFactors = FALSE
    )
    
    required <- c(
      "price", "discount", "footfall",
      "ad_spend", "competitor_price",
      "stock_level", "customer_sentiment",
      "return_rate"
    )
    
    validate(
      need(
        all(required %in% names(data)),
        "Dataset columns do not match the required columns."
      )
    )
    
    data
  })
  
  # Update slider limits to match the uploaded dataset
  observeEvent(raw_data(), {
    data <- raw_data()
    
    updateSliderInput(
      session, "price",
      min = floor(min(data$price, na.rm = TRUE)),
      max = ceiling(max(data$price, na.rm = TRUE)),
      value = c(
        floor(min(data$price, na.rm = TRUE)),
        ceiling(max(data$price, na.rm = TRUE))
      )
    )
    
    updateSliderInput(
      session, "discount",
      min = floor(min(data$discount, na.rm = TRUE)),
      max = ceiling(max(data$discount, na.rm = TRUE)),
      value = c(
        floor(min(data$discount, na.rm = TRUE)),
        ceiling(max(data$discount, na.rm = TRUE))
      )
    )
  }, ignoreInit = FALSE)
  
  # Apply filters and calculate estimated sales
  filtered_data <- reactive({
    data <- raw_data()
    
    data %>%
      filter(
        price >= input$price[1],
        price <= input$price[2],
        discount >= input$discount[1],
        discount <= input$discount[2]
      ) %>%
      mutate(
        estimated_sales =
          price * footfall * (1 - discount / 100)
      )
  })
  
  # Dashboard summary cards
  output$total_sales <- renderText({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    paste0(
      "₹ ",
      format(
        round(sum(data$estimated_sales, na.rm = TRUE), 2),
        big.mark = ",", scientific = FALSE
      )
    )
  })
  
  output$avg_price <- renderText({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    paste0("₹ ", round(mean(data$price, na.rm = TRUE), 2))
  })
  
  output$avg_footfall <- renderText({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    round(mean(data$footfall, na.rm = TRUE), 2)
  })
  
  output$avg_discount <- renderText({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    paste0(round(mean(data$discount, na.rm = TRUE), 2), "%")
  })
  
  output$avg_return <- renderText({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    paste0(round(mean(data$return_rate, na.rm = TRUE) * 100, 2), "%")
  })
  
  # Sales trend by record number
  output$sales_plot <- renderPlot({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    
    data$record_number <- seq_len(nrow(data))
    
    ggplot(data, aes(
      x = record_number,
      y = estimated_sales
    )) +
      geom_line(color = "steelblue", linewidth = 0.6) +
      labs(
        title = "Estimated Sales by Record",
        x = "Record Number",
        y = "Estimated Sales Value"
      ) +
      theme_minimal()
  })
  
  # Customer sentiment
  output$customer_plot <- renderPlot({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    
    ggplot(data, aes(x = customer_sentiment)) +
      geom_histogram(
        bins = 30,
        fill = "seagreen",
        color = "white"
      ) +
      labs(
        title = "Customer Sentiment Distribution",
        x = "Customer Sentiment",
        y = "Number of Records"
      ) +
      theme_minimal()
  })
  
  # Discount compared with estimated sales
  output$discount_plot <- renderPlot({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    
    ggplot(data, aes(
      x = discount,
      y = estimated_sales
    )) +
      geom_point(alpha = 0.35, color = "darkorange") +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Discount vs Estimated Sales",
        x = "Discount (%)",
        y = "Estimated Sales Value"
      ) +
      theme_minimal()
  })
  
  # Stock level analysis
  output$stock_plot <- renderPlot({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching records"))
    
    ggplot(data, aes(
      x = stock_level,
      y = footfall
    )) +
      geom_point(alpha = 0.4, color = "purple") +
      labs(
        title = "Stock Level vs Footfall",
        x = "Stock Level",
        y = "Footfall"
      ) +
      theme_minimal()
  })
  
  # Show dataset preview
  output$data_table <- renderTable({
    head(filtered_data(), 100)
  })
  
  # Reset both filters
  observeEvent(input$reset, {
    data <- raw_data()
    
    updateSliderInput(
      session, "price",
      value = c(
        floor(min(data$price, na.rm = TRUE)),
        ceiling(max(data$price, na.rm = TRUE))
      )
    )
    
    updateSliderInput(
      session, "discount",
      value = c(
        floor(min(data$discount, na.rm = TRUE)),
        ceiling(max(data$discount, na.rm = TRUE))
      )
    )
  })
}

shinyApp(ui = ui, server = server)