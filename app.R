
#loading necessary libraries
library(rsconnect)
library(shiny)
library(dplyr)
library(ggplot2)
#loading fast food sales csv into fast_food_sales variable
fast_food_sales <- read.csv("fast_food_sales.csv")

# Define UI
ui <- fluidPage(
  tags$style(
    #posting a picture on background at top position using HTML body
    HTML(
      "
      body {
        background-image: url('https://i.pinimg.com/originals/3f/12/26/3f12264cad7f964c30ec0af37c19154a.png');
        background-size: cover;
        background-position: top;
        background-repeat: no-repeat;
      }
       
        
      "
    )
  ),
  #adding tittle to the dashboard
  titlePanel("Fast Food Sales Dashboard"), 
  sidebarLayout(
    #adding side inputs on the dashboard
    sidebarPanel( 
      h4("Filters"),
      #creating item_type selection input
      selectInput("item_type", "Item Type:", 
                  choices = c("All", 
                              unique(fast_food_sales$item_type))),
      #creating checkbox for transaction type
      checkboxGroupInput("transaction_type",
                         "Transaction Type:",
                         choices = 
                           unique(fast_food_sales$transaction_type)),
      #creating time of sale as selection input
      selectInput("time_of_sale", 
                  "Time of Sale:", 
                  choices = 
                    c("All", unique(fast_food_sales$time_of_sale))),
      #creating sale_month as selection input
      selectInput("sale_month", 
                  "Sale Month:",
                  choices = 
                    c("All", unique(fast_food_sales$sale_month)))
    ),
    mainPanel(
      tabsetPanel(
        #Creating summary stasts tab to show summary results 
        tabPanel("Summary Statistics",
                 verbatimTextOutput("summary_stats")),
        #creating Most popular items tab to show plot output 
        tabPanel("Most Popular Items",
                 plotOutput("popular_items")),
        #creating sales distribution  tab to show plot output 
        tabPanel("Sales Distribution",
                 plotOutput("sales_distribution"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    filtered_data <- filter_data()
    summary_stats <- summarise(filtered_data,
                               Total_Sales = sum(transaction_amount),
                               Avg_Transaction_Amount = 
                                 mean(transaction_amount))
    summary_stats
  })
  
  # Most popular items plot, which shows only 
  #top 5 items on high sale.
  output$popular_items <- renderPlot({
    filtered_data <- filter_data()
    top_items <- filtered_data %>%
      group_by(item_name) %>%
      summarise(Total_Sales = sum(quantity)) %>%
      top_n(5, Total_Sales)
    
    ggplot(top_items, 
           aes(x = reorder(item_name, Total_Sales),
               y = Total_Sales)) +
      geom_bar(stat = "identity",
               fill = "#4CCD99") +  # Set bar color to green
      labs(title = "Most Popular Items", 
           x = "Item Name", 
           y = "Total Sales") + #giving names 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            # Increase font size for axis labels
            axis.title = element_text(size = 14)) +  
      guides(fill = FALSE)  # Remove legend
  })
  
  # Sales distribution based on sale_month,
  #item_type,transaction_type and time of sale.
  output$sales_distribution <- renderPlot({
    filtered_data <- filter_data()
    sales_distribution <- filtered_data %>%
      #doing group by on the params
      group_by(sale_month, item_type,transaction_type, time_of_sale) %>% 
      summarise(Total_Sales = sum(transaction_amount)) %>%
      ggplot(aes(x = sale_month,y = Total_Sales,fill = item_type)) +
      geom_bar(stat = "identity",position = "dodge") +
      facet_grid(transaction_type ~ .) +
      #giving names to axis
      labs(title = "Sales Distribution by Item Type,Transaction Type, and Month",x = "Months in 2022 year", y = "Total Sales") +
      scale_fill_discrete(name = "Item Type") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            # Increase font size for axis labels
            axis.title = element_text(size = 14)) +
      # Set x-axis scale from 1 to 12
      scale_x_continuous(breaks = 1:12, limits = c(1, 12))  
    sales_distribution
  })
  
  # Function to filter data based on user inputs
  filter_data <- reactive({
    filtered_data <- fast_food_sales
    if (input$item_type != "All") {
      #if filtered data is not equal to ALL 
      #then filter based on itemtype selected
      filtered_data <- filtered_data %>%
        filter(item_type == input$item_type)
    }
    if (!is.null(input$transaction_type) && "All"  %in% input$transaction_type) {
      #if nothing is selected then transaction 
      #type takes both the transaction data 
      filtered_data <- filtered_data
    } else if (!is.null(input$transaction_type)) {
      #filter based on the transaction type.
      filtered_data <- filtered_data %>%
        filter(transaction_type %in% input$transaction_type)
    }
    if (input$time_of_sale != "All") {
      #if filtered data is not equal to ALL 
      #then filter based on itemtype selected
      filtered_data <- filtered_data %>%
        filter(time_of_sale == input$time_of_sale)
    }
    if (input$sale_month != "All") {
      #if filtered data is not equal to ALL 
      #then filter based on itemtype selected
      filtered_data <- filtered_data %>%
        filter(sale_month == input$sale_month)
    }
    filtered_data
  })
}

# Run the application
shinyApp(ui = ui, server = server)