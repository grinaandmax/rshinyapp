#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("ggplot2")
library("scales")
library("dplyr")
library("tidyr")
library("tibble")
library("tidyquant")
library("tidyverse")

# Define UI for application that draws a histogram
ui <- fluidPage( 
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "i_slider",     
        "Select Date Range",
        min = as.Date("2020-01-01"),
        max = as.Date("2023-01-01"),
        value = as.Date("2020-01-01"),
        step = 30,
        animate = TRUE
      ),
      radioButtons( 
        "i_radio",      
        "Select Ticker Symbol",
        choices = c("AAPL", "META", "AMZN"),
        selected = "AAPL"
      )
    ),
    mainPanel(
      plotOutput("stockPlot")
    )
  )
)

# Define the server
server <- function(input, output) {
  output$stockPlot <- renderPlot({
    stock_symbol <- input$i_radio
    start_date <- input$i_slider
    end_date <- start_date + 365
    
    # getSymbols
    stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    
    # ggplot with trendline
    ggplot(data = stock_data, aes(x = index(stock_data), y = Cl(stock_data))) +
      geom_line(color = "purple") +
      geom_smooth(method = "lm", color = "pink") +  
      labs(title = paste("Annual Stock Price Data from: ", end_date),
           x = "Date",
           y = "Closing Price")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)