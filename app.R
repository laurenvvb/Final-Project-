### PROJECT
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(stringr)
library(ggmap)

# load data: this data was loaded from Quandl and is real time data from Zillow
homeprices <- read.csv(file="Michigan Data.csv", header=TRUE, sep=",")
homeprices <- spread(homeprices, key = Place, value = Avg.Price)

ui <- fluidPage(    
  tabPanel("Michigan Home Prices",
           sidebarLayout(      
             sidebarPanel(
               selectInput("Place", "Location:", 
                           choices=c("Ann Arbor", "Detroit", "Flint", "Grand Rapids", "Kalamazoo", "Lansing", "Michigan"))
             ),
             mainPanel(
               plotOutput("homeprices")  
             )
           )
  ))

server <- function(input, output) {
  output$homeprices <- renderPlot({
    subplot <- homeprices %>%
      select(Year, input$Place)
    colnames(subplot) <- c("Year", "Place")
    ggplot(subplot)+
      geom_col(mapping= aes(x = Year, y = Place), data = na.omit(subplot))+
      xlab("Year")+
      ylab("Median Price")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
