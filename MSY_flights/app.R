
library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)

data <- read.csv("https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/NewOrlFlights2022.csv")

data$delay <- as.factor(data$delay)

airports <- unique(data$dest)
carriers <- unique(data$carrier)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("New Orleans Airport (MSY) Flight Delays"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("duration", "Duration (mins)", min = 0, max = 400, value = c(0,400), sep = ""),
      sliderInput("depart", "Departure Time (mins after 12:01 AM)", min =0, max=1440, value = c(0,1440), sep = ""),
      selectInput(inputId = "carrier", 
                         label = "Carrier",
                         choices = carriers,
                         selected = list("DL", "AA", "B6"), 
                  multiple = TRUE),
      selectInput(inputId = "dest",
                         label = "Destination",
                         choices = airports,
                  selected = list("LGA", "DFW", "CLT"),
                  multiple = TRUE),
      checkboxGroupInput(inputId = "day", 
                         label = "Day of the Week",
                         choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7),
                         selected = seq(1:7)),
      checkboxGroupInput(inputId = "month",
                         label = "Month",
                         choices = list("April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12),
                         selected = seq(from = 4, to =12, by =1))
      
    ),
    
    mainPanel(plotOutput("freqdelayPlot"))
  )
)

# Define server logic required to draw a histogramrun
server <- function(input, output) {

    output$freqdelayPlot <- renderPlot({

      ggplot(data %>% filter(carrier %in% input$carrier,
                                    dest %in% input$dest,
                                    day %in% input$day,
                                    month %in% input$month,
                                    between(duration, input$duration[1], input$duration[2]),
                             between(depart, input$depart[1], input$depart[2])),
             aes(fill = delay)
             )+
        geom_bar(aes(x=delay, y = (..count..)/sum(..count..)))+
        scale_y_continuous(labels = percent)+
        scale_x_discrete()+
        ylab("% of Flights Delayed")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
