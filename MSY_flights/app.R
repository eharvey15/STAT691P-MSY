
library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(plotly)

data <- read.csv("https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/NewOrlFlights2022.csv")


data$delay <- as.factor(data$delay)

#retrieve airport names
airports <- unique(data$dest)

#retrieve carrier names
carriers <- unique(data$carrier)

#get airport info from external source
airport_info <- read.csv("https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv")

#filter out non-US airports
airport_info <- airport_info %>% filter(country_code == "US") %>% 
  
  #select only the code, name, and coordinates
  select(iata, airport, latitude, longitude)

#add the name and coordinates to the dataset
data <- left_join(data, airport_info, by = join_by("dest" == "iata"))

#load map parameters
geo <-
  list(
    projection = list(
      type = "mercator",
      rotation = list(lon = -100, lat = 40, roll = 0),
      scale = 8
    ),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    style = "satellite",
    center = list(lat = 39.50, lon =-98.35)
  )


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
    
    mainPanel(
      tabsetPanel(
        tabPanel("Frequency of Delay",plotOutput("freqdelayPlot")),
        tabPanel("Estimated Probability of Delay"),
        tabPanel("Flight Map"))
  )
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
        scale_y_continuous(limits = c(0,1), labels = percent)+
        scale_x_discrete()+
        ylab("% of Flights Delayed")
      
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
