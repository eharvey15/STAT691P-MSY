
library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(plotly)

data <- read.csv("https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/NewOrlFlights2022.csv")


data$delay <- as.factor(data$delay)

#retrieve names for the codes
carrier_codes <- read.csv("https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/airlinesCodes2022.csv")

data <- left_join(data, carrier_codes,by = join_by("carrier" == "Code"))
data <- data %>% rename("carrier_name" = "Description")


#get airport info from external source
airport_info <- read.csv("https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv")

#select only the code, name, and coordinates
airport_info <- airport_info %>% 
  select(iata, airport, latitude, longitude)

#add the name and coordinates to the dataset
data <- left_join(data, airport_info, by = join_by("dest" == "iata"))

#retrieve unique airport names
airports <- unique(data$airport)

#retrieve unique carrier names
carriers <- unique(data$carrier_name)


#filter data down for flight map
map_data <- data %>% select(airport, longitude, latitude)
map_data$MSYlon <- filter(airport_info, airport == "Louis Armstrong New Orleans International Airport")$longitude
map_data$MSYlat <- filter(airport_info, airport == "Louis Armstrong New Orleans International Airport")$latitude
map_data <- map_data %>% distinct(latitude, longitude, .keep_all = TRUE)



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
                         selected = list("American Airlines Inc."), 
                  multiple = TRUE),
      selectInput(inputId = "dest",
                         label = "Destination",
                         choices = airports,
                  selected = list("Charlotte Douglas International Airport"),
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
        tabPanel("Flight Map", plotlyOutput("flight_map")))
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$freqdelayPlot <- renderPlot({

      ggplot(data %>% filter(carrier_name %in% input$carrier,
                                    airport %in% input$dest,
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
    
    output$flight_map <- renderPlotly({
      plot_geo(data = map_data, location_mode = "USA-states") %>% 
        add_segments(x = filter(airport_info, airport == "Louis Armstrong New Orleans International Airport")$longitude, 
                     xend = ~longitude,
                     y = filter(airport_info, airport == "Louis Armstrong New Orleans International Airport")$latitude, 
                     yend = ~latitude,
                     showlegend = TRUE) %>% 
        add_markers(y = ~latitude,
                    x = ~longitude,
                    text = ~paste0(airport, "<br>", "(", longitude, ", ", latitude, ")"),
                    hoverinfo = "text",
                    marker = list(
                      color = "#922820"),
                    alpha = 0.5,
                    showscale = TRUE,
                    showlegend = TRUE) %>% 
        layout(geo = geo)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
