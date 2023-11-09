
library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(readxl)
library(plotly)

# Specify data types before import
flight.data.types <- c('factor',    # Month
                       'factor',    # Day of Week
                       'factor',    # Carrier
                       'factor',    # origin airport
                       'factor',    # origin state
                       'factor',    # destination airport
                       'factor',    # destination state
                       'numeric',   # Departure Time
                       'factor',    # on time or not
                       'numeric',   # elapsed time (duration)
                       'numeric'    # distance
)
missing.values <- c("NA","")

data <- read.csv(
    "https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/NewOrlFlights2022.csv",
    colClasses = flight.data.types,
    na.strings = missing.values
)


# Relabel carrier codes to full airline names
# Also, order levels of carrier factor so that basis of comparisons is ordered by number of flights
# If left as-is, Endeavor Air (9E, first carrier code alphabetically) is chosen
factorColByFreq <- function(df, col) {
    # Sort levels of df column by frequency of observations grouped by col (a string)
    # Return factored column, which can then be set in data optionally
    col.flights <- as.list(table(df[col]))
    return(factor(
        df[[col]],
        levels = names(col.flights)[order(unlist(col.flights), decreasing = TRUE)]
    ))
}

carrier.codes <- read.csv("https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/airlinesCodes2022.csv")

data <- left_join(data, carrier.codes, by = c("carrier" = "Code")) %>%
    mutate(carrier.code = carrier, carrier = Description) %>%
    select(-Description)

data$carrier = factorColByFreq(data, "carrier")
data$carrier.code = factorColByFreq(data, "carrier.code")


# Likewise relabel and order levels of destination factor by number of flights
# The below file from Moodle is helpful for names, but does not supply lat/lon needed for map
# dest.codes <- read_xlsx(
#     "https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/destinationAirport.xlsx",
#     col_names = TRUE
# ) %>%
#     separate_wider_delim(
#         Description,
#         delim = ": ",
#         names = c("Location", "Facility"),
#         too_few = "align_end"
#     )

# data <- left_join(data, dest.codes, by = c("dest" = "Code")) %>%
#     mutate(dest.code = dest, dest = Facility) %>%
#     select(-Location, -Facility)
airport_info <- read.csv("https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv")

data <- left_join(data, airport_info, by = c("dest" = "iata")) %>%
    mutate(dest.code = dest, dest = airport) %>%
    rename(dest.lon = longitude, dest.lat = latitude) %>%
    select(-country_code, -region_name, -icao, -airport)

data$dest = factorColByFreq(data, "dest")
data$dest.code = factorColByFreq(data, "dest.code")

#data$delay <- as.factor(data$delay) # done using flight.data.types

#retrieve names for the codes
#carrier_codes <- read.csv("https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/airlinesCodes2022.csv")

#data <- left_join(data, carrier_codes,by = join_by("carrier" == "Code"))
#data <- data %>% rename("carrier_name" = "Description")


#get airport info from external source
#airport_info <- read.csv("https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv")

#select only the code, name, and coordinates
#airport_info <- airport_info %>% 
#  select(iata, airport, latitude, longitude)

#add the name and coordinates to the dataset
#data <- left_join(data, airport_info, by = join_by("dest" == "iata"))

#retrieve unique airport names
#airports <- unique(data$airport)

#retrieve unique carrier names
#carriers <- unique(data$carrier_name)


#filter data down for flight map
#map_data <- data %>% select(airport, longitude, latitude)
#map_data$MSYlon <- filter(airport_info, airport == "Louis Armstrong New Orleans International Airport")$longitude
#map_data$MSYlat <- filter(airport_info, airport == "Louis Armstrong New Orleans International Airport")$latitude
#map_data <- map_data %>% distinct(latitude, longitude, .keep_all = TRUE)
map_data <- data %>%
    select(dest.code, dest.lon, dest.lat) %>%
    rename(airport = dest.code, longitude = dest.lon, latitude = dest.lat)
map_data$MSYlon <- airport_info$longitude[airport_info$airport == "Louis Armstrong New Orleans International Airport"]
map_data$MSYlat <- airport_info$latitude[airport_info$airport == "Louis Armstrong New Orleans International Airport"]
map_data <- map_data %>% distinct(latitude, longitude, .keep_all = TRUE)

#here I would like to do some work to establish some basic basics about each flight path, such as the top 3 carriers and the frequency of a delay.

#frequency of delay by airport
delay_by_airport <- data %>% group_by(dest.code) %>% 
  summarise(airport.delay.freq = sum(delay ==1)/n()) %>% 
  mutate(airport.delay.freq = round(airport.delay.freq, digits = 3))

#merge frequency of delay by airport with map_data
map_data <- left_join(map_data, delay_by_airport, by = c("airport" = "dest.code"))

#top carrier by airport
top_carrier_by_airport <- data %>% group_by(dest.code, carrier.code) %>% 
  summarise(flight.count = n()) %>% 
  arrange(desc(flight.count)) %>% 
  group_by(dest.code) %>% 
  slice(1) %>% 
  rename("top.carrier" = "carrier.code") %>% 
  select(dest.code, top.carrier)

#merge top carrier by airport with map_data
map_data <- left_join(map_data, top_carrier_by_airport, by = c("airport" = "dest.code"))



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
      uiOutput("carrierInput"),
      uiOutput("destInput"),
      checkboxGroupInput(inputId = "day", 
                         label = "Day of the Week",
                         choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7),
                         selected = seq(1:7)),
      checkboxGroupInput(inputId = "month",
                         label = "Month",
                         choices = list("April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12),
                         selected = seq(from = 4, to =12, by =1)),
      br(),
      checkboxInput(inputId = "fullNames",
                    label = "Use full Carrier/Airport names",
                    value = FALSE)
      
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
    
    observeEvent(input$fullNames, {
        if (input$fullNames) {
            carriers <- levels(data$carrier)
            airports <- levels(data$dest)
            
            # Translate current selections
            if (!is.null(input$carrier))
                selectedCarriers <- carriers[levels(data$carrier.code) %in% input$carrier]

            if (!is.null(input$dest))
                selectedAirports <- airports[levels(data$dest.code) %in% input$dest]
        } else {
            carriers <- levels(data$carrier.code)
            airports <- levels(data$dest.code)
            
            # Translate current selections
            if (!is.null(input$carrier))
                selectedCarriers <- carriers[levels(data$carrier) %in% input$carrier]

            if (!is.null(input$dest))
                selectedAirports <- airports[levels(data$dest) %in% input$dest]
        }
        
        # Initialize selections if none exist
        if (is.null(input$carrier))
            selectedCarriers <- carriers[1:3]
        
        if (is.null(input$dest))
            selectedAirports <- airports[1:3]
        
        output$carrierInput <- renderUI({
            selectInput(inputId = "carrier", 
                        label = "Carrier",
                        choices = carriers,
                        selected = selectedCarriers, #list("DL", "AA", "B6"), 
                        multiple = TRUE)
        })
        
        output$destInput <- renderUI({
            selectInput(inputId = "dest",
                        label = "Destination",
                        choices = airports,
                        selected = selectedAirports, #list("LGA", "DFW", "CLT"),
                        multiple = TRUE)
        })
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    output$freqdelayPlot <- renderPlot({

      ggplot(data %>% filter(carrier %in% input$carrier | carrier.code %in% input$carrier,
                                    dest %in% input$dest | dest.code %in% input$dest,
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
                     name = ~airport,
                     showlegend = FALSE) %>% 
        add_markers(y = ~latitude,
                    x = ~longitude,
                    text = ~paste0(airport, "<br>", 
                                   "(", longitude, ", ", latitude, ")","<br>",
                                   "Frequency of Delay: ", airport.delay.freq, "<br>",
                                   "Top Carrier: ", top.carrier),
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
