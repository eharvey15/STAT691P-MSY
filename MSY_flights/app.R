
library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(readxl)
library(plotly)
library(caret)
library(glmnet)
library(shinythemes)

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

data$carrier <- factorColByFreq(data, "carrier")
data$carrier.code <- factorColByFreq(data, "carrier.code")


# Likewise relabel and order levels of destination factor by number of flights
airport_info <- read.csv("https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv")

data <- left_join(data, airport_info, by = c("dest" = "iata")) %>%
    mutate(dest.code = dest, dest = airport) %>%
    rename(dest.lon = longitude, dest.lat = latitude) %>%
    select(-country_code, -region_name, -icao, -airport)

data$dest <- factorColByFreq(data, "dest")
data$dest.code <- factorColByFreq(data, "dest.code")


# Create single fitted model (for now) to be used within app for prediction only
# Use "model3" from "LassoConfusionFlights.f23.R" since it maximized AUC
# Even though predictions will be done on user input, train on smaller
#  subset, since shiny has memory allocations for larger size vectors
#  (e.g. setting train.percent to 1.0 resultss in "Error: cannot allocate
#  vector of size 226.7 Mb)
train.percent <- 0.05
set.seed(112358)

training.rows <- createDataPartition(data$delay, p = train.percent, list = FALSE)
train.batch <- data[training.rows, ]
#test.batch <- data[-training.rows, ]

model.static.matrix <- model.matrix(delay ~ (day + depart + duration + month)^3 + (carrier + dest)^2, data=train.batch)[,-1]
model.static <- glmnet(model.static.matrix, y=as.factor(train.batch$delay), alpha=1, family='binomial')

model.static.cv <- cv.glmnet(model.static.matrix, y=as.numeric(train.batch$delay), alpha=1)
model.static.best_lambda <- model.static.cv$lambda.min


# Filter data down for flight map
map_data <- data %>%
    select(dest.code, dest.lon, dest.lat, dest) %>%
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
      scale = 8,
      scope = "usa"
    ),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    style = "stamen-terrain",
    showcountries = TRUE,
    showsubunits = T,subunitcolor="Blue",
    center = list(lat = 39.50, lon =-98.35)
  )


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("journal"),
  titlePanel("New Orleans Airport (MSY) Flight Delays"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel("input.tabid != 'mapTab'",
        sliderInput(
          "duration",
          "Duration (mins)",
          min = 0, max = 400, value = c(0,400), sep = ""
        )
      ),
      conditionalPanel("input.tabid != 'mapTab'",
        sliderInput(
          "depart",
          "Departure Time (mins after 12:01 AM)",
          min =0, max=1440, value = c(0,1440), sep = ""
        )
      ),
      conditionalPanel("input.tabid != 'mapTab'",
        uiOutput("carrierInput")
      ),
      uiOutput("destInput"),
      conditionalPanel("input.tabid != 'mapTab'",
        checkboxGroupInput(
          inputId = "day", 
          label = "Day of the Week",
          choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7),
          selected = seq(1:7)
        )
      ),
      conditionalPanel("input.tabid != 'mapTab'",
        checkboxGroupInput(
          inputId = "month",
          label = "Month",
          choices = list("April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12),
          selected = seq(from = 4, to =12, by =1)
        )
      ),
      br(),
      checkboxInput(
        inputId = "fullNames",
        label = "Use full Carrier/Airport names",
        value = FALSE
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabid",
        tabPanel("Frequency of Delay", value = "freqTab", plotOutput("freqdelayPlot")),
        tabPanel("Estimated Probability of Delay", value = "predTab",
                 plotlyOutput("predMonthDayPlot"),
                 br(),
                 plotlyOutput("predDepartDurationPlot"),
                 br(),
                 plotlyOutput("predDestCarrierPlot")
        ),
        tabPanel("Flight Map", value = "mapTab", plotlyOutput("flight_map")))
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
      
      req(input$tabid == "freqTab", !is.null(data), cancelOutput = TRUE)

      ggplot(data %>% filter(carrier %in% input$carrier | carrier.code %in% input$carrier,
                                    dest %in% input$dest | dest.code %in% input$dest,
                                    day %in% input$day,
                                    month %in% input$month,
                                    between(duration, input$duration[1], input$duration[2]),
                             between(depart, input$depart[1], input$depart[2])),
             aes(x=factor(delay), fill = factor(delay))
             )+
        geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE)+
        scale_y_continuous(limits = c(0,1), labels = percent)+
        scale_x_discrete(labels = c("Not Delayed", "Delayed"))+
        ylab("% of Flights Delayed")+
        xlab("Status")+
        theme_economist_white()+
        geom_text(stat = 'count', aes(label = paste0((..count..)/sum(..count..) * 100, "%")),
                  vjust = -0.5)
      
    })
    
    # Create base prediction data once when tab is visible for use in several plots
    prediction <- reactiveValues(pred_data = NULL, plot_palette = NULL)
    observe({
      prediction$pred_data <- data %>% mutate(delayProb = NA)
      prediction$plot_palette <- "Cividis"
    })
    
    # Update predictions
    observeEvent(c(input$tabid == "predTab", input$carrier, input$dest, 
                   input$day, input$month, input$duration, input$depart), {
      
      res <- data %>%
        filter(
          carrier %in% input$carrier | carrier.code %in% input$carrier,
          dest %in% input$dest | dest.code %in% input$dest,
          day %in% input$day,
          month %in% input$month,
          between(duration, input$duration[1], input$duration[2]),
          between(depart, input$depart[1], input$depart[2])
        )
      
      # Order days and months numerically (rather than default of alphabetically)
      days.names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      res$day <- factor(
        days.names[res$day],
        levels = days.names
      )
      
      months.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      res$month <- factor(
        months.names[
          as.numeric(as.character(res$month)) # needed to avoid numbering by factor order
        ],
        levels = months.names[sort(as.numeric(levels(res$month)))]
      )
      
      model.static.testdata <- model.matrix(delay ~ (day + depart + duration + month)^3 + (carrier + dest)^2, data = res)[,-1]
      model.static.pred <- as.vector(predict(model.static, s = model.static.best_lambda, model.static.testdata, type="response"))
      
      prediction$pred_data <- res %>%
        mutate(delayProb = model.static.pred) %>%
        group_by(carrier, dest, day, month, duration, depart) %>%
        reframe(delayProb)
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    output$predMonthDayPlot <- renderPlotly({
      
      req(input$tabid == "predTab", cancelOutput = TRUE)
      
      plot_ly(prediction$pred_data, colorscale = prediction$plot_palette) %>%
        add_heatmap(
          ~month,
          ~day,
          ~delayProb,
          hovertemplate = paste(
            "Month: %{x}",
            "Day: %{y}",
            "<b>Est. Delay Prob.</b>: %{z:.1%}<extra></extra>",
            sep = "<br>"
          )
        ) %>%
        layout(
          title = "Est. Delay Prob. by Month and Day of Flight Departure"
        )
    })
    
    output$predDepartDurationPlot <- renderPlotly({
      
      req(input$tabid == "predTab", cancelOutput = TRUE)
      
      plot_ly(prediction$pred_data, type = "scatterpolar", mode = "markers", marker = list(colorscale = prediction$plot_palette)) %>%
        add_trace(
          r = ~duration,
          theta = ~(depart * (360 / 1440)),
          marker = list(
            color = ~delayProb,
            opacity = 0.4
          ),
          showlegend = FALSE,
          text = ~paste(
            paste("Depart:", as.character(as.POSIXct(depart * 60, origin = as.POSIXct("2022-01-01")), format = "%I:%M %p")),
            paste("Duration:", as.character(as.POSIXct(duration * 60, origin = as.POSIXct("2022-01-01")), format = "%I:%M")),
            sep = "<br>"
          ),
          hovertemplate = "%{text}<br><b>Est. Delay Prob.</b>: %{marker.color:.1%}<extra></extra>"
        ) %>%
        layout(
          title = "Est. Delay Prob. by Flight Departure and Duration",
          polar = list(
            bgcolor = "#bbb",
            radialaxis = list(
              visible = TRUE,
              ticksuffix = "m"
            ),
            angularaxis = list(
              direction = "clockwise",
              thetaunit = "degrees",
              tickmode = "array",
              tickvals = seq(0, 360, by = 360 / 8), # ticks every 3 hours
              ticktext = as.character(seq(
                as.POSIXct("2022-01-01"),
                as.POSIXct("2022-01-02"),
                length.out = 9)[-9],
                format = "%I:%M %p")
            )
          )
        )
    })
    
    output$predDestCarrierPlot <- renderPlotly({
      
      req(input$tabid == "predTab", cancelOutput = TRUE)
      
      plot_ly(
        prediction$pred_data,
        type = "treemap",
        labels = ~dest,
        parents = ~carrier,
        values = ~delayProb,
        marker = list(colorscale = prediction$plot_palette)
      ) %>%
        layout(
          title = "Est. Delay Prob. by Destination Airport and Carrier"
        )
      
      # plot_ly(prediction$pred_data, colorscale = prediction$plot_palette) %>%
      #   add_heatmap(
      #     ~dest,
      #     ~carrier,
      #     ~delayProb,
      #     hovertemplate = paste(
      #       "Destination: %{x}",
      #       "Carrier: %{y}",
      #       "<b>Est. Delay Prob.</b>: %{z:.1%}<extra></extra>",
      #       sep = "<br>"
      #     )
      #   ) %>%
      #   layout(
      #     title = "Est. Delay Prob. by Destination Airport and Carrier"
      #   )
    })
    
    output$flight_map <- renderPlotly({
      
      req(input$tabid == "mapTab", cancelOutput = TRUE)
      
      plot_geo(data = map_data %>% filter(airport %in% input$dest | dest %in% input$dest,),  height = 800) %>% 
        add_segments(x = filter(airport_info, airport == "Louis Armstrong New Orleans International Airport")$longitude, 
                     xend = ~longitude,
                     y = filter(airport_info, airport == "Louis Armstrong New Orleans International Airport")$latitude, 
                     yend = ~latitude,
                     name = ~airport,
                     hoverinfo = "name",
                     showlegend = FALSE, 
                     color = ~airport.delay.freq) %>% 
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
        colorbar(title = "Frequency of Delay") %>% 
        layout(geo = geo)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
