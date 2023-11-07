
library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(readxl)

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
dest.codes <- read_xlsx(
    "https://raw.githubusercontent.com/eharvey15/STAT691P-MSY/master/destinationAirport.xlsx",
    col_names = TRUE
) %>%
    separate_wider_delim(
        Description,
        delim = ": ",
        names = c("Location", "Facility"),
        too_few = "align_end"
    )

data <- left_join(data, dest.codes, by = c("dest" = "Code")) %>%
    mutate(dest.code = dest, dest = Facility) %>%
    select(-Location, -Facility)

data$dest = factorColByFreq(data, "dest")
data$dest.code = factorColByFreq(data, "dest.code")


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
    
    mainPanel(plotOutput("freqdelayPlot"))
  )
)

# Define server logic required to draw a histogramrun
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
        scale_y_continuous(labels = percent)+
        scale_x_discrete()+
        ylab("% of Flights Delayed")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
