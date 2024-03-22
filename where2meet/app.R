library(shiny)
library(leaflet)
library(dplyr)
library(tidygeocoder)

# Define UI
ui <- fluidPage(
  titlePanel("User Intersection Mapper"),
  
  # Form for user input
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Name"),
      actionButton("add_location", "Add Location"),
      uiOutput("additional_locations"),
      actionButton("submit", "Submit")
    ),
    
    # Output for maps
    mainPanel(
      selectInput("selected_name", "Select Name", choices = NULL),
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Define user_data as a reactiveValues object
  user_data <- reactiveValues(data = data.frame())
  
  # Populate initial sample data
  observe({
    if (nrow(user_data$data) == 0) {
      user_data$data <- data.frame(
        Name = "John Doe",
        Start_Date_1 = Sys.Date(),
        End_Date_1 = Sys.Date(),
        Location_1 = "New York, USA",
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Track the number of additional locations
  locations <- reactiveValues(count = 1)
  
  # Dynamically generate additional location inputs
  output$additional_locations <- renderUI({
    inputs <- lapply(1:locations$count, function(i) {
      tagList(
        div(
          textInput(paste0("location_", i), label = paste("Location", i, "(City, State, Country)")),
          dateInput(paste0("start_date_", i), label = paste("Start Date of Location", i), value = Sys.Date()),
          dateInput(paste0("end_date_", i), label = paste("End Date of Location", i), value = Sys.Date())
        )
      )
    })
    do.call(tagList, inputs)
  })
  
  # Increment the number of additional locations
  observeEvent(input$add_location, {
    locations$count <- locations$count + 1
  })
  
  # Data frame to store user submissions
  user_data <- reactiveValues(data = data.frame())
  
  # Function to update user data
  observeEvent(input$submit, {
    new_entry <- data.frame(
      Name = input$name,
      Start_Date_1 = input$start_date_1,
      End_Date_1 = input$end_date_1,
      Location_1 = input$location_1,
      stringsAsFactors = FALSE
    )
    
    for (i in 2:(locations$count + 1)) {
      new_entry[[paste0("Location_", i)]] <- input[[paste0("location_", i)]]
      new_entry[[paste0("Start_Date_", i)]] <- input[[paste0("start_date_", i)]]
      new_entry[[paste0("End_Date_", i)]] <- input[[paste0("end_date_", i)]]
    }
    
    user_data$data <- rbind(user_data$data, new_entry)
    print(paste0("user_data", user_data))
  })
  
  # Update select input choices
  observe({
    updateSelectInput(session, "selected_name", choices = unique(user_data$data$Name))
  })
  
  # Function to filter data based on selected name
  user_subset <- reactive({
    req(input$selected_name)
    filter(user_data$data, Name == input$selected_name)
  })
  
  # Function to generate map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(lng1 = -180, lng2 = 180, lat1 = -90, lat2 = 90)
  })
  
  # Update map
  observe({
    proxy <- leafletProxy("map", data = user_data$data)
    for (i in 1:(locations$count)) {
      proxy <- addMarkers(proxy,
                          lng = as.numeric(user_data$data[[paste0("Location_", i, "_lng")]]),
                          lat = as.numeric(user_data$data[[paste0("Location_", i, "_lat")]]),
                          popup = ~paste("Name:", Name, "<br>Date:", paste0("Start_Date_", input$i), "-", paste0("End_Date_", input$i))
      )
    }
  })
  
  # Function to geocode locations and add to the dataset
  observeEvent(user_data$data, {
    for (i in 1:(locations$count)) {
      loc_name <- user_data$data[[paste0("Location_", i)]]
      loc_coords <- tidygeocoder::geo(
        address = loc_name,
        method = "osm"
      )
      print(user_data$data)
      if (!is.null(loc_coords)) {
        user_data$data[[paste0("Location_", i, "_lat")]] <- loc_coords$lat
        user_data$data[[paste0("Location_", i, "_lng")]] <- loc_coords$long
      } else {
        print(paste("Geocoding failed for location", i, ":", loc_name))
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)