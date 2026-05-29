library(devtools)
load_all("../PolandTripPlanner")
library(shiny)
library(leaflet)

cities <- load_cities()

ui <- fluidPage(
  titlePanel("Trip Planner"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("flight_in", "Flight-in city",
                  choices = c("Select..." = "", cities$city),
                  selected = ""),
      
      selectInput("flight_out", "Flight-out city",
                  choices = c("Select..." = "", cities$city),
                  selected = ""),
      
      selectInput("cities", "Cities to visit",
                  choices = cities$city,
                  multiple = TRUE),
      
      dateInput("start_date", "Start date", value = NULL),
      dateInput("end_date", "End date", value = NULL),
      selectInput("transport", "Transport",
                  choices = c("plane", "train", "bus", "car"),
                  selected = "train",
                  multiple = TRUE),
      checkboxInput("scenic_only", "Scenic mode (enable discoveries)", FALSE),
      conditionalPanel(
        condition = "input.scenic_only == true",
        numericInput(
          "radius_km",
          "Scenic radius (km)",
          value = 50,
          min = 10,
          max = 300,
          step = 10)
      )
    ),
    
    mainPanel(
      h3("Route Map"),
      leafletOutput("map", height = 400),
      
      h3("Route Order"),
      textOutput("route_order"),
      
      fluidRow(
        
        column(
          width = 7,
          h3("Trip Overview"),
          tableOutput("overview")
        ),
        
        column(
          width = 5,
          h3("Time Allocation"),
          tableOutput("allocation")
        )
      ),
      
      conditionalPanel(
        condition = "input.scenic_only == true",
        h3("Scenic Discoveries"),
        tableOutput("discoveries")
      ),
      
      verbatimTextOutput("err")
    )
  )
)

server <- function(input, output, session) {
  
  plan <- reactive({
    
    if (
      input$flight_in == "" &&
      input$flight_out == "" &&
      length(input$cities) == 0 &&
      is.null(input$start_date) &&
      is.null(input$end_date)
    ) return(NULL)
    
    # Flight validation
    if (input$flight_in == "" || input$flight_out == "") {
      return(structure(
        list(error = "Please select both flight-in and flight-out cities."),
        class = "trip_error"
      ))
    }
    
    # Cities validation
    if (length(input$cities) < 2) {
      return(structure(
        list(error = "Please select at least 2 cities to visit."),
        class = "trip_error"
      ))
    }
    
    # Date validation
    if (input$end_date < input$start_date) {
      return(structure(
        list(error = "End date cannot be earlier than start date."),
        class = "trip_error"
      ))
    }
    
    # Cities vs days validation
    days <- as.numeric(input$end_date - input$start_date) + 1L
    n_cities <- length(unique(c(input$flight_in, input$flight_out, input$cities)))
    
    if (days < n_cities) {
      return(structure(
        list(error = paste0(
          "Too many cities (", n_cities,
          ") for ", days, " days. Reduce cities or extend trip."
        )),
        class = "trip_error"
      ))
    }
    
    style <- if (input$scenic_only) {
      "scenic"
    } else {
      "fastest"
    }
    
    primary_transport <- input$transport
    
    tryCatch({
      # One unified TripPlanner instance handles routing + allocation + discovery.
      tp <- suppressMessages(TripPlanner$new(
        selected   = input$cities,
        flight_in  = input$flight_in,
        flight_out = input$flight_out,
        start_date = input$start_date,
        end_date   = input$end_date,
        transport  = primary_transport,
        style      = style
      ))

      # Step 1 — optimal route (TSP)
      p <- tp$plan()

      # Step 2 — time allocation
      alloc <- tp$allocate_time()
      p$allocation <- data.frame(
        City       = alloc$cities,
        Days       = alloc$days,
        Importance = round(alloc$importance, 3),
        stringsAsFactors = FALSE
      )
      
      # Step 3 — scenic discoveries
      if (isTRUE(input$scenic_only)) {
        d <- tp$discover_route(
          radius_km       = input$radius_km,
          max_suggestions = 8
        )
        if (d$n_found > 0L) {
          dd <- d$discoveries
          p$discoveries <- list(
            discoveries = data.frame(
              City               = dd$name,
              Distance_km        = dd$distance_km,
              Importance         = dd$importance,
              Nearest_route_city = dd$nearest_route_city,
              Lat                = dd$lat,
              Lon                = dd$lon,
              stringsAsFactors   = FALSE
            ),
            n_found   = d$n_found,
            radius_km = d$radius_km
          )
        } else {
          p$discoveries <- list(
            discoveries = data.frame(),
            n_found     = 0L,
            radius_km   = input$radius_km
          )
        }
      } else {
        p$discoveries <- NULL
      }
      
      p

    }, error = function(e) {
      structure(
        list(error = conditionMessage(e)),
        class = "trip_error"
      )
    })
  }) |> debounce(400)
  
  
  # Error output
  output$err <- renderText({
    
    p <- plan()
    
    # Do not show errors before user interaction
    if (is.null(p)) return("")
    
    # Show validation/backend errors
    if (inherits(p, "trip_error")) {
      return(paste("Error:", p$error))
    }
    
    ""
  })
  
  output$route_order <- renderText({
    
    p <- plan()
    
    if (is.null(p) || inherits(p, "trip_error")) return("")
    
    paste(
      seq_along(p$route),
      p$route,
      collapse = "   →   "
    )
  })
  
  # Overview table (main table)
  mode_icon <- function(mode) {
    switch(mode,
           plane = "✈️",
           train = "🚆",
           car   = "🚗",
           bus   = "🚌")
  }
  
  output$overview <- renderTable({
    p <- plan()
    
    if (is.null(p)) return(NULL)
    if (inherits(p, "trip_error")) return(NULL)
    
    data.frame(
      Leg = seq_along(p$legs),
      From = sapply(p$legs, function(x)  x$from),
      To = sapply(p$legs, function(x)  x$to),
      
      Mode = sapply(p$legs, function(x)  {
        transport_mode <- x$options[[1]]$mode
        paste0(mode_icon(transport_mode), " ", transport_mode)
      }),
      
      Duration_h = sapply(p$legs, function(x) 
                          round(x$options[[1]]$duration_h, 1)
      ),
      
      Price_EUR = sapply(p$legs, function(x)
                         round(x$options[[1]]$price_eur, 0)
      )
    )
  })
  
  # Allocation
  output$allocation <- renderTable({
    p <- plan()
    if (is.null(p)) return(NULL)
    if (inherits(p, "trip_error") || is.null(p$allocation)) return(NULL)
    p$allocation
  })
  
  
  # Scenic discoveries (only when checkbox is on)
  output$discoveries <- renderTable({
    if (!input$scenic_only) return(NULL)
    
    p <- plan()
    if (inherits(p, "trip_error")) return(NULL)
    
    if (is.null(p$discoveries) || p$discoveries$n_found == 0) {
      return(data.frame(
        Message = paste("No scenic discoveries found within", input$radius_km, "km radius.")
      ))
    }
    
    disc <- p$discoveries$discoveries
    disc$Importance <- round(disc$Importance, 2)
    names(disc)[names(disc) == "Importance"] <- "Scenic_score"
    disc
  })
  
  output$map <- renderLeaflet({
    
    plan_result <- plan()
    
    if (is.null(plan_result) || inherits(plan_result, "trip_error"))
      return(leaflet() %>% addProviderTiles("CartoDB.Positron"))
    
    route_data <- cities[cities$city %in% plan_result$route, ]
    route_data <- route_data[match(plan_result$route, route_data$city), ]
    
    durations <- sapply(plan_result$legs, function(x)
      x$options[[1]]$duration_h
    )
    
    # fixed duration colors
    get_color <- function(d) {
      if (d < 2) {
        "#2563EB"       # blue
      } else if (d < 2.5) {
        "#10B981"       # green
      } else if (d < 3) {
        "#F59E0B"       # orange
      } else {
        "#DC2626"       # red
      }
    }
    
    leg_colors <- sapply(durations, get_color)
    
    m <- leaflet(route_data) %>%
      addProviderTiles("CartoDB.Positron")
    
    # ROUTE LINES
    for (i in seq_len(nrow(route_data) - 1)) {
      
      m <- m %>%
        addPolylines(
          lng = route_data$lon[i:(i+1)],
          lat = route_data$lat[i:(i+1)],
          color = leg_colors[i],
          weight = 5,
          opacity = 0.9
        )
    }
    
    # NUMBERED MARKERS
    m <- m %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = 10,
        color = "#1D4ED8",
        fillColor = "#2563EB",
        fillOpacity = 1,
        stroke = TRUE,
        weight = 2,
      ) %>%
      
      addLabelOnlyMarkers(
        lng = ~lon,
        lat = ~lat,
        label = ~as.character(seq_along(city)),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "white",
            "font-weight" = "bold",
            "font-size" = "14px"
          )
        )
      )
    # DISCOVERY MARKERS
    if (!is.null(plan_result$discoveries) && plan_result$discoveries$n_found > 0) {
      disc <- plan_result$discoveries$discoveries
      m <- m %>%
        addCircleMarkers(
          lng   = disc$Lon,
          lat   = disc$Lat,
          radius = 7,
          color = "#7C3AED",
          fillColor = "#8B5CF6",
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          label = disc$City
        )
    }
    # CLEAN LEGEND
    m %>%
      addLegend(
        position = "bottomright",
        colors = c("#2563EB", "#10B981", "#F59E0B", "#DC2626", "#8B5CF6"),
        labels = c("< 2 h", "2 - 2.5 h", "2.5 - 3 h", "> 3 h", "Discovery"),
        title = "Duration / Points",
        opacity = 1
      )
  })
}
shinyApp(ui, server)
