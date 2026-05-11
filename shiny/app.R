# shiny/app.R
# Minimal reference Shiny app demonstrating how to consume tripPlanner.
# Vika: feel free to replace the layout, but the input -> plan_trip ->
# render flow shown here is the entire integration surface.

library(shiny)
library(tripPlanner)

cities <- load_cities()

ui <- fluidPage(
  titlePanel("Trip Planner — demo UI"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cities", "Cities to visit",
                  choices = cities$city, multiple = TRUE,
                  selected = c("Warsaw", "Krakow", "Wroclaw", "Gdansk")),
      selectInput("flight_in",  "Flight-in city",  choices = cities$city,
                  selected = "Warsaw"),
      selectInput("flight_out", "Flight-out city", choices = cities$city,
                  selected = "Gdansk"),
      dateInput("start_date", "Start date", value = Sys.Date() + 30),
      dateInput("end_date",   "End date",   value = Sys.Date() + 38),
      selectInput("transport", "Transport",
                  choices = c("plane", "train", "bus", "car"),
                  selected = "train"),
      selectInput("style", "Travel style",
                  choices = c("fastest", "cheapest", "scenic"),
                  selected = "fastest"),
      helpText("The plan recomputes automatically whenever you change any input."),
      tags$small(textOutput("counts"))
    ),
    mainPanel(
      h3("Recommended route"),
      verbatimTextOutput("route"),
      h3("Time allocation per city"),
      tableOutput("allocation"),
      h3("Scenic detour suggestions"),
      helpText("Only computed when style = scenic."),
      tableOutput("discoveries"),
      h3("Per-leg breakdown"),
      uiOutput("legs"),
      verbatimTextOutput("err")
    )
  )
)

server <- function(input, output, session) {
  plan <- reactive({
    req(input$cities, input$flight_in, input$flight_out,
        input$start_date, input$end_date, input$transport, input$style)
    tryCatch(
      plan_trip(
        selected   = input$cities,
        flight_in  = input$flight_in,
        flight_out = input$flight_out,
        start_date = input$start_date,
        end_date   = input$end_date,
        transport  = input$transport,
        style      = input$style
      ),
      error = function(e) structure(list(error = conditionMessage(e)),
                                    class = "trip_error")
    )
  }) |> debounce(400)

  output$counts <- renderText({
    n_sel <- length(input$cities)
    extra <- length(unique(c(input$flight_in, input$flight_out,
                             input$cities))) - n_sel
    n_total <- n_sel + extra
    sprintf("Selected %d city/cities (+ %d auto-added endpoints) -> %d legs.",
            n_sel, extra, max(0L, n_total - 1L))
  })

  output$err <- renderText({
    p <- plan()
    if (inherits(p, "trip_error")) paste("Error:", p$error) else ""
  })

  output$route <- renderText({
    p <- plan(); if (inherits(p, "trip_error")) return("")
    unit <- switch(p$style, fastest = "h", cheapest = "EUR", scenic = "km")
    paste0(paste(p$route, collapse = "  ->  "),
           sprintf("\n\nTotal cost: %.1f %s   (solver: %s, %d legs)",
                   p$total_cost, unit, p$method, length(p$legs)))
  })

  output$allocation <- renderTable({
    p <- plan(); if (inherits(p, "trip_error") || is.null(p$allocation)) return(NULL)
    p$allocation
  })

  output$discoveries <- renderTable({
    p <- plan(); if (inherits(p, "trip_error")) return(NULL)
    if (is.null(p$discoveries) || p$discoveries$n_found == 0L) return(NULL)
    p$discoveries$discoveries
  })

  output$legs <- renderUI({
    p <- plan(); if (inherits(p, "trip_error")) return(NULL)
    tagList(lapply(seq_along(p$legs), function(i) {
      leg  <- p$legs[[i]]
      best <- if (length(leg$options)) leg$options[[1]] else NULL
      tags$div(style = "border:1px solid #ddd;padding:8px;margin:6px 0;",
        tags$h4(sprintf("Leg %d: %s -> %s", i, leg$from, leg$to)),
        if (is.null(best)) tags$em("no transport options")
        else tags$ul(
          tags$li(sprintf("Mode: %s", best$mode)),
          tags$li(sprintf("Departs: %s",
                          format(best$depart, "%a %d %b %H:%M"))),
          tags$li(sprintf("Duration: %.1f h", best$duration_h %||% NA)),
          tags$li(sprintf("Price: ~ EUR %.0f", best$price_eur %||% NA)),
          tags$li(sprintf("Provider: %s",
                          if (!is.null(best$provider)) best$provider else "n/a"))
        )
      )
    }))
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
