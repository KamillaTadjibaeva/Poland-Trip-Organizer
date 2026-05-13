#!/usr/bin/env Rscript
# demo.R — human-friendly end-to-end planning example.
suppressPackageStartupMessages(library(PolandTripPlanner))

# --- User input (this is what Vika's UI will collect) ----------------------
input <- list(
  selected   = c("Warsaw", "Krakow", "Wroclaw", "Poznan", "Gdansk", "Lublin"),
  flight_in  = "Warsaw",
  flight_out = "Gdansk",
  start_date = "2026-06-01",
  end_date   = "2026-06-10",
  transport  = "train",
  style      = "fastest"
)

# --- Pretty-print the user's choices --------------------------------------
hr <- function() cat(strrep("-", 72), "\n", sep = "")
hr()
cat("YOUR TRIP\n")
hr()
cat("Cities to visit  : ", paste(input$selected, collapse = ", "), "\n", sep = "")
cat("Flying into      : ", input$flight_in, "\n", sep = "")
cat("Flying out from  : ", input$flight_out, "\n", sep = "")
cat("Travel dates     : ", input$start_date, " to ", input$end_date,
    "  (", as.integer(as.Date(input$end_date) - as.Date(input$start_date)),
    " days)\n", sep = "")
cat("Preferred transport: ", input$transport,
    "  (used between cities once you've landed)\n", sep = "")
cat("Travel style       : ", input$style, "  ",
    switch(input$style,
           fastest  = "(minimise total travel time)",
           cheapest = "(minimise total ticket price)",
           scenic   = "(prefer scenic ground transport)"),
    "\n", sep = "")
cat("\n")

# --- Plan ------------------------------------------------------------------
plan <- plan_trip(
  selected   = input$selected,
  flight_in  = input$flight_in,
  flight_out = input$flight_out,
  start_date = input$start_date,
  end_date   = input$end_date,
  transport  = input$transport,
  style      = input$style
)

hr()
cat("RECOMMENDED ROUTE\n")
hr()
cat(paste(plan$route, collapse = "  ->  "), "\n", sep = "")
cat("\n")

# Translate the abstract objective ("cost") back into something a
# passenger actually understands.
unit_label <- switch(plan$style,
                     fastest  = "hours of travelling",
                     cheapest = "EUR of tickets",
                     scenic   = "km along the ground (lower = less detour)")

cat(sprintf("Total %-25s : %.1f\n", unit_label, plan$total_cost))
cat(sprintf("Optimiser used                 : %s\n",
            switch(plan$method,
                   exact     = "exact (Held-Karp dynamic programming)",
                   heuristic = "heuristic (nearest-neighbour + 2-opt)")))
cat("\n")

# --- Per-leg breakdown -----------------------------------------------------
hr()
cat("LEG-BY-LEG PLAN\n")
hr()

mode_label <- function(m) switch(m,
  plane = "flight", train = "train", bus = "bus", car = "car drive", m
)

for (i in seq_along(plan$legs)) {
  leg <- plan$legs[[i]]
  cat(sprintf("\nLeg %d:  %s  ->  %s\n", i, leg$from, leg$to))
  cat(sprintf("        Optimiser cost for this leg: %.2f %s\n",
              leg$leg_cost,
              switch(plan$style,
                     fastest = "hours", cheapest = "EUR",
                     scenic  = "km (penalised for flying)")))

  if (!length(leg$options)) {
    cat("        (no transport options returned)\n")
    next
  }

  # The first option after style-filtering is the recommended one.
  best <- leg$options[[1]]
  cat(sprintf("    >>> Recommended %s (best %s match):\n",
              mode_label(best$mode), plan$style))
  cat(sprintf("        Departs : %s\n", format(best$depart, "%a %d %b %H:%M")))
  cat(sprintf("        Duration: %.1f h\n", best$duration_h))
  cat(sprintf("        Price   : ~ EUR %.0f\n", best$price_eur))
  cat(sprintf("        Provider: %s\n",
              if (!is.null(best$provider)) best$provider else "n/a"))

  # Show the runners-up so the passenger sees the trade-off.
  if (length(leg$options) > 1L) {
    cat("        Other options considered:\n")
    for (o in leg$options[-1]) {
      cat(sprintf("          - %s at %s | %.1fh | EUR %.0f\n",
                  mode_label(o$mode),
                  format(o$depart, "%a %H:%M"),
                  o$duration_h, o$price_eur))
    }
  }
}

cat("\n")
hr()
cat("HOW TO READ THIS\n")
hr()
cat(
"* The 'optimiser cost' is the number the route solver minimises.\n",
"  - style = fastest  -> cost is travel time in hours\n",
"  - style = cheapest -> cost is approximate ticket price in EUR\n",
"  - style = scenic   -> cost is ground-distance, with a penalty for flights\n",
"* Each leg shows the recommended departure, its duration door-to-door,\n",
"  the rough price, and which provider it came from.\n",
"  'mock-*' means the live API was unavailable and a deterministic estimate\n",
"  was used so the demo still works offline.\n",
sep = "")
