#!/usr/bin/env Rscript
# demo.R — human-friendly end-to-end planning example.
suppressPackageStartupMessages(library(tripPlanner))

# --- User input (this is what Vika's UI will collect) ----------------------
input <- list(
  selected   = c("Warsaw", "Krakow", "Wroclaw", "Poznan", "Gdansk", "Lublin"),
  flight_in  = "Warsaw",
  flight_out = "Gdansk",
  start_date = "2026-06-01",
  end_date   = "2026-06-10",
  transport  = "car",
  style      = "scenic"
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
    "  (", as.integer(as.Date(input$end_date) - as.Date(input$start_date)) + 1L,
    " days)\n", sep = "")
cat("Preferred transport: ", input$transport, "\n", sep = "")
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
  style      = input$style,
  radius_km  = 40
)

hr(); cat("RECOMMENDED ROUTE\n"); hr()
cat(paste(plan$route, collapse = "  ->  "), "\n\n", sep = "")

unit_label <- switch(plan$style,
                     fastest  = "hours of travelling",
                     cheapest = "EUR of tickets",
                     scenic   = "km along the ground (lower = less detour)")
cat(sprintf("Total %-25s : %.1f\n", unit_label, plan$total_cost))
cat(sprintf("Optimiser used                 : %s\n\n",
            switch(plan$method,
                   exact     = "exact (Held-Karp dynamic programming)",
                   heuristic = "heuristic (nearest-neighbour + 2-opt)")))

# --- Per-leg breakdown -----------------------------------------------------
hr(); cat("LEG-BY-LEG PLAN\n"); hr()

mode_label <- function(m) switch(m,
  plane = "flight", train = "train", bus = "bus", car = "car drive", m
)

for (i in seq_along(plan$legs)) {
  leg <- plan$legs[[i]]
  cat(sprintf("\nLeg %d:  %s  ->  %s\n", i, leg$from, leg$to))
  cat(sprintf("        Optimiser cost for this leg: %.2f\n", leg$leg_cost))

  if (!length(leg$options)) { cat("        (no transport options)\n"); next }
  best <- leg$options[[1]]
  cat(sprintf("    >>> Recommended %s (best %s match):\n",
              mode_label(best$mode), plan$style))
  cat(sprintf("        Departs : %s\n", format(best$depart, "%a %d %b %H:%M")))
  cat(sprintf("        Duration: %.1f h\n", best$duration_h))
  cat(sprintf("        Price   : ~ EUR %.0f\n", best$price_eur))
  cat(sprintf("        Provider: %s\n",
              if (!is.null(best$provider)) best$provider else "n/a"))
}

# --- Time allocation (Kamilla feature #2) ----------------------------------
if (!is.null(plan$allocation)) {
  cat("\n"); hr(); cat("TIME ALLOCATION PER CITY\n"); hr()
  a <- plan$allocation
  for (i in seq_len(nrow(a))) {
    cat(sprintf("  %-15s %2d days   (importance %.3f)\n",
                a$city[i], a$days[i], a$importance[i]))
  }
}

# --- Scenic detour discovery (Kamilla feature #3) --------------------------
if (!is.null(plan$discoveries) && plan$discoveries$n_found > 0L) {
  cat("\n"); hr()
  cat(sprintf("SCENIC DETOUR SUGGESTIONS  (radius %g km)\n",
              plan$discoveries$radius_km))
  hr()
  d <- plan$discoveries$discoveries
  for (i in seq_len(nrow(d))) {
    cat(sprintf("  - %-18s  %5.1f km from %-12s  (importance %.2f)\n",
                d$city[i], d$distance_km[i],
                d$nearest_route_city[i], d$importance[i]))
  }
}

cat("\n")
