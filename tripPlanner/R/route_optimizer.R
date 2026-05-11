#' RouteOptimizer — high-level orchestrator (R6).
#'
#' Encapsulates the user's trip request and exposes the pipeline:
#' validate inputs -> build cost matrix -> solve TSP -> attach transport
#' suggestions for each leg.
#'
#' @export
RouteOptimizer <- R6::R6Class(
  "RouteOptimizer",
  public = list(
    #' @field cities      data.frame of available cities (loaded from CSV).
    cities      = NULL,
    #' @field selected    character vector of city names to visit.
    selected    = NULL,
    #' @field flight_in   start city.
    flight_in   = NULL,
    #' @field flight_out  end city (or NULL).
    flight_out  = NULL,
    #' @field start_date  trip start date.
    start_date  = NULL,
    #' @field end_date    trip end date.
    end_date    = NULL,
    #' @field transport   "plane" / "train" / "bus" / "car".
    transport   = NULL,
    #' @field style       "scenic" / "fastest" / "cheapest".
    style       = NULL,

    #' @description Construct a new `RouteOptimizer`.
    #' @param selected   Cities the user wants to visit (must include both
    #'   `flight_in` and `flight_out`).
    #' @param flight_in  Arrival city.
    #' @param flight_out Departure city (defaults to `flight_in`).
    #' @param start_date,end_date Trip dates (anything coercible to `Date`).
    #' @param transport,style See package overview.
    #' @param cities Optional override for the cities reference table.
    initialize = function(selected, flight_in, flight_out = NULL,
                          start_date, end_date,
                          transport = "train", style = "fastest",
                          cities = NULL) {
      self$cities <- cities %||% load_cities()
      .assert_character(selected, min_len = 2L)
      if (anyDuplicated(tolower(selected))) {
        stop("`selected` contains duplicate cities.", call. = FALSE)
      }
      .assert_string(flight_in)
      if (is.null(flight_out)) flight_out <- flight_in
      .assert_string(flight_out)
      if (!tolower(flight_in) %in% tolower(selected)) {
        selected <- c(flight_in, selected)
      }
      if (!tolower(flight_out) %in% tolower(selected)) {
        selected <- c(selected, flight_out)
      }
      sd <- .assert_date(start_date, "start_date")
      ed <- .assert_date(end_date, "end_date")
      if (ed < sd) stop("`end_date` must be on or after `start_date`.", call. = FALSE)
      .assert_choice(transport, .TRANSPORT_TYPES, "transport")
      .assert_choice(style,     .TRAVEL_STYLES,   "style")

      # Eagerly validate that every requested city exists in the reference
      # table — surfaces typos at construction time, not deep in the pipeline.
      .lookup_cities(selected, self$cities)

      self$selected   <- selected
      self$flight_in  <- flight_in
      self$flight_out <- flight_out
      self$start_date <- sd
      self$end_date   <- ed
      self$transport  <- transport
      self$style      <- style
      invisible(self)
    },

    #' @description Resolve the selected city names to a sub-data.frame.
    selected_cities = function() {
      .lookup_cities(self$selected, self$cities)
    },

    #' @description Build the cost matrix matching the user's preferences.
    cost_matrix = function() {
      build_cost_matrix(self$selected_cities(),
                        transport = self$transport,
                        style     = self$style)
    },

    #' @description Solve the TSP and return a `trip_plan` (S3) object.
    #' @param transport_provider Optional function used to fetch transport
    #'   options for each leg. See [get_transport_options()].
    #' @param with_allocation Logical, also compute per-city day allocation
    #'   from the trip length (default: TRUE if the cities table carries
    #'   the scoring columns, otherwise FALSE).
    #' @param discover Logical, also find scenic detours near the route.
    #'   Defaults to TRUE when `style == "scenic"`, FALSE otherwise.
    #' @param radius_km       Detour radius for route discovery.
    #' @param max_suggestions Cap on number of detour suggestions.
    plan = function(transport_provider = get_transport_options,
                    with_allocation = NULL,
                    discover        = NULL,
                    radius_km       = 30,
                    max_suggestions = 10L) {
      cm  <- self$cost_matrix()
      round_trip <- identical(self$flight_in, self$flight_out)
      sol <- solve_tsp(cm,
                       start = self$flight_in,
                       end   = if (round_trip) NULL else self$flight_out)

      route <- sol$order
      total_cost <- sol$cost
      # For a round trip, close the loop: append the start city and add
      # the return-leg cost so the user actually flies/travels back home.
      if (round_trip) {
        route <- c(route, self$flight_in)
        total_cost <- total_cost + unname(cm[tail(sol$order, 1), self$flight_in])
      }

      # Build legs
      legs <- lapply(seq_len(length(route) - 1L), function(i) {
        from <- route[i]; to <- route[i + 1L]
        list(
          from        = from,
          to          = to,
          leg_cost    = unname(cm[from, to]),
          options     = transport_provider(
                          from = from, to = to,
                          date = self$start_date + (i - 1L),
                          transport = self$transport,
                          style     = self$style,
                          cities    = self$cities)
        )
      })

      # ---- Time allocation (Kamilla's feature #2) --------------------
      score_cols <- c("population", "historical_score",
                      "cultural_score", "poi_count")
      has_scores <- all(score_cols %in% names(self$cities))
      if (is.null(with_allocation)) with_allocation <- has_scores
      allocation <- NULL
      if (isTRUE(with_allocation)) {
        if (!has_scores) {
          warning("Cities table lacks scoring columns; skipping time allocation.",
                  call. = FALSE)
        } else {
          total_days <- as.integer(self$end_date - self$start_date) + 1L
          # Unique visit order (drop the closing loop city for round trips).
          visit <- if (round_trip) head(route, -1L) else route
          sub   <- .lookup_cities(visit, self$cities)
          allocation <- tryCatch(
            allocate_days(sub, total_days = total_days),
            error = function(e) { warning(conditionMessage(e), call. = FALSE); NULL }
          )
        }
      }

      # ---- Scenic route discovery (Kamilla's feature #3) -------------
      # Only when the user picked a scenic journey.
      if (is.null(discover)) {
        discover <- identical(self$style, "scenic") && has_scores
      }
      discoveries <- NULL
      if (isTRUE(discover)) {
        if (!has_scores) {
          warning("Cities table lacks scoring columns; skipping discovery.",
                  call. = FALSE)
        } else {
          visit <- if (round_trip) head(route, -1L) else route
          if (length(visit) >= 2L) {
            discoveries <- tryCatch(
              find_route_discoveries(visit, self$cities,
                                     radius_km = radius_km,
                                     max_suggestions = max_suggestions),
              error = function(e) { warning(conditionMessage(e), call. = FALSE); NULL }
            )
          }
        }
      }

      structure(
        list(
          route       = route,
          total_cost  = total_cost,
          method      = sol$method,
          transport   = self$transport,
          style       = self$style,
          start_date  = self$start_date,
          end_date    = self$end_date,
          legs        = legs,
          allocation  = allocation,
          discoveries = discoveries
        ),
        class = "trip_plan"
      )
    },

    #' @description Allocate trip days across the selected cities.
    #'
    #' Distributes `end_date - start_date + 1` days proportionally to each
    #' city's importance score (population + historical + cultural + POI).
    #'
    #' @param min_days Minimum days per city.
    #' @param weights  Forwarded to [calculate_importance()].
    #' @return data.frame with columns city, importance, days.
    allocate_time = function(min_days = 1L,
                             weights = c(population = 0.15,
                                         historical = 0.35,
                                         cultural   = 0.35,
                                         poi        = 0.15)) {
      sub        <- self$selected_cities()
      total_days <- as.integer(self$end_date - self$start_date) + 1L
      allocate_days(sub, total_days = total_days,
                    min_days = min_days, weights = weights)
    },

    #' @description Find scenic detours along the planned route.
    #'
    #' Only meaningful when `style = "scenic"`. The check can be bypassed
    #' with `force = TRUE`.
    #'
    #' @param radius_km       Detour radius in km.
    #' @param max_suggestions Maximum number of suggestions to return.
    #' @param force           Skip the scenic-style guard.
    discover_route = function(radius_km = 30, max_suggestions = 10L,
                              force = FALSE) {
      if (!isTRUE(force) && !identical(self$style, "scenic")) {
        message("Route discovery is only applied for scenic trips. ",
                "Pass force = TRUE to override.")
        return(invisible(NULL))
      }
      # Resolve the route order from the optimiser, then look in the full
      # cities table (not just the selected ones) so we can surface places
      # the user didn't pick.
      cm  <- self$cost_matrix()
      round_trip <- identical(self$flight_in, self$flight_out)
      sol <- solve_tsp(cm,
                       start = self$flight_in,
                       end   = if (round_trip) NULL else self$flight_out)
      find_route_discoveries(sol$order, self$cities,
                             radius_km = radius_km,
                             max_suggestions = max_suggestions)
    },

    #' @description Pretty print.
    print = function(...) {
      cat("<RouteOptimizer>\n")
      cat("  cities:    ", paste(self$selected, collapse = ", "), "\n")
      cat("  flight in: ", self$flight_in, "\n")
      cat("  flight out:", self$flight_out, "\n")
      cat("  dates:     ", format(self$start_date), "->", format(self$end_date), "\n")
      cat("  transport: ", self$transport, "\n")
      cat("  style:     ", self$style, "\n")
      invisible(self)
    }
  )
)

#' Convenience wrapper: build a [RouteOptimizer] and immediately call `$plan()`.
#'
#' @inheritParams RouteOptimizer
#' @param ... Forwarded to `$plan()`.
#' @return A `trip_plan` S3 object.
#' @export
plan_trip <- function(selected, flight_in, flight_out = NULL,
                      start_date, end_date,
                      transport = "train", style = "fastest",
                      cities = NULL, ...) {
  ro <- RouteOptimizer$new(selected, flight_in, flight_out,
                           start_date, end_date,
                           transport, style, cities)
  ro$plan(...)
}

# small infix helper
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @export
print.trip_plan <- function(x, ...) {
  cat("Trip plan (", x$style, " / ", x$transport, ")\n", sep = "")
  cat("Route: ", paste(x$route, collapse = " -> "), "\n")
  cat(sprintf("Total cost (objective): %.2f  [%s solver]\n",
              x$total_cost, x$method))
  cat("Legs:\n")
  for (i in seq_along(x$legs)) {
    leg <- x$legs[[i]]
    cat(sprintf("  %d. %-12s -> %-12s  [%.2f]  options: %d\n",
                i, leg$from, leg$to, leg$leg_cost, length(leg$options)))
  }
  if (!is.null(x$allocation)) {
    cat("\nDay allocation:\n")
    a <- x$allocation
    for (i in seq_len(nrow(a))) {
      cat(sprintf("  %-15s %d days  (importance %.3f)\n",
                  a$city[i], a$days[i], a$importance[i]))
    }
  }
  if (!is.null(x$discoveries) && x$discoveries$n_found > 0L) {
    cat(sprintf("\nScenic detour suggestions within %g km:\n",
                x$discoveries$radius_km))
    d <- x$discoveries$discoveries
    for (i in seq_len(nrow(d))) {
      cat(sprintf("  - %-18s  %5.1f km from %-12s  (imp %.2f)\n",
                  d$city[i], d$distance_km[i],
                  d$nearest_route_city[i], d$importance[i]))
    }
  }
  invisible(x)
}

#' @export
summary.trip_plan <- function(object, ...) {
  legs_df <- data.frame(
    leg       = seq_along(object$legs),
    from      = vapply(object$legs, `[[`, character(1), "from"),
    to        = vapply(object$legs, `[[`, character(1), "to"),
    cost      = vapply(object$legs, `[[`, numeric(1),   "leg_cost"),
    n_options = vapply(object$legs, function(l) length(l$options), integer(1)),
    stringsAsFactors = FALSE
  )
  structure(list(
    route       = object$route,
    total_cost  = object$total_cost,
    method      = object$method,
    legs        = legs_df
  ), class = "summary.trip_plan")
}
