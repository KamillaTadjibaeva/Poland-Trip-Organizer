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
    #' @field transport   character vector of allowed transport modes
    #'   ("plane" / "train" / "bus" / "car"). Length 1 = single mode,
    #'   length > 1 = the optimiser picks the best mode per leg.
    transport   = NULL,
    #' @field style       "scenic" / "fastest" / "cheapest".
    style       = NULL,

    #' @description Construct a new `RouteOptimizer`.
    #' @param selected   Cities the user wants to visit (must include both
    #'   `flight_in` and `flight_out`).
    #' @param flight_in  Arrival city.
    #' @param flight_out Departure city (defaults to `flight_in`).
    #' @param start_date,end_date Trip dates (anything coercible to `Date`).
    #' @param transport Character vector of one or more modes; legs are
    #'   costed against the best (cheapest/fastest/most-scenic) of these.
    #' @param style See package overview.
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
      if (!is.character(transport) || !length(transport)) {
        stop("`transport` must be a non-empty character vector.", call. = FALSE)
      }
      bad <- setdiff(transport, .TRANSPORT_TYPES)
      if (length(bad)) {
        stop("Unknown transport mode(s): ", paste(bad, collapse = ", "),
             call. = FALSE)
      }
      transport <- unique(transport)
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
    plan = function(transport_provider = get_transport_options) {
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

      structure(
        list(
          route       = route,
          total_cost  = total_cost,
          method      = sol$method,
          transport   = self$transport,
          style       = self$style,
          start_date  = self$start_date,
          end_date    = self$end_date,
          legs        = legs
        ),
        class = "trip_plan"
      )
    },

    #' @description Pretty print.
    print = function(...) {
      cat("<RouteOptimizer>\n")
      cat("  cities:    ", paste(self$selected, collapse = ", "), "\n")
      cat("  flight in: ", self$flight_in, "\n")
      cat("  flight out:", self$flight_out, "\n")
      cat("  dates:     ", format(self$start_date), "->", format(self$end_date), "\n")
      cat("  transport: ", paste(self$transport, collapse = ", "), "\n")
      cat("  style:     ", self$style, "\n")
      invisible(self)
    }
  )
)

#' Convenience wrapper: build a [RouteOptimizer] and immediately call `$plan()`.
#'
#' @param selected Character vector of city names to visit.
#' @param flight_in City name where traveller arrives.
#' @param flight_out City name where traveller departs (or NULL for round-trip).
#' @param start_date Trip start date (Date or "YYYY-MM-DD" string).
#' @param end_date Trip end date (Date or "YYYY-MM-DD" string).
#' @param transport Transport mode: "plane", "train", "bus", or "car".
#' @param style Travel style: "fastest", "cheapest", or "scenic".
#' @param cities Optional override for the cities reference data.frame.
#' @param ... Forwarded to `$plan()`.
#' @return A `trip_plan` S3 object.
#' @export
#' @examples
#' \dontrun{
#' plan <- plan_trip(
#'   selected = c("Warsaw", "Krakow", "Gdansk"),
#'   flight_in = "Warsaw", flight_out = "Gdansk",
#'   start_date = "2026-06-01", end_date = "2026-06-10",
#'   transport = "train", style = "fastest"
#' )
#' }
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
  cat("Trip plan (", x$style, " / ",
      paste(x$transport, collapse = "+"), ")\n", sep = "")
  cat("Route: ", paste(x$route, collapse = " -> "), "\n")
  cat(sprintf("Total cost (objective): %.2f  [%s solver]\n",
              x$total_cost, x$method))
  cat("Legs:\n")
  for (i in seq_along(x$legs)) {
    leg <- x$legs[[i]]
    cat(sprintf("  %d. %-12s -> %-12s  [%.2f]  options: %d\n",
                i, leg$from, leg$to, leg$leg_cost, length(leg$options)))
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
