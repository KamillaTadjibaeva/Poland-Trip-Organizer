#' Convenience wrapper: build a [TripPlanner] and immediately call `$plan()`.
#'
#' @param selected Character vector of city names to visit.
#' @param flight_in City name where traveller arrives.
#' @param flight_out City name where traveller departs (or NULL for round-trip).
#' @param start_date Trip start date (Date or "YYYY-MM-DD" string).
#' @param end_date Trip end date (Date or "YYYY-MM-DD" string).
#' @param transport Transport mode(s): "plane" / "train" / "bus" / "car".
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
  tp <- suppressMessages(TripPlanner$new(
    selected   = selected,
    flight_in  = flight_in,
    flight_out = flight_out,
    start_date = start_date,
    end_date   = end_date,
    transport  = transport,
    style      = style,
    cities     = cities
  ))
  tp$plan(...)
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
