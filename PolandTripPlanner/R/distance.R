#' Vectorised great-circle distance matrix (km) for a set of cities.
#'
#' Thin R wrapper around the C++ `haversine_matrix_cpp()` routine.
#'
#' @param cities A data.frame with `city`, `lat`, `lon` columns (e.g.
#'   the output of [load_cities()] subset to the user's selection).
#' @return A numeric symmetric matrix with `dimnames` set to `cities$city`.
#' @export
haversine_matrix <- function(cities) {
  if (!is.data.frame(cities)) stop("`cities` must be a data.frame.", call. = FALSE)
  if (!all(c("city", "lat", "lon") %in% names(cities))) {
    stop("`cities` must have columns: city, lat, lon.", call. = FALSE)
  }
  if (nrow(cities) < 2L) stop("Need at least two cities.", call. = FALSE)
  m <- haversine_matrix_cpp(as.numeric(cities$lat), as.numeric(cities$lon))
  dimnames(m) <- list(cities$city, cities$city)
  m
}

#' Build a "cost" matrix between cities for a given travel style and transport.
#'
#' The TSP solver minimises the sum of entries in this matrix. Costs are
#' derived from the great-circle distance and modulated by transport speed
#' and the user's travel style:
#' \describe{
#'   \item{`fastest`}{cost = travel time in hours}
#'   \item{`cheapest`}{cost = approx. monetary cost (EUR) per leg}
#'   \item{`scenic`}{cost = distance, plus a penalty for plane legs (which
#'     bypass the landscape)}
#' }
#'
#' `transport` may be a vector of modes, in which case the cost for each
#' leg is the minimum across the selected modes — i.e. the optimiser assumes
#' the traveller will pick the best mode available on each leg.
#'
#' @param cities Cities data.frame (rows in the visit set).
#' @param transport Character vector of modes (any of `"plane"`, `"train"`,
#'   `"bus"`, `"car"`). Length 1 = single mode (legacy behaviour).
#' @param style    One of `"fastest"`, `"cheapest"`, `"scenic"`.
#' @return A numeric square cost matrix with `dimnames` set to `cities$city`.
#' @export
build_cost_matrix <- function(cities,
                              transport = c("train", "plane", "bus", "car"),
                              style     = c("fastest", "cheapest", "scenic")) {
  # `transport` may be a single mode or a multi-select. match.arg only works
  # for a single value, so we validate manually.
  if (missing(transport)) transport <- "train"
  if (!is.character(transport) || !length(transport)) {
    stop("`transport` must be a non-empty character vector.", call. = FALSE)
  }
  bad <- setdiff(transport, .TRANSPORT_TYPES)
  if (length(bad)) {
    stop("Unknown transport mode(s): ", paste(bad, collapse = ", "),
         call. = FALSE)
  }
  style <- match.arg(style)

  d <- haversine_matrix(cities) # km, symmetric, zero diagonal

  # Approximate door-to-door speeds (km/h) and EUR/km — rough heuristics
  # that are adequate for ordering legs.
  speed_t      <- c(plane = 450,  train = 120,  bus = 70,   car = 90)
  eur_per_km_t <- c(plane = 0.18, train = 0.10, bus = 0.05, car = 0.12)
  overhead_t   <- c(plane = 2.5,  train = 0.3,  bus = 0.4,  car = 0.2)
  fixed_eur_t  <- c(plane = 40,   train = 5,    bus = 2,    car = 0)

  cost_for <- function(mode) {
    switch(
      style,
      fastest  = d / speed_t[[mode]] + overhead_t[[mode]] * (d > 0),
      cheapest = d * eur_per_km_t[[mode]] + fixed_eur_t[[mode]] * (d > 0),
      scenic   = d * (if (mode == "plane") 3.0 else 1.0)
    )
  }

  # Per-leg minimum across all selected modes.
  cost <- cost_for(transport[[1]])
  for (m in transport[-1]) cost <- pmin(cost, cost_for(m))
  diag(cost) <- 0
  dimnames(cost) <- dimnames(d)
  cost
}
