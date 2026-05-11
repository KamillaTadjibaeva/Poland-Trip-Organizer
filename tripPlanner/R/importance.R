#' Compute composite "importance" scores for cities.
#'
#' Combines normalised population, historical significance, cultural heritage
#' and POI count into a single 0-1 score. Vectorised over the inputs.
#'
#' @param population        Numeric vector of populations (>= 0).
#' @param historical_score  Numeric vector on a 1-10 scale.
#' @param cultural_score    Numeric vector on a 1-10 scale.
#' @param poi_count         Numeric vector of POI counts (>= 0).
#' @param weights           Named numeric of length 4 (population, historical,
#'   cultural, poi). Need not sum to 1; treated as relative weights.
#' @return Numeric vector of scores.
#' @export
calculate_importance <- function(population, historical_score,
                                 cultural_score, poi_count,
                                 weights = c(population = 0.15,
                                             historical = 0.35,
                                             cultural   = 0.35,
                                             poi        = 0.15)) {
  if (!is.numeric(population) || !is.numeric(historical_score) ||
      !is.numeric(cultural_score) || !is.numeric(poi_count)) {
    stop("All score inputs must be numeric.", call. = FALSE)
  }
  if (length(population) == 0L) stop("Empty input.", call. = FALSE)
  if (any(population < 0, na.rm = TRUE)) stop("'population' must be >= 0.", call. = FALSE)
  if (any(poi_count  < 0, na.rm = TRUE)) stop("'poi_count' must be >= 0.",  call. = FALSE)
  w <- as.numeric(weights)
  if (length(w) != 4L) stop("'weights' must have 4 elements.", call. = FALSE)

  # Log-scale population so Warsaw doesn't drown out everything else.
  norm_pop  <- pmin(log1p(population) / log1p(2e6), 1.0)
  norm_hist <- historical_score / 10.0
  norm_cult <- cultural_score   / 10.0
  norm_poi  <- pmin(log1p(poi_count) / log1p(1500), 1.0)

  w[1] * norm_pop + w[2] * norm_hist + w[3] * norm_cult + w[4] * norm_poi
}

#' Allocate a fixed number of trip days across cities by importance.
#'
#' Each city receives `min_days` to start, then the remaining days are
#' distributed proportionally to its share of total importance. Any rounding
#' residual is added to the most important city so the totals match.
#'
#' @param cities      data.frame with columns: city (or name), population,
#'   historical_score, cultural_score, poi_count.
#' @param total_days  Integer trip length in days (>= n_cities).
#' @param min_days    Minimum days per city (default: 1).
#' @param weights     Forwarded to [calculate_importance()].
#' @return A data.frame with columns city, importance, days.
#' @export
allocate_days <- function(cities, total_days, min_days = 1L,
                          weights = c(population = 0.15,
                                      historical = 0.35,
                                      cultural   = 0.35,
                                      poi        = 0.15)) {
  if (!is.data.frame(cities)) stop("'cities' must be a data.frame.", call. = FALSE)
  needed <- c("population", "historical_score", "cultural_score", "poi_count")
  miss <- setdiff(needed, names(cities))
  if (length(miss)) {
    stop("cities is missing columns: ", paste(miss, collapse = ", "),
         "\nDoes the CSV include the scoring columns?", call. = FALSE)
  }
  name_col <- if ("city" %in% names(cities)) "city" else "name"
  n <- nrow(cities)
  if (n < 1L) stop("No cities supplied.", call. = FALSE)
  if (!is.numeric(total_days) || length(total_days) != 1L || total_days < n) {
    stop(sprintf("'total_days' must be >= %d (one per city).", n), call. = FALSE)
  }
  if (!is.numeric(min_days) || min_days < 1L) {
    stop("'min_days' must be >= 1.", call. = FALSE)
  }

  imp <- calculate_importance(cities$population, cities$historical_score,
                              cities$cultural_score, cities$poi_count,
                              weights)
  remaining <- total_days - n * min_days
  share     <- imp / sum(imp)

  # Largest-remainder method: take the integer floor of the proportional
  # share, then hand out the leftover days to the cities with the largest
  # fractional remainder. Guarantees each city gets at least `min_days`
  # and that the totals sum exactly to `total_days`.
  raw   <- remaining * share
  base  <- floor(raw)
  rem   <- raw - base
  short <- as.integer(remaining - sum(base))
  if (short > 0L) {
    winners <- order(-rem, -imp)[seq_len(short)]
    base[winners] <- base[winners] + 1L
  }
  days <- as.integer(min_days) + as.integer(base)

  data.frame(
    city       = cities[[name_col]],
    importance = round(imp, 4),
    days       = as.integer(days),
    stringsAsFactors = FALSE
  )
}
