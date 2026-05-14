
# Importance Score Calculation

#' Calculate composite importance scores for cities
#'
#' Computes weighted importance scores from population, historical
#' significance, cultural heritage, and POI count. Works on single
#' values or vectors (vectorised using pmin for efficiency).
#'
#' @param population Numeric vector of city populations
#' @param historical_score Numeric vector of historical scores (1-10)
#' @param cultural_score Numeric vector of cultural scores (1-10)
#' @param poi_count Numeric vector of POI counts
#' @param weights Numeric vector of 4 weights
#'   (population, historical, cultural, poi). Must sum to 1.
#'
#' @return Numeric vector of importance scores (0-1 range)
#' @export
calculate_importance <- function(population, historical_score,
                                  cultural_score, poi_count,
                                  weights = c(population = 0.15,
                                              historical = 0.35,
                                              cultural   = 0.35,
                                              poi        = 0.15)) {
  if (!is.numeric(population) || !is.numeric(historical_score) ||
      !is.numeric(cultural_score) || !is.numeric(poi_count)) {
    stop("All inputs (population, historical_score, cultural_score, poi_count) must be numeric.")
  }

  if (length(population) == 0) {
    stop("Input vectors must not be empty.")
  }

  if (any(population < 0, na.rm = TRUE)) {
    stop("'population' must be non-negative.")
  }
  if (any(poi_count < 0, na.rm = TRUE)) {
    stop("'poi_count' must be non-negative.")
  }

  w <- as.numeric(weights)
  if (length(w) != 4) {
    stop("'weights' must have exactly 4 elements.")
  }


  # Normalise population to [0, 1] using log scale
  # (prevents Warsaw from dominating everything)
  norm_pop  <- pmin(log1p(population) / log1p(2000000), 1.0)

  # Normalise scores (already on 1-10 scale)
  norm_hist <- historical_score / 10.0
  norm_cult <- cultural_score / 10.0

  # Normalise POI count
  norm_poi  <- pmin(log1p(poi_count) / log1p(1500), 1.0)

  # Weighted combination
  scores <- w[1] * norm_pop + w[2] * norm_hist +
            w[3] * norm_cult + w[4] * norm_poi

  return(scores)
}

