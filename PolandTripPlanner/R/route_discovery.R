
#' Find interesting cities near the travel route
#'
#' For each candidate city not in the itinerary, computes its distance
#' to the closest route city using C++ haversine. Cities within the
#' radius are ranked by importance score and returned.
#'
#' @param route_cities Character vector of city names (in visit order)
#' @param all_cities Data.frame of all Polish cities (from get_polish_cities())
#' @param radius_km Vicinity radius in km (default: 30)
#' @param max_suggestions Maximum number of suggestions (default: 10)
#'
#' @return A list with route discovery results
#' @export
find_route_discoveries <- function(route_cities, all_cities,
                                    radius_km = 30,
                                    max_suggestions = 10) {

  if (!is.character(route_cities) || length(route_cities) < 2) {
    stop("'route_cities' must have at least 2 city names.")
  }
  if (!is.data.frame(all_cities)) {
    stop("'all_cities' must be a data.frame.")
  }

  required_cols <- c("name", "lat", "lon", "population",
                     "historical_score", "cultural_score", "poi_count")
  missing_cols <- setdiff(required_cols, names(all_cities))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in 'all_cities':",
               paste(missing_cols, collapse = ", ")))
  }

  if (!is.numeric(radius_km) || radius_km <= 0) {
    stop("'radius_km' must be a positive number.")
  }

  # Look up route city coordinates
  route_idx <- match(route_cities, all_cities$name) #finds which row number each route city occupies in the data.frame. 
  if (any(is.na(route_idx))) {
    unknown <- route_cities[is.na(route_idx)]
    stop(paste("Unknown cities in route:", paste(unknown, collapse = ", ")))
  }

  route_lats <- all_cities$lat[route_idx]
  route_lons <- all_cities$lon[route_idx]

  # Exclude route cities from candidates
  candidate_mask <- !all_cities$name %in% route_cities
  candidates <- all_cities[candidate_mask, ]

  if (nrow(candidates) == 0) {
    message("No candidate cities available for discovery.")
    return(list(
      discoveries = data.frame(
        name = character(0), distance_km = numeric(0),
        importance = numeric(0), nearest_route_city = character(0),
        lat = numeric(0), lon = numeric(0),
        voivodeship = character(0), stringsAsFactors = FALSE
      ),
      radius_km    = radius_km,
      route_cities = route_cities,
      n_found      = 0
    ))
  }

  # For each candidate, find distance to the closest route city (C++)
  n_candidates <- nrow(candidates)
  min_dist <- numeric(n_candidates)
  nearest_idx <- integer(n_candidates)

  for (i in 1:n_candidates) {
    dists <- vapply(1:length(route_lats), function(j) {
      haversine_cpp(candidates$lat[i], candidates$lon[i],
                    route_lats[j], route_lons[j])
    }, numeric(1))
    min_dist[i] <- min(dists)
    nearest_idx[i] <- which.min(dists) #index of user destination city to which the candidate is closest to
  }

  # Filter cities within radius
  near_mask <- min_dist <= radius_km

  if (!any(near_mask)) {
    message(paste("No discoveries within", radius_km, "km radius."))
    return(list(
      discoveries = data.frame(
        name = character(0), distance_km = numeric(0),
        importance = numeric(0), nearest_route_city = character(0),
        lat = numeric(0), lon = numeric(0),
        voivodeship = character(0), stringsAsFactors = FALSE
      ),
      radius_km    = radius_km,
      route_cities = route_cities,
      n_found      = 0
    ))
  }

  nearby <- candidates[near_mask, ]
  nearby_dists <- min_dist[near_mask]
  nearby_nearest <- route_cities[nearest_idx[near_mask]]

  # Compute importance scores (vectorised)
  imp_scores <- calculate_importance(
    nearby$population, nearby$historical_score,
    nearby$cultural_score, nearby$poi_count
  )

  # Build result data.frame
  result_df <- data.frame(
    name               = nearby$name,
    distance_km        = round(nearby_dists, 1),
    importance         = round(imp_scores, 3),
    nearest_route_city = nearby_nearest,
    lat                = nearby$lat,
    lon                = nearby$lon,
    voivodeship        = nearby$voivodeship,
    stringsAsFactors   = FALSE
  )

  # Rank by importance (descending), limit results
  result_df <- result_df[order(-result_df$importance, result_df$distance_km), ]
  rownames(result_df) <- NULL

  if (nrow(result_df) > max_suggestions) {
    result_df <- result_df[1:max_suggestions, ]
  }

  return(list(
    discoveries  = result_df,
    radius_km    = radius_km,
    route_cities = route_cities,
    n_found      = nrow(result_df)
  ))
}
