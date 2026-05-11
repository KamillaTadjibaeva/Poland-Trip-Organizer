#' Find interesting cities near a planned route.
#'
#' For each candidate city not in `route_cities`, computes its minimum
#' Haversine distance to any city on the route (in C++), keeps those within
#' `radius_km`, and ranks them by importance score.
#'
#' @param route_cities    Character vector of city names already in the plan
#'   (in visit order). Must have at least 2 entries.
#' @param all_cities      data.frame of all candidate cities. Must contain
#'   city/name, lat, lon, population, historical_score, cultural_score,
#'   poi_count. Use [load_cities()] for the bundled dataset.
#' @param radius_km       Detour radius in km (default: 30).
#' @param max_suggestions Maximum number of cities to return (default: 10).
#' @return A list with components `discoveries` (data.frame ranked by
#'   importance), `radius_km`, `route_cities`, `n_found`.
#' @export
find_route_discoveries <- function(route_cities, all_cities,
                                   radius_km = 30, max_suggestions = 10L) {
  .assert_character(route_cities, min_len = 2L)
  if (!is.data.frame(all_cities)) stop("'all_cities' must be a data.frame.", call. = FALSE)
  if (!is.numeric(radius_km) || radius_km <= 0) {
    stop("'radius_km' must be positive.", call. = FALSE)
  }

  name_col <- if ("city" %in% names(all_cities)) "city" else "name"
  needed <- c(name_col, "lat", "lon", "population",
              "historical_score", "cultural_score", "poi_count")
  miss <- setdiff(needed, names(all_cities))
  if (length(miss)) {
    stop("'all_cities' missing columns: ", paste(miss, collapse = ", "),
         call. = FALSE)
  }

  names_lc       <- tolower(all_cities[[name_col]])
  route_idx      <- match(tolower(route_cities), names_lc)
  if (anyNA(route_idx)) {
    stop("Unknown route cities: ",
         paste(route_cities[is.na(route_idx)], collapse = ", "), call. = FALSE)
  }

  route_lats <- all_cities$lat[route_idx]
  route_lons <- all_cities$lon[route_idx]

  cand_mask  <- !names_lc %in% tolower(route_cities)
  candidates <- all_cities[cand_mask, , drop = FALSE]

  empty_result <- function() {
    list(
      discoveries = data.frame(
        city = character(0), distance_km = numeric(0),
        importance = numeric(0), nearest_route_city = character(0),
        lat = numeric(0), lon = numeric(0), stringsAsFactors = FALSE
      ),
      radius_km    = radius_km,
      route_cities = route_cities,
      n_found      = 0L
    )
  }

  if (nrow(candidates) == 0L) return(empty_result())

  # C++ batch: min distance + nearest route-city index for every candidate.
  dr <- min_distance_to_route_cpp(
    as.numeric(candidates$lat), as.numeric(candidates$lon),
    as.numeric(route_lats),     as.numeric(route_lons)
  )
  near_mask <- dr$min_dist <= radius_km
  if (!any(near_mask)) return(empty_result())

  nearby       <- candidates[near_mask, , drop = FALSE]
  nearby_dists <- dr$min_dist[near_mask]
  nearest_name <- route_cities[dr$nearest[near_mask]]

  imp <- calculate_importance(
    nearby$population, nearby$historical_score,
    nearby$cultural_score, nearby$poi_count
  )

  out <- data.frame(
    city               = nearby[[name_col]],
    distance_km        = round(nearby_dists, 1),
    importance         = round(imp, 3),
    nearest_route_city = nearest_name,
    lat                = nearby$lat,
    lon                = nearby$lon,
    stringsAsFactors   = FALSE
  )
  out <- out[order(-out$importance, out$distance_km), , drop = FALSE]
  if (nrow(out) > max_suggestions) out <- out[seq_len(max_suggestions), ]
  rownames(out) <- NULL

  list(
    discoveries  = out,
    radius_km    = radius_km,
    route_cities = route_cities,
    n_found      = nrow(out)
  )
}
