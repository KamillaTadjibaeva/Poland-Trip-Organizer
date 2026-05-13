#' @name City
#' @title R6 Class Representing a Polish City
#' @description
#' Encapsulates all data and behaviour for a single city:
#' geographic coordinates, demographic data, importance scoring,
#' day allocation, and nearby city discovery.
#' @examples
#' \dontrun{
#' city <- City$new(
#'   name = "Krakow", lat = 50.06, lon = 19.94,
#'   population = 800000, historical_score = 10,
#'   cultural_score = 10, poi_count = 1200, voivodeship = "Malopolskie"
#' )
#' city$calculate_importance(c(0.15, 0.35, 0.35, 0.15))
#' }
NULL

#' @name TripPlanner
#' @title R6 Class for Planning a Trip to Poland
#' @description
#' The central class that orchestrates time allocation across cities
#' and route discovery. Encapsulates trip parameters, city data,
#' and provides methods for both core tasks.
#' @examples
#' \dontrun{
#' planner <- TripPlanner$new()
#' planner$set_trip(
#'   must_see = c("Warsaw", "Krakow", "Gdansk"),
#'   start_date = "2026-07-01", end_date = "2026-07-10",
#'   transport = "car"
#' )
#' planner$allocate_time()
#' planner$discover_route(radius_km = 50)
#' }
NULL
