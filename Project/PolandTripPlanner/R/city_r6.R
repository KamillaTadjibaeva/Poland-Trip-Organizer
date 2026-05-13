#---------------------------------------------------------#
#                 PolandTripPlanner Package                #
#              R6 Class: City                             #
#                                                         #
#  Demonstrates R6 OOP: private fields, active bindings,  #
#  method chaining, encapsulation, and mutable reference   #
#  semantics for individual city objects                   #
#---------------------------------------------------------#

#' @importFrom R6 R6Class

#' R6 Class Representing a Polish City
#'
#' Encapsulates all data and behaviour for a single city:
#' geographic coordinates, demographic data, importance scoring,
#' day allocation, and nearby city discovery.
#'
#' @section Private Fields:
#' All city attributes are stored as private fields and exposed
#' through read-only active bindings (encapsulation).
#'
#' @section Public Methods:
#' \describe{
#'   \item{initialize()}{Create a City from raw attributes}
#'   \item{calculate_importance()}{Compute importance score}
#'   \item{calculate_days()}{Allocate days based on share of total importance}
#'   \item{find_nearby_cities()}{Discover cities within a radius using C++}
#' }
#'
#' @export
City <- R6Class("City",

  # ---- Private fields ----
  private = list(
    .name             = NULL,
    .lat              = NULL,
    .lon              = NULL,
    .population       = NULL,
    .historical_score = NULL,
    .cultural_score   = NULL,
    .poi_count        = NULL,
    .voivodeship      = NULL,
    .importance       = NULL,   # computed by calculate_importance()
    .days             = NULL,   # computed by calculate_days()
    .nearby_cities    = list()  # list of city names found nearby
  ),

  active = list(

    #' @field name City name (read-only)
    name = function(value) {
      if (!missing(value)) stop("'name' is read-only.")
      private$.name
    },

    #' @field lat Latitude in degrees (read-only)
    lat = function(value) {
      if (!missing(value)) stop("'lat' is read-only.")
      private$.lat
    },

    #' @field lon Longitude in degrees (read-only)
    lon = function(value) {
      if (!missing(value)) stop("'lon' is read-only.")
      private$.lon
    },

    #' @field population City population (read-only)
    population = function(value) {
      if (!missing(value)) stop("'population' is read-only.")
      private$.population
    },

    #' @field historical_score Historical significance 1-10 (read-only)
    historical_score = function(value) {
      if (!missing(value)) stop("'historical_score' is read-only.")
      private$.historical_score
    },

    #' @field cultural_score Cultural heritage 1-10 (read-only)
    cultural_score = function(value) {
      if (!missing(value)) stop("'cultural_score' is read-only.")
      private$.cultural_score
    },

    #' @field poi_count Number of points of interest (read-only)
    poi_count = function(value) {
      if (!missing(value)) stop("'poi_count' is read-only.")
      private$.poi_count
    },

    #' @field voivodeship Administrative region (read-only)
    voivodeship = function(value) {
      if (!missing(value)) stop("'voivodeship' is read-only.")
      private$.voivodeship
    },

    #' @field importance Computed importance score (read-only)
    importance = function(value) {
      if (!missing(value)) stop("'importance' is read-only. Use calculate_importance().")
      private$.importance
    },

    #' @field days Allocated days to spend (read-only)
    days = function(value) {
      if (!missing(value)) stop("'days' is read-only. Use calculate_days().")
      private$.days
    },

    #' @field nearby_cities List of nearby city names (read-only)
    nearby_cities = function(value) {
      if (!missing(value)) stop("'nearby_cities' is read-only. Use find_nearby_cities().")
      private$.nearby_cities
    }
  ),

  public = list(

    #' @description Create a new City object
    #' @param name City name (character)
    #' @param lat Latitude (numeric)
    #' @param lon Longitude (numeric)
    #' @param population Population count (numeric)
    #' @param historical_score Historical significance 1-10 (numeric)
    #' @param cultural_score Cultural heritage 1-10 (numeric)
    #' @param poi_count Number of points of interest (numeric)
    #' @param voivodeship Administrative region (character)
    initialize = function(name, lat, lon, population,
                          historical_score, cultural_score,
                          poi_count, voivodeship = NA_character_) {

      if (!is.character(name) || length(name) != 1) {
        stop("'name' must be a single character string.")
      }
      if (!is.numeric(lat) || !is.numeric(lon)) {
        stop("'lat' and 'lon' must be numeric.")
      }
      if (!is.numeric(population) || population < 0) {
        stop("'population' must be a non-negative number.")
      }

      private$.name <- name
      private$.lat <- lat
      private$.lon <- lon
      private$.population <- population
      private$.historical_score <- historical_score
      private$.cultural_score <- cultural_score
      private$.poi_count <- poi_count
      private$.voivodeship <- voivodeship

      invisible(self)
    },

    #' @description Compute importance score for this city
    #'
    #' Uses the vectorised calculate_importance() function on this
    #' city's attributes. Stores the result in the private .importance field.
    #'
    #' @param weights Numeric vector of 4 weights
    #' @return self (for method chaining)
    calculate_importance = function(weights = c(population = 0.15,
                                                historical = 0.35,
                                                cultural   = 0.35,
                                                poi        = 0.15)) {
      # function ffrom time_allocation.R
      private$.importance <- calculate_importance(
        private$.population,
        private$.historical_score,
        private$.cultural_score,
        private$.poi_count,
        weights
      )
      invisible(self)
    },

    #' @description Calculate days to spend in this city
    #'
    #' Allocates days based on this city's share of total importance
    #' across all cities in the trip.
    #'
    #' @param total_days Total trip duration in days
    #' @param total_importance Sum of importance scores for all trip cities
    #' @param n_cities Number of cities in the trip
    #' @param min_days Minimum days per city (default: 1)
    #' @return self (for method chaining)
    calculate_days = function(total_days, total_importance, n_cities, min_days = 1) {
      if (is.null(private$.importance)) {
        stop("Importance not computed yet. Call calculate_importance() first.")
      }
      if (!is.numeric(total_days) || total_days <= 0) {
        stop("'total_days' must be a positive number.")
      }
      if (!is.numeric(total_importance) || total_importance <= 0) {
        stop("'total_importance' must be a positive number.")
      }

      # Each city starts with min_days then remaining is distributed
      remaining <- total_days - (n_cities * min_days)
      share <- private$.importance / total_importance
      private$.days <- round(min_days + remaining * share)
      private$.days <- max(private$.days, 1)

      invisible(self)
    },

    #' @description Find cities within a given radius using C++
    #'
    #' Computes haversine distances from this city to all candidates
    #' and stores names of those within the radius.
    #'
    #' @param all_cities_df Data.frame of all cities (from get_polish_cities())
    #' @param radius_km Search radius in km (default: 50)
    #' @return self (for method chaining)
    find_nearby_cities = function(all_cities_df, radius_km = 50) {
      if (!is.data.frame(all_cities_df)) {
        stop("'all_cities_df' must be a data.frame.")
      }
      if (!is.numeric(radius_km) || radius_km <= 0) {
        stop("'radius_km' must be a positive number.")
      }

      # Exclude self from candidates
      candidates <- all_cities_df[all_cities_df$name != private$.name, ]

      # Compute distances using C++ haversine
      distances <- numeric(nrow(candidates))
      for (i in seq_len(nrow(candidates))) {
        distances[i] <- haversine_cpp(
          private$.lat, private$.lon,
          candidates$lat[i], candidates$lon[i]
        )
      }

      # Filter cities within radius
      near_mask <- distances <= radius_km
      nearby_df <- candidates[near_mask, ]
      nearby_dists <- distances[near_mask]

      # Store as named list: city_name -> distance_km
      private$.nearby_cities <- setNames(
        round(nearby_dists, 1),
        nearby_df$name
      )

      invisible(self)
    },

    #' @description Correct allocated days after global rounding adjustment
    #'
    #' When each city rounds its days individually, the total may not
    #' equal the trip length. TripPlanner fixes the mismatch across all
    #' cities and uses this method to write the corrected value back.
    #'
    #' @param new_days New number of days (numeric, >= 1)
    #' @return self (for method chaining)
    # Needed because a single City cannot fix a global rounding error
    set_days = function(new_days) {
      if (!is.numeric(new_days) || new_days < 1) {
        stop("'new_days' must be at least 1.")
      }
      private$.days <- new_days
      invisible(self)
    },

    #' @description Print a summary of this city
    print = function() {
      cat(paste0("City: ", private$.name, "\n"))
      cat(paste0("  Region: ", private$.voivodeship, "\n"))
      cat(paste0("  Location: ", private$.lat, ", ", private$.lon, "\n"))
      cat(paste0("  Population: ", format(private$.population, big.mark = ",",
                                           scientific = FALSE), "\n"))
      cat(paste0("  Historical: ", private$.historical_score, "/10"))
      cat(paste0("  Cultural: ", private$.cultural_score, "/10"))
      cat(paste0("  POIs: ", private$.poi_count, "\n"))

      if (!is.null(private$.importance)) {
        cat(paste0("  Importance: ", round(private$.importance, 3), "\n"))
      }
      if (!is.null(private$.days)) {
        cat(paste0("  Days allocated: ", private$.days, "\n"))
      }
      if (length(private$.nearby_cities) > 0) {
        cat(paste0("  Nearby cities (", length(private$.nearby_cities), "): ",
                   paste(names(private$.nearby_cities), collapse = ", "), "\n"))
      }

      invisible(self)
    }
  )
)
