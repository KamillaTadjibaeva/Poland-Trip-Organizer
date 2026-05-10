
#' @importFrom R6 R6Class

#' R6 Class for Planning a Trip to Poland
#'
#' The central class that orchestrates time allocation across cities
#' and route discovery. Encapsulates trip parameters,
#' city data, and provides methods for both core tasks.
#'
#' @section Public Methods:
#' \describe{
#'   \item{initialize()}{Create a new TripPlanner}
#'   \item{set_trip(...)}{Configure trip parameters}
#'   \item{allocate_time()}{Compute time allocation across must-see cities}
#'   \item{discover_route(radius_km)}{Find interesting stops along the route}
#'   \item{get_cities_data()}{Return the full cities dataset}
#' }
#'
#' @section Active Bindings:
#' \describe{
#'   \item{total_days}{Total trip duration (read-only)}
#'   \item{city_count}{Number of must-see cities (read-only)}
#' }
#'
#' @export
TripPlanner <- R6Class("TripPlanner",

  private = list(
    .cities_data   = NULL,   # data.frame of all Polish cities (raw data)
    .city_objects  = list(), # list of City R6 objects for must-see cities
    .must_see      = NULL,   # character vector of must-see city names
    .start_date    = NULL,   # Date object
    .end_date      = NULL,   # Date object
    .transport     = NULL,   # "car", "train", "bus", or "plane"
    .trip_set      = FALSE,  # flag: has set_trip() been called?
    .allocation    = NULL,   # cached allocation result (list)
    .discovery     = NULL,   # cached discovery result (list)

    # Build City R6 objects for the must-see cities
    .build_city_objects = function() {
      cities_df <- private$.cities_data
      private$.city_objects <- lapply(private$.must_see, function(city_name) {
        idx <- which(cities_df$name == city_name)
        row <- cities_df[idx, ]
        City$new(
          name             = row$name,
          lat              = row$lat,
          lon              = row$lon,
          population       = row$population,
          historical_score = row$historical_score,
          cultural_score   = row$cultural_score,
          poi_count        = row$poi_count,
          voivodeship      = row$voivodeship
        )
      })
      names(private$.city_objects) <- private$.must_see
    },

    .validate_trip_params = function(must_see, start_date, end_date,
                                     transport) {
      # Check must_see cities
      if (!is.character(must_see) || length(must_see) < 2) {
        stop("'must_see' must be a character vector with at least 2 cities.")
      }

      # Check that all cities exist in the dataset
      known <- private$.cities_data$name
      unknown <- must_see[!must_see %in% known]
      if (length(unknown) > 0) {
        stop(paste("Unknown city name(s):",
                   paste(unknown, collapse = ", "),
                   "\nUse get_cities_data()$name to see available cities."))
      }

      # Check no duplicates
      if (any(duplicated(must_see))) {
        stop("'must_see' contains duplicate city names.")
      }

      # Check dates
      if (!inherits(start_date, "Date") || !inherits(end_date, "Date")) {
        stop("'start_date' and 'end_date' must be Date objects.")
      }
      if (end_date <= start_date) {
        stop("'end_date' must be after 'start_date'.")
      }

      total <- as.numeric(difftime(end_date, start_date, units = "days"))
      if (total < length(must_see)) {
        stop(paste("Not enough days for", length(must_see),
                   "cities. Need at least", length(must_see), "days."))
      }

      valid_transport <- c("car", "train", "bus", "plane")
      if (!transport %in% valid_transport) {
        stop(paste("'transport' must be one of:",
                   paste(valid_transport, collapse = ", ")))
      }

      return(TRUE)
    }
  ),

  active = list(

    #' @field total_days Total number of trip days (read-only)
    total_days = function(value) {
      if (!missing(value)) {
        stop("'total_days' is read-only. Use set_trip() to change dates.")
      }
      if (!private$.trip_set) return(NA_real_)
      as.numeric(difftime(private$.end_date, private$.start_date,
                          units = "days"))
    },

    #' @field city_count Number of must-see cities (read-only)
    city_count = function(value) {
      if (!missing(value)) {
        stop("'city_count' is read-only.")
      }
      if (!private$.trip_set) return(0L)
      length(private$.must_see)
    }

  ),

  public = list(

    #' @description Create a new TripPlanner
    #' @param csv_path Optional path to a custom CSV file with city data.
    #'   Must have columns: name, voivodeship, lat, lon, population,
    #'   historical_score, cultural_score, poi_count.
    #'   If NULL (default), loads the built-in polish_cities.csv.
    #' @param use_wikidata Logical, if TRUE fetch cities from Wikidata API
    #'   instead of using the CSV (default: FALSE).
    initialize = function(csv_path = NULL, use_wikidata = FALSE) {
      # Load cities dataset: from Wikidata API, custom CSV, or built-in CSV
      if (use_wikidata) {
        private$.cities_data <- fetch_cities_from_wikidata(verbose = TRUE)
      } else if (!is.null(csv_path)) {
        if (!file.exists(csv_path)) {
          stop(paste("CSV file not found:", csv_path))
        }
        private$.cities_data <- read.csv(csv_path, stringsAsFactors = FALSE)
      } else {
        private$.cities_data <- get_polish_cities()
      }

      message(paste("TripPlanner initialized with",
                    nrow(private$.cities_data), "Polish cities."))

      invisible(self) # R6 convention: invisible return of self
    },

    #' @description Configure the trip parameters
    #' @param must_see Character vector of city names to visit
    #' @param start_date Start date (Date object or "YYYY-MM-DD" string)
    #' @param end_date End date (Date object or "YYYY-MM-DD" string)
    #' @param transport Transport type: "car", "train", "bus", or "plane"
    #' @return self (for method chaining)
    set_trip = function(must_see, start_date, end_date,
                        transport = "car") {
      # Convert string dates to Date objects
      if (is.character(start_date)) start_date <- as.Date(start_date)
      if (is.character(end_date))   end_date   <- as.Date(end_date)

      # Validate all parameters (defensive programming)
      private$.validate_trip_params(must_see, start_date, end_date,
                                    transport)

      # Store parameters in private fields
      private$.must_see   <- must_see
      private$.start_date <- start_date
      private$.end_date   <- end_date
      private$.transport  <- transport
      private$.trip_set   <- TRUE

      private$.build_city_objects()

      # if trip changes we need to clear allocation and discovery (becuase it has not been allocated or discovered)
      private$.allocation <- NULL
      private$.discovery  <- NULL

      message(paste0("Trip set: ", length(must_see), " cities, ",
                     self$total_days, " days, ", transport))

      invisible(self) # enable method chaining
    },

    #' @description Allocate trip days across must-see cities
    #'
    #' Uses importance scores (historical significance, cultural heritage,
    #' population, POI count) to proportionally distribute available days.
    #'
    #' @param min_days Minimum days per city (default: 1)
    #' @param weights Named list of weights for importance components:
    #'   population, historical, cultural, poi (default: equal weights)
    #' @return A list with allocation results
    allocate_time = function(min_days = 1,
                             weights = list(population = 0.15,
                                            historical = 0.35,
                                            cultural   = 0.35,
                                            poi        = 0.15)) {
      # Check that trip has been configured
      if (!private$.trip_set) {
        stop("Trip not configured. Call set_trip() first.")
      }

      # Validate min_days
      if (!is.numeric(min_days) || min_days < 1) {
        stop("'min_days' must be at least 1.")
      }

      w <- as.numeric(weights)

      for (city in private$.city_objects) {
        city$calculate_importance(w)
      }

      total_importance <- sum(vapply(private$.city_objects,
                                     function(c) c$importance,
                                     numeric(1)))

      # days to spend per city
      n <- length(private$.city_objects)
      for (city in private$.city_objects) {
        city$calculate_days(self$total_days, total_importance, n, min_days)
      }

      # rounding of days to sum up to total days
      city_names  <- vapply(private$.city_objects, function(c) c$name, character(1)) #expect one character value (e.g., "Krakow")
      allocated   <- vapply(private$.city_objects, function(c) c$days, numeric(1))
      importances <- vapply(private$.city_objects, function(c) c$importance, numeric(1))

      diff <- self$total_days - sum(allocated)
      if (diff != 0) {
        max_idx <- which.max(importances)
        allocated[max_idx] <- allocated[max_idx] + diff
      }
      allocated <- pmax(allocated, 1)

      # adjust days based on the total allocated days for each city with set_days
      for (i in 1:length(private$.city_objects)) {
        private$.city_objects[[i]]$set_days(allocated[i])
      }

      allocation <- list(
        cities     = city_names,
        days       = allocated,
        importance = importances,
        total_days = self$total_days,
        start_date = private$.start_date,
        end_date   = private$.end_date,
        n_cities   = length(private$.city_objects)
      )

      private$.allocation <- allocation

      return(allocation)
    },

    #' @description Discover interesting cities along the route
    #'
    #' Uses C++ functions for efficient distance computation.
    #'
    #' @param radius_km Vicinity radius in km (default: 50)
    #' @param max_suggestions Maximum discoveries to return (default: 5)
    #' @param suggest_nearby Logical, whether to suggest nearby cities (default: TRUE)
    #' @return A list with route discovery results
    discover_route = function(radius_km = 50, max_suggestions = 5,
                              suggest_nearby = TRUE) {
      if (!private$.trip_set) {
        stop("Trip not configured. Call set_trip() first.")
      }

      if (!is.numeric(radius_km) || radius_km <= 0) {
        stop("'radius_km' must be a positive number.")
      }
      if (!is.numeric(max_suggestions) || max_suggestions < 1) {
        stop("'max_suggestions' must be a positive integer.")
      }

      if (!suggest_nearby) {
        message("Nearby city suggestions are turned off.")

        discovery <- list(
          discoveries  = data.frame(name = character(0),
                                    distance_km = numeric(0),
                                    importance = numeric(0),
                                    nearest_route_city = character(0),
                                    stringsAsFactors = FALSE),
          radius_km    = radius_km,
          route_cities = private$.must_see,
          transport    = private$.transport,
          n_found      = 0
        )
        return(discovery)
      }

      discovery <- find_route_discoveries(
        route_cities    = private$.must_see,
        all_cities      = private$.cities_data,
        radius_km       = radius_km,
        max_suggestions = max_suggestions
      )
      discovery$transport <- private$.transport

      private$.discovery <- discovery

      return(discovery)
    },

    #' @description Return the full Polish cities dataset
    #' @return data.frame of all Polish cities with their attributes
    get_cities_data = function() {
      private$.cities_data
    },

    #' @description Get the City R6 objects for must-see cities
    #' @return Named list of City objects, or empty list if trip not set
    get_city_objects = function() {
      private$.city_objects
    },

    #' @description Get the last computed time allocation
    #' @return list or NULL
    get_allocation = function() {
      private$.allocation
    },

    #' @description Get the last computed route discovery
    #' @return list or NULL
    get_discovery = function() {
      private$.discovery
    },

    #' @description Print time allocation results
    #'
    #' Displays the allocation of days across cities in a
    #' formatted table, including importance scores and dates.
    print_allocation = function() {
      alloc <- private$.allocation
      if (is.null(alloc)) {
        message("No allocation computed yet. Call allocate_time() first.")
        return(invisible(self))
      }

      cat("\n--- Time Allocation ---\n")
      cat("Trip:", as.character(alloc$start_date), "to",
          as.character(alloc$end_date),
          paste0("(", alloc$total_days, " days)\n"))
      cat("Cities:", alloc$n_cities, "\n\n")

      # Print table
      cat(format("City", width = 15),
          format("Days", width = 6),
          format("Importance", width = 12), "\n")
      cat(paste(rep("-", 33), collapse = ""), "\n")

      for (i in 1:length(alloc$cities)) {
        cat(format(alloc$cities[i], width = 15),
            format(alloc$days[i], width = 6),
            format(round(alloc$importance[i], 3), width = 12), "\n")
      }

      invisible(self)
    },

    #' @description Print route discovery results
    #'
    #' Displays discovered cities near the route, ranked by importance.
    print_discovery = function() {
      disc <- private$.discovery
      if (is.null(disc)) {
        message("No discovery computed yet. Call discover_route() first.")
        return(invisible(self))
      }

      cat("\n--- Route Discovery ---\n")
      cat("Route:", paste(disc$route_cities, collapse = " -> "), "\n")
      cat("Radius:", disc$radius_km, "km\n")
      cat("Found:", disc$n_found, "interesting stops\n\n")

      if (disc$n_found == 0) {
        cat("No discoveries found. Try a larger radius.\n")
        return(invisible(self))
      }

      df <- disc$discoveries
      cat(format("City", width = 15),
          format("Dist(km)", width = 10),
          format("Importance", width = 12),
          format("Nearest", width = 15), "\n")
      cat(paste(rep("-", 52), collapse = ""), "\n")

      for (i in 1:nrow(df)) {
        cat(format(df$name[i], width = 15),
            format(df$distance_km[i], width = 10),
            format(df$importance[i], width = 12),
            format(df$nearest_route_city[i], width = 15), "\n")
      }

      invisible(self)
    }
  )
)
