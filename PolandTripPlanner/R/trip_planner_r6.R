
#' @importFrom R6 R6Class

#' R6 Class for Planning a Trip to Poland
#'
#' Unified planner: configures the trip, optimises the route (TSP across
#' selected cities), allocates days across must-see cities, and discovers
#' scenic stops along the route.
#'
#' @section Public Methods:
#' \describe{
#'   \item{initialize()}{Create a new TripPlanner (optionally pre-configured).}
#'   \item{set_trip(...)}{Configure trip parameters (allocation and/or routing).}
#'   \item{plan()}{Solve the TSP and return a `trip_plan` S3 object.}
#'   \item{allocate_time()}{Compute time allocation across must-see cities.}
#'   \item{discover_route(radius_km)}{Find interesting stops along the route.}
#'   \item{selected_cities()}{Resolve `selected` to a sub-data.frame of geo cities.}
#'   \item{cost_matrix()}{Build the cost matrix used by `plan()`.}
#'   \item{get_cities_data()}{Return the full scoring cities dataset.}
#' }
#'
#' @section Active Bindings:
#' \describe{
#'   \item{total_days}{Total trip duration (read-only).}
#'   \item{city_count}{Number of must-see cities (read-only).}
#' }
#'
#' @export
TripPlanner <- R6Class("TripPlanner",

  private = list(
    .cities_data   = NULL,   # data.frame of all Polish cities (scoring data)
    .city_objects  = list(), # list of City R6 objects for scoring-known cities
    .must_see      = NULL,   # character vector of must-see city names
    .start_date    = NULL,   # Date object
    .end_date      = NULL,   # Date object
    .transport     = NULL,   # character vector of transport modes
    .trip_set      = FALSE,  # flag: has set_trip() been called?
    .allocation    = NULL,   # cached allocation result (list)
    .discovery     = NULL,   # cached discovery result (list)

    # Build City R6 objects for the subset of must_see that exists in the
    # scoring dataset. Cities absent from the scoring dataset are simply
    # skipped (they can still be routed, just not scored / allocated).
    .build_city_objects = function() {
      cities_df <- private$.cities_data
      known <- intersect(private$.must_see, cities_df$name)
      private$.city_objects <- lapply(known, function(city_name) {
        row <- cities_df[cities_df$name == city_name, ][1L, ]
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
      names(private$.city_objects) <- known
    },

    # Validation used when the caller only wants allocation/discovery
    # (no routing). Enforces presence in the scoring dataset.
    .validate_trip_params = function(must_see, start_date, end_date,
                                     transport) {
      if (!is.character(must_see) || length(must_see) < 2) {
        stop("'must_see' must be a character vector with at least 2 cities.")
      }
      known <- private$.cities_data$name
      unknown <- must_see[!must_see %in% known]
      if (length(unknown) > 0) {
        stop(paste("Unknown city name(s):",
                   paste(unknown, collapse = ", "),
                   "\nUse get_cities_data()$name to see available cities."))
      }
      if (any(duplicated(must_see))) {
        stop("'must_see' contains duplicate city names.")
      }
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
      if (!is.character(transport) || !length(transport) ||
          !all(transport %in% valid_transport)) {
        stop(paste("'transport' must be one or more of:",
                   paste(valid_transport, collapse = ", ")))
      }
      TRUE
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

    #' @field cities    Geo cities reference data.frame (used for routing).
    cities      = NULL,
    #' @field selected  Character vector of cities to visit (= must_see).
    selected    = NULL,
    #' @field flight_in Arrival city (NULL if routing not configured).
    flight_in   = NULL,
    #' @field flight_out Departure city (NULL if routing not configured).
    flight_out  = NULL,
    #' @field style    Travel style: "fastest" / "cheapest" / "scenic".
    style       = NULL,

    #' @description Create a new TripPlanner
    #'
    #' Can be called with no arguments and then configured via `set_trip()`,
    #' or pre-configured by passing the routing parameters directly.
    #'
    #' @param csv_path Optional path to a custom scoring CSV. Must have
    #'   columns: name, voivodeship, lat, lon, population, historical_score,
    #'   cultural_score, poi_count. If NULL (default), loads the built-in
    #'   `polish_cities.csv`.
    #' @param selected,flight_in,flight_out,start_date,end_date,transport,style,cities
    #'   Optional routing parameters; if `selected` and `flight_in` are
    #'   supplied, `set_trip()` is called immediately with the routing args.
    initialize = function(csv_path = NULL,
                          selected   = NULL,
                          flight_in  = NULL,
                          flight_out = NULL,
                          start_date = NULL,
                          end_date   = NULL,
                          transport  = NULL,
                          style      = NULL,
                          cities     = NULL) {
      # Scoring dataset (drives allocate_time / discover_route)
      if (!is.null(csv_path)) {
        if (!file.exists(csv_path)) {
          stop(paste("CSV file not found:", csv_path))
        }
        private$.cities_data <- read.csv(csv_path, stringsAsFactors = FALSE)
      } else {
        private$.cities_data <- get_polish_cities()
      }

      # Geo dataset (drives plan() / cost_matrix() / transport providers)
      self$cities <- cities %||% load_cities()

      message(paste("TripPlanner initialized with",
                    nrow(private$.cities_data), "Polish cities."))

      # If routing args were passed, configure immediately.
      if (!is.null(selected) && !is.null(flight_in) &&
          !is.null(start_date) && !is.null(end_date)) {
        self$set_trip(
          must_see   = selected,
          start_date = start_date,
          end_date   = end_date,
          transport  = transport %||% "train",
          flight_in  = flight_in,
          flight_out = flight_out,
          style      = style %||% "fastest"
        )
      }

      invisible(self)
    },

    #' @description Configure the trip parameters
    #'
    #' If `flight_in` is supplied, the planner is configured for routing
    #' (TSP optimisation) and validation is done against the geo dataset.
    #' Otherwise legacy allocation-only validation against the scoring
    #' dataset is used.
    #'
    #' @param must_see Character vector of city names to visit.
    #' @param start_date Start date (Date or "YYYY-MM-DD" string).
    #' @param end_date End date (Date or "YYYY-MM-DD" string).
    #' @param transport Transport modes: "car" / "train" / "bus" / "plane".
    #' @param flight_in,flight_out,style Optional routing parameters.
    #' @return self (for method chaining).
    set_trip = function(must_see, start_date, end_date,
                        transport = "car",
                        flight_in = NULL, flight_out = NULL,
                        style = NULL) {
      if (is.character(start_date)) start_date <- as.Date(start_date)
      if (is.character(end_date))   end_date   <- as.Date(end_date)

      routing <- !is.null(flight_in)

      if (routing) {
        # Routing-mode validation: against the geo dataset.
        .assert_character(must_see, min_len = 2L)
        if (anyDuplicated(tolower(must_see))) {
          stop("`must_see` contains duplicate cities.", call. = FALSE)
        }
        .assert_string(flight_in)
        if (is.null(flight_out)) flight_out <- flight_in
        .assert_string(flight_out)
        if (!tolower(flight_in)  %in% tolower(must_see)) {
          must_see <- c(flight_in, must_see)
        }
        if (!tolower(flight_out) %in% tolower(must_see)) {
          must_see <- c(must_see, flight_out)
        }
        sd <- .assert_date(start_date, "start_date")
        ed <- .assert_date(end_date, "end_date")
        if (ed < sd) {
          stop("`end_date` must be on or after `start_date`.", call. = FALSE)
        }
        if (!is.character(transport) || !length(transport)) {
          stop("`transport` must be a non-empty character vector.",
               call. = FALSE)
        }
        bad <- setdiff(transport, .TRANSPORT_TYPES)
        if (length(bad)) {
          stop("Unknown transport mode(s): ", paste(bad, collapse = ", "),
               call. = FALSE)
        }
        transport <- unique(transport)
        if (is.null(style)) style <- "fastest"
        .assert_choice(style, .TRAVEL_STYLES, "style")

        # Validate against geo dataset (where TSP runs).
        .lookup_cities(must_see, self$cities)

        start_date <- sd; end_date <- ed
      } else {
        # Legacy allocation-only path.
        private$.validate_trip_params(must_see, start_date, end_date, transport)
      }

      private$.must_see   <- must_see
      private$.start_date <- start_date
      private$.end_date   <- end_date
      private$.transport  <- transport
      private$.trip_set   <- TRUE

      self$selected   <- must_see
      self$flight_in  <- flight_in
      self$flight_out <- flight_out
      self$style      <- style

      private$.build_city_objects()

      private$.allocation <- NULL
      private$.discovery  <- NULL

      message(paste0("Trip set: ", length(must_see), " cities, ",
                     self$total_days, " days, ",
                     paste(transport, collapse = "+")))

      invisible(self)
    },

    #' @description Resolve `selected` to a sub-data.frame of geo cities.
    selected_cities = function() {
      if (is.null(self$selected)) {
        stop("Trip not configured. Call set_trip() first.", call. = FALSE)
      }
      .lookup_cities(self$selected, self$cities)
    },

    #' @description Build the cost matrix matching the user's preferences.
    cost_matrix = function() {
      build_cost_matrix(self$selected_cities(),
                        transport = private$.transport,
                        style     = self$style %||% "fastest")
    },

    #' @description Solve the TSP and return a `trip_plan` (S3) object.
    #' @param transport_provider Optional function used to fetch transport
    #'   options for each leg. See [get_transport_options()].
    plan = function(transport_provider = get_transport_options) {
      if (is.null(self$flight_in)) {
        stop("Routing not configured. Call set_trip() with `flight_in`.",
             call. = FALSE)
      }
      cm  <- self$cost_matrix()
      round_trip <- identical(self$flight_in, self$flight_out)
      sol <- solve_tsp(cm,
                       start = self$flight_in,
                       end   = if (round_trip) NULL else self$flight_out)

      route <- sol$order
      total_cost <- sol$cost
      if (round_trip) {
        route <- c(route, self$flight_in)
        total_cost <- total_cost +
          unname(cm[tail(sol$order, 1), self$flight_in])
      }

      legs <- lapply(seq_len(length(route) - 1L), function(i) {
        from <- route[i]; to <- route[i + 1L]
        list(
          from        = from,
          to          = to,
          leg_cost    = unname(cm[from, to]),
          options     = transport_provider(
                          from = from, to = to,
                          date = private$.start_date + (i - 1L),
                          transport = private$.transport,
                          style     = self$style %||% "fastest",
                          cities    = self$cities)
        )
      })

      structure(
        list(
          route       = route,
          total_cost  = total_cost,
          method      = sol$method,
          transport   = private$.transport,
          style       = self$style %||% "fastest",
          start_date  = private$.start_date,
          end_date    = private$.end_date,
          legs        = legs
        ),
        class = "trip_plan"
      )
    },

    #' @description Allocate trip days across must-see cities
    #'
    #' Uses importance scores (historical significance, cultural heritage,
    #' population, POI count) to proportionally distribute available days.
    #' Operates on cities present in the scoring dataset only; route-only
    #' cities are skipped silently.
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
      if (!private$.trip_set) {
        stop("Trip not configured. Call set_trip() first.")
      }
      if (!length(private$.city_objects)) {
        stop("No must-see cities are present in the scoring dataset.")
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
        route_cities    = intersect(private$.must_see,
                                    private$.cities_data$name),
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
