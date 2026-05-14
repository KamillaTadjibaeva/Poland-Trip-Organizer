#' Fetch transport options for a single leg.
#'
#' Tries the provider implied by `transport`:
#' \itemize{
#'   \item `plane` -> Aviationstack (schedules) if `AVIATIONSTACK_KEY` is
#'     set, otherwise Amadeus Self-Service if `AMADEUS_CLIENT_ID/SECRET`
#'     are set, otherwise mock data.
#'   \item `train` -> koleo.pl public endpoints (currently flaky -> mock)
#'   \item `car`   -> Google Routes API if `GOOGLE_MAPS_API_KEY` is set,
#'     otherwise deterministic synthetic estimate.
#'   \item `bus`   -> Google Routes API (TRANSIT mode, filtered to bus legs)
#'     if `GOOGLE_MAPS_API_KEY` is set, otherwise deterministic estimate.
#' }
#' If a network call fails (no API key, offline, rate-limit) the function
#' degrades gracefully to a deterministic mock so the rest of the pipeline
#' keeps working — the Shiny UI can still demonstrate behaviour without
#' credentials.
#'
#' @param from,to City names.
#' @param date    Travel `Date`.
#' @param transport,style See package overview.
#' @param cities  Cities reference data.frame (used for IATA / coordinates).
#' @return A list of `transport_option` S3 objects, sorted by the user's
#'   travel style.
#' @export
get_transport_options <- function(from, to, date,
                                  transport = "train",
                                  style     = "fastest",
                                  cities    = NULL) {
  .assert_string(from); .assert_string(to)
  if (!is.character(transport) || !length(transport)) {
    stop("`transport` must be a non-empty character vector.", call. = FALSE)
  }
  bad <- setdiff(transport, .TRANSPORT_TYPES)
  if (length(bad)) {
    stop("Unknown transport mode(s): ", paste(bad, collapse = ", "),
         call. = FALSE)
  }
  .assert_choice(style,     .TRAVEL_STYLES)
  date <- .assert_date(date, "date")
  if (is.null(cities)) cities <- load_cities()

  fetch_one <- function(mode) {
    tryCatch(
      switch(mode,
             plane = .plane_provider(from, to, date, cities),
             train = .mock_options(from, to, date, "train", cities, n = 3L),
             bus   = .bus_provider(from, to, date, cities),
             car   = .car_provider(from, to, date, cities)),
      error = function(e) {
        message("Transport API failed for ", mode, " (",
                conditionMessage(e), "); falling back to mock data.")
        .mock_options(from, to, date, mode, cities, n = 3L)
      }
    )
  }

  raw <- unlist(lapply(unique(transport), fetch_one), recursive = FALSE)
  filter_by_style(raw, style)
}

#' Sort/filter transport options according to the user's travel style.
#'
#' @param options A list of `transport_option` objects.
#' @param style   One of `"fastest"`, `"cheapest"`, `"scenic"`.
#' @return The list, sorted ascending by the relevant key (and trimmed).
#' @export
filter_by_style <- function(options, style) {
  .assert_choice(style, .TRAVEL_STYLES)
  if (!length(options)) return(options)
  key <- switch(style,
                fastest  = "duration_h",
                cheapest = "price_eur",
                scenic   = "scenic_score")  # higher = better, sort desc
  vals <- vapply(options, function(o) {
    v <- o[[key]]; if (is.null(v)) NA_real_ else as.numeric(v)
  }, numeric(1))
  ord <- if (style == "scenic") order(-vals, na.last = TRUE)
         else order(vals, na.last = TRUE)
  options[ord]
}

#' @export
print.transport_option <- function(x, ...) {
  cat(sprintf("[%s] %s -> %s on %s\n",
              x$mode, x$from, x$to, format(x$depart)))
  cat(sprintf("    duration: %.1fh   price: EUR %.0f   scenic: %.2f%s\n",
              x$duration_h %||% NA, x$price_eur %||% NA,
              x$scenic_score %||% NA,
              if (!is.null(x$provider)) paste0("   via ", x$provider) else ""))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------
.make_option <- function(mode, from, to, depart, duration_h, price_eur,
                         scenic_score, provider = NULL, extra = list()) {
  structure(c(list(
    mode         = mode,
    from         = from,
    to           = to,
    depart       = as.POSIXct(depart),
    duration_h   = duration_h,
    price_eur    = price_eur,
    scenic_score = scenic_score,
    provider     = provider
  ), extra), class = "transport_option")
}

.haversine_pair <- function(from, to, cities) {
  rows <- .lookup_cities(c(from, to), cities)
  haversine_cpp(rows$lat[1], rows$lon[1], rows$lat[2], rows$lon[2])
}

.mock_options <- function(from, to, date, transport, cities, n = 3L) {
  km <- .haversine_pair(from, to, cities)
  speed <- c(plane = 450, train = 120, bus = 70, car = 90)[[transport]]
  base_h <- km / speed +
            c(plane = 2.5, train = 0.3, bus = 0.4, car = 0.2)[[transport]]
  base_p <- km * c(plane = 0.18, train = 0.10, bus = 0.05, car = 0.12)[[transport]] +
            c(plane = 40, train = 5, bus = 2, car = 0)[[transport]]
  scen   <- if (transport == "plane") 0.2 else if (transport == "car") 0.9
            else if (transport == "train") 0.7 else 0.5

  set.seed(as.integer(date) + nchar(from) + nchar(to))   # reproducible
  lapply(seq_len(n), function(i) {
    jitter_h <- stats::runif(1, 0.9, 1.2)
    jitter_p <- stats::runif(1, 0.85, 1.25)
    depart   <- as.POSIXct(date) + (6 + 3 * i) * 3600
    .make_option(
      mode = transport, from = from, to = to,
      depart = depart,
      duration_h   = base_h * jitter_h,
      price_eur    = base_p * jitter_p,
      scenic_score = scen + stats::runif(1, -0.05, 0.05),
      provider     = paste0("mock-", transport)
    )
  })
}

# Real API integrations are kept thin and behind env-var gates. The Shiny app
# can set AVIATIONSTACK_KEY (preferred) or AMADEUS_CLIENT_ID/SECRET; without
# either we fall through to the mock above.
.plane_provider <- function(from, to, date, cities) {
  if (nzchar(Sys.getenv("AVIATIONSTACK_KEY"))) {
    return(.aviationstack_flights(from, to, date, cities))
  }
  if (nzchar(Sys.getenv("AMADEUS_CLIENT_ID")) &&
      nzchar(Sys.getenv("AMADEUS_CLIENT_SECRET"))) {
    return(.amadeus_flights(from, to, date, cities))
  }
  .mock_options(from, to, date, "plane", cities, n = 4L)
}

# --- Aviationstack -----------------------------------------------------------
# Free plan returns flight *schedules* (no prices) over plain HTTP and does
# NOT allow the `flight_date` parameter (that's a paid feature). We therefore
# pull the current/next schedule for the city pair and shift the departure
# times onto the requested date so they line up with the trip plan. Prices
# are estimated from the great-circle distance (same heuristic as the mock).
.aviationstack_flights <- function(from, to, date, cities) {
  key <- Sys.getenv("AVIATIONSTACK_KEY")
  if (!nzchar(key)) {
    return(.mock_options(from, to, date, "plane", cities, n = 4L))
  }
  rows <- .lookup_cities(c(from, to), cities)
  iata_from <- rows$iata[1]; iata_to <- rows$iata[2]
  if (is.na(iata_from) || iata_from == "NA" ||
      is.na(iata_to)   || iata_to   == "NA") {
    return(.mock_options(from, to, date, "plane", cities, n = 1L))
  }

  resp <- httr::GET(
    "http://api.aviationstack.com/v1/flights",
    query = list(access_key = key,
                 dep_iata   = iata_from,
                 arr_iata   = iata_to,
                 limit      = 20),
    httr::timeout(10)
  )
  httr::stop_for_status(resp)
  body <- httr::content(resp, as = "parsed", type = "application/json")
  if (!is.null(body$error)) {
    stop("Aviationstack: ", body$error$message %||% body$error$type)
  }

  km <- .haversine_pair(from, to, cities)
  est_price <- km * 0.18 + 40

  flights <- body$data %||% list()
  if (!length(flights)) {
    return(.mock_options(from, to, date, "plane", cities, n = 2L))
  }

  parse_ts <- function(s) {
    if (is.null(s) || is.na(s)) return(as.POSIXct(NA))
    as.POSIXct(sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", s),
               format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  }
  shift_to <- function(ts, target_date) {
    if (is.na(ts)) return(ts)
    # Keep the time-of-day, replace the date.
    tod <- format(ts, "%H:%M:%S", tz = "UTC")
    as.POSIXct(paste(format(target_date), tod), tz = "UTC")
  }

  options <- lapply(flights, function(f) {
    dep <- parse_ts(f$departure$scheduled)
    arr <- parse_ts(f$arrival$scheduled)
    if (is.na(dep) || is.na(arr)) return(NULL)
    dur <- as.numeric(difftime(arr, dep, units = "hours"))
    label <- f$airline$name %||% "Unknown airline"
    if (!is.null(f$flight$iata)) label <- paste0(label, " ", f$flight$iata)
    .make_option(
      mode = "plane", from = from, to = to,
      depart       = shift_to(dep, date),
      duration_h   = dur,
      price_eur    = est_price,
      scenic_score = 0.2,
      provider     = paste0("Aviationstack (", label, ")")
    )
  })
  Filter(Negate(is.null), options)
}

.amadeus_flights <- function(from, to, date, cities) {
  cid <- Sys.getenv("AMADEUS_CLIENT_ID")
  sec <- Sys.getenv("AMADEUS_CLIENT_SECRET")
  if (!nzchar(cid) || !nzchar(sec)) {
    return(.mock_options(from, to, date, "plane", cities, n = 4L))
  }
  rows <- .lookup_cities(c(from, to), cities)
  iata_from <- rows$iata[1]; iata_to <- rows$iata[2]
  if (is.na(iata_from) || iata_from == "NA" ||
      is.na(iata_to)   || iata_to   == "NA") {
    return(.mock_options(from, to, date, "plane", cities, n = 1L))
  }

  tok <- httr::POST(
    "https://test.api.amadeus.com/v1/security/oauth2/token",
    body = list(grant_type = "client_credentials",
                client_id = cid, client_secret = sec),
    encode = "form"
  )
  httr::stop_for_status(tok)
  access <- httr::content(tok)$access_token

  resp <- httr::GET(
    "https://test.api.amadeus.com/v2/shopping/flight-offers",
    query = list(originLocationCode = iata_from,
                 destinationLocationCode = iata_to,
                 departureDate = format(date, "%Y-%m-%d"),
                 adults = 1, max = 5, currencyCode = "EUR"),
    httr::add_headers(Authorization = paste("Bearer", access))
  )
  httr::stop_for_status(resp)
  body <- httr::content(resp, as = "parsed")
  offers <- body$data %||% list()
  lapply(offers, function(o) {
    itin <- o$itineraries[[1]]
    dur  <- .iso8601_duration_h(itin$duration)
    seg  <- itin$segments[[1]]
    .make_option(
      mode = "plane", from = from, to = to,
      depart = as.POSIXct(seg$departure$at, format = "%Y-%m-%dT%H:%M:%S"),
      duration_h   = dur,
      price_eur    = as.numeric(o$price$total),
      scenic_score = 0.2,
      provider     = "Amadeus"
    )
  })
}

.iso8601_duration_h <- function(s) {
  if (is.null(s) || is.na(s)) return(NA_real_)
  m <- regmatches(s, regexec("PT(?:(\\d+)H)?(?:(\\d+)M)?", s))[[1]]
  if (length(m) < 3) return(NA_real_)
  h <- suppressWarnings(as.numeric(m[2])); if (is.na(h)) h <- 0
  mn <- suppressWarnings(as.numeric(m[3])); if (is.na(mn)) mn <- 0
  h + mn / 60
}

# --- Google Routes API (car) -------------------------------------------------
# Uses the v2 Routes API (POST https://routes.googleapis.com/directions/v2:computeRoutes).
# Requires GOOGLE_MAPS_API_KEY with the "Routes API" enabled. Falls back to a
# deterministic mock if the key is missing or the call fails.
.car_provider <- function(from, to, date, cities) {
  if (nzchar(Sys.getenv("GOOGLE_MAPS_API_KEY"))) {
    return(.google_routes_car(from, to, date, cities))
  }
  .mock_options(from, to, date, "car", cities, n = 1L)
}

.google_routes_car <- function(from, to, date, cities) {
  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (!nzchar(key)) {
    return(.mock_options(from, to, date, "car", cities, n = 1L))
  }
  rows <- .lookup_cities(c(from, to), cities)
  body <- list(
    origin      = list(location = list(latLng = list(
      latitude = rows$lat[1], longitude = rows$lon[1]))),
    destination = list(location = list(latLng = list(
      latitude = rows$lat[2], longitude = rows$lon[2]))),
    travelMode           = "DRIVE",
    routingPreference    = "TRAFFIC_AWARE",
    computeAlternativeRoutes = TRUE,
    units                = "METRIC"
  )
  resp <- httr::POST(
    "https://routes.googleapis.com/directions/v2:computeRoutes",
    httr::add_headers(
      "Content-Type"   = "application/json",
      "X-Goog-Api-Key" = key,
      "X-Goog-FieldMask" =
        "routes.distanceMeters,routes.duration,routes.description"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    httr::timeout(10)
  )
  httr::stop_for_status(resp)
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  routes <- parsed$routes %||% list()
  if (!length(routes)) {
    return(.mock_options(from, to, date, "car", cities, n = 1L))
  }
  depart <- as.POSIXct(date) + 9 * 3600   # default 09:00 departure
  lapply(seq_along(routes), function(i) {
    r   <- routes[[i]]
    km  <- (r$distanceMeters %||% NA_real_) / 1000
    # duration is a string like "12345s"
    dur_s <- suppressWarnings(as.numeric(sub("s$", "", r$duration %||% "")))
    dur_h <- if (is.na(dur_s)) NA_real_ else dur_s / 3600
    # EUR estimate: fuel + tolls heuristic (~0.12 EUR/km), same as mock.
    price <- if (is.na(km)) NA_real_ else km * 0.12
    desc  <- r$description %||% sprintf("route %d", i)
    .make_option(
      mode = "car", from = from, to = to,
      depart       = depart,
      duration_h   = dur_h,
      price_eur    = price,
      scenic_score = 0.9,
      provider     = paste0("Google Routes (", desc, ")"),
      extra        = list(distance_km = km)
    )
  })
}

# --- Google Routes API (bus, via TRANSIT mode) -------------------------------
# TRANSIT routes can mix bus / rail / tram; we keep only those whose first
# transit leg is a BUS. Same key + same SKU as the car path.
.bus_provider <- function(from, to, date, cities) {
  if (nzchar(Sys.getenv("GOOGLE_MAPS_API_KEY"))) {
    return(.google_routes_bus(from, to, date, cities))
  }
  .mock_options(from, to, date, "bus", cities, n = 4L)
}

.google_routes_bus <- function(from, to, date, cities) {
  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (!nzchar(key)) {
    return(.mock_options(from, to, date, "bus", cities, n = 4L))
  }
  rows <- .lookup_cities(c(from, to), cities)
  # Default 09:00 local-ish departure; Routes wants RFC3339 UTC.
  depart_ts <- as.POSIXct(date) + 9 * 3600
  body <- list(
    origin      = list(location = list(latLng = list(
      latitude = rows$lat[1], longitude = rows$lon[1]))),
    destination = list(location = list(latLng = list(
      latitude = rows$lat[2], longitude = rows$lon[2]))),
    travelMode               = "TRANSIT",
    departureTime            = format(depart_ts, "%Y-%m-%dT%H:%M:%SZ",
                                      tz = "UTC"),
    computeAlternativeRoutes = TRUE,
    transitPreferences       = list(allowedTravelModes = list("BUS")),
    units                    = "METRIC"
  )
  resp <- httr::POST(
    "https://routes.googleapis.com/directions/v2:computeRoutes",
    httr::add_headers(
      "Content-Type"   = "application/json",
      "X-Goog-Api-Key" = key,
      "X-Goog-FieldMask" = paste(
        "routes.distanceMeters",
        "routes.duration",
        "routes.description",
        "routes.legs.steps.transitDetails",
        sep = ",")
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    httr::timeout(10)
  )
  httr::stop_for_status(resp)
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  routes <- parsed$routes %||% list()
  if (!length(routes)) {
    return(.mock_options(from, to, date, "bus", cities, n = 2L))
  }

  km_total <- .haversine_pair(from, to, cities)
  est_price <- km_total * 0.05 + 2   # same heuristic as the bus mock

  has_bus <- function(r) {
    legs  <- r$legs %||% list()
    for (lg in legs) for (st in lg$steps %||% list()) {
      td <- st$transitDetails %||% NULL
      vt <- td$transitLine$vehicle$type %||% ""
      if (identical(vt, "BUS")) return(TRUE)
    }
    FALSE
  }
  bus_routes <- Filter(has_bus, routes)
  if (!length(bus_routes)) {
    # Google returned only rail/tram; fall back rather than mislabel.
    return(.mock_options(from, to, date, "bus", cities, n = 2L))
  }

  lapply(seq_along(bus_routes), function(i) {
    r     <- bus_routes[[i]]
    km    <- (r$distanceMeters %||% NA_real_) / 1000
    dur_s <- suppressWarnings(as.numeric(sub("s$", "", r$duration %||% "")))
    dur_h <- if (is.na(dur_s)) NA_real_ else dur_s / 3600
    # Pick the first bus leg's line name as the provider label.
    label <- "transit"
    for (lg in r$legs %||% list()) for (st in lg$steps %||% list()) {
      td <- st$transitDetails %||% NULL
      if (identical(td$transitLine$vehicle$type %||% "", "BUS")) {
        label <- td$transitLine$nameShort %||% td$transitLine$name %||% label
        break
      }
    }
    .make_option(
      mode = "bus", from = from, to = to,
      depart       = depart_ts,
      duration_h   = dur_h,
      price_eur    = est_price,
      scenic_score = 0.5,
      provider     = paste0("Google Routes (", label, ")"),
      extra        = list(distance_km = km)
    )
  })
}
