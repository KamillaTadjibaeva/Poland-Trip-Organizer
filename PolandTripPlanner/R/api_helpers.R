WIKIDATA_SPARQL_ENDPOINT <- "https://query.wikidata.org/sparql"


#' Fetch Polish cities from Wikidata SPARQL API
#'
#' Queries Wikidata for Polish cities with population > min_population,
#' retrieving name, coordinates, population, and voivodeship.
#' Falls back to the built-in dataset on failure.
#'
#' @param min_population Numeric, minimum population to include (default: 2000)
#' @param verbose Logical, print progress? (default: TRUE)
#'
#' @return A data.frame with columns: name, voivodeship, lat, lon, population
#'
#' @export
fetch_cities_from_wikidata <- function(min_population = 2000,
                                        verbose = TRUE) {
  if (!is.numeric(min_population) || min_population < 0) {
    stop("'min_population' must be a non-negative number.")
  }

  sparql_query <- paste0('
SELECT DISTINCT ?cityLabel ?lat ?lon ?population ?voivodeshipLabel WHERE {
  ?city wdt:P31/wdt:P279* wd:Q515 .        # instance of city (or subclass)
  ?city wdt:P17 wd:Q36 .                     # country = Poland
  ?city wdt:P625 ?coords .                   # has coordinates
  ?city wdt:P1082 ?population .              # has population
  ?city wdt:P131 ?voivodeship .              # located in admin region

  # Filter: voivodeship should be a voivodeship of Poland
  ?voivodeship wdt:P31 wd:Q150093 .

  # Extract lat/lon from coordinate
  BIND(geof:latitude(?coords) AS ?lat)
  BIND(geof:longitude(?coords) AS ?lon)

  FILTER(?population > ', as.integer(min_population), ')

  SERVICE wikibase:label { bd:serviceParam wikibase:language "en,pl". }
}
ORDER BY DESC(?population)
LIMIT 200
')

  if (verbose) {
    message("Fetching Polish cities from Wikidata SPARQL API...")
  }

  response <- tryCatch({
    httr::GET(
      WIKIDATA_SPARQL_ENDPOINT,
      query = list(query = sparql_query, format = "json"),
      httr::timeout(30),
      httr::add_headers(
        "Accept" = "application/sparql-results+json",
        "User-Agent" = "PolandTripPlanner/0.1 (R package; educational project)"
      )
    )
  }, error = function(e) {
    warning(paste("Wikidata API connection failed:", e$message))
    return(NULL)
  })

  if (is.null(response)) {
    message("Falling back to built-in dataset.")
    return(get_polish_cities())
  }

  status <- httr::status_code(response)
  if (status != 200) {
    warning(paste("Wikidata returned HTTP", status, ". Falling back to built-in dataset."))
    return(get_polish_cities())
  }

  content_text <- httr::content(response, as = "text", encoding = "UTF-8")

  parsed <- tryCatch({
    jsonlite::fromJSON(content_text, simplifyVector = FALSE)
  }, error = function(e) {
    warning(paste("Failed to parse Wikidata response:", e$message))
    return(NULL)
  })

  if (is.null(parsed) || is.null(parsed$results) || is.null(parsed$results$bindings)) {
    message("No results from Wikidata. Falling back to built-in dataset.")
    return(get_polish_cities())
  }

  bindings <- parsed$results$bindings
  if (length(bindings) == 0) {
    message("Empty results from Wikidata. Falling back to built-in dataset.")
    return(get_polish_cities())
  }

  if (verbose) {
    message(paste("  Received", length(bindings), "city records from Wikidata."))
  }

  n <- length(bindings)
  names_vec <- character(n)
  lat_vec   <- numeric(n)
  lon_vec   <- numeric(n)
  pop_vec   <- numeric(n)
  voiv_vec  <- character(n)

  for (i in seq_len(n)) {
    b <- bindings[[i]]
    names_vec[i] <- b$cityLabel$value
    lat_vec[i]   <- as.numeric(b$lat$value)
    lon_vec[i]   <- as.numeric(b$lon$value)
    pop_vec[i]   <- as.numeric(b$population$value)
    voiv_vec[i]  <- gsub(" Voivodeship", "", b$voivodeshipLabel$value)
  }

  temp_df <- data.frame(
    name = names_vec, lat = lat_vec, lon = lon_vec,
    population = pop_vec, voivodeship = voiv_vec,
    stringsAsFactors = FALSE
  )
  # Sort by population descending, then take first occurrence
  temp_df <- temp_df[order(-temp_df$population), ]
  temp_df <- temp_df[!duplicated(temp_df$name), ]

  # Filter out aggregate entries (like "urban area") that are not single cities
  temp_df <- temp_df[!grepl("urban area|metropolitan|agglomeration",
                             temp_df$name, ignore.case = TRUE), ]

  strip_diacritics <- function(x) {
    from <- c("\u0105","\u0107","\u0119","\u0142","\u0144","\u00f3",
              "\u015b","\u017a","\u017c",
              "\u0104","\u0106","\u0118","\u0141","\u0143","\u00d3",
              "\u015a","\u0179","\u017b")
    to   <- c("a","c","e","l","n","o","s","z","z",
              "A","C","E","L","N","O","S","Z","Z")
    for (j in seq_along(from)) {
      x <- gsub(from[j], to[j], x, fixed = TRUE)
    }
    return(x)
  }

  # Create ASCII name column for matching
  temp_df$name_ascii <- strip_diacritics(temp_df$name)

  builtin <- get_polish_cities()

  # Match on ASCII-normalised names
  merged <- merge(temp_df, builtin[, c("name", "historical_score",
                                         "cultural_score", "poi_count")],
                  by.x = "name_ascii", by.y = "name", all.x = TRUE)

  # Use the original (Wikidata) name, drop the helper column
  merged$name_ascii <- NULL

  # For cities not in built-in dataset, estimate scores from population
  missing_scores <- is.na(merged$historical_score)
  if (any(missing_scores)) {
    # Estimate: larger cities tend to have higher scores
    log_pop <- log10(pmax(merged$population[missing_scores], 1000))
    merged$historical_score[missing_scores] <- pmin(round(log_pop - 1), 10)
    merged$cultural_score[missing_scores]   <- pmin(round(log_pop - 1.5), 10)
    merged$poi_count[missing_scores]        <- as.integer(merged$population[missing_scores] / 500)

    if (verbose) {
      message(paste("  Estimated scores for", sum(missing_scores),
                    "cities not in built-in dataset."))
    }
  }

  result <- data.frame(
    name             = merged$name,
    voivodeship      = merged$voivodeship,
    lat              = merged$lat,
    lon              = merged$lon,
    population       = merged$population,
    historical_score = merged$historical_score,
    cultural_score   = merged$cultural_score,
    poi_count        = merged$poi_count,
    stringsAsFactors = FALSE
  )
  result <- result[order(-result$population), ]
  rownames(result) <- NULL

  if (verbose) {
    message(paste("Done. Returned", nrow(result), "Polish cities."))
  }

  return(result)
}
