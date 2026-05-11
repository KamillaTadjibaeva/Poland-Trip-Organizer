#' Fetch Polish cities live from the Wikidata SPARQL endpoint.
#'
#' Queries Wikidata for Polish cities with population > `min_population`,
#' retrieving name, coordinates, population and voivodeship. Historical /
#' cultural scores aren't available in Wikidata, so the function merges the
#' bundled [load_cities()] table to fill those columns where possible, and
#' estimates them from population for the rest.
#'
#' Falls back to [load_cities()] if the API is unreachable.
#'
#' @param min_population Minimum population to include (default: 50000).
#' @param verbose        Print progress messages? (default: TRUE).
#' @return data.frame compatible with [load_cities()] (columns: city, lat,
#'   lon, population, voivodeship, historical_score, cultural_score, poi_count).
#' @export
fetch_cities_from_wikidata <- function(min_population = 50000L, verbose = TRUE) {
  if (!is.numeric(min_population) || min_population < 0) {
    stop("'min_population' must be >= 0.", call. = FALSE)
  }

  sparql <- paste0('
SELECT DISTINCT ?cityLabel ?lat ?lon ?population ?voivodeshipLabel WHERE {
  ?city wdt:P31/wdt:P279* wd:Q515 .
  ?city wdt:P17 wd:Q36 .
  ?city wdt:P625 ?coords .
  ?city wdt:P1082 ?population .
  ?city wdt:P131 ?voivodeship .
  ?voivodeship wdt:P31 wd:Q150093 .
  BIND(geof:latitude(?coords)  AS ?lat)
  BIND(geof:longitude(?coords) AS ?lon)
  FILTER(?population > ', as.integer(min_population), ')
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en,pl". }
}
ORDER BY DESC(?population)
LIMIT 200')

  if (verbose) message("Fetching Polish cities from Wikidata SPARQL...")

  resp <- tryCatch(
    httr::GET(
      "https://query.wikidata.org/sparql",
      query = list(query = sparql, format = "json"),
      httr::timeout(30),
      httr::add_headers(
        "Accept"     = "application/sparql-results+json",
        "User-Agent" = "tripPlanner/0.1 (R package; educational project)"
      )
    ),
    error = function(e) { warning("Wikidata API failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("Wikidata unreachable; falling back to bundled cities.")
    return(load_cities())
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  bindings <- parsed$results$bindings
  if (is.null(bindings) || !length(bindings)) {
    message("Empty Wikidata result; falling back to bundled cities.")
    return(load_cities())
  }

  n <- length(bindings)
  names_v <- character(n); lat_v <- numeric(n); lon_v <- numeric(n)
  pop_v   <- numeric(n);   voiv_v <- character(n)
  for (i in seq_len(n)) {
    b <- bindings[[i]]
    names_v[i] <- b$cityLabel$value
    lat_v[i]   <- as.numeric(b$lat$value)
    lon_v[i]   <- as.numeric(b$lon$value)
    pop_v[i]   <- as.numeric(b$population$value)
    voiv_v[i]  <- gsub(" Voivodeship", "", b$voivodeshipLabel$value)
  }

  # Wikidata returns "Kraków"; the bundled CSV uses "Krakow". Strip diacritics
  # for matching so we can merge in the historical/cultural scores.
  strip <- function(x) {
    from <- c("\u0105","\u0107","\u0119","\u0142","\u0144","\u00f3",
              "\u015b","\u017a","\u017c",
              "\u0104","\u0106","\u0118","\u0141","\u0143","\u00d3",
              "\u015a","\u0179","\u017b")
    to <- c("a","c","e","l","n","o","s","z","z",
            "A","C","E","L","N","O","S","Z","Z")
    for (j in seq_along(from)) x <- gsub(from[j], to[j], x, fixed = TRUE)
    x
  }

  df <- data.frame(city = names_v, lat = lat_v, lon = lon_v,
                   population = pop_v, voivodeship = voiv_v,
                   stringsAsFactors = FALSE)
  df <- df[order(-df$population), ]
  df <- df[!duplicated(df$city), ]
  df <- df[!grepl("urban area|metropolitan|agglomeration", df$city,
                  ignore.case = TRUE), ]
  df$ascii <- strip(df$city)

  builtin <- load_cities()
  merged <- merge(df,
                  builtin[, c("city", "historical_score", "cultural_score", "poi_count")],
                  by.x = "ascii", by.y = "city", all.x = TRUE)
  merged$ascii <- NULL

  missing_scores <- is.na(merged$historical_score)
  if (any(missing_scores)) {
    log_pop <- log10(pmax(merged$population[missing_scores], 1000))
    merged$historical_score[missing_scores] <- pmin(round(log_pop - 1),   10)
    merged$cultural_score[missing_scores]   <- pmin(round(log_pop - 1.5), 10)
    merged$poi_count[missing_scores]        <- as.integer(merged$population[missing_scores] / 500)
  }

  out <- data.frame(
    city             = merged$city,
    country          = "PL",
    lat              = merged$lat,
    lon              = merged$lon,
    iata             = NA_character_,
    rail_code        = NA_character_,
    population       = merged$population,
    voivodeship      = merged$voivodeship,
    historical_score = merged$historical_score,
    cultural_score   = merged$cultural_score,
    poi_count        = merged$poi_count,
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$population), ]
  rownames(out) <- NULL

  if (verbose) message("Returned ", nrow(out), " Polish cities from Wikidata.")
  out
}
