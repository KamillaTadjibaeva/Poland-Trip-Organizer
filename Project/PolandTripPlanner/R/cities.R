#' Load the bundled top-20 cities table.
#'
#' Reads the package CSV (`inst/extdata/cities.csv`) describing the cities
#' that populate the UI dropdown. A custom `path` may be supplied to load
#' a different dataset (must contain `city`, `lat`, `lon` columns).
#'
#' @param path Optional path to a CSV file. Defaults to the package data.
#' @return A data.frame with rows for each city.
#' @export
#' @examples
#' head(load_cities())
load_cities <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "cities.csv", package = "PolandTripPlanner")
    if (!nzchar(path)) {
      # Allow running from source tree before install
      cand <- file.path("inst", "extdata", "cities.csv")
      if (file.exists(cand)) path <- cand
    }
  }
  if (!file.exists(path)) {
    stop("Cities CSV not found at: ", path, call. = FALSE)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  required <- c("city", "lat", "lon")
  missing  <- setdiff(required, names(df))
  if (length(missing)) {
    stop("Cities CSV missing columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (anyNA(df$lat) || anyNA(df$lon)) {
    stop("Cities CSV contains NA coordinates.", call. = FALSE)
  }
  df
}

#' Look up a row in the cities table by city name (case-insensitive).
#' @keywords internal
#' @noRd
.lookup_cities <- function(names, cities) {
  .assert_character(names)
  idx <- match(tolower(names), tolower(cities$city))
  if (anyNA(idx)) {
    stop("Unknown cities: ", paste(names[is.na(idx)], collapse = ", "),
         call. = FALSE)
  }
  cities[idx, , drop = FALSE]
}
