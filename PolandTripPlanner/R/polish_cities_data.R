#' Built-in Dataset of Polish Cities (loaded from CSV)
#'
#' Reads the bundled polish_cities.csv file shipped with the package
#' and returns a data.frame with geographic, demographic, and cultural
#' data for notable Polish cities.
#'
#' @return A data.frame with columns: name, voivodeship, lat, lon, population,
#'   historical_score (1-10), cultural_score (1-10), poi_count (estimated
#'   points of interest)
#'
#' @details The CSV file lives in inst/extdata/polish_cities.csv and is
#'   located at runtime via system.file(). The dataset includes major cities,
#'   important tourist destinations, UNESCO World Heritage sites, and smaller
#'   towns with notable historical or cultural significance.
#'
#' @export
get_polish_cities <- function() {
  csv_path <- system.file("extdata", "polish_cities.csv",
                           package = "PolandTripPlanner")
  if (csv_path == "") {
    stop("Could not find polish_cities.csv. Is the package installed?")
  }
  cities <- read.csv(csv_path, stringsAsFactors = FALSE)
  return(cities)
}
