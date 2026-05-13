#---------------------------------------------------------#
#          Prepare Polish Cities Dataset                  #
#  Run this script to regenerate the CSV data file        #
#  for the PolandTripPlanner package from Wikidata API    #
#---------------------------------------------------------#

# Load helper functions needed to fetch data from Wikidata
source("../R/polish_cities_data.R")
source("../R/api_helpers.R")

# Fetch city data from Wikidata SPARQL API
cat("Fetching Polish cities from Wikidata API...\n")
polish_cities <- fetch_cities_from_wikidata(min_population = 40000,
                                            verbose = TRUE)

# Display summary
cat("\nPolish Cities Dataset\n")
cat("=====================\n")
cat("Number of cities:", nrow(polish_cities), "\n")
cat("Columns:", paste(names(polish_cities), collapse = ", "), "\n\n")

cat("Voivodeships represented:\n")
print(table(polish_cities$voivodeship))

cat("\nTop 10 cities by historical score:\n")
top_hist <- polish_cities[order(-polish_cities$historical_score,
                                  -polish_cities$cultural_score), ]
print(top_hist[1:10, c("name", "voivodeship", "historical_score",
                         "cultural_score", "population")])

# Save as CSV for the package (no scientific notation)
csv_path <- "../inst/extdata/polish_cities.csv"
options(scipen = 999)
write.csv(polish_cities, file = csv_path, row.names = FALSE)
cat("\nDataset saved to inst/extdata/polish_cities.csv\n")
cat(paste("Wrote", nrow(polish_cities), "cities to CSV.\n"))
