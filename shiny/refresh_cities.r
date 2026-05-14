pkgload::load_all("PolandTripPlanner")

cities <- fetch_cities_from_wikidata(verbose = TRUE)
if ("name" %in% names(cities) && !"city" %in% names(cities)) {
	names(cities)[names(cities) == "name"] <- "city"
}
write.csv(cities, "PolandTripPlanner/inst/extdata/cities.csv", row.names = FALSE)

message("Done! Saved ", nrow(cities), " cities to PolandTripPlanner/inst/extdata/cities.csv")
