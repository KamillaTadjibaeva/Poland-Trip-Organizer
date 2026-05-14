# PolandTripPlanner - Demo Script

# ---- Setup ----

library(R6)

pkg_dir <- tryCatch(
  file.path(dirname(rstudioapi::getSourceEditorContext()$path),
            "PolandTripPlanner"),
  error = function(e) "PolandTripPlanner"
)
if (!dir.exists(pkg_dir)) pkg_dir <- "PolandTripPlanner"

for (f in list.files(file.path(pkg_dir, "R"), "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

library(Rcpp)
sourceCpp(file.path(pkg_dir, "src", "distances.cpp"))


# ---- Time allocation ----

planner <- TripPlanner$new()

planner$set_trip(
  must_see   = c("Warsaw", "Krakow", "Gdansk", "Wroclaw", "Poznan"),
  start_date = "2026-07-01",
  end_date   = "2026-07-15",
  transport  = "car"
)

allocation <- planner$allocate_time(min_days = 1)
planner$print_allocation()

city_objects <- planner$get_city_objects()
for (city in city_objects) {
  city$print()
}

# Find nearby cities for each must-see city
cities_df <- planner$get_cities_data()
for (city in city_objects) {
  city$find_nearby_cities(cities_df, radius_km = 80)
}

# History-focused weights
planner$allocate_time(
  weights = list(population = 0.05, historical = 0.50,
                 cultural = 0.35, poi = 0.10)
)
planner$print_allocation()


# ---- Route discovery ----

discovery <- planner$discover_route(radius_km = 50, max_suggestions = 8, suggest_nearby = TRUE)
planner$print_discovery()

planner$discover_route(radius_km = 25, max_suggestions = 5, suggest_nearby = TRUE)
planner$print_discovery()

# Suggestions turned off
planner2 <- TripPlanner$new()
planner2$set_trip(c("Warsaw", "Krakow", "Gdansk"), "2026-08-01",
                  "2026-08-10", transport = "train")
planner2$discover_route(radius_km = 50, max_suggestions = 5,
                        suggest_nearby = FALSE)
planner2$print_discovery()

