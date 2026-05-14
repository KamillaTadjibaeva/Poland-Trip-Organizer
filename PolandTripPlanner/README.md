# PolandTripPlanner

## Install

```r
install.packages(c("R6", "httr", "jsonlite", "Rcpp"))
devtools::install("PolandTripPlanner")
```

## Usage

```r
library(PolandTripPlanner)

planner <- TripPlanner$new()

planner$set_trip(
  must_see   = c("Warsaw", "Krakow", "Gdansk"),
  start_date = "2026-07-01",
  end_date   = "2026-07-10",
  transport  = "car"
)

planner$allocate_time(min_days = 1)
planner$print_allocation()

planner$discover_route(radius_km = 50, max_suggestions = 5, suggest_nearby = TRUE)
planner$print_discovery()
```

## Methods

| Method | Description |
|--------|-------------|
| `TripPlanner$new(csv_path)` | Create planner |
| `$set_trip(must_see, start_date, end_date, transport)` | Configure trip |
| `$allocate_time(min_days, weights)` | Distribute days across cities |
| `$discover_route(radius_km, max_suggestions, suggest_nearby)` | Find nearby cities |
| `$print_allocation()` | Print time allocation |
| `$print_discovery()` | Print discovered cities |
| `$get_city_objects()` | Get City R6 objects |
| `$get_cities_data()` | Get full cities data.frame |
| `$total_days` | Read-only: trip duration |
| `$city_count` | Read-only: number of cities |

## Parameters

- `transport`: `"car"`, `"train"`, `"bus"`, `"plane"`
- `suggest_nearby`: `TRUE`/`FALSE` — toggle nearby city suggestions
- `weights`: `list(population, historical, cultural, poi)` — importance weights

## Standalone functions

```r
get_polish_cities()
calculate_importance(population, historical_score, cultural_score, poi_count)
find_route_discoveries(route_cities, all_cities, radius_km, max_suggestions)
```
