# PolandTripPlanner

Group project for *Advanced R*. Plans multi-city trips across Poland: allocates time per city, discovers nearby detours, optimises visit order (TSP), and suggests transport for each leg.

## Team

| Member  | Responsibility |
|---------|----------------|
| Kamilla | Time allocation + route discovery (`TripPlanner`, `City`) |
| Nijat   | TSP optimisation + transport (`RouteOptimizer`, `plan_trip`) |
| Vika    | Shiny UI |

## Setup

Requires R ≥ 4.1 and a C++ toolchain.

```sh
make          # install deps + build package
make demo     # run demo
make shiny    # launch Shiny app
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
planner$allocate_time()
planner$print_allocation()
planner$discover_route(radius_km = 50)
planner$print_discovery()
```

## TripPlanner methods

| Method | Description |
|--------|-------------|
| `TripPlanner$new(csv_path)` | Create planner (optional custom CSV) |
| `$set_trip(must_see, start_date, end_date, transport)` | Configure trip |
| `$allocate_time(min_days, weights)` | Distribute days across cities |
| `$discover_route(radius_km, max_suggestions, suggest_nearby)` | Find nearby cities |
| `$print_allocation()` / `$print_discovery()` | Print results |
| `$get_city_objects()` / `$get_cities_data()` | Access data |
| `$total_days` / `$city_count` | Read-only bindings |

Parameters:
- `transport`: `"car"`, `"train"`, `"bus"`, `"plane"`
- `weights`: `list(population, historical, cultural, poi)`
- `suggest_nearby`: `TRUE`/`FALSE`

## Standalone functions

```r
get_polish_cities()
calculate_importance(population, historical_score, cultural_score, poi_count)
find_route_discoveries(route_cities, all_cities, radius_km, max_suggestions)
load_cities()
plan_trip(cities, start_city, budget, style)
solve_tsp(cost_matrix, method)
get_transport_options(from, to)
```

## Techniques

| Technique | Where |
|-----------|-------|
| R6 OOP | `TripPlanner`, `City`, `RouteOptimizer` |
| S3 OOP | `trip_plan`, `transport_option` print/summary methods |
| Rcpp | `src/tsp.cpp` (TSP solvers), `src/distances.cpp` (Haversine) |
| Vectorisation | C++ Haversine matrix, `build_cost_matrix()`, `calculate_importance()` |
| R package | DESCRIPTION / NAMESPACE / R / src / inst / man |
| Shiny | `shiny/app.R` |
| API integration | Aviationstack, Amadeus, koleo.pl |
| Defensive programming | Input validation in `utils.R`, `TripPlanner`, `RouteOptimizer` |

## Repository layout

```
├── Makefile
├── README.md
├── PolandTripPlanner/
│   ├── R/
│   ├── src/
│   ├── inst/extdata/
│   └── man/
├── shiny/
├── scripts/
└── docs/
```

## API keys (optional)

Transport lookups work offline with mocks. For live data:

```sh
export AVIATIONSTACK_KEY=...
export GOOGLE_MAPS_API_KEY='AIza...'
# or persist for R sessions:
echo 'AVIATIONSTACK_KEY=...' >> ~/.Renviron
echo 'GOOGLE_MAPS_API_KEY=...' >> ~/.Renviron
```
