# tripPlanner

**Nijat's module** of the multi-city trip planner (Advanced R, group project with Vika & Kamilla).

## What this module does

1. **Optimal visit order** — given the cities the user picked plus the flight-in / flight-out endpoints, computes the visit order that minimises the user-selected objective (time / cost / scenic distance).
2. **Per-leg transport suggestions** — queries flight (Amadeus Self-Service) and train (koleo.pl) APIs and returns options filtered by the preferred transport type and travel style. Falls back to deterministic mock data when no API credentials are set, so the Shiny UI always has something to display.

## Techniques used

| Technique from the brief | Where |
|---|---|
| Advanced R + defensive programming | `R/utils.R` (`.assert_*`), input validation in `RouteOptimizer$initialize` |
| OOP — R6 | `RouteOptimizer` (`R/route_optimizer.R`) |
| OOP — S3 | `trip_plan`, `summary.trip_plan`, `transport_option` print methods |
| Rcpp | `src/tsp.cpp` — Held–Karp DP, 2-opt, vectorised Haversine |
| Vectorisation / performance | C++ Haversine matrix; vectorised cost matrix in `build_cost_matrix()` |
| R package structure | this directory (DESCRIPTION/NAMESPACE/R/src/inst) |
| Shiny | not in this module — Vika's UI consumes `plan_trip()` and `RouteOptimizer` |

## Public API

```r
library(tripPlanner)

cities <- load_cities()            # top-20 PL cities (CSV in inst/extdata)

plan <- plan_trip(
  selected   = c("Warsaw", "Krakow", "Wroclaw", "Poznan", "Gdansk"),
  flight_in  = "Warsaw",
  flight_out = "Gdansk",
  start_date = "2026-06-01",
  end_date   = "2026-06-08",
  transport  = "train",
  style      = "fastest"
)
print(plan)
summary(plan)
plan$legs[[1]]$options          # ranked transport options for leg 1
```

For finer control:

```r
ro  <- RouteOptimizer$new(...)   # same args
cm  <- ro$cost_matrix()
sol <- solve_tsp(cm, start = "Warsaw", end = "Gdansk")
ro$plan()
```

## Building

```sh
R -e 'Rcpp::compileAttributes("tripPlanner"); install.packages("tripPlanner", repos=NULL, type="source")'
```

## API credentials (optional)

Set environment variables before launching R/Shiny:

```
AVIATIONSTACK_KEY=...           # preferred: real flight schedules (free tier)
GOOGLE_MAPS_API_KEY=...
```

Without them, `get_transport_options()` returns deterministic mock results.
