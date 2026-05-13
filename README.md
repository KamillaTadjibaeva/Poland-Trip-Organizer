# advanced-R — Multi-City Trip Planner

Group project for the *Advanced R* course. The app helps a traveller pick a set
of cities, then plans the most efficient way to visit them, allocates time per
city based on importance, discovers scenic detours, and surfaces transport
options for each leg.

## Module split

| Member    | Module / responsibility                                                               |
|-----------|----------------------------------------------------------------------------------------|
| Vika      | Shiny UI: dropdown of top 20 cities, flight-in / flight-out, dates, transport, style  |
| **Nijat** | TSP route optimisation + per-leg transport suggestions (`RouteOptimizer`)              |
| **Kamilla** | Time-allocation per city + scenic detour discovery (`TripPlanner`, `City`)           |

Everything is integrated into the single **`PolandTripPlanner`** R package under
[Project/PolandTripPlanner/](Project/PolandTripPlanner/).

## Quick start

Prerequisites:
- R ≥ 4.1
- A C++ toolchain (macOS: `xcode-select --install`; Debian/Ubuntu: `apt install build-essential`)

From a fresh clone:
```sh
make            # installs deps, builds the package
make demo       # runs an end-to-end sample trip plan
make help       # list other targets
```

## What the `PolandTripPlanner` package provides

### Time allocation & discovery (Kamilla)
- **`TripPlanner`** (R6) — orchestrates time allocation and route discovery.
- **`City`** (R6) — individual city with importance scoring and day allocation.
- **`get_polish_cities()`** — bundled 40-city dataset with cultural/historical scores.
- **`calculate_importance()`** — vectorised composite importance scoring.
- **`find_route_discoveries()`** — finds interesting stops near the travel route.
- **`fetch_cities_from_wikidata()`** — live city data from Wikidata SPARQL.

### Route optimisation & transport (Nijat)
- **`load_cities()`** — reads the bundled top-20 Polish cities CSV used by the UI dropdown.
- **`plan_trip(...)`** — one-call pipeline: validates inputs → builds a cost matrix → solves the TSP → attaches ranked transport options for each leg.
- **`RouteOptimizer`** (R6) — same pipeline, step-by-step.
- **`solve_tsp()`** — exact Held–Karp DP (n ≤ 12) or nearest-neighbour + 2-opt (larger n), implemented in C++ via Rcpp.
- **`get_transport_options()`** — queries Amadeus (planes) / koleo.pl (trains) and degrades to deterministic mocks if no credentials or the API is down.

## Techniques covered (per the course brief)

| Technique                                  | Where                                                                         |
|--------------------------------------------|-------------------------------------------------------------------------------|
| Advanced functions + defensive programming | `R/utils.R`, input validation in `RouteOptimizer` and `TripPlanner`           |
| OOP — R6                                   | `TripPlanner`, `City`, `RouteOptimizer`                                       |
| OOP — S3                                   | `trip_plan` / `transport_option` `print` & `summary` methods                  |
| Rcpp                                       | `src/tsp.cpp` (TSP solvers), `src/distances.cpp` (Haversine)                  |
| Vectorisation / performance                | C++ Haversine matrix; vectorised `build_cost_matrix()`, `calculate_importance()`|
| R package structure                        | Full package: DESCRIPTION/NAMESPACE/R/src/inst/man                            |
| Shiny app                                  | `shiny/app.R` (consumes `plan_trip()`)                                        |
| API integration                            | Wikidata SPARQL, Aviationstack, Amadeus, koleo.pl                             |

## Repository layout

```
.
├── Makefile                  # one-command bootstrap (deps + install)
├── README.md                 # this file
├── PolandTripPlanner/        # the unified R package
│   ├── DESCRIPTION
│   ├── NAMESPACE
│   ├── R/                   # R sources (both modules)
│   ├── src/                 # C++ (Rcpp) sources
│   ├── inst/extdata/        # cities.csv + polish_cities.csv
│   └── man/                 # generated documentation
├── shiny/                    # Shiny front-end
├── scripts/                  # stand-alone demo scripts
└── docs/                     # integration guide
```

## API credentials (optional)

For real flight schedules, set an Aviationstack key (free tier, schedules only — prices are estimated):

```sh
export AVIATIONSTACK_KEY=...
# or persist for R sessions:
echo 'AVIATIONSTACK_KEY=...' >> ~/.Renviron
```

Amadeus is also supported (`AMADEUS_CLIENT_ID` / `AMADEUS_CLIENT_SECRET`) but their
self-service portal is being decommissioned in July 2026 and signups are closed.

Without any keys set, `get_transport_options()` returns deterministic mocks so the
pipeline (and Vika's UI) stays usable offline.
