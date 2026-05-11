# advanced-R — Multi-City Trip Planner

Group project for the *Advanced R* course. The app helps a traveller pick a set
of cities, then plans the most efficient way to visit them and surfaces
transport options for each leg.

## Module split

| Member  | Module / responsibility                                                                 |
|---------|------------------------------------------------------------------------------------------|
| Vika    | Shiny UI: dropdown of cities, flight-in / flight-out, dates, transport, style           |
| Nijat   | TSP route optimisation + per-leg transport suggestions                                  |
| Kamilla | Time allocation per city + scenic detour discovery along the route                      |

All three modules now live in the single [tripPlanner/](tripPlanner/) R
package — see [tripPlanner/README.md](tripPlanner/README.md).

## Quick start

Prerequisites:
- R ≥ 4.1
- A C++ toolchain (macOS: `xcode-select --install`; Debian/Ubuntu: `apt install build-essential`)

From a fresh clone:
```sh
make            # installs deps, builds the package, runs all 27 tests
make demo       # runs an end-to-end sample trip plan
make help       # list other targets
```

## What the `tripPlanner` package provides

- **`load_cities()`** — reads the bundled Polish cities CSV (with population, voivodeship and historical / cultural scores).
- **`fetch_cities_from_wikidata()`** — live Wikidata SPARQL alternative; degrades to the bundled CSV when offline.
- **`plan_trip(...)`** — one-call pipeline: validates inputs → builds a cost matrix → solves the TSP → attaches ranked transport options for each leg → allocates trip days per city → (car+scenic only) finds scenic detours.
- **`RouteOptimizer`** (R6) — same pipeline, step-by-step (`$cost_matrix()`, `$plan()`, `$allocate_time()`, `$discover_route()`).
- **`solve_tsp()`** — exact Held–Karp DP (n ≤ 12) or nearest-neighbour + 2-opt (larger n), implemented in C++ via Rcpp.
- **`calculate_importance()` / `allocate_days()`** — vectorised scoring + day distribution across cities.
- **`find_route_discoveries()`** — uses a C++ batched min-distance-to-route to rank off-path stops.
- **`get_transport_options()`** — queries Amadeus / Aviationstack / koleo.pl and degrades to deterministic mocks if no credentials.

## Techniques covered (per the course brief)

| Technique                                  | Where                                               |
|--------------------------------------------|-----------------------------------------------------|
| Advanced functions + defensive programming | [tripPlanner/R/utils.R](tripPlanner/R/utils.R), input validation in `RouteOptimizer$initialize` |
| OOP — R6                                   | [tripPlanner/R/route_optimizer.R](tripPlanner/R/route_optimizer.R) |
| OOP — S3                                   | `trip_plan` / `transport_option` `print` & `summary` methods |
| Rcpp                                       | [tripPlanner/src/tsp.cpp](tripPlanner/src/tsp.cpp)  |
| Vectorisation / performance                | C++ Haversine matrix; vectorised `build_cost_matrix()` |
| R package structure                        | [tripPlanner/](tripPlanner/) (DESCRIPTION/NAMESPACE/R/src/inst/tests) |
| Shiny app                                  | Vika's module (consumes `plan_trip()`)              |

## Repository layout

```
.
├── Makefile              # one-command bootstrap (deps + install + tests)
├── README.md             # this file
└── tripPlanner/          # Nijat's R package
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R/                # R sources
    ├── src/              # C++ (Rcpp) sources
    ├── inst/extdata/     # cities.csv reference data
    └── tests/testthat/   # unit tests
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
