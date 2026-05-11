# Vika — integration guide

This is everything you need to consume `tripPlanner` from Shiny.
You should not need to read the C++, the optimiser, or the API code.

## Setup (one time)

```sh
git clone <repo>
cd advanced-R
make            # installs all R deps incl. shiny, builds the package
```

Prereqs on a fresh machine:
- R ≥ 4.1
- A C++ toolchain
  - macOS: `xcode-select --install`
  - Debian/Ubuntu: `sudo apt install r-base build-essential`
  - Windows: install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)

If `make` succeeds you can immediately:

```sh
make shiny      # launches a working Shiny demo in your browser
make demo       # prints a planning example to the console
```

The `make shiny` target runs [shiny/app.R](../shiny/app.R) — copy/rename it
and replace the layout. The data wiring is already done.

## The whole API surface

You only ever touch three things:

```r
library(tripPlanner)

cities <- load_cities()    # data.frame: city, country, lat, lon, iata,
                           # population, voivodeship, historical_score,
                           # cultural_score, poi_count
                           # use `cities$city` to populate the dropdown.

plan <- plan_trip(
  selected   = c("Warsaw","Krakow","Gdansk"),  # character vector
  flight_in  = "Warsaw",
  flight_out = "Gdansk",                        # or NULL = same as flight_in
  start_date = "2026-06-01",                    # Date or "YYYY-MM-DD"
  end_date   = "2026-06-08",
  transport  = "train",                         # plane / train / bus / car
  style      = "fastest",                       # fastest / cheapest / scenic
  radius_km  = 30                               # scenic-detour radius (km)
)
```

Bad inputs throw with a clear message — wrap in `tryCatch` and show
`conditionMessage(e)` in the UI.

If you need a richer city catalogue, call
`fetch_cities_from_wikidata(min_population = 50000)` instead of
`load_cities()`. It queries Wikidata SPARQL and degrades to the bundled
CSV when offline.

## What `plan` looks like

```r
plan$route        # c("Warsaw","Krakow","Wroclaw","Gdansk")
plan$total_cost   # numeric, units depend on style (see below)
plan$method       # "exact" or "heuristic"
plan$transport    # echoes input
plan$style        # echoes input
plan$start_date   # Date
plan$end_date     # Date

plan$legs         # list, length = length(route) - 1
                  # each element:
plan$legs[[1]]$from        # "Warsaw"
plan$legs[[1]]$to          # "Krakow"
plan$legs[[1]]$leg_cost    # numeric
plan$legs[[1]]$options     # list of transport_option (already ranked best-first)

opt <- plan$legs[[1]]$options[[1]]
opt$mode         # "train"
opt$depart       # POSIXct
opt$duration_h   # numeric (hours)
opt$price_eur    # numeric
opt$scenic_score # numeric, higher = more scenic
opt$provider     # "Aviationstack (LOT LO123)" / "koleo.pl" / "mock-train" ...

# --- Kamilla's features, included in the same plan object ---

plan$allocation   # data.frame or NULL
                  # one row per city in the route, ranked by importance:
                  # $city        chr   "Krakow"
                  # $importance  num   0.812
                  # $days        int   3
                  # The `days` column always sums to (end_date - start_date + 1).

plan$discoveries  # NULL unless transport == "car" AND style == "scenic"
                  # When set:
plan$discoveries$n_found            # integer, may be 0
plan$discoveries$radius_km          # the search radius used
plan$discoveries$route_cities       # character vector of route stops
plan$discoveries$discoveries        # data.frame:
                                    # $city               chr
                                    # $distance_km        num   (off route)
                                    # $importance         num   (ranking key)
                                    # $nearest_route_city chr
                                    # $lat, $lon          num   (for a map)
```

## Units of `total_cost` / `leg_cost` (for tooltips)

| `style`    | Unit                                              |
|------------|---------------------------------------------------|
| `fastest`  | hours of travel                                   |
| `cheapest` | EUR                                               |
| `scenic`   | km of ground distance (planes are penalised x3)   |

## When `plan$allocation` / `plan$discoveries` are NULL

- `plan$allocation` is computed whenever the cities table has the
  scoring columns (the bundled `load_cities()` does). If you ever swap in
  a custom CSV without those columns, allocation will be skipped silently
  and you'll get a warning at the console — guard with `is.null()` in the UI.
- `plan$discoveries` is **only** populated when `style == "scenic"`
  (any transport mode). Otherwise it's `NULL` — guard with `is.null()`
  in the UI. To force it on regardless, call
  `RouteOptimizer$new(...)$discover_route(force = TRUE)` directly.

## Optional: real flight data

Without keys the package returns deterministic mocks (every leg works).
For real airline schedules:

```sh
# in ~/.Renviron (R reads this on startup):
AVIATIONSTACK_KEY=your_key
```

Sign up at <https://aviationstack.com/signup/free>. Free tier returns real
schedules (LOT, Ryanair, etc.); prices are estimated from distance.

## When something doesn't render

Most likely cause: the user picked `flight_in` / `flight_out` that aren't in
their `selected` list. The package auto-adds them, so nothing breaks, but
double-check `plan$route` to see what was actually planned.
