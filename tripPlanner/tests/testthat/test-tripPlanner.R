test_that("cities CSV loads with required columns", {
  cs <- load_cities()
  expect_true(all(c("city", "lat", "lon") %in% names(cs)))
  expect_gte(nrow(cs), 20L)
  expect_false(anyNA(cs$lat))
})

test_that("haversine matrix is symmetric and zero-diagonal", {
  cs <- load_cities()[1:5, ]
  m  <- haversine_matrix(cs)
  expect_equal(dim(m), c(5L, 5L))
  expect_equal(unname(diag(m)), rep(0, 5))
  expect_equal(m, t(m))
  # Warsaw -> Krakow is roughly ~250 km
  expect_lt(abs(m["Warsaw", "Krakow"] - 252), 15)
})

test_that("cost matrix respects style selection", {
  cs <- load_cities()[1:4, ]
  fast  <- build_cost_matrix(cs, "train", "fastest")
  cheap <- build_cost_matrix(cs, "train", "cheapest")
  expect_false(identical(fast, cheap))
  expect_equal(unname(diag(fast)), rep(0, 4))
})

test_that("solve_tsp produces a valid permutation including endpoints", {
  cs  <- load_cities()[1:6, ]
  cm  <- build_cost_matrix(cs, "train", "fastest")
  sol <- solve_tsp(cm, start = "Warsaw", end = "Gdansk")
  expect_setequal(sol$order, rownames(cm))
  expect_equal(sol$order[1], "Warsaw")
  expect_equal(tail(sol$order, 1), "Gdansk")
  expect_true(sol$cost > 0)
})

test_that("RouteOptimizer rejects bad inputs (defensive programming)", {
  expect_error(RouteOptimizer$new(
    selected = c("Warsaw"),
    flight_in = "Warsaw",
    start_date = "2026-06-01", end_date = "2026-06-10"
  ), "selected")

  expect_error(RouteOptimizer$new(
    selected = c("Warsaw", "Atlantis"),
    flight_in = "Warsaw",
    start_date = "2026-06-01", end_date = "2026-06-10"
  ), "Unknown cities")

  expect_error(RouteOptimizer$new(
    selected = c("Warsaw", "Krakow"),
    flight_in = "Warsaw",
    start_date = "2026-06-10", end_date = "2026-06-01"
  ), "end_date")

  expect_error(RouteOptimizer$new(
    selected = c("Warsaw", "Krakow"),
    flight_in = "Warsaw",
    start_date = "2026-06-01", end_date = "2026-06-10",
    transport = "rocket"
  ), "transport")
})

test_that("plan_trip returns a trip_plan with correct legs", {
  plan <- plan_trip(
    selected   = c("Warsaw", "Krakow", "Wroclaw", "Poznan", "Gdansk"),
    flight_in  = "Warsaw",
    flight_out = "Gdansk",
    start_date = "2026-06-01",
    end_date   = "2026-06-08",
    transport  = "train",
    style      = "fastest",
    transport_provider = function(...) list()  # skip API
  )
  expect_s3_class(plan, "trip_plan")
  expect_equal(plan$route[1], "Warsaw")
  expect_equal(tail(plan$route, 1), "Gdansk")
  expect_length(plan$legs, length(plan$route) - 1L)

  # S3 print/summary should not error
  expect_output(print(plan), "Trip plan")
  s <- summary(plan)
  expect_s3_class(s, "summary.trip_plan")
  expect_equal(nrow(s$legs), length(plan$legs))
})

test_that("filter_by_style sorts as expected", {
  o1 <- structure(list(duration_h = 5, price_eur = 100, scenic_score = 0.3),
                  class = "transport_option")
  o2 <- structure(list(duration_h = 3, price_eur = 200, scenic_score = 0.9),
                  class = "transport_option")
  expect_identical(filter_by_style(list(o1, o2), "fastest")[[1]], o2)
  expect_identical(filter_by_style(list(o1, o2), "cheapest")[[1]], o1)
  expect_identical(filter_by_style(list(o1, o2), "scenic")[[1]], o2)
})
