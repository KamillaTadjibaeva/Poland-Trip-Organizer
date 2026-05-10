#' Solve the (open) Travelling Salesman Problem.
#'
#' Computes the visit order that minimises the total cost, starting from
#' `start` and (optionally) ending at `end`. Uses the exact Held–Karp DP
#' for small instances (n <= `exact_threshold`) and falls back to a
#' nearest-neighbour seed + 2-opt local search for larger ones.
#'
#' @param cost_matrix Numeric square cost matrix (e.g. [build_cost_matrix()]).
#' @param start City name or 1-based index of the flight-in city.
#' @param end   City name or 1-based index of the flight-out city, or `NULL`
#'   for an open tour ending wherever is cheapest.
#' @param exact_threshold Use exact DP when `n <= exact_threshold`.
#' @return A list with elements `order` (city names in visit order),
#'   `indices` (1-based), `cost` (total), `method` (`"exact"`/`"heuristic"`).
#' @export
solve_tsp <- function(cost_matrix, start, end = NULL, exact_threshold = 12L) {
  .assert_square_matrix(cost_matrix)
  n <- nrow(cost_matrix)
  city_names <- rownames(cost_matrix)
  if (is.null(city_names)) city_names <- as.character(seq_len(n))

  resolve <- function(x, label) {
    if (is.numeric(x)) {
      if (length(x) != 1L || x < 1 || x > n || x != as.integer(x)) {
        stop(sprintf("`%s` index out of range.", label), call. = FALSE)
      }
      as.integer(x)
    } else if (is.character(x) && length(x) == 1L) {
      idx <- match(tolower(x), tolower(city_names))
      if (is.na(idx)) stop(sprintf("`%s` city '%s' not in matrix.", label, x),
                           call. = FALSE)
      idx
    } else {
      stop(sprintf("`%s` must be a city name or 1-based index.", label), call. = FALSE)
    }
  }

  s_idx <- resolve(start, "start")
  e_idx <- if (is.null(end)) -1L else resolve(end, "end")
  if (e_idx == s_idx && !is.null(end) && n > 1L) {
    stop("`start` and `end` must differ when both are given.", call. = FALSE)
  }

  use_exact <- n <= exact_threshold
  res <- if (use_exact) {
    tsp_held_karp(cost_matrix, s_idx - 1L, if (e_idx > 0) e_idx - 1L else -1L)
  } else {
    tsp_two_opt(cost_matrix, s_idx - 1L, if (e_idx > 0) e_idx - 1L else -1L)
  }

  list(
    order   = city_names[res$order],
    indices = as.integer(res$order),
    cost    = as.numeric(res$cost),
    method  = if (use_exact) "exact" else "heuristic"
  )
}
