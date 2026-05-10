#' tripPlanner: Optimal trip routing and transport suggestions.
#'
#' @description
#' The Nijat module of the multi-city trip planner. It provides:
#' \itemize{
#'   \item a fast Travelling Salesman solver implemented in C++ via Rcpp,
#'   \item an [R6::R6Class()] [RouteOptimizer] orchestrating the trip,
#'   \item transport-leg suggestions filtered by user preferences.
#' }
#'
#' @keywords internal
"_PACKAGE"
