#' PolandTripPlanner: Trip Planning, Route Optimization, and Transport Suggestions
#'
#' @description
#' Provides tools for planning trips to Poland including:
#' \itemize{
#'   \item Intelligent time allocation across cities based on cultural and
#'     historical significance (TripPlanner R6 class)
#'   \item Scenic route discovery identifying interesting stops along the way
#'   \item Optimal visit-order routing via a C++ TSP solver (unified TripPlanner R6 class)
#'   \item Per-leg transport suggestions filtered by user preferences
#' }
#'
#' @useDynLib PolandTripPlanner, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom R6 R6Class
#' @importFrom httr GET content status_code timeout add_headers POST
#' @importFrom jsonlite fromJSON
#' @importFrom stats setNames runif
#' @importFrom utils read.csv head URLencode
#' @keywords internal
"_PACKAGE"
