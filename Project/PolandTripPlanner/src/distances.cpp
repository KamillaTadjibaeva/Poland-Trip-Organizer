#include <Rcpp.h>
using namespace Rcpp;

#include <cmath>

// ---------------------------------------------------------------------------
// C++ functions for distance computation in PolandTripPlanner
// Uses Rcpp for integration with R
// These functions provide high-performance geographic distance calculations
// needed for route discovery (finding cities near a travel route)
// ---------------------------------------------------------------------------

const double EARTH_RADIUS_KM = 6371.0;
const double DEG_TO_RAD = M_PI / 180.0;


// Haversine distance between two geographic points (in km)
//
// @param lat1 Latitude of point 1 (degrees)
// @param lon1 Longitude of point 1 (degrees)
// @param lat2 Latitude of point 2 (degrees)
// @param lon2 Longitude of point 2 (degrees)
// @return Distance in kilometers
// [[Rcpp::export]]
double haversine_cpp(double lat1, double lon1, double lat2, double lon2) {
  double dlat = (lat2 - lat1) * DEG_TO_RAD;
  double dlon = (lon2 - lon1) * DEG_TO_RAD;

  double a = sin(dlat / 2.0) * sin(dlat / 2.0) +
             cos(lat1 * DEG_TO_RAD) * cos(lat2 * DEG_TO_RAD) *
             sin(dlon / 2.0) * sin(dlon / 2.0);

  double c = 2.0 * atan2(sqrt(a), sqrt(1.0 - a));

  return EARTH_RADIUS_KM * c;
}


// Compute a full distance matrix for a set of geographic points
// This is O(n^2) but the inner loop is very fast in C++
//
// @param lats Numeric vector of latitudes
// @param lons Numeric vector of longitudes
// @return Symmetric distance matrix (km)
// [[Rcpp::export]]
NumericMatrix haversine_matrix_cpp(NumericVector lats, NumericVector lons) {
  int n = lats.size();
  NumericMatrix dist_mat(n, n);

  for (int i = 0; i < n; i++) {
    dist_mat(i, i) = 0.0;
    for (int j = i + 1; j < n; j++) {
      double d = haversine_cpp(lats[i], lons[i], lats[j], lons[j]);
      dist_mat(i, j) = d;
      dist_mat(j, i) = d;
    }
  }

  return dist_mat;
}
