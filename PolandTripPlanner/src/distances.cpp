#include <Rcpp.h>
using namespace Rcpp;

#include <cmath>

const double EARTH_RADIUS_KM = 6371.0;

// Haversine formula adapted from:
// https://www.geeksforgeeks.org/haversine-formula-to-find-distance-between-two-points-on-a-sphere/
// [[Rcpp::export]]
double haversine_cpp(double lat1, double lon1, double lat2, double lon2) {
  double dLat = (lat2 - lat1) * M_PI / 180.0;
  double dLon = (lon2 - lon1) * M_PI / 180.0;

  lat1 = lat1 * M_PI / 180.0;
  lat2 = lat2 * M_PI / 180.0;

  double a = pow(sin(dLat / 2), 2) +
             pow(sin(dLon / 2), 2) *
             cos(lat1) * cos(lat2);
  double c = 2 * asin(sqrt(a));
  return EARTH_RADIUS_KM * c;
}


// Compute a full distance matrix for a set of geographic points
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
