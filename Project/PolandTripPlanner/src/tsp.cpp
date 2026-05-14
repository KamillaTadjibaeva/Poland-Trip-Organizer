// tsp.cpp — Travelling Salesman Problem solvers
// Exposed to R via Rcpp.
#include <Rcpp.h>
#include <vector>
#include <limits>
#include <algorithm>
using namespace Rcpp;

// ---------------------------------------------------------------------------
// Held–Karp exact DP, O(n^2 * 2^n). Suitable for n <= ~15.
// `start` and `end` are 0-indexed; if end < 0 the tour is open from `start`.
// Returns the optimal order (as 1-indexed integer vector) with attribute "cost".
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
List tsp_held_karp(NumericMatrix d, int start, int end) {
    const int n = d.nrow();
    if (d.ncol() != n) stop("Cost matrix must be square");
    if (start < 0 || start >= n) stop("Invalid start index");
    const bool fixed_end = (end >= 0);
    if (fixed_end && (end >= n)) stop("Invalid end index");
    if (n > 20) stop("Held-Karp is limited to n <= 20 nodes");

    const double INF = std::numeric_limits<double>::infinity();
    const int FULL = 1 << n;
    // dp[mask][j] = min cost to start at `start`, visit set `mask` (incl. start & j), end at j
    std::vector<std::vector<double>> dp(FULL, std::vector<double>(n, INF));
    std::vector<std::vector<int>>    parent(FULL, std::vector<int>(n, -1));

    dp[1 << start][start] = 0.0;

    for (int mask = 0; mask < FULL; ++mask) {
        if (!(mask & (1 << start))) continue;
        for (int j = 0; j < n; ++j) {
            if (!(mask & (1 << j))) continue;
            double cur = dp[mask][j];
            if (cur == INF) continue;
            for (int k = 0; k < n; ++k) {
                if (mask & (1 << k)) continue;
                double cand = cur + d(j, k);
                int nmask = mask | (1 << k);
                if (cand < dp[nmask][k]) {
                    dp[nmask][k] = cand;
                    parent[nmask][k] = j;
                }
            }
        }
    }

    int full_mask = FULL - 1;
    double best = INF;
    int best_end = -1;
    if (fixed_end) {
        best = dp[full_mask][end];
        best_end = end;
    } else {
        for (int j = 0; j < n; ++j) {
            if (dp[full_mask][j] < best) {
                best = dp[full_mask][j];
                best_end = j;
            }
        }
    }
    if (best_end < 0 || best == INF) stop("No feasible tour found");

    // Reconstruct
    std::vector<int> path;
    int mask = full_mask, cur = best_end;
    while (cur != -1) {
        path.push_back(cur);
        int p = parent[mask][cur];
        mask ^= (1 << cur);
        cur = p;
    }
    std::reverse(path.begin(), path.end());

    IntegerVector order(path.size());
    for (size_t i = 0; i < path.size(); ++i) order[i] = path[i] + 1; // 1-indexed

    return List::create(_["order"] = order, _["cost"] = best);
}

// ---------------------------------------------------------------------------
// Heuristic: nearest neighbour + 2-opt local improvement.
// Used for larger n (where Held–Karp is infeasible).
// ---------------------------------------------------------------------------
static double tour_cost(const NumericMatrix& d,
                        const std::vector<int>& tour,
                        bool fixed_end) {
    double s = 0.0;
    for (size_t i = 1; i < tour.size(); ++i) s += d(tour[i - 1], tour[i]);
    (void)fixed_end;
    return s;
}

// [[Rcpp::export]]
List tsp_two_opt(NumericMatrix d, int start, int end, int max_iter = 1000) {
    const int n = d.nrow();
    if (d.ncol() != n) stop("Cost matrix must be square");
    if (start < 0 || start >= n) stop("Invalid start index");
    const bool fixed_end = (end >= 0);
    if (fixed_end && (end >= n)) stop("Invalid end index");

    // Nearest neighbour seed
    std::vector<int> tour;
    std::vector<bool> visited(n, false);
    tour.push_back(start);
    visited[start] = true;
    while ((int)tour.size() < n) {
        int last = tour.back();
        int best = -1; double bd = std::numeric_limits<double>::infinity();
        for (int j = 0; j < n; ++j) {
            if (visited[j]) continue;
            if (fixed_end && j == end && (int)tour.size() < n - 1) continue;
            if (d(last, j) < bd) { bd = d(last, j); best = j; }
        }
        if (best < 0) {
            // fall back: pick any unvisited (e.g. fixed end is the only one left)
            for (int j = 0; j < n; ++j) if (!visited[j]) { best = j; break; }
        }
        tour.push_back(best);
        visited[best] = true;
    }

    // 2-opt — never swap positions that would move start (idx 0) or fixed end (last idx)
    bool improved = true;
    int iter = 0;
    while (improved && iter < max_iter) {
        improved = false;
        ++iter;
        int last_swappable = fixed_end ? n - 2 : n - 1;
        for (int i = 1; i <= last_swappable - 1; ++i) {
            for (int k = i + 1; k <= last_swappable; ++k) {
                int a = tour[i - 1], b = tour[i];
                int c = tour[k];
                int dnext = (k + 1 < n) ? tour[k + 1] : -1;
                double before = d(a, b) + (dnext >= 0 ? d(c, dnext) : 0.0);
                double after  = d(a, c) + (dnext >= 0 ? d(b, dnext) : 0.0);
                if (after + 1e-12 < before) {
                    std::reverse(tour.begin() + i, tour.begin() + k + 1);
                    improved = true;
                }
            }
        }
    }

    IntegerVector order(n);
    for (int i = 0; i < n; ++i) order[i] = tour[i] + 1;
    return List::create(_["order"] = order,
                        _["cost"]  = tour_cost(d, tour, fixed_end),
                        _["iterations"] = iter);
}

// ---------------------------------------------------------------------------
// Vectorised Haversine distance matrix — pure C++ for speed.
// lat / lon in decimal degrees; returns kilometres.
// NOTE: Not exported to R (name conflict with distances.cpp haversine_cpp).
// The R layer uses haversine_matrix_cpp() from distances.cpp instead.
// ---------------------------------------------------------------------------
NumericMatrix haversine_cpp_matrix_internal(NumericVector lat, NumericVector lon) {
    const int n = lat.size();
    if (lon.size() != n) stop("lat and lon must have the same length");
    NumericMatrix m(n, n);
    const double R = 6371.0088;
    const double DEG = M_PI / 180.0;

    std::vector<double> rlat(n), rlon(n);
    for (int i = 0; i < n; ++i) { rlat[i] = lat[i] * DEG; rlon[i] = lon[i] * DEG; }

    for (int i = 0; i < n; ++i) {
        m(i, i) = 0.0;
        for (int j = i + 1; j < n; ++j) {
            double dlat = rlat[j] - rlat[i];
            double dlon = rlon[j] - rlon[i];
            double a = std::sin(dlat / 2.0);
            double b = std::sin(dlon / 2.0);
            double h = a * a + std::cos(rlat[i]) * std::cos(rlat[j]) * b * b;
            double c = 2.0 * std::atan2(std::sqrt(h), std::sqrt(1.0 - h));
            double dist = R * c;
            m(i, j) = dist;
            m(j, i) = dist;
        }
    }
    return m;
}
