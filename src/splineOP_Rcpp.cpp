#include <Rcpp.h>
#include "splineOP.h"



//' Dynamic Programming for Quadratic Spline Segmentation
//'
//' @description
//' This function performs dynamic programming (DP) to segment a univariate signal
//' using a quadratic spline model with penalized transitions. It is a wrapper
//' around a pure C++ backend (`splineOP`) that performs all numerical computations.
//'
//' The algorithm searches for an optimal sequence of state transitions (segment boundaries)
//' minimizing the total quadratic reconstruction cost plus a regularization penalty `beta`.
//' Each segment is modeled by a quadratic function defined by positions and velocities
//' at its endpoints.
//'
//' @param data Numeric vector. The observed signal (e.g., time series) to be segmented.
//' @param beta Numeric scalar. Regularization penalty controlling the number of segments
//' (higher values favor fewer segments).
//' @param S Integer. Number of discrete states considered in the dynamic programming grid
//' (default = 10).
//' @param nb_initSpeed Integer. Number of candidate initial speeds to test (default = 5).
//' @param data_var Numeric. Variance of Gaussian noise used for random initialization of
//' the state matrix (default = 1.0).
//'
//' @return
//' An integer vector giving the changepoint locations
//'
//' @details
//' The algorithm has time complexity approximately \\(O(N^2 S^2)\\), where `N` is
//' the signal length and `S` is the number of states. The quadratic cost function
//' is precomputed using cumulative sums for efficiency, reducing per-interval
//' evaluations to constant time.
//'
//' @examples
//' # Simulate a noisy quadratic signal
//' N <- 1000
//' x <- 1:N
//' signal <- 0.01 * (x - N/2)^2 + rnorm(N, 0, 0.5)
//'
//' # Run the dynamic programming segmentation
//' system.time({
//'   chpt <- splineOP_Rcpp(signal, beta = 500, S = 12, data_var = 0.01)
//' })
//'
//' # Visualize the resulting segmentation
//' plot(signal, type = "l", main = "DP Quadratic Spline Segmentation")
//' abline(v = chpt, col = "red", lwd = 2)
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector splineOP_Rcpp(std::vector<double> data,
                                  double beta,
                                  int S = 10,
                                  int nb_initSpeed = 5,
                                  double data_var = 1.0)
{
  std::vector<int> result = splineOP(data, beta, S, nb_initSpeed, data_var);
  return Rcpp::wrap(result);
}



