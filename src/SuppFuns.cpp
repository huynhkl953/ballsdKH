#include <Rcpp.h>
using namespace Rcpp;

//' Ackley's function in 2D
 //'
 //' @param x A numeric vector of length 2 representing the coordinates (x, y).
 //' @return The value of Ackley's function at the given coordinates.
 //' @export
 // [[Rcpp::export]]
 double ackley2D(NumericVector x) {
   double term1 = -20.0 * exp(-0.2 * sqrt(0.5 * (x[0] * x[0] + x[1] * x[1])));
   double term2 = -exp(0.5 * (cos(2 * M_PI * x[0]) + cos(2 * M_PI * x[1])));
   return term1 + term2 + 20 + exp(1);
 }

 //' Griewank's function in 2D
 //'
 //' @param x A numeric vector of length 2 representing the coordinates (x, y).
 //' @return The value of Griewank's function at the given coordinates.
 //' @export
 // [[Rcpp::export]]
 double griewank2D(Rcpp::NumericVector x) {
   double sum = 0.0, prod = 1.0;
   for(int i = 0; i < x.size(); i++) {
     sum += (x[i] * x[i]) / 4000.0;
     prod *= cos(x[i] / sqrt(i + 1));
   }
   return sum - prod + 1;
 }
