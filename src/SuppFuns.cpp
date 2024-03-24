#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double ackley2D(NumericVector x) {
  double term1 = -20.0 * exp(-0.2 * sqrt(0.5 * (x[0]* x[0] + x[1]* x[1])));
  double term2 = -exp(0.5 * (cos(2 * M_PI * x[0]) + cos(2 * M_PI * x[1])));
  double result = term1 + term2 + 20 + exp(1);
  return result;
}


// [[Rcpp::export]]
double griewank2D(Rcpp::NumericVector x) {
  double sum = 0.0;
  double prod = 1.0;
  
  for(int i = 0; i < x.size(); i++) {
    sum += (x[i] * x[i]) / 4000.0;
    prod *= cos(x[i] / sqrt(i+1));
  }
  
  return sum - prod + 1;
}
