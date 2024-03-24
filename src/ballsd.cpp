#include <Rcpp.h>
using namespace Rcpp;


// Function to manually select a direction based on probabilities
int sampleDirection(const NumericVector& probs) {
  double randValue = R::runif(0, 1); // Generate a random number between 0 and 1
  double cumulative = 0.0;
  for (int i = 0; i < probs.size(); ++i) {
    cumulative += probs[i];
    if (randValue <= cumulative) {
      return i; // Return the selected direction
    }
  }
  return probs.size() - 1; // Return the last index if none chosen (should not happen)
}

//' Ball's Algorithm for Stochastic Descent (ballsd)
 //'
 //' This function implements the Ball's Algorithm for Stochastic Descent to
 //' minimize or maximize an objective function `objFunc` with respect to its parameters.
 //'
 //' @param x0 Initial parameters as a numeric vector.
 //' @param objFunc The objective function to be optimized. It must take a single argument, the parameter vector, and return a single numeric value representing the objective function's value at those parameters.
 //' @param sinc Step size increase factor, applied when a new parameter set improves the objective function value.
 //' @param sdec Step size decrease factor, applied when a new parameter set does not improve the objective function value.
 //' @param pinc Probability increase factor, applied to increase the selection probability of a successful direction.
 //' @param pdec Probability decrease factor, applied to decrease the selection probability of an unsuccessful direction.
 //' @param maxIter Maximum number of iterations to perform.
 //' @return Returns a numeric vector of optimized parameters.
 //' @examples
 //' objFunc <- function(x) { sum((x - 1)^2) }  # Simple quadratic objective
 //' initialParams <- c(0, 0)
 //' result <- ballsd(initialParams, objFunc, sinc = 2, sdec = 2, pinc = 2, pdec = 2, maxIter = 100)
 //' @export

// [[Rcpp::export]]
NumericVector ballsd(NumericVector x0, Function objFunc, 
                     double sinc, double sdec, double pinc, double pdec, 
                     int maxIter) {
  int n = x0.size();
  int directions = 2 * n;
  NumericVector s(directions);
  NumericVector p(directions, 1.0 / directions);
  NumericVector x = clone(x0);
  double E = as<double>(objFunc(x));
  
  // Initialize step sizes
  for (int i = 0; i < directions; ++i) {
    s[i] = 2;
  }
  
  for (int k = 0; k < maxIter; ++k) {
    int selectedDirection = sampleDirection(p);
    
    int r = selectedDirection % n;
    bool isPositive = selectedDirection < n;   //Choose position
    
    NumericVector delta = clone(x);
    if (isPositive) {
      delta[r] = delta[r] + s[selectedDirection]; // updated delta
    } else {
      delta[r] = delta[r] - s[selectedDirection]; // updated delta
    }
    
    double E_new = as<double>(objFunc(delta));  //test updated delta
    
    if (E_new < E) {
      x[r] = delta[r];
      E = E_new;
      s[selectedDirection] = s[selectedDirection] * sinc;
      p[selectedDirection] = p[selectedDirection] * pinc;
    } else {
      s[selectedDirection] = s[selectedDirection] / sdec;
      p[selectedDirection] = p[selectedDirection] / pdec;
    }
    
    p = p / sum(p); // Renormalize probabilities
  }
  
  return x;
}

