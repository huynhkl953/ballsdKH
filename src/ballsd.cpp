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
