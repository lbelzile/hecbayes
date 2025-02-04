// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
#include "Rcpp.h"

//Generation of Kolmogorov-Smirnov variates
bool rightmost_interval(double U, double lambda){
  double Z = 1;
  double E = exp(-0.5*lambda);
  int j=0;
  bool result = false;
  //Infinite loop
  while(true){
    j = j+1;
    Z = Z - pow(j + 1, 2.0) * pow(E, pow(j + 1, 2.0) - 1);
    if(Z > U){
      result = true;
      break;
    }
    j=j+1;
   Z = Z + pow(j + 1, 2.0) * pow(E, pow(j + 1, 2.0) - 1);
    if(Z < U){
      result = false;
      break;
    }
  }
  return result;
}

bool leftmost_interval(double U, double lambda){
  double PI = 3.14159265358979323846;
  double PIsq = pow(PI, 2.0);
  double H = 0.5 * log(2) + 2.5 * log(PI) - 2.5 * log(lambda) - PIsq / (2 * lambda) + 0.5 * lambda;
  double lU = log(U);
  double Z = 1;
  double E = exp(-PIsq / (2 * lambda));
  double K = lambda/PIsq;
  int j = 0;
  bool result;
  while(true){
    j = j+1;
    Z = Z - K * pow(E, pow(j, 2.0) - 1);
    if(H + log(Z) > lU){
      result = true;
      break;
    }
    j++;
    Z = Z + pow(j + 1, 2.0) * pow(E, pow(j + 1, 2.0) - 1);
    if(H + log(Z) < lU){
      result = false;
      break;
    }
  }
  return result;
}

//' Conditional Kolmogorov-Smirnov distribution draw
//'
//' Follows the logistic regression, generates from the conditional
//' distribution of \eqn{\kappa} and \eqn{\lambda} given
//' the value of \eqn{R = |y - X\beta|} outlined in Appendix A.4 of Holmes and Held (2006)
//'
//' @param r vector of absolute value of centered augmented data
//' @return vector of scale parameters \eqn{\lambda}
//' @references Leonhard Held, Chris C. Holmes (2006). \emph{Bayesian auxiliary variable models for binary and multinomial regression}, Bayesian Analysis, 1(\bold{1}), 145-168.
// [[Rcpp::export]]
Rcpp::NumericVector logist_KS(Rcpp::NumericVector r){
  //Generator for the conditional density of kappa, lambda|R^2
  //Holmes and Held, section A4
  double Y;
  double X;
  Rcpp::NumericVector lambda(r.size());
  bool OK;
  for(int i=0; i < r.size(); i++){
   OK = false;
    while(!OK){
      Y = pow(Rcpp::rnorm(1)[0],2.0);
      X = 1 + (Y - sqrt(Y * (4 * r[i] + Y))) / (2 * r[i]);
      if(Rcpp::runif(1)[0] <= 1 / (1 + X)){
        lambda[i] = r[i] / X;
      } else {
        lambda[i] = r[i] * X;
      }
    //End of GIG random number generation.
      double U = Rcpp::runif(1)[0];
      if(lambda[i] > 4.0/3.0){
        OK = rightmost_interval(U, lambda[i]);
      } else{
        OK = leftmost_interval(U, lambda[i]);
      }
    }
  }
  return lambda;
}


