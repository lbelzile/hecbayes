

//Generation of Kolmogorov-Smirnov variates
bool rightmost_interval(double U, double lambda){
  double Z = 1;
  double E;
  E = exp(-0.5*lambda);
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
      result=false;
      break;
    }
  }
  return result;
}

bool leftmost_interval(double U, double lambda){
  double H;
  double PIsq = pow(PI, 2.0);
  H = 0.5 * log(2) + 2.5 * log(PI) - 2.5 * log(lambda) - PIsq / (2 * lambda) + 0.5 * lambda;
  double lU;
  lU = log(U);
  double Z = 1;
  double E = exp(-PIsq / (2 * lambda));
  double K = lambda/PIsq;
  int j = 0;
  bool result;
  while(true){
    j = j+1;
    Z = Z - K * pow(E, pow(j, 2.0) - 1);
    if(H + log(Z) > lU){
      result[0]=true; 
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


// [[Rcpp::export]]
NumericVector cond_KS(NumericVector r){
  //Generator for the conditional density of kappa, lambda|R^2
  //Holmes and Held, section A4
  double Y;
  double X;
  NumericVector lambda(r.size());
  bool OK;
  for(int i=0; i < r.size(); i++){
   OK = false;
    while(!OK){
      Y = pow(rnorm(1)[0],2.0);
      X = 1 + (Y - sqrt(Y * (4 * r[i] + Y))) / (2 * r[i]);
      if(runif(1)[0] <= 1 / (1 + X)){
        lambda[i] = r[i] / X;
      } else {
        lambda[i] = r[i] * X;
      }
    //End of GIG random number generation.  
      double U = runif(1)[0];
      if(lambda[i] > 4.0/3.0){
        OK = rightmost_interval(U, lambda[i]);
      } else{
        OK = leftmost_interval(U, lambda[i]);
      } 
    }
  }
  return lambda;
}


