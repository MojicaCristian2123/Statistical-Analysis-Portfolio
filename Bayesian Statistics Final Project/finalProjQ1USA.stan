data {
  int<lower=0> N; // number of counties
  int<lower=0> N2; // size of new_X matrix
  int<lower=0> K; // number of columns in the model matrix
  
  real y[N]; // response, so pred_fc_rate
  matrix[N,K] X; // model matrix
  matrix[N2, K] new_X; // matrix for predicted values
}


parameters {
  real inter; // intercept value (since it won't take on a distribution)
  vector[K] beta; // regression parameters
  vector[K] theta; // priors on beta vector
  cov_matrix[K] Sigma; // standard deviation, perhaps should be a covariance matrix Sigma
}


transformed parameters{
  vector[N] linpred;
  linpred <- X*beta;
}


// The model to be estimated.
model {

  for (i in 1:K)
  beta[1:i, 1] ~ multi_normal(theta, Sigma);
  
  y ~ normal(linpred + inter, Sigma); // maybe this has a covariance matrix
}

generated quantities{
  vector[N2] y_pred;
  y_pred <- new_X*beta; // y-values predicted by the model
}

