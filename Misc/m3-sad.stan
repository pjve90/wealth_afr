                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          functions {

  matrix GP(int A, real kappa, real tau, real delta) {

    matrix[A, A] omega;

    for (a in 1:(A-1)) {
    for (b in (a+1):A) {
    omega[a, b] = kappa * exp(-tau * ((b-a)^2 / A^2));
    omega[b, a] = omega[a, b];
    }
    }

    for (i in 1:A) {
    omega[i, i] = 1;
    }

    return delta * cholesky_decompose(omega);
  }

}

data {

  int N;
  int Q;
  int A;
  int K [N, Q];
  int <lower = 1, upper = 2> sex [N];
  int age [N];

  vector [N] edu;
  vector [N] goods;
  vector [N] diff;

  int n_edu_miss;
  int loc_edu_miss [n_edu_miss];

}

parameters{

  ordered [4] c; //# 5 possible responses on each question

  vector <lower = 0> [Q] alpha; 
  vector [Q] gamma;

  vector [N] xi;           //# try with xi<lower=0>
  real <lower = 0> sigma;

  vector [3] beta [2];

  vector [A] theta_raw [2]; 

  real eta [2];
  real <lower = 0, upper = 1> kappa [2];
  real <lower = 0> tau [2];
  real <lower = 0> delta [2];

  vector [n_edu_miss] edu_impute;
}

transformed parameters {

  vector [A] theta [2];

  vector [N] married_full;
  vector [N] edu_full;

  vector [N] zeta;

  for (s in 1:2) {
    theta[s] = eta[s] + GP(A, kappa[s], tau[s], delta[s])*theta_raw[s];
  }

  edu_full = edu;

  if (n_edu_miss > 0) {
  for (i in 1:n_edu_miss) {

    edu_full[loc_edu_miss[i]] = edu_impute[i];

  }
  }

  for (i in 1:N) {

  if (diff[i] != -99) {

    zeta[i] = xi[i] 
            + theta[sex[i], age[i]] 
            + beta[sex[i], 1] * goods[i] 
            + beta[sex[i], 2] * edu_full[i]
            + beta[sex[i], 3] * diff[i];

  } 

  else {
    zeta[i] = 0;
  }

  }

}

model {

  real phi;

  alpha ~ exponential(1);
  gamma ~ normal(0, 1);

  sigma ~ exponential(0.3);
  xi ~ normal(0, sigma);    //# make into a gamma prior gamma(15*sigma,sigma);

  for (s in 1:2) {
  for (b in 1:3) {
    beta[s, b] ~ normal(0, 1);
  }
  }

  for (s in 1:2) {
    theta_raw[s] ~ normal(0, 1);
  }

  edu_impute ~ normal(0, 1);
  
  eta ~ normal(0, 10);
  kappa ~ beta(12, 2);
  tau ~ exponential(1);
  delta ~ exponential(1);

  for (i in 1:N) {

    if (K[i, 1] != -99) {
    if (diff[i] != -99) {

      phi = 1*(zeta[i] - 0);
      K[i, 1]  ~ ordered_logistic(phi, c);

    }
    }

  for (q in 2:Q) {

    if (K[i, q] != -99) {
    if (diff[i] != -99) {

      phi = alpha[q]*(zeta[i] - gamma[q]);
      K[i, q]  ~ ordered_logistic(phi, c);

  }
  }

  }
  }

}
