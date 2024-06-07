functions {

  matrix GP(int K, real C, real D, real S) { // Gaussian process function
    matrix[K, K] Rho;
    real KR;
    KR = K;

    for (i in 1:(K-1)) {
    for (j in (i+1):K) {
    Rho[i, j] = C * exp(-D * ((j-i)^2 / KR^2));
    Rho[j, i] = Rho[i, j];
    }
    }

    for (i in 1:K) {
    Rho[i, i] = 1;
    }

    return S * cholesky_decompose(Rho);
  }
}

data {

  int N; // sample size of women
  int A; // maximum age of women
  
  matrix[N,A] wealth; // age-specific current wealth change
  
  int N_miss; // number of missing data
  array[N,A] int wealth_miss; // indicator matrix for missing values (1 if missing and 0 if not)
  
  array[N,A] int baby; // probability of FR

}

parameters {

// global intercept
  real alpha;
// Gaussian process of age
  vector [A] mu_raw;
  real <lower = 0, upper = 1> mu_kappa;
  real <lower = 0> mu_tau;
  real <lower = 0> mu_delta;
// wealth
  vector [A] delta_wealth; // cumulative moving variance
// missing wealth data
  matrix[N,A] wealth_impute;
  real nu;
  real<lower=0> sigma_wealth_miss;
}

transformed parameters {

//Time-varying Gaussian process of age
  vector [A] mu;
  
    mu = GP(A, mu_kappa, mu_tau, mu_delta) * mu_raw;
    
//Bayesian data imputation
  matrix[N,A] wealth_full; //full wealth data (original + imputed)
  
  wealth_full = wealth; // making the merged data as the same as the original data
  
  if(N_miss > 0){       //telling where is the missing data that needs to be imputed  
    for(n in 1:N){
      for(a in 1:A){
        if(wealth_miss[n,a] == 1){
          wealth_full[n,a] = wealth_impute[n,a];  
        }else{
          wealth_full[n,a] = wealth[n,a];
        }
      }
      }
    }
}

model {
// global intercept
    alpha ~ normal(0,1);
// Gaussian process of age    
    mu_raw ~ normal(0, 1);
    mu_kappa ~ beta(12, 2);
    mu_tau ~ exponential(1);
    mu_delta ~ exponential(1);
// cumulative moving variance
    delta_wealth ~ normal(0,1);
// missing wealth data
  for (n in 1:N) {
  for (a in 1:A) {
    if(wealth_miss[n,a]==1){
      wealth_impute[n,a] ~ normal(nu,sigma_wealth_miss);  
    }
  }
  }
    nu ~ normal(0,1);
    sigma_wealth_miss ~ exponential(1);
    
    
  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        delta_wealth[a]*wealth_full[n,a] // cumulative moving variance
        );
          
    }
    }
    }

}
