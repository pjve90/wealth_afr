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
  
  vector merge_missing( array[] int miss_indexes , vector x_obs , vector x_miss ) { // imputation function
    int N = dims(x_obs)[1];
    int N_miss = dims(x_miss)[1];
    vector[N] merged;
    merged = x_obs;
    for ( i in 1:N_miss )
    	merged[ miss_indexes[i] ] = x_miss[i];
    return merged;
}
}

data {

  int N; // sample size of women
  int A; // maximum age of women
  int miss; //number of missing values
  
  vector[N*A] wealth; // age-specific absolute wealth
  
  array[N,A] int baby; // probability of FR

  array[miss] int wealth_m; // missing wealth data

}

parameters {

// global intercept
  real alpha;
// Gaussian process of age
  vector [A] mu_raw;
  real <lower = 0, upper = 1> mu_kappa;
  real <lower = 0> mu_tau;
  real <lower = 0> mu_delta;
// absolute wealth
  vector [A] beta_wealth;
// missing wealth data
  vector [miss] wealth_impute;
  real nu;
  real<lower=0> sigma_wealth_m;
}


transformed parameters {

//Time-varying Gaussian process of age
  vector [A] mu;
  
    mu = GP(A, mu_kappa, mu_tau, mu_delta) * mu_raw;
  }

model {
// global intercept
    alpha ~ normal(0,1);
// Gaussian process of age    
    mu_raw ~ normal(0, 1);
    mu_kappa ~ beta(12, 2);
    mu_tau ~ exponential(1);
    mu_delta ~ exponential(1);
// absolute wealth
    beta_wealth ~ normal(0,1);
// missing wealth data
    vector[N*A] beta_merge;
    beta_merge = merge_missing(wealth_m,to_vector(wealth),wealth_impute);
    beta_merge ~ normal(nu,sigma_wealth_m);
    nu ~ normal(0,1);
    sigma_wealth_m ~ exponential(1);
    
    
  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        beta_wealth[a]*beta_merge[(n-1)*a+a] // absolute wealth
        );
          
    }
    }
    }

}
