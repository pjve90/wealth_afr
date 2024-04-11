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
  
  array[N,A] int wealth; // age-specific absolute wealth
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
// absolute wealth
  vector [A] beta_wealth;
// missing wealth data
  vector [N] wealth_impute;
  real nu;
  real<lower=0> sigma_wealth_m;
}


transformed parameters {

//Time-varying Gaussian process of age
  vector [A] mu;
  
    mu = GP(A, mu_kappa, mu_tau, mu_delta) * mu_raw;

//identify missing values
    vector[N*A] wealth_merge;
    for (i in 1:N) {
        for(j in 1:A) {
        if (wealth[i,j] ==  -99) {
            wealth_merge[(i-1)*A+j] = wealth_impute[i];
        } else {
            wealth_merge[(i-1)*A+j] = wealth[i,j];
        }}
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
// absolute wealth
    beta_wealth ~ normal(0,1);
// missing wealth data
    wealth_merge ~ normal(nu,sigma_wealth_m);
    nu ~ normal(0,1);
    sigma_wealth_m ~ exponential(1);
    
    
  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        beta_wealth[a]*wealth_merge[(n-1)*a+a] // absolute wealth
        );
          
    }
    }
    }

}
