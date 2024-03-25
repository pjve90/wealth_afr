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
  
  int wealth[N,A]; // age-specific absolute wealth
  array[N,A] int diffwealth; // age-specific absolute wealth
  
  int baby[N,A]; // probability of FR

}

parameters {

// global intercept
  real alpha;
// Gaussian process of age
  vector [A] mu_raw;
  real <lower = 0, upper = 1> mu_kappa;
  real <lower = 0> mu_tau;
  real <lower = 0> mu_delta;
// Gaussian process of absolute wealth
  vector [wealth[N,A]] beta_raw;
  real <lower = 0, upper = 1> beta_kappa;
  real <lower = 0> beta_tau;
  real <lower = 0> beta_delta;
}


transformed parameters {

//Time-varying Gaussian process of age
  vector [A] mu;
  
    mu = GP(A, mu_kappa, mu_tau, mu_delta) * mu_raw;
  }

//Time-varying Gaussian process of absolute wealth
  vector [wealth[N,A]] beta;
  
    beta_wealth = GP(wealth[N,A], beta_kappa, beta_tau, beta_delta) * beta_raw;
  }


model {

    alpha ~ normal(0,1);
    
    mu_raw ~ normal(0, 1);
    mu_kappa ~ beta(12, 2);
    mu_tau ~ exponential(1);
    mu_delta ~ exponential(1);
    
    beta_raw ~ normal(0, 1);
    beta_kappa ~ beta(12, 2);
    beta_tau ~ exponential(1);
    beta_delta ~ exponential(1);
    
    gamma_wealth ~ normal(0,1); // wealth variability

    
  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        beta_wealth[a]*abswealth[n,a] + // absolute wealth
        gamma_wealth[a]*diffwealth[n,a] // wealth variability
        );
    }
    }
    }

}
