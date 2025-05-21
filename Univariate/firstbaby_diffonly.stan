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
  
  matrix[N,A] wealth; // age-specific absolute wealth [raw data with missing values coded -99]
  
  vector[N] median_wealth; // individual median wealth for data imputation at birth

  array[N,A] int baby; // first birth (0=no,1=yes,-99=censored)

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
  // wealth change
  vector [A] gamma_wealth_z; 
  real <lower = 0> gamma_wealth_sigma;

  // missing wealth data
  real <lower = 0, upper = 1> alpha_miss;
  real beta_miss;
  real <lower=0> sigma_miss;
}

transformed parameters {

//Gaussian Process of age
  vector [A] mu; //vector containing mu
  
    mu = GP(A, mu_kappa, mu_tau, mu_delta) * mu_raw; // calculating mu from the Gaussian process
    
//Bayesian data imputation
  matrix[N,A] wealth_full; // full wealth data (raw with missing + imputed)

  wealth_full = wealth; // initialize wealth full with wealth (raw)

//short-term wealth variability
  matrix[N,A] wealth_change; //matrix containing wealth change

  for(n in 1:N){
    for(a in 1:2){
      wealth_change[n,a] = 0; //setting zero change at birth and first year, since wealth change is calculated with a 2-years lag
    }
    for(a in 3:A){
      wealth_change[n,a] = abs(wealth_full[n,a] - wealth_full[n,a-2]); //calculating the 2-years lagged wealth change
    }
  }

}

model {

// global intercept
    alpha ~ normal(0, 1);

// Gaussian process of age    
    mu_raw ~ normal(0, 1);
    mu_kappa ~ beta(12, 2);
    mu_tau ~ exponential(1);
    mu_delta ~ exponential(1);

// wealth
    // wealth change
    gamma_wealth_z ~ normal(0, 1);
    gamma_wealth_sigma ~ exponential(1);

// missing wealth parameters
    alpha_miss ~ uniform(0.5, 1);
    beta_miss ~ normal(0, 1);
    sigma_miss ~ exponential(3);

//Wealth data imputation
//Data imputation at birth
for (n in 1:N){
  if(wealth[n,1] == -99){
    wealth_full[n,1] ~ normal(median_wealth[n], 1); //data imputation at birth
  }

//Data imputation in later ages
  for(a in 2:A){
    if(wealth[n,a] == -99){
    wealth_full[n,a] ~ normal( (alpha_miss*wealth_full[n, a-1] + (1-alpha_miss)*(beta_miss) ), sigma_miss); //autoregressive data imputation
  }
}
}

//Probability of first birth
  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        (gamma_wealth_z[a]*gamma_wealth_sigma)*wealth_change[n,a] // 2-years lagged wealth change
        );
          
    }
    }
    }

}

generated quantities {
  matrix[N, A] imputed_wealth;

  for (n in 1:N) {
    for (a in 1:A) {
      if (wealth[n, a] == -99) {
        imputed_wealth[n, a] = wealth_full[n, a];
      } else {
        imputed_wealth[n, a] = -999; // signal that the value was not imputed
      }
    }
  }
}
