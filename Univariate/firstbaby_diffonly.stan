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

  int N_miss; // number of missing data points for wealth
  array[N_miss, 2] int wealth_miss; // positions of missing wealth
  
  real wc_mean_obs; //mean of observed short-term wealth variability
  real wc_sd_obs; //sd of observed short-term wealth variability
  
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
  vector[N_miss] wealth_impute_z; 
  real <lower = 0, upper = 1> alpha_miss;
//  real beta_miss;
  real <lower=0> sigma_miss;
}

transformed parameters {

//Gaussian Process of age
  vector [A] mu; //vector containing mu
  
    mu = GP(A, mu_kappa, mu_tau, mu_delta) * mu_raw; // calculating mu from the Gaussian process
    
//Bayesian data imputation
  matrix[N,A] wealth_full; // full wealth data (raw with missing + imputed)

  wealth_full = wealth; // initialize wealth full with wealth (raw)

//Data imputation
  vector[N_miss] wealth_impute; // initialize vector with imputed data

for (n in 1:N_miss) {
    int i = wealth_miss[n, 1]; // individual
    int a = wealth_miss[n, 2]; // age

    if (a == 1) {
      // At birth: centered on median_wealth
      wealth_impute[n] = median_wealth[i] + sigma_miss * wealth_impute_z[n];
    } else {
      // After birth: AR(1)-like imputation
      real mu_miss = alpha_miss * wealth_full[i, a - 1] +
                     (1 - alpha_miss) * median_wealth[i];
      wealth_impute[n] = mu_miss + sigma_miss * wealth_impute_z[n];
    }
    // Fill missing spots with imputed values
    wealth_full[i, a] = wealth_impute[n];
}

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

  // Standardize wealth change
  matrix[N, A] wealth_change_std;

  for (n in 1:N) {
    for (a in 1:2) {
      wealth_change_std[n, a] = 0;
    }
    for (a in 3:A) {
      wealth_change_std[n, a] = (wealth_change[n, a] - wc_mean_obs) / wc_sd_obs;
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
    gamma_wealth_sigma ~ normal(0,1);

// missing wealth parameters
alpha_miss ~ beta(1, 1);           
sigma_miss ~ normal(0, 0.1); 
wealth_impute_z ~ normal(0, 1);

//Probability of first birth
  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        (gamma_wealth_z[a]*gamma_wealth_sigma)*wealth_change_std[n,a] // 2-years lagged wealth change
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
