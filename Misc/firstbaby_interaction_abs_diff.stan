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
  
  matrix[N,A] wealth; // age-specific absolute wealth

  int N_miss; // number of missing data of absolute wealth

  array[N_miss,2] int wealth_miss; // indicator of position of missing values in the wealth matrix

  array[N,A] int baby; // probability of FR

}

parameters {

// global intercept
  real alpha;
// Gaussian process of age
  vector [A] mu_raw;
  real <lower = 0, upper = 1> mu_kappa;
  real <lower = 0> mu_tau;
  real <lower = 0> mu_gamma;
// wealth
  // absolute wealth
  vector [A] beta_wealth_z;
  real <lower = 0> beta_wealth_sigma;
  // absolute wealth change
  vector [A] gamma_wealth_z; 
  real <lower = 0> gamma_wealth_sigma;
  // interaction
  vector [A] zeta_wealth_z;
  real <lower = 0> zeta_wealth_sigma;
  // missing wealth data
  vector[N_miss] wealth_impute;
  real alpha_miss;
  real beta_miss;
  real<lower=0> sigma_miss;
}

transformed parameters {

//Gaussian Process of age
  vector [A] mu; //vector containing mu
  
    mu = GP(A, mu_kappa, mu_tau, mu_gamma) * mu_raw; // calculating mu from the Gaussian process
    
//Bayesian data imputation
  matrix[N,A] wealth_full; // full wealth data (original + imputed)

  wealth_full = wealth; // making the merged data as the same as the original data

  for(n in 1:N_miss){ // telling where is the missing data that needs to be imputed
        wealth_full[wealth_miss[n,1],wealth_miss[n,2]] = wealth_impute[n];
      }

//short-term wealth variability
  matrix[N,A] wealth_change; //matrix containing wealth change

  for(n in 1:N){
    for(a in 1:2){
      wealth_change[n,a] = 0; //setting zero change at birth and first year, since wealth change is calculated with a 2-years lag
    }
    for(a in 3:A){
      wealth_change[n,a] = abs(wealth[n,a] - wealth[n,a-2]); //calculating the 2-years lagged wealth change
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
    mu_gamma ~ exponential(1);
// wealth
    // absolute wealth
    beta_wealth_z ~ normal(0, 1); 
    beta_wealth_sigma ~ exponential(1);
    // absolute wealth change
    gamma_wealth_z ~ normal(0, 1);
    gamma_wealth_sigma ~ exponential(1);
    //interaction
    zeta_wealth_z ~ normal(0, 1);
    zeta_wealth_sigma ~ exponential(1);
// missing wealth data
    alpha_miss ~ normal(0, 1);
    beta_miss ~ normal(0, 1);
    sigma_miss ~ exponential(1);

//Wealth data imputation
 for(n in 1:N){
     wealth_full[n,1] ~ normal(0, 1); //data imputation at birth
  for(a in 2:A){
     wealth_full[n,a] ~ normal(alpha_miss*wealth_full[n, a-1] + (1-alpha_miss)*(beta_miss), sigma_miss);
  }
 }

//Probability of first birth
  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        (beta_wealth_z[a]*beta_wealth_sigma)*wealth_full[n,a] + // absolute wealth
        (gamma_wealth_z[a]*gamma_wealth_sigma)*wealth_change[n,a] + // absolute wealth change
        (zeta_wealth_z[a]*zeta_wealth_sigma)*(wealth_full[n,a]*wealth_change[n,a]) //interaction
        );
          
    }
    }
    }

}
