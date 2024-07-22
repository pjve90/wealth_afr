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
  
  vector[N*A] wealth; // age-specific absolute wealth
  
  int N_miss; // number of missing data
  array[N_miss] int id_wealth_miss; // indexes of missing wealth data
  
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
// 1-year lagged absolute wealth change
  vector [A] epsilon_wealth;
// missing wealth data
  vector[N_miss] wealth_impute;
  real nu;
  real<lower=0> sigma_wealth_miss;
}

transformed parameters {

//Time-varying Gaussian process of age
  vector [A] mu;
  
    mu = GP(A, mu_kappa, mu_tau, mu_delta) * mu_raw;
    
//Bayesian data imputation
  vector[N*A] wealth_full; //full wealth data (original + imputed)
  
  wealth_full = wealth; // making the merged data as the same as the original data
  
  if(N_miss > 0){       //telling where is the missing data that needs to be imputed  
    for(i in 1:N_miss){
        wealth_full[id_wealth_miss[i]] = wealth_impute[i];
      }
    }

//reverse standardisation
  vector[N*A] rev_wealth_full; // vector containig the reversed standardised wealth data 
  
  for (i in 1:(N*A)){
    rev_wealth_full[i] = wealth_full[i]*sd(wealth)+mean(wealth); 
  }
  
//reverse log transformation
  vector[N*A] og_wealth_full; //vector containing the reversed log transformed wealth data
  
  for (i in 1:(N*A)){
    og_wealth_full[i] = exp(rev_wealth_full[i]); 
  }

//1-year lagged absolute wealth change
  vector[N*A] lagg1diff; //1-year lagged absolute change
//setting the lagged absolute wealth change at birth and one year old as zero
  for (n in 1:N){
  for (a in 1:2){
  
    lagg1diff[(n-1)*A+a] = 0;
    
  }
  }
//defining the 1-year lagged absolute wealth change  
  for (n in 1:N){
  for (a in 3:A){
  
    lagg1diff[(n-1)*A+a] = og_wealth_full[(n-1)*A+a] - og_wealth_full[(n-1)*A+(a-2)];
    
  }
  }
//standadrise the 1-year lagged absolute wealth change
  vector[N*A] std_lagg1diff =  (abs(lagg1diff) - mean(abs(lagg1diff))) / sd(abs(lagg1diff)) ; // absolute standardised wealth
  
}

model {
// global intercept
    alpha ~ normal(0,1);
// Gaussian process of age    
    mu_raw ~ normal(0, 1);
    mu_kappa ~ beta(12, 2);
    mu_tau ~ exponential(1);
    mu_delta ~ exponential(1);
// lagged absolute wealth change
    epsilon_wealth ~ normal(0,1);
// missing wealth data
    wealth_impute ~ normal(nu,sigma_wealth_miss);
    nu ~ normal(0,1);
    sigma_wealth_miss ~ exponential(1);
    
    
  for (n in 1:N) {
  for (a in 3:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        epsilon_wealth[a]*std_lagg1diff[(n-1)*A+a] // lagged absolute wealth
        );
          
    }
    }
    }

}