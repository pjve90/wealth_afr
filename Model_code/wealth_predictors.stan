data {

  int N; // sample size of women
  int A; // maximum age of women
  
  matrix[N,A] wealth; // age-specific absolute wealth

  int N_miss_wealth; // number of missing data of absolute wealth

  matrix[N,A] wealth_miss; // indicator matrix for missing values (1 if missing and 0 if not) of absolute wealth

}

parameters {

// global intercept
  real alpha;
//absolute wealth
  vector[A] beta_wealth;
// missing wealth data
  matrix[N,A] wealth_impute;
  real nu;
  real<lower=0> sigma_wealth_miss;
}

transformed parameters {

//Bayesian data imputation

//absolute wealth
  matrix[N,A] wealth_full; //full wealth data (original + imputed)
  
  wealth_full = wealth; // making the merged data as the same as the original data
  
  if(N_miss_wealth > 0){       //telling where is the missing data that needs to be imputed  
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
// absolute wealth
    beta_wealth ~ normal(0,1);
// missing wealth data
    nu ~ normal(0,1);
    sigma_wealth_miss ~ exponential(1);
  
  for (n in 1:N) {
   if(wealth_miss[n,1]==1){
      wealth_impute[n,1] ~ normal(nu,sigma_wealth_miss);  
  }
  }
  
  for (n in 1:N) {
  for (a in 1:A) {
    if(wealth_miss[n,a]==1){
      if(a == 1){
        wealth_impute[n,1] ~ normal(nu,sigma_wealth_miss);  
      } else {
     wealth_impute[n,a] ~ normal(alpha*wealth[n,(a-1)]+(1-alpha)*beta_wealth[a],sigma_wealth_miss);      
      }
    }
  }
  }

}
