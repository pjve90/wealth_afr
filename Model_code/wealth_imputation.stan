data {

  int N; // sample size of women
  int A; // maximum age of women
  
  matrix[N,A] wealth; // age-specific absolute wealth

  int N_miss; // number of missing data of absolute wealth

  array[N_miss,2] int wealth_miss; // indicator of position of missing values in the wealth matrix

}

parameters {

  vector[N_miss] wealth_impute; //vector for the imputed values
  real alpha_miss; //intercept
  real beta_miss; //beta parameter
  real<lower=0> sigma_miss; //sigma parameter
  
}

transformed parameters{
  
  //Bayesian data imputation
  matrix[N,A] wealth_full; // full wealth data (original + imputed)
  
  wealth_full = wealth; // making the merged data as the same as the original data
  
  for(n in 1:N_miss){ // telling where is the missing data that needs to be imputed
        wealth_full[wealth_miss[n,1],wealth_miss[n,2]] = wealth_impute[n];  
      }

}

model {
    alpha_miss ~ normal(0, 1);
    beta_miss ~ normal(0, 1);
    sigma_miss ~ exponential(1);

 for(n in 1:N){
     wealth_full[n,1] ~ normal(0, 1); //data imputation at birth
  for(a in 2:A){
     wealth_full[n,a] ~ normal(alpha_miss*wealth_full[n, a-1] + (1-alpha_miss)*(beta_miss), sigma_miss);
  }
 }

}
 
 generated quantities {
   
  matrix[N,A] wealth_complete;
  
  wealth_complete = wealth_full;
}
