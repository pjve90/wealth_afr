
data {

  int N; // sample size of women
  int A; // maximum age of women
  
  matrix[N,A] wealth; // age-specific absolute wealth [raw data with missing values coded -99]
  vector[N] median_wealth; // individual median wealth for data imputation at birth

  int N_miss; // number of missing data points for wealth
  array[N_miss, 2] int wealth_miss; // positions of missing wealth
  
  array[N,A] int baby; // first birth (0=no,1=yes,-99=censored)

}

parameters {

// global intercept
  real alpha;

// wealth
  // absolute wealth
  real beta_wealth_z;
  real <lower = 0> beta_wealth_sigma;
  // wealth change
  real gamma_wealth_z; 
  real <lower = 0> gamma_wealth_sigma;
  // moving standard deviation
  real delta_wealth_z; 
  real <lower = 0> delta_wealth_sigma;

  // missing wealth data
  vector[N_miss] wealth_impute_z; 
  real <lower = 0, upper = 1> alpha_miss;
  real <lower=0> sigma_miss;
}

transformed parameters {

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
matrix[N,A] wealth_change;
matrix[N,A-2] non_zero_change;  // Correct dimension: A-2 columns for ages 3 to A

for(n in 1:N){
  for(a in 1:2){
    wealth_change[n,a] = 0;
  }
  for(a in 3:A){
    wealth_change[n,a] = abs(wealth_full[n,a] - wealth_full[n,a-2]);
    non_zero_change[n,a-2] = wealth_change[n,a];  // Note: a-2 to index correctly
  }
}

// Calculate mean and standard deviation
real wc_mean_model = mean(to_vector(non_zero_change));  // Need to_vector() for matrix
real wc_sd_model = sd(to_vector(non_zero_change));

// Standardize wealth_change
matrix[N, A] wealth_change_std;
for (n in 1:N) {
  for (a in 1:2) {  // Should be 1:2, not 1:10
    wealth_change_std[n, a] = 0;
  }
  for (a in 3:A) {  // Should be 3:A, not 11:A
    wealth_change_std[n, a] = (wealth_change[n, a] - wc_mean_model) / wc_sd_model;
  }
}

//long-term variability
matrix[N,A] wealth_msd;
matrix[N,A-10] non_zero_msd;  // Correct dimension: A-10 columns for ages 11 to A

for(n in 1:N){
  for(a in 1:10){
    wealth_msd[n,a] = 0;
  }
  for(a in 11:A){
    wealth_msd[n,a] = sd(segment(wealth_full[n],a-9,10));
    non_zero_msd[n,a-10] = wealth_msd[n,a];  // Note: a-10 to index correctly
  }
}

// Calculate mean and standard deviation
real lv_mean_model = mean(to_vector(non_zero_msd));
real lv_sd_model = sd(to_vector(non_zero_msd));

// Standardize wealth_msd
matrix[N, A] wealth_msd_std;
for (n in 1:N) {
  for (a in 1:10) {
    wealth_msd_std[n, a] = 0;
  }
  for (a in 11:A) {
    wealth_msd_std[n, a] = (wealth_msd[n, a] - lv_mean_model) / lv_sd_model;
  }
}
}

model {

// global intercept
    alpha ~ normal(0, 1);


// wealth
    // absolute wealth
    beta_wealth_z ~ normal(0, 1); 
    beta_wealth_sigma ~ normal(0,0.25);
    // wealth change
    gamma_wealth_z ~ normal(0, 1);
    gamma_wealth_sigma ~ normal(0,0.25);
    // moving standard deviation
    delta_wealth_z ~ normal(0, 1);
    delta_wealth_sigma ~ normal(0,0.25);
    
// missing wealth parameters
    alpha_miss ~ beta(2, 2);           
    sigma_miss ~ normal(0, 0.5); 
    wealth_impute_z ~ normal(0, 1);

//Probability of first birth
  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        (beta_wealth_z*beta_wealth_sigma)*wealth_full[n,a] + // absolute wealth
        (gamma_wealth_z*gamma_wealth_sigma)*wealth_change_std[n,a] + // 2-years lagged wealth change
        (delta_wealth_z*delta_wealth_sigma)*wealth_msd_std[n,a]  // moving standard deviation
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
