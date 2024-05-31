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
// wealth
  vector[A] beta_wealth; // current absolute level of wealth
  vector[A] gamma_wealth; // current absolute wealth change
  vector[A] delta_wealth; // 1-year lagged absolute level of wealth
  vector[A] epsilon_wealth; // 1-year lagged absolute wealth change
  vector[A] zeta_wealth; // 2-year lagged absolute level of wealth
  vector[A] eta_wealth; // 2-year lagged absolute wealth change
  vector[A] theta_wealth; // 3-year lagged absolute level of wealth
  vector[A] iota_wealth; // 3-year lagged absolute wealth change
  vector[A] kappa_wealth; // cumulative moving average of wealth
  vector[A] lambda_wealth; // cumulative moving variance of wealth
  
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

//current absolute wealth change
  vector[N*A] diffwealth; //current absolute wealth change
// setting the wealth change as zero at birth
  for (n in 1:N)
  {
    
    diffwealth[(n-1)*A+1] = 0;
    
  }
//calculate the current absolute wealth change
  for (n in 1:N) {
  for (a in 2:A) {
    
      diffwealth[(n-1)*A+a] = og_wealth_full[(n-1)*A+a] - og_wealth_full[(n-1)*A+(a-1)];
      
  }
  }
//standardise the current absolute wealth change
  vector[N*A] std_diffwealth =  (abs(diffwealth) - mean(abs(diffwealth))) / sd(abs(diffwealth)) ;

//1-year lagged absolute wealth
  vector[N*A] lagg1wealth; // 1-year lagged absolute wealth
//setting the lagged absolute wealth at birth as zero
  for (n in 1:N)
  {
    
    lagg1wealth[(n-1)*A+1] = 0;
    
  }
//defining the 1-year lagged absolute wealth
  for (n in 1:N) {
  for (a in 2:A) {
    
      lagg1wealth[(n-1)*A+a] = wealth_full[(n-1)*A+(a-1)];
      
  }
  }

//1-year lagged absolute wealth change
  vector[N*A] lagg1diff; //1-year lagged absolute change
//setting the lagged absolute wealth change at birth and one year old as zero
  for (n in 1:N){
  for (a in 1:2){
  
    lagg1wealth[(n-1)*A+a] = 0;
    
  }
  }
//defining the 1-year lagged absolute wealth change  
  for (n in 1:N){
  for (a in 3:A){
  
    lagg1wealth[(n-1)*A+a] = og_wealth_full[(n-1)*A+a] - og_wealth_full[(n-1)*A+(a-2)];
    
  }
  }
//standadrise the 1-year lagged absolute wealth change
  vector[N*A] std_lagg1diff =  (abs(lagg1diff) - mean(abs(lagg1diff))) / sd(abs(lagg1diff)) ; // absolute standardised wealth
  
//2-year lagged absolute wealth
  vector[N*A] lagg2wealth; // 2-year lagged absolute wealth
//setting the lagged absolute wealth at birth and one year old as zero
  for (n in 1:N) {
  for (a in 1:2) {
    
      lagg2wealth[(n-1)*A+1] = 0;
      
  }
  }
//defining the 2-year lagged absolute wealth
  for (n in 1:N) {
  for (a in 3:A) {
    
      lagg2wealth[(n-1)*A+a] = wealth_full[(n-1)*A+(a-2)];
      
  }
  }

//2-year lagged absolute wealth change
  vector[N*A] lagg2diff; //2-year lagged absolute change
//setting the lagged absolute wealth change at birth and one and two years old as zero
  for (n in 1:N) {
  for (a in 1:3) {
  
    lagg2wealth[(n-1)*A+a] = 0;
    
  }
  }
//defining the 2-year lagged absolute wealth change  
  for (n in 1:N)
  for (a in 4:A){
  {
    
    lagg2wealth[(n-1)*A+a] = og_wealth_full[(n-1)*A+a] - og_wealth_full[(n-1)*A+(a-3)];
    
  }
  }
//standadrise the 2-year lagged absolute wealth change
  vector[N*A] std_lagg2diff =  (abs(lagg2diff) - mean(abs(lagg2diff))) / sd(abs(lagg2diff)) ; // absolute standardised wealth change

//3-year lagged absolute wealth
  vector[N*A] lagg3wealth; // 3-year lagged absolute wealth
//setting the lagged absolute wealth at birth and one and two years old as zero
  for (n in 1:N) {
  for (a in 1:3) {
    
      lagg3wealth[(n-1)*A+1] = 0;
      
  }
  }
//defining the 3-year lagged absolute wealth
  for (n in 1:N) {
  for (a in 4:A) {
    
      lagg3wealth[(n-1)*A+a] = wealth_full[(n-1)*A+(a-3)];
      
  }
  }

//3-year lagged absolute wealth change
  vector[N*A] lagg3diff; //3-year lagged absolute change
//setting the lagged absolute wealth change at birth and one, two, and three years old as zero
  for (n in 1:N) {
  for (a in 1:4) {
  
    lagg3wealth[(n-1)*A+a] = 0;
    
  }
  }
//defining the 3-year lagged absolute wealth change  
  for (n in 1:N)
  for (a in 5:A){
  {
    
    lagg3wealth[(n-1)*A+a] = og_wealth_full[(n-1)*A+a] - og_wealth_full[(n-1)*A+(a-4)];
    
  }
  }
//standadrise the 3-year lagged absolute wealth change
  vector[N*A] std_lagg3diff =  (abs(lagg3diff) - mean(abs(lagg3diff))) / sd(abs(lagg3diff)) ; // absolute standardised wealth change

//Cumulative moving average
  vector[N*A] cmawealth; // vector to store the cumulative moving averages

  for (n in 1:N)
  {
    cmawealth[(n-1)*A+1] = og_wealth_full[(n-1)*A+1]; // adding in the vector the wealth at birth
  }
  
  for (n in 1:N) {
  for (a in 2:A) {
    
    cmawealth[(n-1)*A+a] = (cmawealth[(n-1)*A+(a-1)] * (a-1) + og_wealth_full[(n-1)*A+a]) / a; //calculate the cumulate moving average
          
    }
    }

//standardise the cumulative moving average
   vector[N*A] std_cma=(log(cmawealth) - mean(log(cmawealth)))/sd(log(cmawealth)); // standardised cumulative moving average
    
//Cumulative moving variance
  vector[N*A] cmvwealth; // vector to store the cumulative moving variances
  real sum_squared_diff;      // Sum of squared differences from the mean

  for (n in 1:N)
  {
    cmvwealth[(n-1)*A+1] = 0.0; // adding in the vector the variability at birth (column 1 in matrix)
    sum_squared_diff = 0.0;
  }
  
  for (n in 1:N) {
  for (a in 2:A) {
    
    sum_squared_diff += (og_wealth_full[(n-1)*A+a] - cmawealth[(n-1)*A+a]) * (og_wealth_full[(n-1)*A+a] - cmawealth[(n-1)*A+a]); // sum of squared differences from the mean
    cmvwealth[(n-1)*A+a] = sum_squared_diff / a; // cumulative moving variance

    }
    }
//standardise the cumulative moving variance   
  vector[N*A] std_cmv=(log(cmvwealth) - mean(log(cmvwealth)))/sd(log(cmvwealth)); // standardised cumulative moving variance 

}

model {
// global intercept
    alpha ~ normal(0,1);
// Gaussian process of age    
    mu_raw ~ normal(0, 1);
    mu_kappa ~ beta(12, 2);
    mu_tau ~ exponential(1);
    mu_delta ~ exponential(1);
// wealth
    beta_wealth ~ normal(0,1); // current absolute level of wealth
    gamma_wealth ~ normal(0,1); // current absolute wealth change
    delta_wealth ~ normal(0,1); // 1-year lagged absolute level of wealth
    epsilon_wealth ~ normal(0,1); // 1-year lagged absolute wealth change
    zeta_wealth ~ normal(0,1); // 2-year lagged absolue level of wealth
    eta_wealth ~ normal(0,1); // 2-year lagged absolute wealth change
    theta_wealth ~ normal(0,1); // 3-year lagged absolute level of wealth
    iota_wealth ~ normal(0,1); // 3-year lagged absolute wealth change
    kappa_wealth ~ normal(0,1); // cumulative moving average of wealth
    lambda_wealth ~ normal(0,1); // cumulative moving variance of wealth
// missing wealth data
    wealth_impute ~ normal(nu,sigma_wealth_miss);
    nu ~ normal(0,1);
    sigma_wealth_miss ~ exponential(1);

  for (n in 1:N) {
  for (a in 5:A) { // modelling from age 4 years old onwards, due to the lagged effects
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        mu[a] + // age
        beta_wealth[a]*wealth_full[(n-1)*A+a] + // current absolute level of wealth
        gamma_wealth[a]*std_diffwealth[(n-1)*A+a] + // current absolue wealth change
        delta_wealth[a]*lagg1wealth[(n-1)*A+a] + // 1-year lagged absolute level of wealth
        epsilon_wealth[a]*std_lagg1diff[(n-1)*A+a] + // 1-year lagged absolute wealth change
        zeta_wealth[a]*lagg2wealth[(n-1)*A+a] + // 2-year lagged absolute level of wealth
        eta_wealth[a]*std_lagg2diff[(n-1)*A+a] + // 2-year lagged absolute wealth change
        theta_wealth[a]*lagg3wealth[(n-1)*A+a] + // 3-year lagged absolute level of wealth
        iota_wealth[a]*std_lagg3diff[(n-1)*A+a] + // 3-year lagged absolute wealth change
        kappa_wealth[a]*std_cma[(n-1)*A+a] + // cumulative moving average of wealth
        lambda_wealth[a]*std_cmv[(n-1)*A+a] // cumulative moving variance of wealth
        );
    }
    }
    }

}
