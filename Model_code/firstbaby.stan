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
  
  vector cumulative_product(vector X) { // cumulative product function

    return exp(cumulative_sum(log(X)));

  }

}

data {
  
  int N; // sample size of women
  int A; // maximum age of women

  array[N,A] int baby; // probability of AFR
  
}

parameters {

//global intercept
  real alpha;
//Gaussian process of age
  vector [A] mu_raw;
  real <lower = 0, upper = 1> mu_kappa;
  real <lower = 0> mu_tau;
  real <lower = 0> mu_delta;

}


transformed parameters {

//Time-varying Gaussian proceess of age
  vector [A] mu; 

    mu = GP(A, mu_kappa, mu_tau, mu_delta) * mu_raw;

}

model {
    
    alpha ~ normal(0,1);
    
    mu_raw ~ normal(0, 1);
    mu_kappa ~ beta(12,2);
    mu_tau ~ exponential(1);
    mu_delta ~ exponential(1);


  for (n in 1:N) {
  for (a in 1:A) {
    
    if(baby[n,a] != -99){

      baby[n, a] ~ bernoulli_logit( //probability of having your first child
        alpha + // global intercept
        mu[a]); // age
    }
    }
    }

}

// generated quantities {
//   vector[A] p_base; 
//   vector[A] cp_base;
  
//   for(a in 1:A){
//     p_base[a] = inv_logit(
//       alpha +
//       mu[a]
//       );
//   }
  
//   for(a in 1:A){
//     cp_base[a] = cumulative_product(
//       p_base[a]
//       );
//   }
// }