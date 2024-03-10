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
  
  int abswealth[N,A]; // age-specific absolute wealth
  int diffwealth[N,A]; // age-specific absolute wealth
  
  int baby[N, A]; // probability of FR

}

parameters {

// global intercept
  real alpha;
// Gaussian process of age
  vector [A] age_raw;
  real <lower = 0, upper = 1> age_kappa;
  real <lower = 0> age_tau;
  real <lower = 0> age_delta;
// wealth
  vector[A] wealth_beta;
  vector[A] wealth_gamma;

}


transformed parameters {

//Time-varying Gaussian process of age
  vector [A] age;
  
    age = GP(A, age_kappa, age_tau, age_delta) * age_raw;
  }

model {

    alpha ~ normal(0,1);
    
    age_raw ~ normal(0, 1);
    age_kappa ~ beta(12, 2);
    age_tau ~ exponential(1);
    age_delta ~ exponential(1);
    
    wealth_beta ~ normal(0,1);
    wealth_gamma ~ normal(0,1);

    
  for (n in 1:N) {
  for (a in 1:A) {

      baby[n, a] ~ bernoulli_logit( // Prob of having your first child
        alpha + // global intercept
        age[a] + // age
        wealth_beta[a]*abswealth[n,a] + // absolute wealth
        wealth_gamma[a]*diffwealth[n,a] // wealth variability
        );
    }
    }

}
