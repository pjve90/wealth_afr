# PhD project on wealth and age at first reproduction ----

# This script is meant to collect all the necessary code to build up the models and explore the data necessary to understand the relationship between wealth and age at first reproduction.

#install package for bayesian analysis
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#remotes::install_github("stan-dev/cmdstanr")
library(cmdstanr)

## Data exploration ----

sample_2 <- read.csv(".~/sample2.csv",header=TRUE)

## Building the model ----

### Listing all the variables ----

#Outcome: 
## Age-specific probability of having your first child
#Predictor(s):
## Age
## Residence
## Absolute values of wealth at time t
## Change of wealth from time t-1 to time t

## Building model with gaussian process for age ----

#model description in mathematical form
## Prob(AFR_i) ~ Binomial(1,p_i)
## logit(p_i) = alpha_age[i]
## alpha_age ~ Normal(0,w)

#create synthetic data
data <- list(N = 1e3, 
             A = 90,
             baby = 1)






## Multiple linear regression ----

### Model with only alpha ----

#afr_i ~ normal(mu,sigma)
#mu ~ normal(?,?) 
#sigma ~ uniform(?,?)

#prior predictive simulation
set.seed(1990)
#values for mu
hist(rnorm(1e4,20,5))
sample_mu <- rnorm(1e4,20,5)
curve(dnorm(x,20,5),from=0,to=45)
#values for sigma
hist(runif(1e4,0,5))
sample_sigma <- runif(1e4,0,5)
curve(dunif(x,0,5),from=-1,to=6)
#prior for afr
prior_afr <- rnorm(1e4,sample_mu,sample_sigma)
hist(prior_afr)
plot(density(prior_afr))

#afr_i ~ normal(mu,sigma)
#mu ~ normal(20,5) 
#sigma ~ uniform(0,5)

#create synthetic data
synth_data <- data.frame(AFR=rnorm(1e4,sample_mu,sample_sigma))
hist(synth_data$AFR)
#prepare data for model
synth_data_list <- list(
  AFR=standardize(synth_data$AFR)
)

#define model
#mu~normal(20,5) and sigma~uniform(0,5)
mprior1 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu ~ dnorm(20,5),
    sigma ~ dunif(0,5)
  ),data=synth_data_list,chains = 4
)
#mu~normal(20,5) and sigma~uniform(0,10)
mprior2 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu ~ dnorm(20,5),
    sigma ~ dunif(0,10)
  ),data=synth_data_list,chains = 4
)
#mu~normal(20,5) and sigma~uniform(0,1)
mprior3 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu ~ dnorm(20,5),
    sigma ~ dunif(0,1)
  ),data=synth_data_list,chains = 4
)
#mu~normal(18,8) and sigma~uniform(0,15)
mprior4 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu ~ dnorm(18,8),
    sigma ~ dunif(0,15)
  ),data=synth_data_list,chains = 4
)

#check priors
set.seed(1960)
#mu~normal(20,5) and sigma~uniform(0,5)
prior1 <- extract.prior(mprior1,n=350)
mu1 <- prior1$mu
sigma1 <- prior1$sigma
#check model
precis(mprior1)
#plot prior for mu
plot(density(mu1))
#plot prior for sigma
plot(density(sigma1))
#plot prior for afr
plot(density(rnorm(1e4,rnorm(1e4,20,5),runif(1e4,0,5))))

#mu~normal(20,5) and sigma~uniform(0,10)
prior2 <- extract.prior(mprior2,n=350)
mu2 <- prior2$mu
sigma2 <- prior2$sigma
#check model
precis(mprior2)
#plot prior for mu
plot(density(mu2))
#plot prior for sigma
plot(density(sigma2))
#plot prior for afr
plot(density(rnorm(1e4,rnorm(1e4,20,5),runif(1e4,0,10))))

#mu~normal(20,5) and sigma~uniform(0,1)
prior3 <- extract.prior(mprior3,n=350)
mu3 <- prior3$mu
sigma3 <- prior3$sigma
#check model
precis(mprior3)
#plot prior for mu
plot(density(mu3))
#plot prior for sigma
plot(density(sigma3))
#plot prior for afr
plot(density(rnorm(1e4,rnorm(1e4,20,5),runif(1e4,0,1))))

#mu~normal(18,8) and sigma~uniform(0,15)
prior4 <- extract.prior(mprior4,n=350)
mu4 <- prior4$mu
sigma4 <- prior4$sigma
#check model
precis(mprior4)
#plot prior for mu
plot(density(mu4))
#plot prior for sigma
plot(density(sigma4))
#plot prior for afr
plot(density(rnorm(1e4,rnorm(1e4,20,5),runif(1e4,0,15))))

#priors
#AFR~dnorm(mu,sigma)
#mu~dnorm(20,5) -> alpha
#sigma~dunif(0,5)

### Model with one predictor  ----

#afr_i ~ normal(mu,sigma)
#mu = alpha + beta*x_i 
#alpha ~ normal(20,5)
#beta ~ normal(0,1)
#sigma ~ uniform(0,5)

#prior predictive simulation
set.seed(1990)
#values for alpha
hist(rnorm(1e4,20,5))
sample_alpha <- rnorm(1e4,20,5)
curve(dnorm(x,20,5),from=0,to=45)
#values for beta
hist(rnorm(1e4,0,1))
sample_beta <- rnorm(1e4,0,1)
curve(dnorm(x,0,1),from=-5,to=5)
#values for sigma
hist(runif(1e4,0,5))
sample_sigma <- runif(1e4,0,5)
curve(dunif(x,0,5),from=-1,to=6)

#prior for afr
set.seed(2791)
N <- 100
a <- rnorm(N,20,8)
b <- rnorm(N,0,0.03)

plot( NULL , xlim=c(0,200) , ylim=c(-10,60) ,
      xlab="absolute wealth" , ylab="AFR" )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:N ) curve( a[i] + b[i]*x ,
                        from=0 , to=200 , add=TRUE ,
                        col=col.alpha("black",0.2) )

#afr_i ~ normal(mu,sigma)
#mu = alpha + beta*x_i 
#alpha ~ normal(20,8)
#beta ~ normal(0,0.03)
#sigma ~ uniform(0,5)

#create synthetic data
sample_alpha <- rnorm(1e4,20,8)
sample_beta <- rnorm(1e4,0,0.03)
sample_mu <- sample_alpha+sample_beta
sample_sigma <- runif(1e4,0,5)
sample_AFR <- rnorm(1e4,sample_mu,sample_sigma)
for(i in 1:length(sample_AFR)){
  if(sample_AFR[i] < 0){
    sample_AFR[i] <- 0
  }
}
sample_cumabs <- rnorm(1e4,50,30)
for(i in 1:length(sample_cumabs)){
  if(sample_cumabs[i] < 0){
    sample_cumabs[i] <- 0
  }
}

synth_data <- data.frame(AFR=sample_AFR,wabs=sample_cumabs)
hist(synth_data$AFR)
hist(synth_data$wabs)
#prepare data for model
synth_data_list <- list(
  AFR=standardize(synth_data$AFR),
  wabs=standardize(synth_data$wabs)
)

#define model
#alpha~normal(20,8), beta~normal(0,0.03) and sigma~uniform(0,5)
mprior1 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b*wabs,
    a ~ dnorm(20,8),
    b ~ dnorm(0,0.03),
    sigma ~ dunif(0,5)
  ),data=synth_data_list,chains = 4
)

#check priors
set.seed(2986)
#alpha~normal(20,8), beta~normal(0,0.03) and sigma~uniform(0,5)
prior1 <- extract.prior(mprior1,n=350)
a1 <- prior1$a
b1 <- prior1$b
sigma1 <- prior1$sigma
#check model
precis(mprior1)
#plot prior for alpha
plot(density(a1))
#plot prior for beta
plot(density(b1))
#plot prior for sigma
plot(density(sigma1))
#plot prior for afr
plot(density(rnorm(1e4,rnorm(1e4,20,5),runif(1e4,0,5))))

#prior for afr
set.seed(4450)
N <- 100
a <- sample(a1,N)
b <- sample(b1,N)

plot( NULL , xlim=c(0,200) , ylim=c(-10,60) ,
      xlab="absolute wealth" , ylab="AFR" )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:N ) curve( a[i] + b[i]*x ,
                        from=0 , to=200 , add=TRUE ,
                        col=col.alpha("black",0.2) )

#check priors
prior <- extract.prior( mprior1 )
mu <- link( mprior1 , post=prior , data=list( wabs=c(0,200) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , mu[i,] , col=col.alpha("black",0.4) )

### Model with two predictors ----

#afr_i ~ normal(mu,sigma)
#mu = alpha + beta_abs*wabs_i + beta_mean*wmean 
#alpha ~ normal(20,8)
#beta_abs ~ normal(0,1)
#beta_mean ~ normal(0,1)
#sigma ~ uniform(0,5)

#prior predictive simulation
set.seed(9863)
#values for alpha
hist(rnorm(1e4,20,8))
sample_alpha <- rnorm(1e4,20,8)
curve(dnorm(x,20,8),from=0,to=45)
#values for beta_abs
hist(rnorm(1e4,0,1))
sample_beta_abs <- rnorm(1e4,0,1)
curve(dnorm(x,0,1),from=-5,to=5)
#values for beta_abs
hist(rnorm(1e4,0,1))
sample_beta_mean <- rnorm(1e4,0,1)
curve(dnorm(x,0,1),from=-5,to=5)
#values for sigma
hist(runif(1e4,0,5))
sample_sigma <- runif(1e4,0,5)
curve(dunif(x,0,5),from=-1,to=6)

#prior for afr
set.seed(1111)
N <- 100
a <- rnorm(N,20,8)
b_abs <- rnorm(N,0,0.02)
b_mean <- rnorm(N,0,0.02)

plot( NULL , xlim=c(0,200) , ylim=c(-10,60) ,
      xlab="absolute wealth" , ylab="AFR" )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:N ) curve( a[i] + b_abs[i]*x + b_mean[i]*x ,
                        from=0 , to=200 , add=TRUE ,
                        col=col.alpha("black",0.2) )

#afr_i ~ normal(mu,sigma)
#mu = alpha + beta*x_i 
#alpha ~ normal(20,8)
#beta_abs ~ normal(0,0.02)
#beta_mean
#sigma ~ uniform(0,5)


#create synthetic data
sample_alpha <- rnorm(1e4,20,8)
sample_beta_abs <- rnorm(1e4,0,0.02)
sample_beta_mean <- rnorm(1e4,0,0.02)
sample_mu <- sample_alpha+sample_beta_abs+sample_beta_mean
sample_sigma <- runif(1e4,0,5)
sample_AFR <- rnorm(1e4,sample_mu,sample_sigma)
for(i in 1:length(sample_AFR)){
  if(sample_AFR[i] < 0){
    sample_AFR[i] <- 0
  }
}
sample_cumabs <- rnorm(1e4,50,30)
for(i in 1:length(sample_cumabs)){
  if(sample_cumabs[i] < 0){
    sample_cumabs[i] <- 0
  }
}
sample_cummean <- rnorm(1e4,20,15)
for(i in 1:length(sample_cummean)){
  if(sample_cummean[i] < 0){
    sample_cummean[i] <- 0
  }
}

synth_data <- data.frame(AFR=sample_AFR,wabs=sample_cumabs,wmean=sample_cummean)
hist(synth_data$AFR)
hist(synth_data$wabs)
hist(synth_data$wmean)
#prepare data for model
synth_data_list <- list(
  AFR=standardize(synth_data$AFR),
  wabs=standardize(synth_data$wabs),
  wmean=standardize(synth_data$wmean)
)

#define model
#alpha~normal(20,8), beta_abs~normal(0,0.02), beta_mean~normal(0,0.02), and sigma~uniform(0,5)
mprior1 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean,
    a ~ dnorm(20,8),
    b_abs ~ dnorm(0,0.02),
    b_mean ~ dnorm(0,0.02),
    sigma ~ dunif(0,5)
  ),data=synth_data_list,chains = 4
)

#check priors
prior <- extract.prior( mprior1 )
mu <- link( mprior1 , post=prior , data=list( wabs=c(0,200),wmean=c(0,60) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , mu[i,] , col=col.alpha("black",0.4) )

### Model with three predictors ----

#afr_i ~ normal(mu,sigma)
#mu = alpha + beta_abs*wabs_i + beta_mean*wmean + beta_sd*wsd
#alpha ~ normal(20,8)
#beta_abs ~ normal(0,1)
#beta_mean ~ normal(0,1)
#beta_sd ~ normal(0,1)
#sigma ~ uniform(0,5)

#prior predictive simulation
set.seed(3435)
#values for alpha
hist(rnorm(1e4,20,8))
sample_alpha <- rnorm(1e4,20,8)
curve(dnorm(x,20,8),from=0,to=45)
#values for beta_abs
hist(rnorm(1e4,0,1))
sample_beta_abs <- rnorm(1e4,0,1)
curve(dnorm(x,0,1),from=-5,to=5)
#values for beta_mean
hist(rnorm(1e4,0,1))
sample_beta_mean <- rnorm(1e4,0,1)
curve(dnorm(x,0,1),from=-5,to=5)
#values for beta_sd
hist(rnorm(1e4,0,1))
sample_beta_sd <- rnorm(1e4,0,1)
curve(dnorm(x,0,1),from=-5,to=5)
#values for sigma
hist(runif(1e4,0,5))
sample_sigma <- runif(1e4,0,5)
curve(dunif(x,0,5),from=-1,to=6)

#prior for afr
set.seed(8520)
N <- 100
a <- rnorm(N,20,8)
b_abs <- rnorm(N,0,0.02)
b_mean <- rnorm(N,0,0.02)
b_sd <- rnorm(N,0,0.02)

plot( NULL , xlim=c(0,200) , ylim=c(-10,60) ,
      xlab="absolute wealth" , ylab="AFR" )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:N ) curve( a[i] + b_abs[i]*x + b_mean[i]*x + b_sd[i]*x ,
                        from=0 , to=200 , add=TRUE ,
                        col=col.alpha("black",0.2) )

#afr_i ~ normal(mu,sigma)
#mu = alpha + beta*x_i 
#alpha ~ normal(20,8)
#beta_abs ~ normal(0,0.02)
#beta_mean ~ normal(0,0.02)
#beta_sd ~ normal(0,0.02)
#sigma ~ uniform(0,5)

#create synthetic data
sample_alpha <- rnorm(1e4,20,8)
sample_beta_abs <- rnorm(1e4,0,0.02)
sample_beta_mean <- rnorm(1e4,0,0.02)
sample_beta_sd <- rnorm(1e4,0,0.02)
sample_mu <- sample_alpha+sample_beta_abs+sample_beta_mean+sample_beta_sd
sample_sigma <- runif(1e4,0,5)
sample_AFR <- rnorm(1e4,sample_mu,sample_sigma)
for(i in 1:length(sample_AFR)){
  if(sample_AFR[i] < 0){
    sample_AFR[i] <- 0
  }
}
sample_cumabs <- rnorm(1e4,50,30)
for(i in 1:length(sample_cumabs)){
  if(sample_cumabs[i] < 0){
    sample_cumabs[i] <- 0
  }
}
sample_cummean <- rnorm(1e4,20,15)
for(i in 1:length(sample_cummean)){
  if(sample_cummean[i] < 0){
    sample_cummean[i] <- 0
  }
}
sample_cumsd <- rnorm(1e4,10,10)
for(i in 1:length(sample_cumsd)){
  if(sample_cumsd[i] < 0){
    sample_cumsd[i] <- 0
  }
}

synth_data <- data.frame(AFR=sample_AFR,wabs=sample_cumabs,wmean=sample_cummean,wsd=sample_cumsd)
hist(synth_data$AFR)
hist(synth_data$wabs)
hist(synth_data$wmean)
hist(synth_data$wsd)
#prepare data for model
synth_data_list <- list(
  AFR=standardize(synth_data$AFR),
  wabs=standardize(synth_data$wabs),
  wmean=standardize(synth_data$wmean),
  wsd=standardize(synth_data$wsd)
)

#define model
#alpha~normal(20,8), beta_abs~normal(0,0.02), beta_mean~normal(0,0.02), and sigma~uniform(0,5)
mprior1 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(20,8),
    b_abs ~ dnorm(0,0.02),
    b_mean ~ dnorm(0,0.02),
    b_sd ~ dnorm(0,0.02),
    sigma ~ dunif(0,5)
  ),data=synth_data_list,chains = 4
)

#check priors
prior <- extract.prior( mprior1 )
mu <- link( mprior1 , post=prior , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , mu[i,] , col=col.alpha("black",0.4) )

#alpha~normal(20,8), beta_abs~normal(0,0.02), beta_mean~normal(0,0.02), and sigma~exponential(1)
mprior2 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(20,8),
    b_abs ~ dnorm(0,0.02),
    b_mean ~ dnorm(0,0.02),
    b_sd ~ dnorm(0,0.02),
    sigma ~ dexp(1)
  ),data=synth_data_list,chains = 4
)

#check priors
prior2 <- extract.prior( mprior2 )
mu <- link( mprior2 , post=prior2 , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , mu[i,] , col=col.alpha("black",0.4) )

#alpha~normal(20,8), beta_abs~normal(0,1), beta_mean~normal(0,1), and sigma~exponential(1)
mprior3 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(20,8),
    b_abs ~ dnorm(0,1),
    b_mean ~ dnorm(0,1),
    b_sd ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),data=synth_data_list,chains = 4
)

#check priors
prior3 <- extract.prior( mprior3 )
mu <- link( mprior3 , post=prior3 , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , mu[i,] , col=col.alpha("black",0.4) )

#alpha~normal(20,8), beta_abs~normal(0,0.5), beta_mean~normal(0,0.5), and sigma~exponential(1)
mprior4 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(20,8),
    b_abs ~ dnorm(0,0.5),
    b_mean ~ dnorm(0,0.5),
    b_sd ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),data=synth_data_list,chains = 4
)

#check priors
prior4 <- extract.prior( mprior4 )
mu <- link( mprior4 , post=prior4 , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , mu[i,] , col=col.alpha("black",0.4) )

#alpha~normal(20,8), beta_abs~normal(0,0.1), beta_mean~normal(0,0.1), and sigma~exponential(1)
mprior5 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(20,8),
    b_abs ~ dnorm(0,0.1),
    b_mean ~ dnorm(0,0.1),
    b_sd ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),data=synth_data_list,chains = 4
)

#check priors
prior5 <- extract.prior( mprior5 )
mu <- link( mprior5 , post=prior5 , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , mu[i,] , col=col.alpha("black",0.4) )


#Final priors would be then
#alpha~normal(20,8)
#beta_abs~normal(0,0.02)
#beta_mean~normal(0,0.02)
#beta_sd~normal(0,0.02)
#sigma~exponential(1)

### Final model ----

#prepare data for model
data_list <- list(
  AFR=sample_2$AFB,
  wabs=standardize(sample_2$cumabsafb),
  wmean=standardize(sample_2$cummeanafb),
  wsd=standardize(sample_2$cumsdafb)
)

#define model
#alpha~normal(20,8), beta_abs~normal(0,0.02), beta_mean~normal(0,0.02), and sigma~exponential(1)
model <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(20,8),
    b_abs ~ dnorm(0,0.02),
    b_mean ~ dnorm(0,0.02),
    b_sd ~ dnorm(0,0.02),
    sigma ~ dexp(1)
  ),data=data_list,chains = 4
)

#explore the model
precis(model,depth=2)
plot(precis(model,depth=2))

#check priors
priorf <- extract.prior( model )
muf <- link( model , post=priorf , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , muf[i,] , col=col.alpha("black",0.4) )

#plot it!

#absolute wealth
#create list for link function
dat <- list(
  wabs=seq(0,200,length.out=215),
  wmean=rep(0,215),
  wsd=rep(0,215)
)
#get the posterior mean and ci
#link function
mu <- link(model,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,200),ylim=c(10,35),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )
points(sample_2$cumabsafb,sample_2$AFB)

#mean wealth
#create list for link function
dat <- list(
  wabs=rep(0,215),
  wmean=seq(from=min(sample_2$cummeanafb)-0.15,to=max(sample_2$cummeanafb)+0.15,length.out=215),
  wsd=rep(0,215)
)
#get the posterior mean and ci
#link function
mu <- link(model,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,60),ylim=c(5,35),xlab="Mean material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )
points(sample_2$cumabsafb,sample_2$AFB)

#sd wealth
#create list for link function
dat <- list(
  wabs=rep(0,215),
  wmean=rep(0,215),
  wsd=seq(from=min(sample_2$cumsdafb)-0.15,to=max(sample_2$cumsdafb)+0.15,length.out=215)
)
#get the posterior mean and ci
#link function
mu <- link(model,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,60),ylim=c(5,35),xlab="SD material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )
points(sample_2$cumabsafb,sample_2$AFB)

### Update model ----

#prepare data for model
data_list <- list(
  AFR=standardize(sample_2$AFB),
  wabs=standardize(sample_2$cumabsafb),
  wmean=standardize(sample_2$cummeanafb),
  wsd=standardize(sample_2$cumsdafb)
)

#define model
#alpha~normal(20,5), beta_abs~normal(0,1), beta_mean~normal(0,1), and sigma~exponential(1)
model2 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(20,5),
    b_abs ~ dnorm(0,1),
    b_mean ~ dnorm(0,1),
    b_sd ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),data=data_list,chains = 4
)

#explore the model
precis(model2,depth=2)
plot(precis(model2,depth=2))

#check priors
priorf2 <- extract.prior( model2 )
muf2 <- link( model2 , post=priorf2 , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , muf[i,] , col=col.alpha("black",0.4) )

#plot it!

#absolute wealth
#create list for link function
dat <- list(
  wabs=seq(0,200,length.out=215),
  wmean=rep(0,215),
  wsd=rep(0,215)
)
#get the posterior mean and ci
#link function
mu <- link(model2,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,200),ylim=c(-5,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#mean wealth
#create list for link function
dat <- list(
  wabs=rep(0,215),
  wmean=seq(from=min(sample_2$cummeanafb)-0.15,to=max(sample_2$cummeanafb)+0.15,length.out=215),
  wsd=rep(0,215)
)
#get the posterior mean and ci
#link function
mu <- link(model2,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,60),ylim=c(-5,50),xlab="Mean material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#sd wealth
#create list for link function
dat <- list(
  wabs=rep(0,215),
  wmean=rep(0,215),
  wsd=seq(from=min(sample_2$cumsdafb)-0.15,to=max(sample_2$cumsdafb)+0.15,length.out=215)
)
#get the posterior mean and ci
#link function
mu <- link(model2,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,60),ylim=c(-5,50),xlab="Mean material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#define model
#alpha~normal(18,8), beta_abs~normal(0,1), beta_mean~normal(0,1), beta_sd~normal(0,1), and sigma~exponential(1)
model3 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(18,8),
    b_abs ~ dnorm(0,1),
    b_mean ~ dnorm(0,1),
    b_sd ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),data=data_list,chains = 4
)

#explore the model
precis(model3,depth=2)
plot(precis(model3,depth=2))

#check priors
priorf3 <- extract.prior( model3 )
muf3 <- link( model3 , post=priorf3 , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , muf[i,] , col=col.alpha("black",0.4) )

#plot it!

#absolute wealth
#create list for link function
dat <- list(
  wabs=seq(0,200,length.out=215),
  wmean=rep(0,215),
  wsd=rep(0,215)
)
#get the posterior mean and ci
#link function
mu <- link(model3,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,200),ylim=c(-5,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#mean wealth
#create list for link function
dat <- list(
  wabs=rep(0,215),
  wmean=seq(from=min(sample_2$cummeanafb)-0.15,to=max(sample_2$cummeanafb)+0.15,length.out=215),
  wsd=rep(0,215)
)
#get the posterior mean and ci
#link function
mu <- link(model3,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,60),ylim=c(-5,50),xlab="Mean material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#sd wealth
#create list for link function
dat <- list(
  wabs=rep(0,215),
  wmean=rep(0,215),
  wsd=seq(from=min(sample_2$cumsdafb)-0.15,to=max(sample_2$cumsdafb)+0.15,length.out=215)
)
#get the posterior mean and ci
#link function
mu <- link(model3,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,60),ylim=c(-5,50),xlab="Mean material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

plot(coeftab(model,model2,model3),par=c("b_abs","b_mean","b_sd"))


#define model
#alpha~normal(20,5), beta_abs~normal(0,0.5), beta_mean~normal(0,0.5), beta_sd~normal(0,0.5), and sigma~exponential(1)
model4 <- ulam(
  alist(
    AFR ~ dnorm(mu,sigma),
    mu <- a + b_abs*wabs + b_mean*wmean + b_sd*wsd,
    a ~ dnorm(20,5),
    b_abs ~ dnorm(0,0.5),
    b_mean ~ dnorm(0,0.5),
    b_sd ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),data=data_list,chains = 4
)

#explore the model
precis(model4,depth=2)
plot(precis(model4,depth=2))

#check priors
priorf4 <- extract.prior( model4 )
muf4 <- link( model4 , post=priorf4 , data=list( wabs=c(0,200),wmean=c(0,60),wsd=c(0,40) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , muf[i,] , col=col.alpha("black",0.4) )

#plot it!

#absolute wealth
#create list for link function
dat <- list(
  wabs=seq(0,200,length.out=215),
  wmean=rep(0,215),
  wsd=rep(0,215)
)
#get the posterior mean and ci
#link function
mu <- link(model4,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,200),ylim=c(-5,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#mean wealth
#create list for link function
dat <- list(
  wabs=rep(0,215),
  wmean=seq(from=min(sample_2$cummeanafb)-0.15,to=max(sample_2$cummeanafb)+0.15,length.out=215),
  wsd=rep(0,215)
)
#get the posterior mean and ci
#link function
mu <- link(model4,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,60),ylim=c(-5,50),xlab="Mean material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#sd wealth
#create list for link function
dat <- list(
  wabs=rep(0,215),
  wmean=rep(0,215),
  wsd=seq(from=min(sample_2$cumsdafb)-0.15,to=max(sample_2$cumsdafb)+0.15,length.out=215)
)
#get the posterior mean and ci
#link function
mu <- link(model4,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,60),ylim=c(-5,50),xlab="Mean material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

plot(coeftab(model,model2,model3,model4),par=c("b_abs","b_mean","b_sd"))

# Model for EHBEA -----

#prepare data for model
data_list <- list(
  AFR=sample_2$AFB,
  absw=standardize(sample_2$cumabsafb),
  meanw=standardize(sample_2$cummeanafb),
  sdw=standardize(sample_2$cumsdafb),
  birthy=as.integer(as.factor(sample_2$DOBYR))
)

##Mean wealth -----

#define the model
mgaus11 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*meanw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.01),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus11,depth=3)
plot(precis(mgaus11,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus11)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
Mean_wealth <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + c_by[birthy]*meanw,
    c(a_by,c_by)[birthy] ~ multi_normal( c(a,Mean) , Rho , sigma_by ),
    a ~ normal(20,8),
    Mean ~ normal(0,0.02),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus12,depth=3)
plot(precis(mgaus12,depth=3))

#define the model
mgaus13 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*meanw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.03),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus13,depth=3)
plot(precis(mgaus13,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus13)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus14 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*meanw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.04),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus14,depth=3)
plot(precis(mgaus14,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus14)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus14)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus15 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*meanw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.05),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus15,depth=3)
plot(precis(mgaus15,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus15)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

plot(coeftab(mgaus11,mgaus12,mgaus13,mgaus14,mgaus15),par=c("a"))
plot(coeftab(mgaus11,mgaus12,mgaus13,mgaus14,mgaus15),par=c("b"))
plot(coeftab(mgaus11,mgaus12,mgaus13,mgaus14,mgaus15),par=c("sigma"))

##Variability of wealth -----

#define the model
mgaus21 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + d_by[birthy]*sdw,
    c(a_by,d_by)[birthy] ~ multi_normal( c(a,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    d ~ normal(0,0.01),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus21,depth=3)
plot(precis(mgaus21,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus21)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
SD_wealth <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + d_by[birthy]*sdw,
    c(a_by,d_by)[birthy] ~ multi_normal( c(a,SD) , Rho , sigma_by ),
    a ~ normal(20,8),
    SD ~ normal(0,0.02),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus22,depth=3)
plot(precis(mgaus22,depth=3))

#define the model
mgaus23 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*sdw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.03),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus23,depth=3)
plot(precis(mgaus23,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus23)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus24 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*sdw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.04),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus24,depth=3)
plot(precis(mgaus24,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus24)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus24)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus25 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*sdw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.05),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus25,depth=3)
plot(precis(mgaus25,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus25)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

plot(coeftab(mgaus21,mgaus22,mgaus23,mgaus24,mgaus25),par=c("a"))
plot(coeftab(mgaus21,mgaus22,mgaus23,mgaus24,mgaus25),par=c("b"))
plot(coeftab(mgaus21,mgaus22,mgaus23,mgaus24,mgaus25),par=c("sigma"))

##Absolute wealth -----

#define the model
mgaus31 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.01),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus31,depth=3)
plot(precis(mgaus31,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus31)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
Absolute_wealth <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,Absolute) , Rho , sigma_by ),
    a ~ normal(20,8),
    Absolute ~ normal(0,0.02),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus32,depth=3)
plot(precis(mgaus32,depth=3))

#define the model
mgaus33 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.03),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus33,depth=3)
plot(precis(mgaus33,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus33)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus34 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.04),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus34,depth=3)
plot(precis(mgaus34,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus34)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus34)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus35 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw,
    c(a_by,b_by)[birthy] ~ multi_normal( c(a,b) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.05),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus35,depth=3)
plot(precis(mgaus35,depth=3))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus35)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

plot(coeftab(mgaus31,mgaus32,mgaus33,mgaus34,mgaus35),par=c("a"))
plot(coeftab(mgaus31,mgaus32,mgaus33,mgaus34,mgaus35),par=c("b"))
plot(coeftab(mgaus31,mgaus32,mgaus33,mgaus34,mgaus35),par=c("sigma"))

## Multiple regression with Gaussian process ----

#define the model
mgaus41 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw + c_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,c_by,d_by)[birthy] ~ multi_normal( c(a,b,c,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.01),
    c ~ normal(0,0.01),
    d ~ normal(0,0.01),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus41,depth=3)
plot(precis(mgaus41,pars=c("a","b","c","d","sigma")))
plot(precis(mgaus41,pars=c("b","c","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus41)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus42 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw + c_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,c_by,d_by)[birthy] ~ multi_normal( c(a,b,c,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.02),
    c ~ normal(0,0.02),
    d ~ normal(0,0.02),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus42,depth=3)
plot(precis(mgaus42,pars=c("a","b","c","d","sigma")))
plot(precis(mgaus42,pars=c("b","c","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus42)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=4 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus43 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw + c_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,c_by,d_by)[birthy] ~ multi_normal( c(a,b,c,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.03),
    c ~ normal(0,0.03),
    d ~ normal(0,0.03),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus43,depth=3)
plot(precis(mgaus43,pars=c("a","b","c","d","sigma")))
plot(precis(mgaus43,pars=c("b","c","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus43)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus44 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw + c_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,c_by,d_by)[birthy] ~ multi_normal( c(a,b,c,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.04),
    c ~ normal(0,0.04),
    d ~ normal(0,0.04),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus44,depth=3)
plot(precis(mgaus44,pars=c("a","b","c","d","sigma")))
plot(precis(mgaus44,pars=c("b","c","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus44)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus45 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw + c_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,c_by,d_by)[birthy] ~ multi_normal( c(a,b,c,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.05),
    c ~ normal(0,0.05),
    d ~ normal(0,0.05),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus45,depth=3)
plot(precis(mgaus45,pars=c("a","b","c","d","sigma")))
plot(precis(mgaus45,pars=c("b","c","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus45)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus46 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw + c_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,c_by,d_by)[birthy] ~ multi_normal( c(a,b,c,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.5),
    c ~ normal(0,0.5),
    d ~ normal(0,0.5),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus46,depth=3)
plot(precis(mgaus46,pars=c("a","b","c","d","sigma")))
plot(precis(mgaus46,pars=c("b","c","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus46)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

#define the model
mgaus47 <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw + c_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,c_by,d_by)[birthy] ~ multi_normal( c(a,b,c,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,1),
    c ~ normal(0,1),
    d ~ normal(0,1),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus47,depth=3)
plot(precis(mgaus47,pars=c("a","b","c","d","sigma")))
plot(precis(mgaus47,pars=c("b","c","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus47)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

### Final model ----

#define the model
All_wealth <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*absw + c_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,c_by,d_by)[birthy] ~ multi_normal( c(a,Absolute,Mean,SD) , Rho , sigma_by ),
    a ~ normal(20,8),
    Absolute ~ normal(0,0.02),
    Mean ~ normal(0,0.02),
    SD ~ normal(0,0.02),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaus42,depth=3)
plot(precis(mgaus42,depth=3))
plot(precis(mgaus42,pars=c("a","b","c","d","sigma")))
plot(precis(mgaus42,pars=c("b","c","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaus42)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=4 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

plot(coeftab(SD_wealth,Mean_wealth,Absolute_wealth,All_wealth),par=c("Absolute","Mean","SD"),by.model=TRUE)

#plot it!

#absolute wealth
#create list for link function
dat <- list(
  absw=seq(0,200,length.out=215),
  meanw=rep(0,215),
  sdw=rep(0,215),
  birthy=rep(1,215)
)
#get the posterior mean and ci
#link function
mu <- link(mgaus42,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,200),ylim=c(0,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#mean wealth
#create list for link function
dat <- list(
  absw=rep(0,215),
  meanw=seq(0,55,length.out=215),
  sdw=rep(0,215),
  birthy=rep(1,215)
)
#get the posterior mean and ci
#link function
mu <- link(mgaus42,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,55),ylim=c(0,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,55,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,55,length.out=215) )

#SD wealth
#create list for link function
dat <- list(
  absw=rep(0,215),
  meanw=rep(0,215),
  sdw=seq(0,30,length.out=215),
  birthy=rep(1,215)
)
#get the posterior mean and ci
#link function
mu <- link(mgaus42,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,30),ylim=c(0,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,30,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,30,length.out=215) )

#define the model
mgaust <- ulam(
  alist(
    AFR ~ normal( mu , sigma ),
    mu <- a_by[birthy] + b_by[birthy]*meanw + d_by[birthy]*sdw,
    c(a_by,b_by,d_by)[birthy] ~ multi_normal( c(a,b,d) , Rho , sigma_by ),
    a ~ normal(20,8),
    b ~ normal(0,0.002),
    d ~ normal(0,0.002),
    sigma_by ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=data_list , chains=4 , cores=4 
)
#check the model
precis(mgaust,depth=3)
plot(precis(mgaust,pars=c("a","b","d","sigma")))
plot(precis(mgaust,pars=c("b","d")))

#check posterior correlation between intercepts and slopes
post <- extract.samples(mgaust)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=4 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )

plot(coeftab(mgaus12,mgaus22,mgaus32,mgaus42),par=c("b","c","d"))

#plot it!

#absolute wealth
#create list for link function
dat <- list(
  absw=seq(0,200,length.out=215),
  meanw=rep(0,215),
  sdw=rep(0,215),
  birthy=rep(1,215)
)
#get the posterior mean and ci
#link function
mu <- link(mgaust,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,200),ylim=c(0,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,200,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,200,length.out=215) )

#mean wealth
#create list for link function
dat <- list(
  meanw=seq(0,55,length.out=215),
  sdw=rep(0,215),
  birthy=1969:1998
)
#get the posterior mean and ci
#link function
mu <- link(mgaust,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,55),ylim=c(0,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,55,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,55,length.out=215) )

#SD wealth
#create list for link function
dat <- list(
  meanw=rep(0,215),
  sdw=seq(0,30,length.out=215),
  birthy=rep(1,215)
)
#get the posterior mean and ci
#link function
mu <- link(mgaust,data=dat)
#get the mean
mu_mean <- apply(mu,2,mean)
#get the ci
mu_ci <- apply(mu,2,PI)
#plot
plot(NULL,xlim=c(0,30),ylim=c(0,50),xlab="Absolute material wealth",ylab="Age at first reproduction")
# plot the line
lines( seq(0,30,length.out=215), mu_mean )
# plot a shaded region for 89% PI
shade( mu_ci , seq(0,30,length.out=215) )





