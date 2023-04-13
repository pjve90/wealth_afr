# PhD project on wealth and age at first reproduction ----

# This script is meant to collect all the necessary code to build up the models and explore the data necessary to understand the relationship between wealth and age at first reproduction.

#install package to import excel file
#install.packages("readxl")
library(readxl)
#install package for bayesian analysis
#install.packages("rethinking")
library(rethinking)

## Data exploration ----

sample_2

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


prior <- extract.prior( mprior1 )
mu <- link( mprior1 , post=prior , data=list( wabs=c(0,200) ) )
plot( NULL , xlim=c(0,200) , ylim=c(-5,50) )
abline( h=0 , lty=2 )
abline( h=45 , lty=1 , lwd=0.5 )
for ( i in 1:100 ) lines( c(0,200) , mu[i,] , col=col.alpha("black",0.4) )


#FOLLOW PAGE 111 IN RETHINKING

# 
# - Define a generative model of the sample.
# - Define a specific estimand.
# - Design a statistical way to produce estimate.
# - Test the statistical model using the generative model.
# - Analyse the sample (summarise).
# 

