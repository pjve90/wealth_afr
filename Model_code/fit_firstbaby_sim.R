# Model with Gaussian process of age using simulated data ----

#The code in this script is meant to fit a Bayesian model with a Gaussian process of age, in order to see the relationship between age and the probability of first reproduction of women.
#First, there is data simulation that follows the expectations from the literature.
#Second, the simulated data is fitted to the Bayesian model.
#Third, we use the real data to see how the model fits. 
#Finally, there is plotting code

#Load packages
#install.packages("cmdstanr")
library(cmdstanr)
#install.packages("rethinking")
library(rethinking)

## Data simulation ----

#The script in this section is to create synthetic data that follows the causal relationship between age and the probability of first reproduction.

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

#Age at first reproduction (AFR)

#simulate an age-specific parameter for AFR (mu)
mu_age<-c(rep(0,13),seq(from=0.001,to=0.2,length=6),seq(from=0.14,to=0.01,length=5),seq(from=0.01, to=0.001,length=15),rep(0,52))
mu_age
#check that they sum to 1
sum(mu_age)
#plot it!
plot(mu_age~c(1:length(mu_age)))

#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#assign first column a zero, because they cannot have their first baby when born
afrs[,1] <- 0
afrs
#randomly assign a positive output of AFR for individuals
for(j in 2:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(!is.na(afrs[i,j-1])){
      if(afrs[i,j-1] == 0){
        afr_prob <- mu_age[j]
        afrs[i,j] <- rbinom(1,1,afr_prob)
      } else{
        afrs[i,j] <- NA
      }
    } else{
      afrs[i,j] <- NA
   }
  }
}
#check the data
head(afrs)
#check the age-specific frequency of FR
apply(afrs,2,sum,na.rm = T)
apply(afrs,2,sum,na.rm = T)/N
#plot it
plot(apply(afrs,2,sum,na.rm = T)/N,
     ylim=c(0,0.2),
     xlab="Age",
     ylab="Probability of first reproduction",
     pch=16) #data
lines(mu_age,col=hcl.colors(10,"ag_Sunset")[5]) #mu

## Fit simulated data ----

#replace NAs with -99
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(afrs[i,j])){
      afrs[i,j] <- -99
    } else{
      afrs[i,j] <- afrs[i,j]
    }
  }
}
#check the data
afrs


#put all the data together
#create dataset
data1 <- list(N = N, #population size
              A = A+1, #age until which women have their first child in reality
              baby = afrs) #AFR

# compile model
m1 <- cmdstan_model("Model_code/firstbaby.stan")

# fit model
fit1 <- m1$sample(data = data1,
                  chains = 4, 
                  parallel_chains = 15, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)

# save fit 
fit_1 <- rstan::read_stan_csv(fit1$output_files())
saveRDS(fit_1, "firstbaby1.rds")
#load RDS file
rds1 <- readRDS("firstbaby1.rds")
#extract samples
post1 <- extract.samples(rds1)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds1,pars="alpha")
#mu
#traceplot(rds1,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds1,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds1,pars="mu_tau")
#mu_kappa
traceplot(rds1,pars="mu_kappa")
#mu_delta
traceplot(rds1,pars="mu_delta")

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab1 <- precis(rds1,depth=3,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tab1
#create summary table for mu
tab1_mu <- precis(rds1,depth=3,pars="mu")
#check table
tab1_mu
plot(inv_logit(tab1_mu[,1]))

## Plot the fit of the simulated data ----

#plot the predictions from the model with the simulated data
#compute probability of FR at each age
p1 <- matrix(nrow=nrow(post1$mu),ncol=ncol(post1$mu))
p1
#fill it in
for(j in 1:ncol(post1$mu)){
  for(i in 1:nrow(post1$mu)){
    p1[i,j] <- inv_logit(post1$alpha[i] + #inv logit to convert to probability scale
                           post1$mu[i,j]) #age
  }
}
#check data
p1

#plot it!
#prepare model prediction data
plot_data1 <- data.frame(age = 1:ncol(post1$mu), 
                         mu_mean = apply(p1, 2, mean), 
                         mu_upp = apply(p1, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                         mu_low = apply(p1, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 

#change -99 to NAs
#create a matrix
plot_afr1 <- afrs
#change -99 to NAs
for(j in 1:ncol(plot_afr1)){
  for(i in 1:nrow(plot_afr1)){
    if(plot_afr1[i,j]==-99){
      plot_afr1[i,j] <- NA
    }
  }
}
#check the data
plot_afr1

#plot age random effect
plot(plot_data1$mu_mean~plot_data1$age,
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Model with Gaussian process of age",
     lwd=2,
     ylim=c(0,0.25),
     col=hcl.colors(10,"ag_Sunset")[5]
     )
polygon(c(plot_data1$age,rev(plot_data1$age)),c(plot_data1$mu_low,rev(plot_data1$mu_upp)),col=alpha(hcl.colors(10,"ag_Sunset")[5],0.5))
lines(plot_data1$mu_upp~plot_data1$age,col=hcl.colors(10,"ag_Sunset")[5])
lines(plot_data1$mu_low~plot_data1$age,col=hcl.colors(10,"ag_Sunset")[5])
points(apply(plot_afr1,2,sum,na.rm = T)/N~plot_data1$age,pch=16,col=alpha("black",0.5))
