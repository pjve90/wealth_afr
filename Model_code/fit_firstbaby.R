# Model with Gaussian process of age ----

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
mu_age<-c(rep(0,10),seq(from=0.01,to=0.1,length=11),rep(0.1,4),seq(from=0.1,to=0.01,length=17),rep(0,49))
#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
afrs[,1:10] <- 0
#randomly assign a positive output of AFR for individuals between ages 13 and 32
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afr_prob <- mu_age[j]
      if(afr_prob<0){afr_prob<-0}
      afrs[i,j] <- rbinom(1,1,afr_prob)
    } else{
      afrs[i,j] <- 0
    }  
  }
}
#check the data
head(afrs)
#check the age-specific frequency of FR
apply(afrs,2,sum)/100
#plot it
plot(apply(afrs,2,sum),xlab="Age",ylab="Probability of first reproduction")

## Fit simulated data ----

#put all the data together
#create dataset
data1 <- list(N = N, #population size
A = A+1, #age
baby = afrs) #AFR

# compile model
m1 <- cmdstan_model("Model_code/firstbaby.stan")

# fit model
fit1 <- m1$sample(data = data1, 
                chains = 4, 
                parallel_chains = 4, 
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
#mu_raw, mu_tau, mu_delta
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

## Plot the fit of the simulated data ----

#plot the predictions from the model with the simulated data
#compute probability of FR at each age
p1 <- matrix(nrow=nrow(post1$mu),ncol=ncol(post1$mu))
p1
#fill it in
for(j in 1:ncol(post1$mu)){
  for(i in 1:nrow(post1$mu)){
    p1[i,j] <- inv_logit(post1$alpha[i] + post1$mu[i,j]) #inv logit because originally is logit
  }
}
#check data
p1
#plot it!
#prepare model prediction data
plot_data1 <- data.frame(age = 1:91, 
                        mu_mean = apply(p, 2, mean), 
                        mu_upp = apply(p, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                        mu_low = apply(p, 2, function(x) HPDI(x, prob = 0.9))[2, ]
                        ) 

#plot age random effect
plot(plot_data1$mu_mean~plot_data1$age,
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Model with Gaussian process of age",
     type="l")
lines(plot_data1$mu_upp~plot_data1$age,col="red")
lines(plot_data1$mu_low~plot_data1$age,col="blue")
points(colSums(as.data.frame(afrs))/100~plot_data1$age,col="gold",pch=16)

## Data wrangling of real data ----

#Load data
real_data1 <- read.csv("~/wealth_afr/dataf.csv")[,-1]
head(real_data1)

#Calculate age-specific age at first reproduction
#create a matrix to store the age-specific age of censor
afr_matrix1 <- matrix(nrow=nrow(real_data1),ncol=91)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix1)){
  afr <- real_data1$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data1$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix1[i,1:(afr-1)] <- rep(0,length(afr_matrix1[i,1:(afr-1)]))
    afr_matrix1[i,afr] <- 1
  } else{
    afr_matrix1[i,1:aoc] <- rep(0,length(afr_matrix1[i,1:aoc]))
  }
}
#check the data
afr_matrix1
#check the age-specific probability of FR
colSums(as.data.frame(afr_matrix1),na.rm=T)/100
#plot it
plot(colSums(as.data.frame(afr_matrix1),na.rm = T)/100~c(1:91),xlab="Age",ylab="Probability of first reproduction",ylim=c(0,1))

#replace NAs with -99
for(j in 1:ncol(afr_matrix1)){
  for(i in 1:nrow(afr_matrix1)){
    if(is.na(afr_matrix1[i,j])){
      afr_matrix1[i,j] <- -99
    } else{
      afr_matrix1[i,j] <- afr_matrix1[i,j]
    }
  }
}
#check the data
afr_matrix1

## Fit real data ----

#put all the data together
#create dataset
real_list1 <- list(N = nrow(real_data1), #population size
              A = ncol(afr_matrix1), #age
              baby = afr_matrix1) #AFR

# fit model
fit1_real <- m1$sample(data = real_list1, 
                  chains = 4, 
                  parallel_chains = 10, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)

# save fit 
fit_1_real <- rstan::read_stan_csv(fit1_real$output_files())
saveRDS(fit_1_real, "firstbaby1_real.rds")
#load RDS file
rds1_real <- readRDS("firstbaby1_real.rds")
#extract samples
post1_real <- extract.samples(rds1_real)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds1_real,pars="alpha")
#mu
#traceplot(rds1_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds1_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds1_real,pars="mu_tau")
#mu_kappa
traceplot(rds1_real,pars="mu_kappa")
#mu_delta
traceplot(rds1_real,pars="mu_delta")

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab1_real <- precis(rds1_real,depth=3,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tab1_real
#create summary table for mu
tab1_mu_real <- precis(rds1_real,depth=3,pars="mu")
#check table
tab1_mu_real

## Plot the fit of the real data ----

#plot the predictions from the model with the data
#compute probability of FR at each age
p1_real <- matrix(nrow=nrow(post1_real$mu),ncol=ncol(post1_real$mu))
p1_real
#fill it in
for(j in 1:ncol(post1_real$mu)){
  for(i in 1:nrow(post1_real$mu)){
    p1_real[i,j] <- inv_logit(post1_real$alpha[i] + post1_real$mu[i,j]) #inv logit because originally is logit
  }
}
#check data
p1_real
#plot it!
#prepare model prediction data
plot_data1_real <- data.frame(age = 1:91, 
                         mu_mean = apply(p1_real, 2, mean), 
                         mu_upp = apply(p1_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                         mu_low = apply(p1_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr1 <- afr_matrix1
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
plot(plot_data1_real$mu_mean~plot_data1_real$age,
     ylim=c(0,1),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Model with Gaussian process of age",
     type="l")
lines(plot_data1_real$mu_upp~plot_data1_real$age,col="red")
lines(plot_data1_real$mu_low~plot_data1_real$age,col="blue")
points(colSums(plot_afr1,na.rm=T)/100~plot_data1_real$age,col="gold",pch=16)
