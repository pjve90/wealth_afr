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

## Data wrangling of real data ----

#Load data
real_data1 <- read.csv("dataf.csv")[,-1]
head(real_data1)

#Calculate age-specific age at first reproduction
#create a matrix to store the age-specific age of censor
afr_matrix1 <- matrix(nrow=nrow(real_data1),ncol=A+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix1)){
  afr <- real_data1$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data1$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix1[i,1:(afr-1)] <- 0
    afr_matrix1[i,afr] <- 1
    afr_matrix1[i,(afr+1):aoc] <- 0
  } else{
    afr_matrix1[i,1:aoc] <- rep(0,length(afr_matrix1[i,1:aoc]))
  }
}
#check the data
afr_matrix1
#check the age-specific probability of AFR, proportion relative to all women whose AFR is known
apply(afr_matrix1,2,sum,na.rm=T)/sum(apply(afr_matrix1,2,sum,na.rm=T))
#plot it
plot(apply(afr_matrix1,2,sum,na.rm=T)/sum(apply(afr_matrix1,2,sum,na.rm=T))~c(1:(A+1)),xlab="Age",ylab="Probability of first reproduction",ylim=c(0,0.2))

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

# reduce computation by only calculating probabilities for the realistic data range - if we do not observe AFR above a certain age, we cannot model the probabilities. We add 5 years as buffer
max(real_data1$afr,na.rm=T) # 32 years old
afr_matrix1_b <- afr_matrix1[,1:(max(real_data1$afr,na.rm=T)+5)]
#check the data
afr_matrix1_b

## Fit real data ----

#put all the data together
#create dataset
real_list1 <- list(N = nrow(real_data1), #population size
              A = ncol(afr_matrix1[,1:41]), #age
              baby = afr_matrix1[,1:41]) #AFR

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

# To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
tab1_mu_real[,1]<-round(inv_logit(tab1_mu_real[,1]),3)
tab1_mu_real[,3]<-round(inv_logit(tab1_mu_real[,3]),3)
tab1_mu_real[,4]<-round(inv_logit(tab1_mu_real[,4]),3)

## Plot the fit of the real data ----

#plot the predictions from the model with the data
#compute probability of FR at each age
p1_real <- matrix(nrow=nrow(post1_real$mu),ncol=ncol(post1_real$mu))
p1_real
#fill it in
for(j in 1:ncol(post1_real$mu)){
  for(i in 1:nrow(post1_real$mu)){
    p1_real[i,j] <- inv_logit(post1_real$alpha[i] + #inv logit to convert to probability scale
                                post1_real$mu[i,j]) #age
  }
}
#check data
p1_real
#plot it!
#prepare model prediction data
plot_data1_real <- data.frame(age = 1:ncol(p1_real[,1:41]), 
                         mu_mean = apply(p1_real[,1:41], 2, mean), 
                         mu_upp = apply(p1_real[,1:41], 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                         mu_low = apply(p1_real[,1:41], 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr1 <- afr_matrix1[,1:41]
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
     ylim=c(0,0.25),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Model with Gaussian process of age",
     col=hcl.colors(10,"ag_Sunset")[5],
     type="l")
lines(plot_data1_real$mu_upp~plot_data1_real$age,lty=2,col=hcl.colors(10,"ag_Sunset")[5])
lines(plot_data1_real$mu_low~plot_data1_real$age,lty=2,col=hcl.colors(10,"ag_Sunset")[5])
points(apply(plot_afr1[,1:41],2,sum,na.rm=T)/sum(is.na(real_data1$afr)==F)~plot_data1_real$age,pch=16)
