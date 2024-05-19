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
#install.packages("scales")
library(scales)

## Data wrangling of real data ----

#Load data
real_data1 <- read.csv("dataf.csv")[,-1]
head(real_data1)

#Calculate age-specific age at first reproduction
#create a matrix to store the age-specific age of first reproduction
afr_matrix1 <- matrix(nrow=nrow(real_data1),ncol=max(real_data1$aoc)+1) #ages defined by the oldest woman in the sample
#calculate for each age when the woman has her first baby (1) or not (0)
for(i in 1:nrow(afr_matrix1)){
  afr <- real_data1$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data1$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix1[i,1:(afr-1)] <- 0
    afr_matrix1[i,afr] <- 1
  } else{
    afr_matrix1[i,1:aoc] <- rep(0,length(afr_matrix1[i,1:aoc]))
  }
}
#check the data
afr_matrix1
#check the age-specific probability of AFR, proportion relative to all women whose AFR is known
apply(afr_matrix1,2,sum,na.rm=T)/sum(apply(afr_matrix1,2,sum,na.rm=T))
#plot it
plot(cumprod(1-apply(afr_matrix1,2,sum,na.rm=T)/sum(apply(afr_matrix1,2,sum,na.rm=T)))~c(1:(max(real_data1$aoc)+1)),xlab="Age",ylab="Cumulative probability of first birth,ylim=c(0,1))

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

## Fit real data ----

#put all the data together
#create dataset
real_list1 <- list(N = nrow(real_data1), #population size
              A = ncol(afr_matrix1), #age
              baby = afr_matrix1) #AFR

# compile model
m1 <- cmdstan_model("Model_code/firstbaby.stan")

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
rstan::traceplot(rds1_real,pars="alpha")
#mu
#traceplot(rds1_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds1_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
rstan::traceplot(rds1_real,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds1_real,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds1_real,pars="mu_delta")

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
plot(tab1_mu_real)
plot(cumprod(1-inv_logit(tab1_mu_real[,1])),ylim=c(0,1))

# # To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
# tab1_mu_real[,1]<-round(inv_logit(tab1_mu_real[,1]),3)
# tab1_mu_real[,3]<-round(inv_logit(tab1_mu_real[,3]),3)
# tab1_mu_real[,4]<-round(inv_logit(tab1_mu_real[,4]),3)

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
plot_data1_real <- data.frame(age = 1:ncol(p1_real), 
                         mu_mean = apply(p1_real, 2, mean), 
                         mu_upp = apply(p1_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                         mu_low = apply(p1_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr1_real <- afr_matrix1
#change -99 to NAs
for(j in 1:ncol(plot_afr1_real)){
  for(i in 1:nrow(plot_afr1_real)){
    if(plot_afr1_real[i,j]==-99){
      plot_afr1_real[i,j] <- NA
    }
  }
}
#check the data
plot_afr1_real

#plot age random effect
plot(cumprod(1-plot_data1_real$mu_mean)~plot_data1_real$age,
     ylab="Cumulative probability of first birth,
     xlab="Age",
     main="Model with Gaussian process of age",
     pch=16,
     lwd=2,
     ylim=c(0,1),
     col=hcl.colors(4,"temps")[2]
)
lines(cumprod(1-plot_data1_real$mu_mean)~plot_data1_real$age,col=hcl.colors(4,"temps")[2],lwd=2)
polygon(c(plot_data1_real$age,rev(plot_data1_real$age)),c(cumprod(1-plot_data1_real$mu_low),rev(cumprod(1-plot_data1_real$mu_upp))),col=alpha(hcl.colors(4,"temps")[2],0.5),border=NA)
points(cumprod(1-apply(plot_afr1_real,2,sum,na.rm=T)/sum(apply(plot_afr1_real,2,sum,na.rm=T)))~plot_data1_real$age,pch=16,col=hcl.colors(4,"temps")[3])
lines(cumprod(1-apply(plot_afr1_real,2,sum,na.rm=T)/sum(apply(plot_afr1_real,2,sum,na.rm=T)))~plot_data1_real$age,pch=16,col=hcl.colors(4,"temps")[3],lwd=2)

