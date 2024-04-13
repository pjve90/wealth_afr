# Model with both wealth predictors ----

#The code in this script is meant to fit a Bayesian model with absolute wealth and a Gaussian process of age, in order to see the relationship between absolute wealth and the probability of first reproduction of women.
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
real_data4 <- read.csv("dataf.csv")[,-1]
head(real_data4)

# Age at first reproduction 

#create a matrix to store the age-specific age of censor
afr_matrix4 <- matrix(nrow=nrow(real_data4),ncol=A+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix4)){
  afr <- real_data4$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data4$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix4[i,1:(afr-1)] <- 0
    afr_matrix4[i,afr] <- 1
    afr_matrix4[i,(afr+1):aoc] <- 0
  } else{
    afr_matrix4[i,1:aoc] <- rep(0,length(afr_matrix4[i,1:aoc]))
  }
}
#check the data
afr_matrix4
#check the age-specific probability of FR
apply(afr_matrix4,2,sum,na.rm=T)/sum(apply(afr_matrix4,2,sum,na.rm=T))
#plot it
plot(apply(afr_matrix4,2,sum,na.rm=T)/sum(apply(afr_matrix4,2,sum,na.rm=T))~c(1:ncol(afr_matrix4)),xlab="Age",ylab="Probability of first reproduction",ylim=c(0,0.2))

#replace NAs with -99
for(j in 1:ncol(afr_matrix4)){
  for(i in 1:nrow(afr_matrix4)){
    if(is.na(afr_matrix4[i,j])){
      afr_matrix4[i,j] <- -99
    } else{
      afr_matrix4[i,j] <- afr_matrix4[i,j]
    }
  }
}
#check the data
afr_matrix4

#Age-specific absolute wealth

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix4 <- matrix(nrow = nrow(real_data4),ncol=A+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw95[i]
  age_absw <- real_data4$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix4[i,age_absw] <- absw
  } else{
    absw_matrix4[i,age_absw] <- NA
  }
}
#check data
absw_matrix4
#98
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw98[i]
  age_absw <- real_data4$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix4[i,age_absw] <- absw
  } else{
    absw_matrix4[i,age_absw] <- NA
  }
}
#check data
absw_matrix4
#00
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw00[i]
  age_absw <- real_data4$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix4[i,age_absw] <- absw
  } else{
    absw_matrix4[i,age_absw] <- NA
  }
}
#check data
absw_matrix4
#02
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw02[i]
  age_absw <- real_data4$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix4[i,age_absw] <- absw
  } else{
    absw_matrix4[i,age_absw] <- NA
  }
}
#check data
absw_matrix4
#04
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw04[i]
  age_absw <- real_data4$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix4[i,age_absw] <- absw
  } else{
    absw_matrix4[i,age_absw] <- NA
  }
}
#check data
absw_matrix4
#06
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw06[i]
  age_absw <- real_data4$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix4[i,age_absw] <- absw
  } else{
    absw_matrix4[i,age_absw] <- NA
  }
}
#check data
absw_matrix4
#10
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw10[i]
  age_absw <- real_data4$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix4[i,age_absw] <- absw
  } else{
    absw_matrix4[i,age_absw] <- NA
  }
}
#check data
absw_matrix4
#check the age-specific average of absolute wealth
apply(absw_matrix4,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix4,2,mean,na.rm=T)~c(1:ncol(absw_matrix4)),xlab="Age",ylab="Average absolute wealth")

# #NaN in columns where there are no values of wealth
# 
# # Simple data imputation 
# 
# #replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
# for(i in 1:length(absw_matrix4[,1])){
#   if(is.na(absw_matrix4[i,1]) & is.na(absw_matrix4[i,2])){
#     absw_matrix4[i,1] <- mean(absw_matrix4[,1],na.rm = T)
#   }else if(is.na(absw_matrix4[i,1]) & !is.na(absw_matrix4[i,2])){
#     absw_matrix4[i,1] <- absw_matrix4[i,2]
#   }
# }
# #check the data
# absw_matrix4
# sum(is.na(absw_matrix4[,1]))
# #n=0
# #replace the missing wealth data by putting the mean between the two ages it is
# for(j in 2:(ncol(absw_matrix4)-1)){
#   for(i in 1:nrow(absw_matrix4)){
#     if(is.na(absw_matrix4[i,j])&!is.na(absw_matrix4[i,j-1])&!is.na(absw_matrix4[i,j+1])){
#       absw_matrix4[i,j] <- mean(c(absw_matrix4[i,j-1],absw_matrix4[i,j+1]))
#     } else if(is.na(absw_matrix4[i,j])&!is.na(absw_matrix4[i,j-1])&is.na(absw_matrix4[i,j+1])){
#       absw_matrix4[i,j] <- mean(absw_matrix4[,j],na.rm=T)
#     } 
#   }
# }
# #check the data
# absw_matrix4
# sum(is.na(absw_matrix4))
# #n=21824
# #replace the missing wealth data by putting the mean between the two ages it is
# for(j in 2:(ncol(absw_matrix4)-1)){
#   for(i in 1:nrow(absw_matrix4)){
#     if(is.na(absw_matrix4[i,j])&!is.na(absw_matrix4[i,j-1])&!is.na(absw_matrix4[i,j+1])){
#       absw_matrix4[i,j] <- mean(c(absw_matrix4[i,j-1],absw_matrix4[i,j+1]))
#     } else if(is.na(absw_matrix4[i,j])&!is.na(absw_matrix4[i,j-1])&is.na(absw_matrix4[i,j+1])){
#       absw_matrix4[i,j] <- mean(absw_matrix4[,j],na.rm=T)
#     } 
#   }
# }
# #check the data
# absw_matrix4
# sum(is.na(absw_matrix4))
# #n=9179
# #replace the missing wealth data by putting either the mean between the two ages it is or by repeating the value from previous year
# for(j in 2:(ncol(absw_matrix4)-1)){
#   for(i in 1:nrow(absw_matrix4)){
#     if(is.na(absw_matrix4[i,j])&length(is.na(absw_matrix4[i,j:91]))!=sum(is.na(absw_matrix4[i,j:91]))){
#       absw_matrix4[i,j] <- mean(c(absw_matrix4[i,j-1],absw_matrix4[i,max(which(!is.na(absw_matrix4[i,])==T))]))
#     } else if(is.na(absw_matrix4[i,j])&length(is.na(absw_matrix4[i,j:91]))==sum(is.na(absw_matrix4[i,j:91]))){
#       absw_matrix4[i,j] <- absw_matrix4[i,j-1]
#     }
#   }
# }
# #check the data
# absw_matrix4
# sum(is.na(absw_matrix4))
# #n=540
# #replace last column with the values from last year
# absw_matrix4[,max(ncol(absw_matrix4))] <- absw_matrix4[,max(ncol(absw_matrix4))-1]
# #check the data
# absw_matrix4
# sum(is.na(absw_matrix4))
# #n=0
# #check the age-specific frequency of absolute wealth
# apply(absw_matrix4,2,mean)
# #plot it
# plot(apply(absw_matrix4,2,mean)~c(1:91),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix4 <- matrix(standardize(log(as.vector(absw_matrix4))),ncol=ncol(absw_matrix4),nrow=nrow(absw_matrix4))
#check the data
std_absw_matrix4
#check the age-specific average of absolute wealth
apply(std_absw_matrix4,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix4,2,mean,na.rm=T)~c(1:(A+1)),xlab="Age",ylab="Average absolute wealth")

#change NAs for -99
#replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
for(j in 1:ncol(std_absw_matrix4)){
  for(i in 1:nrow(std_absw_matrix4)){
    if(is.na(std_absw_matrix4[i,j])){
      std_absw_matrix4[i,j] <- -99
    }
  }
}
#check the data
std_absw_matrix4

## Fit real data, using the combined data imputation approach ----

# Only take the years when individuals have a first baby
#check min and max ages at first reproduction
#min
min(real_data4$afr,na.rm=T)
#13
#max
max(real_data4$afr,na.rm=T)
#32

std_wealth_restricted <- std_absw_matrix4[,round(min(real_data4$afr,na.rm=T)):round(max(real_data4$afr,na.rm=T))+1]
afrs_restricted <- afr_matrix4[,round(min(real_data4$afr,na.rm=T)):round(max(real_data4$afr,na.rm=T))+1]


#put all the data together
#create dataset
real_list4 <- list(N = nrow(afrs_restricted), #population size
                   A = ncol(afrs_restricted), #age
                   wealth = as.vector(t(std_wealth_restricted)), #absolute wealth
                   baby = afrs_restricted, #AFR
                   N_miss = sum((std_wealth_restricted)== -99), # number of missing values that need imputation
                   id_wealth_miss =which(as.vector(t(std_wealth_restricted))== -99)) # provide the indexes for the missing data
#check data
real_list4

# compile model

m4_add <- cmdstan_model("Model_code/firstbaby_twowealth_additive.stan")

# fit model

fit4_add_real <- m4_add$sample(data = real_list4, 
                          chains = 4, 
                          parallel_chains = 15, 
                          adapt_delta = 0.95,
                          max_treedepth = 13,
                          init = 0)

# save fit 
fit_4_add_real <- rstan::read_stan_csv(fit4_add_real$output_files())
saveRDS(fit_4_add_real, "firstbaby4_add_real.rds")
#load RDS file
rds4_add_real <- readRDS("firstbaby4_add_real.rds")
#extract samples
post4_add_real <- extract.samples(rds4_add_real)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds4_add_real,pars="alpha")
#mu
#traceplot(rds4_add_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds4_add_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds4_add_real,pars="mu_tau")
#mu_kappa
traceplot(rds4_add_real,pars="mu_kappa")
#mu_delta
traceplot(rds4_add_real,pars="mu_delta")
#beta_wealth
#traceplot(rds4_add_real,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds4_add_real,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab4_add_real <- precis(rds4_add_real,depth=2,pars=c("alpha",
                                           "mu_raw",
                                           "mu_tau",
                                           "mu_delta"))
#check table
tab4_add_real
#create summary table for mu
tab4_add_real_mu <- precis(rds4_add_real,depth=2,pars="mu")
#check table
tab4_add_real_mu
plot(inv_logit(tab4_add_real_mu[,1]))
#create summary table for beta
tab4_add_real_beta <- precis(rds4_add_real,depth=2,pars="beta_wealth")
#check table
tab4_add_real_beta
plot((tab4_add_real_beta[,1]))
#create summary table for gamma
tab4_add_real_gamma <- precis(rds4_add_real,depth=2,pars="gamma_wealth")
#check table
tab4_add_real_gamma
plot((tab4_add_real_gamma[,1]))

## Plot the fit of the simulated data ----

### Absolute wealth ----

### Age ----

#### All ages ----

#simulate wealth values
#absolute wealth
simwealth_add_real_b <- seq(from=round(min(post4_add_real$wealth_full),1),to=round(max(post4_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_b
#wealth variability
simwealth_add_real_g <- seq(from=round(min(post4_add_real$diffwealth),1),to=round(max(post4_add_real$diffwealth),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_add_real$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"zissou 1") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,7))
plot(c(0,1)~c(min(simwealth_add_real_b),max(simwealth_add_real_b)),
     ylab="Probability of first reproduction",
     xlab="Standardised absolute wealth",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.lab=1.5)
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)
legend(3.8,0.75,c(age_quantiles+12),lty=1,col=palette,title="Age")

#add lines
for(k in 1:length(age_quantiles)){
  #create matrix to store the data
  p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_b))
  p4_add_real
  #fill it in with values for age 25
  for(j in 1:length(simwealth_add_real_b)){
    for(i in 1:nrow(post4_add_real$mu)){
      p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                      post4_add_real$mu[i,age_quantiles[k]] + #age
                                      post4_add_real$beta_wealth[i,age_quantiles[k]]*simwealth_add_real_b[j] + #absolute wealth
                                      post4_add_real$gamma_wealth[i,age_quantiles[k]]*0) #wealth variability
    }
  }
  #check data
  p4_add_real
  #plot it!
  #prepare model prediction data
  plot_data4_add_real <- data.frame(wealth = simwealth_add_real_b,
                                    mean = apply(p4_add_real, 2, mean), 
                                    upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                    low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[k])
}

#### De-couple plot by age ----

#simulate wealth values
simwealth_add_real_b <- seq(from=round(min(post4_add_real$wealth_full),1),to=round(max(post4_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_b

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_add_real$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"zissou 1") #darker lines = younger ages, lighter lines = older ages

#define layout of plots
layout( matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow=2, byrow=TRUE) )

#### Age 13 ----

plot(c(0,1)~c(min(simwealth_add_real_b),max(simwealth_add_real_b)),
     ylab="Probability of first reproduction",
     xlab="Standardised absolute wealth",
     main="Age 13",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_b))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_b)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[1]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[1]]*simwealth_add_real_b[j] + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[1]]*0) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_b,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[1])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[1],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[1],lty=2)
points(plot_afr4[,age_quantiles[1]]~plot_data4_add_real$wealth,col=alpha(palette[1],0.25),pch=16)

#### Age 18 ----

plot(c(0,1)~c(min(simwealth_add_real_b),max(simwealth_add_real_b)),
     ylab="Probability of first reproduction",
     xlab="Standardised absolute wealth",
     main="Age 18",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_b))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_b)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[2]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[2]]*simwealth_add_real_b[j] + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[2]]*0) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_b,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[2])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[2],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[2],lty=2)
points(plot_afr4[,age_quantiles[2]]~plot_data4_add_real$wealth,col=alpha(palette[2],0.25),pch=16)

#### Age 23 ----

plot(c(0,1)~c(min(simwealth_add_real_b),max(simwealth_add_real_b)),
     ylab="Probability of first reproduction",
     xlab="Standardised absolute wealth",
     main="Age 23",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_b))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_b)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[3]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[3]]*simwealth_add_real_b[j] + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[3]]*0) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_b,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[3])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[3],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[3],lty=2)
points(plot_afr4[,age_quantiles[3]]~plot_data4_add_real$wealth,col=alpha(palette[3],0.25),pch=16)

#### Age 27 ----

plot(c(0,1)~c(min(simwealth_add_real_b),max(simwealth_add_real_b)),
     ylab="Probability of first reproduction",
     xlab="Standardised absolute wealth",
     main="Age 27",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_b))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_b)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[4]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[4]]*simwealth_add_real_b[j] + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[4]]*0) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_b,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[4])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[4],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[4],lty=2)
points(plot_afr4[,age_quantiles[4]]~plot_data4_add_real$wealth,col=alpha(palette[4],0.25),pch=16)

#### Age 32 ----

plot(c(0,1)~c(min(simwealth_add_real_b),max(simwealth_add_real_b)),
     ylab="Probability of first reproduction",
     xlab="Standardised absolute wealth",
     main="Age 32",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_b))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_b)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[5]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[5]]*simwealth_add_real_b[j] + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[5]]*0) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_b,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[5])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[5],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[5],lty=2)
points(plot_afr4[,age_quantiles[5]]~plot_data4_add_real$wealth,col=alpha(palette[5],0.25),pch=16)

### Wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_add_real_b <- seq(from=round(min(post4_add_real$wealth_full),1),to=round(max(post4_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_b

#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real_b,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"berlin") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,7))
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post4_add_real$mu),by=1),labels=(round(min(real_data4$afr,na.rm=T))-1):round(max(real_data4$afr,na.rm=T)),cex.axis=1.2)
axis(2,cex.axis=1.2)
legend(21,0.75,c("Poor","Middle","Rich"),lty=1,col=palette_b,title="Wealth")

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_add_real_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
  p4_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_add_real$mu)){
    for(i in 1:nrow(post4_add_real$mu)){
      p4_add_real_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                        post4_add_real$mu[i,j] + #age
                                        post4_add_real$beta_wealth[i,j]*deciles[k] + #absolute wealth
                                        post4_add_real$gamma_wealth[i,j]*0) #wealth variability
    }
  }
  #check data
  p4_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data4_add_real_b <- data.frame(age = 1:ncol(p4_add_real_b),
                                      mean = apply(p4_add_real_b, 2, mean), 
                                      upp = apply(p4_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p4_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  points(plot_data4_add_real_b$mean~plot_data4_add_real_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data4_add_real_b$mean~plot_data4_add_real_b$age,col=palette_b[k])
}

#### De-couple plot by wealth quantile ----

#simulate wealth values
simwealth_add_real_b <- seq(from=round(min(post4_add_real$wealth_full),1),to=round(max(post4_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_b

#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real_b,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"berlin") #darker lines = younger ages, lighter lines = older ages

#define layout of plots
par(mfrow=c(1,3))

#### Minimum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Poor",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post4_add_real$mu),by=1),labels=(round(min(real_data4$afr,na.rm=T))-1):round(max(real_data4$afr,na.rm=T)),cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p4_add_real_0_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_0_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_0_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                        post4_add_real$mu[i,j] + #age
                                        post4_add_real$beta_wealth[i,j]*deciles[1] + #absolute wealth
                                        post4_add_real$gamma_wealth[i,j]*0) #wealth variability
  }
}
#check data
p4_add_real_0_b
#plot it!
#prepare model prediction data
plot_data4_add_real_0_b <- data.frame(age = 1:ncol(p4_add_real_0_b),
                                      mean = apply(p4_add_real_0_b, 2, mean), 
                                      upp = apply(p4_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p4_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

points(plot_data4_add_real_0_b$mean~plot_data4_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=15)
lines(plot_data4_add_real_0_b$mean~plot_data4_add_real_0_b$age,col=palette_b[1])
points(plot_data4_add_real_0_b$upp~plot_data4_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=4)
lines(plot_data4_add_real_0_b$upp~plot_data4_add_real_0_b$age,col=palette_b[1],lty=2)
points(plot_data4_add_real_0_b$low~plot_data4_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=4)
lines(plot_data4_add_real_0_b$low~plot_data4_add_real_0_b$age,col=palette_b[1],lty=2)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Medium",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post4_add_real$mu),by=1),labels=(round(min(real_data4$afr,na.rm=T))-1):round(max(real_data4$afr,na.rm=T)),cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p4_add_real_50_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_50_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_50_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                         post4_add_real$mu[i,j] + #age
                                         post4_add_real$beta_wealth[i,j]*deciles[2] + #absolute wealth
                                         post4_add_real$gamma_wealth[i,j]*0) #wealth variability
  }
}
#check data
p4_add_real_50_b
#plot it!
#prepare model prediction data
plot_data4_add_real_50_b <- data.frame(age = 1:ncol(p4_add_real_50_b),
                                       mean = apply(p4_add_real_50_b, 2, mean), 
                                       upp = apply(p4_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p4_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

points(plot_data4_add_real_50_b$mean~plot_data4_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=15)
lines(plot_data4_add_real_50_b$mean~plot_data4_add_real_50_b$age,col=palette_b[2])
points(plot_data4_add_real_50_b$upp~plot_data4_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=4)
lines(plot_data4_add_real_50_b$upp~plot_data4_add_real_50_b$age,col=palette_b[2],lty=2)
points(plot_data4_add_real_50_b$low~plot_data4_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=4)
lines(plot_data4_add_real_50_b$low~plot_data4_add_real_50_b$age,col=palette_b[2],lty=2)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Rich",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post4_add_real$mu),by=1),labels=(round(min(real_data4$afr,na.rm=T))-1):round(max(real_data4$afr,na.rm=T)),cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p4_add_real_100_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_100_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_100_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                          post4_add_real$mu[i,j] + #age
                                          post4_add_real$beta_wealth[i,j]*deciles[3] + #absolute wealth
                                          post4_add_real$gamma_wealth[i,j]*0) #wealth variability
  }
}
#check data
p4_add_real_100_b
#plot it!
#prepare model prediction data
plot_data4_add_real_100_b <- data.frame(age = 1:ncol(p4_add_real_100_b),
                                        mean = apply(p4_add_real_100_b, 2, mean), 
                                        upp = apply(p4_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                        low = apply(p4_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

points(plot_data4_add_real_100_b$mean~plot_data4_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=15)
lines(plot_data4_add_real_100_b$mean~plot_data4_add_real_100_b$age,col=palette_b[3])
points(plot_data4_add_real_100_b$upp~plot_data4_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=4)
lines(plot_data4_add_real_100_b$upp~plot_data4_add_real_100_b$age,col=palette_b[3],lty=2)
points(plot_data4_add_real_100_b$low~plot_data4_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=4)
lines(plot_data4_add_real_100_b$low~plot_data4_add_real_100_b$age,col=palette_b[3],lty=2)


### Wealth variability ----

#### All ages ----

#simulate wealth values
#absolute wealth
simwealth_add_real_b <- seq(from=round(min(post4_add_real$wealth_full),1),to=round(max(post4_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_b
#wealth variability
simwealth_add_real_g <- seq(from=round(min(post4_add_real$diffwealth),1),to=round(max(post4_add_real$diffwealth),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_add_real$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"zissou 1") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,7))
plot(c(0,1)~c(min(simwealth_add_real_g),max(simwealth_add_real_g)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.lab=1.5)
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)
legend(4.3,0.75,c(age_quantiles+12),lty=1,col=palette,title="Age")

#add lines
for(k in 1:length(age_quantiles)){
  #create matrix to store the data
  p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_g))
  p4_add_real
  #fill it in with values for age 25
  for(j in 1:length(simwealth_add_real_g)){
    for(i in 1:nrow(post4_add_real$mu)){
      p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                      post4_add_real$mu[i,age_quantiles[k]] + #age
                                      post4_add_real$beta_wealth[i,age_quantiles[k]]*0 + #absolute wealth
                                      post4_add_real$gamma_wealth[i,age_quantiles[k]]*simwealth_add_real_g[j]) #wealth variability
    }
  }
  #check data
  p4_add_real
  #plot it!
  #prepare model prediction data
  plot_data4_add_real <- data.frame(wealth = simwealth_add_real_g,
                                    mean = apply(p4_add_real, 2, mean), 
                                    upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                    low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[k])
}

#### De-couple plot by age ----

#simulate wealth values
#absolute wealth
simwealth_add_real_b <- seq(from=round(min(post4_add_real$wealth_full),1),to=round(max(post4_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_b
#wealth variability
simwealth_add_real_g <- seq(from=round(min(post4_add_real$diffwealth),1),to=round(max(post4_add_real$diffwealth),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_add_real$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"zissou 1") #darker lines = younger ages, lighter lines = older ages

#define layout of plots
layout( matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow=2, byrow=TRUE) )

#### Age 13 ----

plot(c(0,1)~c(min(simwealth_add_real_g),max(simwealth_add_real_g)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 13",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_g))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_g)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[1]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[1]]*0 + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[1]]*simwealth_add_real_g[j]) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_g,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[1])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[1],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[1],lty=2)
points(plot_afr4[,age_quantiles[1]]~plot_data4_add_real$wealth,col=alpha(palette[1],0.25),pch=16)

#### Age 18 ----

plot(c(0,1)~c(min(simwealth_add_real_g),max(simwealth_add_real_g)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 18",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_g))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_g)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[2]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[2]]*0 + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[2]]*simwealth_add_real_g[j]) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_g,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[2])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[2],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[2],lty=2)
points(plot_afr4[,age_quantiles[2]]~plot_data4_add_real$wealth,col=alpha(palette[2],0.25),pch=16)

#### Age 23 ----

plot(c(0,1)~c(min(simwealth_add_real_g),max(simwealth_add_real_g)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 23",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_g))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_g)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[3]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[3]]*0 + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[3]]*simwealth_add_real_g[j]) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_g,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[3])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[3],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[3],lty=2)
points(plot_afr4[,age_quantiles[3]]~plot_data4_add_real$wealth,col=alpha(palette[3],0.25),pch=16)

#### Age 27 ----

plot(c(0,1)~c(min(simwealth_add_real_g),max(simwealth_add_real_g)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 27",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_g))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_g)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[4]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[4]]*0 + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[4]]*simwealth_add_real_g[j]) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_g,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[4])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[4],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[4],lty=2)
points(plot_afr4[,age_quantiles[4]]~plot_data4_add_real$wealth,col=alpha(palette[4],0.25),pch=16)

#### Age 32 ----

plot(c(0,1)~c(min(simwealth_add_real_g),max(simwealth_add_real_g)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 32",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.3)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p4_add_real <- matrix(nrow=nrow(post4_add_real$mu),ncol=length(simwealth_add_real_g))
p4_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real_g)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                    post4_add_real$mu[i,age_quantiles[5]] + #age
                                    post4_add_real$beta_wealth[i,age_quantiles[5]]*0 + #absolute wealth
                                    post4_add_real$gamma_wealth[i,age_quantiles[5]]*simwealth_add_real_g[j]) #wealth variability
  }
}
#check data
p4_add_real
#plot it!
#prepare model prediction data
plot_data4_add_real <- data.frame(wealth = simwealth_add_real_g,
                                  mean = apply(p4_add_real, 2, mean), 
                                  upp = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

lines(plot_data4_add_real$mean~plot_data4_add_real$wealth,col=palette[5])
lines(plot_data4_add_real$upp~plot_data4_add_real$wealth,col=palette[5],lty=2)
lines(plot_data4_add_real$low~plot_data4_add_real$wealth,col=palette[5],lty=2)
points(plot_afr4[,age_quantiles[5]]~plot_data4_add_real$wealth,col=alpha(palette[5],0.25),pch=16)

### Wealth ----

#### All wealth classes ----

#simulate wealth values
#absolute wealth
simwealth_add_real_b <- seq(from=round(min(post4_add_real$wealth_full),1),to=round(max(post4_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_b
#wealth variability
simwealth_add_real_g <- seq(from=round(min(post4_add_real$diffwealth),1),to=round(max(post4_add_real$diffwealth),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_g

#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real_g,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"berlin") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,7))
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post4_add_real$mu),by=1),labels=(round(min(real_data4$afr,na.rm=T))-1):round(max(real_data4$afr,na.rm=T)),cex.axis=1.2)
axis(2,cex.axis=1.2)
legend(21,0.75,c("Loss","Stable","Gain"),lty=1,col=palette_b,title="Wealth")

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_add_real_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
  p4_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_add_real$mu)){
    for(i in 1:nrow(post4_add_real$mu)){
      p4_add_real_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                        post4_add_real$mu[i,j] + #age
                                        post4_add_real$beta_wealth[i,j]*0 + #absolute wealth
                                        post4_add_real$gamma_wealth[i,j]*deciles[k]) #wealth variability
    }
  }
  #check data
  p4_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data4_add_real_b <- data.frame(age = 1:ncol(p4_add_real_b),
                                      mean = apply(p4_add_real_b, 2, mean), 
                                      upp = apply(p4_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p4_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  points(plot_data4_add_real_b$mean~plot_data4_add_real_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data4_add_real_b$mean~plot_data4_add_real_b$age,col=palette_b[k])
}

#### De-couple plot by wealth quantile ----

#simulate wealth values
#absolute wealth
simwealth_add_real_b <- seq(from=round(min(post4_add_real$wealth_full),1),to=round(max(post4_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_b
#wealth variability
simwealth_add_real_g <- seq(from=round(min(post4_add_real$diffwealth),1),to=round(max(post4_add_real$diffwealth),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real_g

#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real_g,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"berlin") #darker lines = younger ages, lighter lines = older ages

#define layout of plots
par(mfrow=c(1,3))

#### Minimum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Loss",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post4_add_real$mu),by=1),labels=(round(min(real_data4$afr,na.rm=T))-1):round(max(real_data4$afr,na.rm=T)),cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p4_add_real_0_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_0_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_0_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                        post4_add_real$mu[i,j] + #age
                                        post4_add_real$beta_wealth[i,j]*0 + #absolute wealth
                                        post4_add_real$gamma_wealth[i,j]*deciles[1]) #wealth variability
  }
}
#check data
p4_add_real_0_b
#plot it!
#prepare model prediction data
plot_data4_add_real_0_b <- data.frame(age = 1:ncol(p4_add_real_0_b),
                                      mean = apply(p4_add_real_0_b, 2, mean), 
                                      upp = apply(p4_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p4_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

points(plot_data4_add_real_0_b$mean~plot_data4_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=15)
lines(plot_data4_add_real_0_b$mean~plot_data4_add_real_0_b$age,col=palette_b[1])
points(plot_data4_add_real_0_b$upp~plot_data4_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=4)
lines(plot_data4_add_real_0_b$upp~plot_data4_add_real_0_b$age,col=palette_b[1],lty=2)
points(plot_data4_add_real_0_b$low~plot_data4_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=4)
lines(plot_data4_add_real_0_b$low~plot_data4_add_real_0_b$age,col=palette_b[1],lty=2)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Stable",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post4_add_real$mu),by=1),labels=(round(min(real_data4$afr,na.rm=T))-1):round(max(real_data4$afr,na.rm=T)),cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p4_add_real_50_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_50_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_50_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                         post4_add_real$mu[i,j] + #age
                                         post4_add_real$beta_wealth[i,j]*0 + #absolute wealth
                                         post4_add_real$gamma_wealth[i,j]*deciles[2]) #wealth variability
  }
}
#check data
p4_add_real_50_b
#plot it!
#prepare model prediction data
plot_data4_add_real_50_b <- data.frame(age = 1:ncol(p4_add_real_50_b),
                                       mean = apply(p4_add_real_50_b, 2, mean), 
                                       upp = apply(p4_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p4_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

points(plot_data4_add_real_50_b$mean~plot_data4_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=15)
lines(plot_data4_add_real_50_b$mean~plot_data4_add_real_50_b$age,col=palette_b[2])
points(plot_data4_add_real_50_b$upp~plot_data4_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=4)
lines(plot_data4_add_real_50_b$upp~plot_data4_add_real_50_b$age,col=palette_b[2],lty=2)
points(plot_data4_add_real_50_b$low~plot_data4_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=4)
lines(plot_data4_add_real_50_b$low~plot_data4_add_real_50_b$age,col=palette_b[2],lty=2)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Gain",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post4_add_real$mu),by=1),labels=(round(min(real_data4$afr,na.rm=T))-1):round(max(real_data4$afr,na.rm=T)),cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p4_add_real_100_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_100_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_100_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                          post4_add_real$mu[i,j] + #age
                                          post4_add_real$beta_wealth[i,j]*0 + #absolute wealth
                                          post4_add_real$gamma_wealth[i,j]*deciles[3]) #wealth variability
  }
}
#check data
p4_add_real_100_b
#plot it!
#prepare model prediction data
plot_data4_add_real_100_b <- data.frame(age = 1:ncol(p4_add_real_100_b),
                                        mean = apply(p4_add_real_100_b, 2, mean), 
                                        upp = apply(p4_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                        low = apply(p4_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr4)){
  for(i in 1:nrow(plot_afr4)){
    if(plot_afr4[i,j]==-99){
      plot_afr4[i,j] <- NA
    }
  }
}
#check the data
plot_afr4

points(plot_data4_add_real_100_b$mean~plot_data4_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=15)
lines(plot_data4_add_real_100_b$mean~plot_data4_add_real_100_b$age,col=palette_b[3])
points(plot_data4_add_real_100_b$upp~plot_data4_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=4)
lines(plot_data4_add_real_100_b$upp~plot_data4_add_real_100_b$age,col=palette_b[3],lty=2)
points(plot_data4_add_real_100_b$low~plot_data4_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=4)
lines(plot_data4_add_real_100_b$low~plot_data4_add_real_100_b$age,col=palette_b[3],lty=2)


## Fit simulated data, using the combined data imputation approach and multiplicative ----

#put all the data together
#create data
data4 <- list(N = nrow(afrs_restricted), #population size
              A = ncol(afrs_restricted), #age
              wealth = as.vector(t(std_wealth_restricted)), #absolute wealth
              N_miss = sum((std_wealth_restricted)== -99), # number of missing values that need imputation
              id_wealth_miss = which(as.vector(t(std_wealth_restricted))== -99), # provide the indexes for the missing data
              baby = afrs_restricted #AFR
) 

#check data
data4

# compile model

m4_mult <- cmdstan_model("Model_code/firstbaby_twowealth_multiplicative.stan")

# fit model

fit4_mult<- m4_mult$sample(data = data4, 
                           chains = 4, 
                           parallel_chains = 10, 
                           adapt_delta = 0.95,
                           max_treedepth = 13,
                           init = 0)

# save fit 
fit_4_mult <- rstan::read_stan_csv(fit4_mult$output_files())
saveRDS(fit_4_mult, "firstbaby4_mult.rds")
#load RDS file
rds4_mult <- readRDS("firstbaby4.rds")
#extract samples
post4_mult <- extract.samples(rds4_mult)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds4_mult,pars="alpha")
#mu
#traceplot(rds4_mult,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds4_mult,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds4_mult,pars="mu_tau")
#mu_kappa
traceplot(rds4_mult,pars="mu_kappa")
#mu_delta
traceplot(rds4_mult,pars="mu_delta")
#beta_wealth
#traceplot(rds4_mult,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds4_mult,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab4 <- precis(rds4_mult,depth=2,pars=c("alpha",
                                        "mu_raw",
                                        "mu_tau",
                                        "mu_delta"))
#check table
tab4
#create summary table for mu
tab4_mu <- precis(rds4_mult,depth=2,pars="mu")
#check table
tab4_mu
plot(inv_logit(tab4_mu[,1]))
#create summary table for beta
tab4_beta <- precis(rds4_mult,depth=2,pars="beta_wealth")
#check table
tab4_beta
plot((tab4_beta[,1]))
#create summary table for gamma
tab4_gamma <- precis(rds4_mult,depth=2,pars="gamma_wealth")
#check table
tab4_gamma
plot((tab4_gamma[,1]))

## Plot the fit of the simulated data ----

### Absolute wealth ----

### Age ----

#simulate wealth values
simwealth_b <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_b

#simulate wealth variability values
simwealth_g <- seq(from=round(min(diffwealth),1),to=round(max(diffwealth),1),length.out=nrow(diffwealth)) #specify according to range and length related to sample size
simwealth_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_mult$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
plot(c(0,0.4)~c(min(diffwealth),max(diffwealth)),
     ylab="Prob. FR",
     xlab="Std. Absolute wealth",
     main="Model with both wealth predictors",
     type="n")

#add lines
for(k in seq(15,25,by=1)){
  #create matrix to store the data
  p4 <- matrix(nrow=nrow(post4_mult$mu),ncol=length(simwealth_b))
  p4
  #fill it in with values for age 25
  for(j in 1:length(simwealth_b)){
    for(i in 1:nrow(post4_mult$mu)){
      p4_real[i,j] <- inv_logit(post4_mult_real$alpha[i] + #inv logit because originally is logit
                                  post4_mult_real$mu[i,k] + #age
                                  post4_mult_real$beta_wealth[i,k]*simwealth_b[j] + # absolute wealth 
                                  post4_mult_real$gamma_wealth[i,k]*0) #wealth variability / zero because that is the average from the standardisation
    }
  }
  #check data
  p4
  #plot it!
  #prepare model prediction data
  plot_data4_mult_a <- data.frame(wealth = simwealth_b,
                                  mean = apply(p4, 2, mean), 
                                  upp = apply(p4, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  lines(plot_data4_mult_a$mean~plot_data4_mult_a$wealth,col=palette[k])
}

### Wealth ----

#simulate wealth values
simwealth_mult <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_mult
#get the deciles
deciles <- as.numeric(quantile(simwealth_mult,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(0,ncol(post4_mult$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth",
     type="n")
axis(1,at=seq(0,ncol(post4_mult$mu),by=1),labels=14:29)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_mult_b <- matrix(nrow=nrow(post4_mult$mu),ncol=ncol(post4_mult$mu))
  p4_mult_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_mult$mu)){
    for(i in 1:nrow(post4_mult$mu)){
      p4_real[i,j] <- inv_logit(post4_mult_real$alpha[i] + #inv logit because originally is logit
                                  post4_mult_real$mu[i,k] + #age
                                  post4_mult_real$beta_wealth[i,k]*simwealth_b[j] + # absolute wealth 
                                  post4_mult_real$gamma_wealth[i,k]*0) #wealth variability / zero because that is the average from the standardisation
    }
  }
  #check data
  p4_mult_b
  #plot it!
  #prepare model prediction data
  plot_data4_mult_b <- data.frame(age = 1:ncol(p4_mult_b),
                                  mean = apply(p4_mult_b, 2, mean), 
                                  upp = apply(p4_mult_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_mult_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  points(plot_data4_mult_b$mean~plot_data4_mult_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data4_mult_b$mean~plot_data4_mult_b$age,col=palette_b[k])
}

### Wealth variability ----

### Age ----

#simulate wealth values
simwealth_b <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_b

#simulate wealth variability values
simwealth_g <- seq(from=round(min(diffwealth),1),to=round(max(diffwealth),1),length.out=nrow(diffwealth)) #specify according to range and length related to sample size
simwealth_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_mult$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
plot(c(0,0.4)~c(min(diffwealth),max(diffwealth)),
     ylab="Prob. FR",
     xlab="Std. Absolute wealth",
     main="Model with both wealth predictors",
     type="n")

#add lines
for(k in seq(15,25,by=1)){
  #create matrix to store the data
  p4 <- matrix(nrow=nrow(post4_mult$mu),ncol=length(simwealth_b))
  p4
  #fill it in with values for age 25
  for(j in 1:length(simwealth_b)){
    for(i in 1:nrow(post4_mult$mu)){
      p4_real[i,j] <- inv_logit(post4_mult_real$alpha[i] + #inv logit because originally is logit
                                  post4_mult_real$mu[i,k] + #age
                                  post4_mult_real$beta_wealth[i,k]*0 + # absolute wealth / zero because that is the average from the standardisation
                                  post4_mult_real$gamma_wealth[i,k]*simwealth_g[j]) #wealth variability 
    }
  }
  #check data
  p4
  #plot it!
  #prepare model prediction data
  plot_data4_mult_a <- data.frame(wealth = simwealth_b,
                                  mean = apply(p4, 2, mean), 
                                  upp = apply(p4, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  lines(plot_data4_mult_a$mean~plot_data4_mult_a$wealth,col=palette[k])
}

### Wealth ----

#simulate wealth values
simwealth_mult <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_mult
#get the deciles
deciles <- as.numeric(quantile(simwealth_mult,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(0,ncol(post4_mult$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth",
     type="n")
axis(1,at=seq(0,ncol(post4_mult$mu),by=1),labels=14:29)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_mult_b <- matrix(nrow=nrow(post4_mult$mu),ncol=ncol(post4_mult$mu))
  p4_mult_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_mult$mu)){
    for(i in 1:nrow(post4_mult$mu)){
      p4_real[i,j] <- inv_logit(post4_mult_real$alpha[i] + #inv logit because originally is logit
                                  post4_mult_real$mu[i,k] + #age
                                  post4_mult_real$beta_wealth[i,k]*0 + # absolute wealth / zero because that is the average from the standardisation
                                  post4_mult_real$gamma_wealth[i,k]*simwealth_g[j]) #wealth variability 
    }
  }
  #check data
  p4_mult_b
  #plot it!
  #prepare model prediction data
  plot_data4_mult_b <- data.frame(age = 1:ncol(p4_mult_b),
                                  mean = apply(p4_mult_b, 2, mean), 
                                  upp = apply(p4_mult_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p4_mult_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  points(plot_data4_mult_b$mean~plot_data4_mult_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data4_mult_b$mean~plot_data4_mult_b$age,col=palette_b[k])
}

