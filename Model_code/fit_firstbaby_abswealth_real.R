# Model with absolute wealth ----

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
real_data2 <- read.csv("dataf.csv")[,-1]
head(real_data2)

# Age at first reproduction 

#create a matrix to store the age-specific age of censor
afr_matrix2 <- matrix(nrow=nrow(real_data2),ncol=A+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix2)){
  afr <- real_data2$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data2$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix2[i,1:(afr-1)] <- 0
    afr_matrix2[i,afr] <- 1
    afr_matrix2[i,(afr+1):aoc] <- 0
  } else{
    afr_matrix2[i,1:aoc] <- rep(0,length(afr_matrix2[i,1:aoc]))
  }
}
#check the data
afr_matrix2
#check the age-specific probability of FR
apply(afr_matrix2,2,sum,na.rm=T)/sum(apply(afr_matrix2,2,sum,na.rm=T))
#plot it
plot(apply(afr_matrix2,2,sum,na.rm=T)/sum(apply(afr_matrix2,2,sum,na.rm=T))~c(1:ncol(afr_matrix2)),xlab="Age",ylab="Probability of first reproduction",ylim=c(0,0.2))

#replace NAs with -99
for(j in 1:ncol(afr_matrix2)){
  for(i in 1:nrow(afr_matrix2)){
    if(is.na(afr_matrix2[i,j])){
      afr_matrix2[i,j] <- -99
    } else{
      afr_matrix2[i,j] <- afr_matrix2[i,j]
    }
  }
}
#check the data
afr_matrix2

#Age-specific absolute wealth

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix2 <- matrix(nrow = nrow(real_data2),ncol=A+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw95[i]
  age_absw <- real_data2$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#98
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw98[i]
  age_absw <- real_data2$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#00
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw00[i]
  age_absw <- real_data2$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#02
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw02[i]
  age_absw <- real_data2$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#04
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw04[i]
  age_absw <- real_data2$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#06
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw06[i]
  age_absw <- real_data2$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#10
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw10[i]
  age_absw <- real_data2$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#check the age-specific average of absolute wealth
apply(absw_matrix2,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix2,2,mean,na.rm=T)~c(1:ncol(absw_matrix2)),xlab="Age",ylab="Average absolute wealth")

# #NaN in columns where there are no values of wealth
# 
# # Simple data imputation 
# 
# #replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
# for(i in 1:length(absw_matrix2[,1])){
#   if(is.na(absw_matrix2[i,1]) & is.na(absw_matrix2[i,2])){
#     absw_matrix2[i,1] <- mean(absw_matrix2[,1],na.rm = T)
#   }else if(is.na(absw_matrix2[i,1]) & !is.na(absw_matrix2[i,2])){
#     absw_matrix2[i,1] <- absw_matrix2[i,2]
#   }
# }
# #check the data
# absw_matrix2
# sum(is.na(absw_matrix2[,1]))
# #n=0
# #replace the missing wealth data by putting the mean between the two ages it is
# for(j in 2:(ncol(absw_matrix2)-1)){
#   for(i in 1:nrow(absw_matrix2)){
#     if(is.na(absw_matrix2[i,j])&!is.na(absw_matrix2[i,j-1])&!is.na(absw_matrix2[i,j+1])){
#       absw_matrix2[i,j] <- mean(c(absw_matrix2[i,j-1],absw_matrix2[i,j+1]))
#     } else if(is.na(absw_matrix2[i,j])&!is.na(absw_matrix2[i,j-1])&is.na(absw_matrix2[i,j+1])){
#       absw_matrix2[i,j] <- mean(absw_matrix2[,j],na.rm=T)
#     } 
#   }
# }
# #check the data
# absw_matrix2
# sum(is.na(absw_matrix2))
# #n=21824
# #replace the missing wealth data by putting the mean between the two ages it is
# for(j in 2:(ncol(absw_matrix2)-1)){
#   for(i in 1:nrow(absw_matrix2)){
#     if(is.na(absw_matrix2[i,j])&!is.na(absw_matrix2[i,j-1])&!is.na(absw_matrix2[i,j+1])){
#       absw_matrix2[i,j] <- mean(c(absw_matrix2[i,j-1],absw_matrix2[i,j+1]))
#     } else if(is.na(absw_matrix2[i,j])&!is.na(absw_matrix2[i,j-1])&is.na(absw_matrix2[i,j+1])){
#       absw_matrix2[i,j] <- mean(absw_matrix2[,j],na.rm=T)
#     } 
#   }
# }
# #check the data
# absw_matrix2
# sum(is.na(absw_matrix2))
# #n=9179
# #replace the missing wealth data by putting either the mean between the two ages it is or by repeating the value from previous year
# for(j in 2:(ncol(absw_matrix2)-1)){
#   for(i in 1:nrow(absw_matrix2)){
#     if(is.na(absw_matrix2[i,j])&length(is.na(absw_matrix2[i,j:91]))!=sum(is.na(absw_matrix2[i,j:91]))){
#       absw_matrix2[i,j] <- mean(c(absw_matrix2[i,j-1],absw_matrix2[i,max(which(!is.na(absw_matrix2[i,])==T))]))
#     } else if(is.na(absw_matrix2[i,j])&length(is.na(absw_matrix2[i,j:91]))==sum(is.na(absw_matrix2[i,j:91]))){
#       absw_matrix2[i,j] <- absw_matrix2[i,j-1]
#     }
#   }
# }
# #check the data
# absw_matrix2
# sum(is.na(absw_matrix2))
# #n=540
# #replace last column with the values from last year
# absw_matrix2[,max(ncol(absw_matrix2))] <- absw_matrix2[,max(ncol(absw_matrix2))-1]
# #check the data
# absw_matrix2
# sum(is.na(absw_matrix2))
# #n=0
# #check the age-specific frequency of absolute wealth
# apply(absw_matrix2,2,mean)
# #plot it
# plot(apply(absw_matrix2,2,mean)~c(1:91),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix2 <- matrix(standardize(as.vector(absw_matrix2)),ncol=ncol(absw_matrix2),nrow=nrow(absw_matrix2))
#check the data
std_absw_matrix2
#check the age-specific average of absolute wealth
apply(std_absw_matrix2,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix2,2,mean,na.rm=T)~c(1:(A+1)),xlab="Age",ylab="Average absolute wealth")

#change NAs for -99
#replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
for(j in 1:ncol(std_absw_matrix2)){
  for(i in 1:nrow(std_absw_matrix2)){
    if(is.na(std_absw_matrix2[i,j])){
      std_absw_matrix2[i,j] <- -99
    }
  }
}
#check the data
std_absw_matrix2

## Additive model: fit real data ----

# Only take the years when individuals have a first baby
#check min and max ages at first reproduction
#min
min(real_data2$afr,na.rm=T)
#13
#max
max(real_data2$afr,na.rm=T)
#32

std_wealth_restricted <- std_absw_matrix2[,min(real_data2$afr,na.rm=T):max(real_data2$afr,na.rm=T)]
afrs_restricted <- afr_matrix2[,min(real_data2$afr,na.rm=T):max(real_data2$afr,na.rm=T)]

#put all the data together
#create dataset
real_list2 <- list(N = nrow(afrs_restricted), #population size
                   A = ncol(afrs_restricted), #age
                   wealth = as.vector(t(std_wealth_restricted)), #absolute wealth
                   baby = afrs_restricted, #AFR
                   miss = sum((std_wealth_restricted)== -99), # number of missing values that need imputation
                   wealth_m=which(as.vector(t(std_wealth_restricted))== -99)) # provide the indexes for the missing data
#check data
real_list2

# compile model
m2_add <- cmdstan_model("Model_code/firstbaby_abswealth_additive.stan")

# fit model
fit2_add_real <- m2_addv$sample(data = real_list2, 
                            chains = 4, 
                            parallel_chains = 15, 
                            adapt_delta = 0.95,
                            max_treedepth = 13,
                            init = 0)

# save fit 
fit_2_add_real <- rstan::read_stan_csv(fit2_add_real$output_files())
saveRDS(fit_2_add_real, "firstbaby2_add_real.rds")
#load RDS file
rds2_add_real <- readRDS("firstbaby2_add_real.rds")
#extract samples
post2_add_real <- extract.samples(rds2_add_real)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds2_add_real,pars="alpha")
#mu
#traceplot(rds2_add_real,pars="mu") #only run if needed, because they are 92 plots
#mu_raw
#traceplot(rds2_add_real,pars="mu_raw") #only run if needed, because they are 92 plots
#mu_tau
traceplot(rds2_add_real,pars="mu_tau")
#mu_kappa
traceplot(rds2_add_real,pars="mu_kappa")
#mu_delta
traceplot(rds2_add_real,pars="mu_delta")
#beta_wealth
#traceplot(rds2_add_real,pars="beta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab2_add_real <- precis(rds2_add_real,depth=3,pars=c("alpha",
                                             "mu_raw",
                                             "mu_tau",
                                             "mu_delta"))
#check table
tab2_add_real
#create summary table for mu
tab2_mu_add_real <- precis(rds2_add_real,depth=3,pars="mu")
#check table
tab2_mu_add_real
plot(inv_logit(tabs2_mu_add_real[,1]))
#create summary table for beta
tab2_beta_add_real <- precis(rds2_add_real,depth=3,pars="beta_wealth")
#check table
tab2_beta_add_real
plot((tabs2_beta_add_real[,1]))

# To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
#mu
tab2_mu_add_real[,1]<-round(inv_logit(tab2_mu_add_real[,1]),3)
tab2_mu_add_real[,3]<-round(inv_logit(tab2_mu_add_real[,3]),3)
tab2_mu_add_real[,4]<-round(inv_logit(tab2_mu_add_real[,4]),3)
#beta_wealth
tab2_beta_add_real[,1]<-round(inv_logit(tab2_beta_add_real[,1]),3)
tab2_beta_add_real[,3]<-round(inv_logit(tab2_beta_add_real[,3]),3)
tab2_beta_add_real[,4]<-round(inv_logit(tab2_beta_add_real[,4]),3)

## Plot the fit of the real data ----

### Age ----

#### All ages ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(real_list2$wealth[which(real_list2$wealth>-99)]),1),to=round(max(real_list2$wealth[which(real_list2$wealth>-99)]),1),length.out=nrow(real_list2$wealth)) #specify according to range and length related to sample size
simwealth_add_real

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post2_add_real$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth",
     type="n")

#add lines
for(k in 1:length(age_quantiles)){
  #create matrix to store the data
  p2_add_real <- matrix(nrow=nrow(post2_add_real$mu),ncol=length(simwealth_add_real))
  p2_add_real
  #fill it in with values for age 25
  for(j in 1:length(simwealth_add_real)){
    for(i in 1:nrow(post2_add_real$mu)){
      p2_add_real[i,j] <- inv_logit(post2_add_real$alpha[i] + #inv logit because originally is logit
                                  post2_add_real$mu[i,age_quantiles[k]] + #age
                                  post2_add_real$beta_wealth[i,age_quantiles[k]]*simwealth_add_real[j]) #wealth
    }
  }
  #check data
  p2_add_real
  #plot it!
  #prepare model prediction data
  plot_data2_add_real <- data.frame(wealth = simwealth_add_real,
                                mean = apply(p2_add_real, 2, mean), 
                                upp = apply(p2_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_add_real$mean~plot_data2_add_real$wealth,col=palette[k])
}

#plot one plot per age between 13 and 31

#### Age 13 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 13",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,1] + #age
                                post2_real$beta_wealth[i,1]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[1])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[1],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[1],lty=2)
points(plot_afr2[,1]~plot_data2_real$wealth,col=alpha(palette[1],0.5),pch=16)

#### Age 14 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 14",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,2] + #age
                                post2_real$beta_wealth[i,2]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[2])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[2],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[2],lty=2)
points(plot_afr2[,2]~plot_data2_real$wealth,col=alpha(palette[2],0.5),pch=16)

#### Age 15 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 15",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,3] + #age
                                post2_real$beta_wealth[i,3]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[3])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[3],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[3],lty=2)
points(plot_afr2[,3]~plot_data2_real$wealth,col=alpha(palette[3],0.5),pch=16)

#### Age 16 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 16",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,4] + #age
                                post2_real$beta_wealth[i,4]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[4])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[4],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[4],lty=2)
points(plot_afr2[,4]~plot_data2_real$wealth,col=alpha(palette[4],0.5),pch=16)

#### Age 17 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 17",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,5] + #age
                                post2_real$beta_wealth[i,5]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[5])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[5],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[5],lty=2)
points(plot_afr2[,5]~plot_data2_real$wealth,col=alpha(palette[5],0.5),pch=16)

#### Age 18 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 18",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,6] + #age
                                post2_real$beta_wealth[i,6]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[6])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[6],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[6],lty=2)
points(plot_afr2[,6]~plot_data2_real$wealth,col=alpha(palette[6],0.5),pch=16)

#### Age 19 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 19",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,7] + #age
                                post2_real$beta_wealth[i,7]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[7])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[7],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[7],lty=2)
points(plot_afr2[,7]~plot_data2_real$wealth,col=alpha(palette[7],0.5),pch=16)

#### Age 20 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 20",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,8] + #age
                                post2_real$beta_wealth[i,8]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[8])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[8],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[8],lty=2)
points(plot_afr2[,8]~plot_data2_real$wealth,col=alpha(palette[8],0.5),pch=16)

#### Age 21 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 21",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,9] + #age
                                post2_real$beta_wealth[i,9]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[9])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[9],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[9],lty=2)
points(plot_afr2[,9]~plot_data2_real$wealth,col=alpha(palette[9],0.5),pch=16)

#### Age 22 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 22",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,10] + #age
                                post2_real$beta_wealth[i,10]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[10])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[10],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[10],lty=2)
points(plot_afr2[,10]~plot_data2_real$wealth,col=alpha(palette[10],0.5),pch=16)

#### Age 23 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 23",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,11] + #age
                                post2_real$beta_wealth[i,11]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[11])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[11],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[11],lty=2)
points(plot_afr2[,11]~plot_data2_real$wealth,col=alpha(palette[11],0.5),pch=16)

#### Age 24 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 24",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,12] + #age
                                post2_real$beta_wealth[i,12]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[12])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[12],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[12],lty=2)
points(plot_afr2[,12]~plot_data2_real$wealth,col=alpha(palette[12],0.5),pch=16)

#### Age 25 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 25",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,13] + #age
                                post2_real$beta_wealth[i,13]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[13])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[13],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[13],lty=2)
points(plot_afr2[,13]~plot_data2_real$wealth,col=alpha(palette[13],0.5),pch=16)

#### Age 26 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 26",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,14] + #age
                                post2_real$beta_wealth[i,14]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[14])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[14],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[14],lty=2)
points(plot_afr2[,14]~plot_data2_real$wealth,col=alpha(palette[14],0.5),pch=16)

#### Age 27 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 27",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,15] + #age
                                post2_real$beta_wealth[i,15]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[15])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[15],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[15],lty=2)
points(plot_afr2[,15]~plot_data2_real$wealth,col=alpha(palette[15],0.5),pch=16)

#### Age 28 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 28",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,16] + #age
                                post2_real$beta_wealth[i,16]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[16])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[16],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[16],lty=2)
points(plot_afr2[,16]~plot_data2_real$wealth,col=alpha(palette[16],0.5),pch=16)

#### Age 29 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 29",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,17] + #age
                                post2_real$beta_wealth[i,17]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[17])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[17],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[17],lty=2)
points(plot_afr2[,17]~plot_data2_real$wealth,col=alpha(palette[17],0.5),pch=16)

#### Age 30 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 30",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,18] + #age
                                post2_real$beta_wealth[i,18]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[18])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[18],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[18],lty=2)
points(plot_afr2[,18]~plot_data2_real$wealth,col=alpha(palette[18],0.5),pch=16)

#### Age 31 ----
plot(c(0,1)~c(min(real_list2$wealth[which(real_list2$wealth>-99)]),max(real_list2$wealth[which(real_list2$wealth>-99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth: Age 31",
     type="n")
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 13
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                post2_real$mu[i,19] + #age
                                post2_real$beta_wealth[i,19]*simwealth_real[j]) #wealth
  }
}
#check data
p2_real
#plot it!
#prepare model prediction data
plot_data2_real <- data.frame(wealth = simwealth_real,
                              mean = apply(p2_real, 2, mean), 
                              upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afrs_restricted
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[19])
lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[19],lty=2)
lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[19],lty=2)
points(plot_afr2[,19]~plot_data2_real$wealth,col=alpha(palette[19],0.5),pch=16)

### Wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(real_list2$wealth[which(real_list2$wealth>-99)]),1),to=round(max(real_list2$wealth[which(real_list2$wealth>-99)]),1),length.out=nrow(real_list2$wealth)) #specify according to range and length related to sample size
simwealth_add_real

#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real,seq(0,1,0.1)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_add_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
#     xaxt="n",
     main="Model with absolute wealth",
     type="n")
#axis(1,at=seq(0,ncol(post2_add_real$mu),by=1),labels=min(which(apply(afrs,2,sum)>0)):(max(which(apply(afrs,2,sum)>0))+1))

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p2_add_real_b <- matrix(nrow=nrow(post2_add_real$mu),ncol=ncol(post2_add_real$mu))
  p2_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post2_add_real$mu)){
    for(i in 1:nrow(post2_add_real$mu)){
      p2_add_real_b[i,j] <- inv_logit(post2_add_real$alpha[i] + #inv logit because originally is logit
                                    post2_add_real$mu[i,j] + #age
                                    post2_add_real$beta_wealth[i,j]*deciles[k]) #wealth
    }
  }
  #check data
  p2_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data2_add_real_b <- data.frame(age = 1:ncol(p2_add_real_b),
                                  mean = apply(p2_add_real_b, 2, mean), 
                                  upp = apply(p2_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p2_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  points(plot_data2_add_real_b$mean~plot_data2_add_real_b$age,col=alpha(palette_b[k],0.75),pch=15)
}

#plot one plot per wealth quantile

#define the layout
par(mfrow=c(1,3))

#### Quantile 0% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 0%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_0 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_0
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_0[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[1]) #wealth
  }
}
#check data
p2_real_0
#plot it!
#prepare model prediction data
plot_data2_0 <- data.frame(age = 1:ncol(p2_real_0),
                            mean = apply(p2_real_0, 2, mean), 
                            upp = apply(p2_real_0, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_0, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_0$mean~plot_data2_0$age,col=palette_b[1],pch=16)
points(plot_data2_0$upp~plot_data2_0$age,col=palette_b[1],pch=4)
points(plot_data2_0$low~plot_data2_0$age,col=palette_b[1],pch=4)

#### Quantile 10% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 10%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_10 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_10
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_10[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[2]) #wealth
  }
}
#check data
p2_real_10
#plot it!
#prepare model prediction data
plot_data2_10 <- data.frame(age = 1:ncol(p2_real_10),
                              mean = apply(p2_real_10, 2, mean), 
                              upp = apply(p2_real_10, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real_10, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_10$mean~plot_data2_10$age,col=palette_b[2],pch=16)
points(plot_data2_10$upp~plot_data2_10$age,col=palette_b[2],pch=4)
points(plot_data2_10$low~plot_data2_10$age,col=palette_b[2],pch=4)

#### Quantile 20% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 20%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_20 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_20
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_20[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[3]) #wealth
  }
}
#check data
p2_real_20
#plot it!
#prepare model prediction data
plot_data2_20 <- data.frame(age = 1:ncol(p2_real_20),
                              mean = apply(p2_real_20, 2, mean), 
                              upp = apply(p2_real_20, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p2_real_20, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_20$mean~plot_data2_20$age,col=palette_b[3],pch=16)
points(plot_data2_20$upp~plot_data2_20$age,col=palette_b[3],pch=4)
points(plot_data2_20$low~plot_data2_20$age,col=palette_b[3],pch=4)

#### Quantile 30% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 30%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_30 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_30
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_30[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[3]) #wealth
  }
}
#check data
p2_real_30
#plot it!
#prepare model prediction data
plot_data2_30 <- data.frame(age = 1:ncol(p2_real_30),
                            mean = apply(p2_real_30, 2, mean), 
                            upp = apply(p2_real_30, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_30, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_30$mean~plot_data2_30$age,col=palette_b[4],pch=16)
points(plot_data2_30$upp~plot_data2_30$age,col=palette_b[4],pch=4)
points(plot_data2_30$low~plot_data2_30$age,col=palette_b[4],pch=4)

#### Quantile 40% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 40%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_40 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_40
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_40[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[4]) #wealth
  }
}
#check data
p2_real_40
#plot it!
#prepare model prediction data
plot_data2_40 <- data.frame(age = 1:ncol(p2_real_40),
                            mean = apply(p2_real_40, 2, mean), 
                            upp = apply(p2_real_40, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_40, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_40$mean~plot_data2_40$age,col=palette_b[5],pch=16)
points(plot_data2_40$upp~plot_data2_40$age,col=palette_b[5],pch=4)
points(plot_data2_40$low~plot_data2_40$age,col=palette_b[5],pch=4)


#### Quantile 50% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 50%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_50 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_50
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_50[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[5]) #wealth
  }
}
#check data
p2_real_50
#plot it!
#prepare model prediction data
plot_data2_50 <- data.frame(age = 1:ncol(p2_real_50),
                            mean = apply(p2_real_50, 2, mean), 
                            upp = apply(p2_real_50, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_50, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_50$mean~plot_data2_50$age,col=palette_b[6],pch=16)
points(plot_data2_50$upp~plot_data2_50$age,col=palette_b[6],pch=4)
points(plot_data2_50$low~plot_data2_50$age,col=palette_b[6],pch=4)

#### Quantile 60% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 60%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_60 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_60
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_60[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[6]) #wealth
  }
}
#check data
p2_real_60
#plot it!
#prepare model prediction data
plot_data2_60 <- data.frame(age = 1:ncol(p2_real_60),
                            mean = apply(p2_real_60, 2, mean), 
                            upp = apply(p2_real_60, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_60, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:36]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_60$mean~plot_data2_60$age,col=palette_b[7],pch=16)
points(plot_data2_60$upp~plot_data2_60$age,col=palette_b[7],pch=4)
points(plot_data2_60$low~plot_data2_60$age,col=palette_b[7],pch=4)

#### Quantile 70% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 70%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_70 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_70
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_70[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[7]) #wealth
  }
}
#check data
p2_real_70
#plot it!
#prepare model prediction data
plot_data2_70 <- data.frame(age = 1:ncol(p2_real_70),
                            mean = apply(p2_real_70, 2, mean), 
                            upp = apply(p2_real_70, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_70, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:37]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_70$mean~plot_data2_70$age,col=palette_b[8],pch=16)
points(plot_data2_70$upp~plot_data2_70$age,col=palette_b[8],pch=4)
points(plot_data2_70$low~plot_data2_70$age,col=palette_b[8],pch=4)

#### Quantile 80% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 80%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_80 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_80
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_80[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[8]) #wealth
  }
}
#check data
p2_real_80
#plot it!
#prepare model prediction data
plot_data2_80 <- data.frame(age = 1:ncol(p2_real_80),
                            mean = apply(p2_real_80, 2, mean), 
                            upp = apply(p2_real_80, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_80, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:38]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_80$mean~plot_data2_80$age,col=palette_b[9],pch=16)
points(plot_data2_80$upp~plot_data2_80$age,col=palette_b[9],pch=4)
points(plot_data2_80$low~plot_data2_80$age,col=palette_b[9],pch=4)

#### Quantile 90% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 90%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_90 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_90
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_90[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[9]) #wealth
  }
}
#check data
p2_real_90
#plot it!
#prepare model prediction data
plot_data2_90 <- data.frame(age = 1:ncol(p2_real_90),
                            mean = apply(p2_real_90, 2, mean), 
                            upp = apply(p2_real_90, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_90, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:39]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_90$mean~plot_data2_90$age,col=palette_b[10],pch=16)
points(plot_data2_90$upp~plot_data2_90$age,col=palette_b[10],pch=4)
points(plot_data2_90$low~plot_data2_90$age,col=palette_b[10],pch=4)

#### Quantile 100% ----

par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_real$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth: decile 100%",
     type="n")
axis(1,at=seq(0,ncol(post2_real$mu),by=1),labels=12:31)
#create matrix to store the data
p2_real_100 <- matrix(nrow=nrow(post2_real$mu),ncol=ncol(post2_real$mu))
p2_real_100
#fill it in with values for quantile 25%
for(j in 1:ncol(post2_real$mu)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real_100[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                   post2_real$mu[i,j] + #age
                                   post2_real$beta_wealth[i,j]*deciles[9]) #wealth
  }
}
#check data
p2_real_100
#plot it!
#prepare model prediction data
plot_data2_100 <- data.frame(age = 1:ncol(p2_real_100),
                            mean = apply(p2_real_100, 2, mean), 
                            upp = apply(p2_real_100, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                            low = apply(p2_real_100, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr2 <- afr_matrix2[,1:39]
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

points(plot_data2_100$mean~plot_data2_100$age,col=palette_b[11],pch=16)
points(plot_data2_100$upp~plot_data2_100$age,col=palette_b[11],pch=4)
points(plot_data2_100$low~plot_data2_100$age,col=palette_b[11],pch=4)
