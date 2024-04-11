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
real_data3 <- read.csv("dataf.csv")[,-1]
head(real_data3)

# Age at first reproduction 

#create a matrix to store the age-specific age of censor
afr_matrix3 <- matrix(nrow=nrow(real_data3),ncol=A+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix3)){
  afr <- real_data3$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data3$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix3[i,1:(afr-1)] <- 0
    afr_matrix3[i,afr] <- 1
    afr_matrix3[i,(afr+1):aoc] <- 0
  } else{
    afr_matrix3[i,1:aoc] <- rep(0,length(afr_matrix3[i,1:aoc]))
  }
}
#check the data
afr_matrix3
#check the age-specific probability of FR
apply(afr_matrix3,2,sum,na.rm=T)/sum(apply(afr_matrix3,2,sum,na.rm=T))
#plot it
plot(apply(afr_matrix3,2,sum,na.rm=T)/sum(apply(afr_matrix3,2,sum,na.rm=T))~c(1:ncol(afr_matrix3)),xlab="Age",ylab="Probability of first reproduction",ylim=c(0,0.2))

#replace NAs with -99
for(j in 1:ncol(afr_matrix3)){
  for(i in 1:nrow(afr_matrix3)){
    if(is.na(afr_matrix3[i,j])){
      afr_matrix3[i,j] <- -99
    } else{
      afr_matrix3[i,j] <- afr_matrix3[i,j]
    }
  }
}
#check the data
afr_matrix3

#Age-specific absolute wealth

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix3 <- matrix(nrow = nrow(real_data3),ncol=A+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw95[i]
  age_absw <- real_data3$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
  } else{
    absw_matrix3[i,age_absw] <- NA
  }
}
#check data
absw_matrix3
#98
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw98[i]
  age_absw <- real_data3$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
  } else{
    absw_matrix3[i,age_absw] <- NA
  }
}
#check data
absw_matrix3
#00
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw00[i]
  age_absw <- real_data3$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
  } else{
    absw_matrix3[i,age_absw] <- NA
  }
}
#check data
absw_matrix3
#02
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw02[i]
  age_absw <- real_data3$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
  } else{
    absw_matrix3[i,age_absw] <- NA
  }
}
#check data
absw_matrix3
#04
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw04[i]
  age_absw <- real_data3$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
  } else{
    absw_matrix3[i,age_absw] <- NA
  }
}
#check data
absw_matrix3
#06
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw06[i]
  age_absw <- real_data3$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
  } else{
    absw_matrix3[i,age_absw] <- NA
  }
}
#check data
absw_matrix3
#10
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw10[i]
  age_absw <- real_data3$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
  } else{
    absw_matrix3[i,age_absw] <- NA
  }
}
#check data
absw_matrix3
#check the age-specific average of absolute wealth
apply(absw_matrix3,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix3,2,mean,na.rm=T)~c(1:ncol(absw_matrix3)),xlab="Age",ylab="Average absolute wealth")

# #NaN in columns where there are no values of wealth
# 
# # Simple data imputation 
# 
# #replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
# for(i in 1:length(absw_matrix3[,1])){
#   if(is.na(absw_matrix3[i,1]) & is.na(absw_matrix3[i,2])){
#     absw_matrix3[i,1] <- mean(absw_matrix3[,1],na.rm = T)
#   }else if(is.na(absw_matrix3[i,1]) & !is.na(absw_matrix3[i,2])){
#     absw_matrix3[i,1] <- absw_matrix3[i,2]
#   }
# }
# #check the data
# absw_matrix3
# sum(is.na(absw_matrix3[,1]))
# #n=0
# #replace the missing wealth data by putting the mean between the two ages it is
# for(j in 2:(ncol(absw_matrix3)-1)){
#   for(i in 1:nrow(absw_matrix3)){
#     if(is.na(absw_matrix3[i,j])&!is.na(absw_matrix3[i,j-1])&!is.na(absw_matrix3[i,j+1])){
#       absw_matrix3[i,j] <- mean(c(absw_matrix3[i,j-1],absw_matrix3[i,j+1]))
#     } else if(is.na(absw_matrix3[i,j])&!is.na(absw_matrix3[i,j-1])&is.na(absw_matrix3[i,j+1])){
#       absw_matrix3[i,j] <- mean(absw_matrix3[,j],na.rm=T)
#     } 
#   }
# }
# #check the data
# absw_matrix3
# sum(is.na(absw_matrix3))
# #n=21824
# #replace the missing wealth data by putting the mean between the two ages it is
# for(j in 2:(ncol(absw_matrix3)-1)){
#   for(i in 1:nrow(absw_matrix3)){
#     if(is.na(absw_matrix3[i,j])&!is.na(absw_matrix3[i,j-1])&!is.na(absw_matrix3[i,j+1])){
#       absw_matrix3[i,j] <- mean(c(absw_matrix3[i,j-1],absw_matrix3[i,j+1]))
#     } else if(is.na(absw_matrix3[i,j])&!is.na(absw_matrix3[i,j-1])&is.na(absw_matrix3[i,j+1])){
#       absw_matrix3[i,j] <- mean(absw_matrix3[,j],na.rm=T)
#     } 
#   }
# }
# #check the data
# absw_matrix3
# sum(is.na(absw_matrix3))
# #n=9179
# #replace the missing wealth data by putting either the mean between the two ages it is or by repeating the value from previous year
# for(j in 2:(ncol(absw_matrix3)-1)){
#   for(i in 1:nrow(absw_matrix3)){
#     if(is.na(absw_matrix3[i,j])&length(is.na(absw_matrix3[i,j:91]))!=sum(is.na(absw_matrix3[i,j:91]))){
#       absw_matrix3[i,j] <- mean(c(absw_matrix3[i,j-1],absw_matrix3[i,max(which(!is.na(absw_matrix3[i,])==T))]))
#     } else if(is.na(absw_matrix3[i,j])&length(is.na(absw_matrix3[i,j:91]))==sum(is.na(absw_matrix3[i,j:91]))){
#       absw_matrix3[i,j] <- absw_matrix3[i,j-1]
#     }
#   }
# }
# #check the data
# absw_matrix3
# sum(is.na(absw_matrix3))
# #n=540
# #replace last column with the values from last year
# absw_matrix3[,max(ncol(absw_matrix3))] <- absw_matrix3[,max(ncol(absw_matrix3))-1]
# #check the data
# absw_matrix3
# sum(is.na(absw_matrix3))
# #n=0
# #check the age-specific frequency of absolute wealth
# apply(absw_matrix3,2,mean)
# #plot it
# plot(apply(absw_matrix3,2,mean)~c(1:91),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix3 <- matrix(standardize(as.vector(absw_matrix3)),ncol=ncol(absw_matrix3),nrow=nrow(absw_matrix3))
#check the data
std_absw_matrix3
#check the age-specific average of absolute wealth
apply(std_absw_matrix3,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix3,2,mean,na.rm=T)~c(1:(A+1)),xlab="Age",ylab="Average absolute wealth")

#change NAs for -99
#replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
for(j in 1:ncol(std_absw_matrix3)){
  for(i in 1:nrow(std_absw_matrix3)){
    if(is.na(std_absw_matrix3[i,j])){
      std_absw_matrix3[i,j] <- -99
    }
  }
}
#check the data
std_absw_matrix3

## Additive model: fit real data ----

# Only take the years when individuals have a first baby
#check min and max ages at first reproduction
#min
min(real_data3$afr,na.rm=T)
#13
#max
max(real_data3$afr,na.rm=T)
#32

std_wealth_restricted <- std_absw_matrix3[,min(real_data3$afr,na.rm=T):max(real_data3$afr,na.rm=T)]
afrs_restricted <- afr_matrix3[,min(real_data3$afr,na.rm=T):max(real_data3$afr,na.rm=T)]

#put all the data together
#create dataset
real_list3 <- list(N = nrow(afrs_restricted), #population size
                   A = ncol(afrs_restricted), #age
                   wealth = as.vector(t(std_wealth_restricted)), #absolute wealth
                   baby = afrs_restricted, #AFR
                   miss = sum((std_wealth_restricted)== -99), # number of missing values that need imputation
                   wealth_m=which(as.vector(t(std_wealth_restricted))== -99)) # provide the indexes for the missing data
#check data
real_list3

fit3_real <- m3$sample(data = real_list3, 
                       chains = 4, 
                       parallel_chains = 15, 
                       adapt_delta = 0.95,
                       max_treedepth = 13,
                       init = 0)


# save fit 
fit_3_real <- rstan::read_stan_csv(fit3_real$output_files())
saveRDS(fit_3_real, "firstbaby3_real.rds")
#load RDS file
rds3_real <- readRDS("firstbaby3_real.rds")
#extract samples
post3_real <- extract.samples(rds3_real)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds3_real,pars="alpha")
#mu
#traceplot(rds3_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds3_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds3_real,pars="mu_tau")
#mu_kappa
traceplot(rds3_real,pars="mu_kappa")
#mu_delta
traceplot(rds3_real,pars="mu_delta")
#beta_wealth
#traceplot(rds3_real,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds3_real,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab3_real <- precis(rds3_real,depth=2,pars=c("alpha",
                                             "mu_raw",
                                             "mu_tau",
                                             "mu_delta"))
#check table
tab3_real
#create summary table for mu
tab3_real_mu <- precis(rds3_real,depth=2,pars="mu")
#check table
tab3_real_mu
#create summary table for beta
tab3_real_beta <- precis(rds3_real,depth=2,pars="beta_wealth")
#check table
tab3_real_beta
#create summary table for gamma
tab3_real_gamma <- precis(rds3_real,depth=2,pars="gamma_wealth")
#check table
tab3_real_gamma

# To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
#mu
tab3_real_mu[,1]<-round(inv_logit(tab3_real_mu[,1]),3)
tab3_real_mu[,3]<-round(inv_logit(tab3_real_mu[,3]),3)
tab3_real_mu[,4]<-round(inv_logit(tab3_real_mu[,4]),3)
#beta_wealth
tab3_real_beta[,1]<-round(inv_logit(tab3_real_beta[,1]),3)
tab3_real_beta[,3]<-round(inv_logit(tab3_real_beta[,3]),3)
tab3_real_beta[,4]<-round(inv_logit(tab3_real_beta[,4]),3)
#gamma_wealth
tab3_real_gamma[,1]<-round(inv_logit(tab3_real_gamma[,1]),3)
tab3_real_gamma[,3]<-round(inv_logit(tab3_real_gamma[,3]),3)
tab3_real_gamma[,4]<-round(inv_logit(tab3_real_gamma[,4]),3)

## Plot the fit of the real data ----

### All ages ----

#simulate wealth variability values
range(real_list3$diffwealth) #use the range of values of real data as reference for simulation
simwealth_g_real <- seq(from=round(min(range(real_list3$diffwealth)),1),to=round(max(range(real_list3$diffwealth)),1),length.out=nrow(diffw_matrix3)) #specify according to range and length according to sample size
simwealth_g_real

#colour palette
palette<-c(rep("NA",14),palette(gray(seq(0,.7,length.out = 11)))) #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(min(simwealth_g_real),max(simwealth_g_real)),
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability",
     type="n")
#add lines
for(k in seq(15,25,by=2)){
  #create matrix to store the data
  p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
  p3_real
  #fill it in with values for age 35
  for(j in 1:length(simwealth_g_real)){
    for(i in 1:nrow(post3_real$mu)){
      p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                  post3_real$mu[i,k] +
                                  post3_real$beta_wealth[i,k]*0 + #zero because that is the average from the standardization
                                  post3_real$gamma_wealth[i,k]*simwealth_g[j])
    }
  }
  #check data
  p3_real
  #plot it!
  #prepare model prediction data
  plot_data3_real <- data.frame(wealth = simwealth_g_real,
                                mean = apply(p3_real, 2, mean), 
                                upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr3 <- afr_matrix3
  #change -99 to NAs
  for(j in 1:ncol(plot_afr3)){
    for(i in 1:nrow(plot_afr3)){
      if(plot_afr3[i,j]==-99){
        plot_afr3[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr3
  
  lines(plot_data3_real$mean~plot_data3_real$wealth,col=palette[k])
}

#plot one plot per age between 15 and 25

#define the layout
par(mfrow=c(2,3))

### Age 15 ----
plot(c(0,1)~c(min(simwealth_g_real),max(simwealth_g_real)),
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability: Age 15",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 15
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,15] +
                                post3_real$beta_wealth[i,15]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,15]*simwealth_g[j])
  }
}
#check data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=palette[15])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=palette[15],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=palette[15],lty=2)
points(plot_afr3[,15]~plot_data3_real$wealth,col=alpha(palette[15],0.5),pch=16)

### Age 17 ----
plot(c(0,1)~c(min(simwealth_g_real),max(simwealth_g_real)),
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability: Age 17",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 17
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,17] +
                                post3_real$beta_wealth[i,17]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,17]*simwealth_g[j])
  }
}
#check data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=palette[17])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=palette[17],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=palette[17],lty=2)
points(plot_afr3[,17]~plot_data3_real$wealth,col=alpha(palette[17],0.5),pch=16)

### Age 19 ----
plot(c(0,1)~c(min(simwealth_g_real),max(simwealth_g_real)),
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability: Age 19",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 19
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,19] +
                                post3_real$beta_wealth[i,19]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,19]*simwealth_g[j])
  }
}
#check data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=palette[19])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=palette[19],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=palette[19],lty=2)
points(plot_afr3[,19]~plot_data3_real$wealth,col=alpha(palette[19],0.5),pch=16)

### Age 21 ----
plot(c(0,1)~c(min(simwealth_g_real),max(simwealth_g_real)),
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability: Age 21",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 21
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,21] +
                                post3_real$beta_wealth[i,21]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,21]*simwealth_g[j])
  }
}
#check data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=palette[21])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=palette[21],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=palette[21],lty=2)
points(plot_afr3[,21]~plot_data3_real$wealth,col=alpha(palette[21],0.5),pch=16)

### Age 23 ----
plot(c(0,1)~c(min(simwealth_g_real),max(simwealth_g_real)),
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability: Age 23",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 23
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,23] +
                                post3_real$beta_wealth[i,23]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,23]*simwealth_g[j])
  }
}
#check data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=palette[23])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=palette[23],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=palette[23],lty=2)
points(plot_afr3[,23]~plot_data3_real$wealth,col=alpha(palette[23],0.5),pch=16)

### Age 25 ----
plot(c(0,1)~c(min(simwealth_g_real),max(simwealth_g_real)),
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability: Age 25",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 25
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,25] +
                                post3_real$beta_wealth[i,25]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,25]*simwealth_g[j])
  }
}
#check data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=palette[25])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=palette[25],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=palette[25],lty=2)
points(plot_afr3[,25]~plot_data3_real$wealth,col=alpha(palette[25],0.5),pch=16)

### Wealth ----

#### All wealth classes ----

#simulate wealth values
range(real_list3$diffwealth) #use the range of values of real data as reference for simulation
simwealth_g_real <- seq(from=round(min(range(real_list3$diffwealth)),1),to=round(max(range(real_list3$diffwealth)),1),length.out=nrow(std_absw_matrix3)) #specify according to range and length related to sample size
simwealth_g_real
#get the quantiles
quantiles <- as.numeric(quantile(simwealth_g_real,seq(0,1,0.25)))
#0= -21.1  25%= -11.725  50%=-2.35 75%=7.025 100%=16.4

#colour palette
palette_b<- c(NA,palette(gray(seq(0,0.9,length.out = (length(quantiles)-2))))) #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,1)~c(0,35),
     ylab="Prob. FR",
     xlab="Age",
     main="Model with wealth variability",
     type="n")

#add lines
for(k in 2:(length(quantiles)-1)){
  #create matrix to store the data
  p3_real_b <- matrix(nrow=nrow(post3_real$mu),ncol=ncol(post3_real$mu))
  p3_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post3_real$mu)){
    for(i in 1:nrow(post3_real$mu)){
      p3_real_b[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                    post3_real$mu[i,j] + #age
                                    post3_real$beta_wealth[i,j]*quantiles[k]) #wealth
    }
  }
  #check data
  p3_real_b
  #plot it!
  #prepare model prediction data
  plot_data3_real_b <- data.frame(age = 1:ncol(p3_real_b),
                                  mean = apply(p3_real_b, 2, mean), 
                                  upp = apply(p3_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p3_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr3 <- afr_matrix3
  #change -99 to NAs
  for(j in 1:ncol(plot_afr3)){
    for(i in 1:nrow(plot_afr3)){
      if(plot_afr3[i,j]==-99){
        plot_afr3[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr3
  
  lines(plot_data3_real_b$mean~plot_data3_real_b$age,col=palette_b[k])
}

#plot one plot per wealth quantile

#define the layout
par(mfrow=c(1,3))

#### Quantile 25% ----

#simulate wealth values
range(real_list3$diffwealth) #use the range of values of real data as reference for simulation
simwealth_g_real <- seq(from=round(min(range(real_list3$diffwealth)),1),to=round(max(range(real_list3$diffwealth)),1),length.out=nrow(std_absw_matrix3)) #specify according to range and length related to sample size
simwealth_g_real

#colour palette
palette_b<- c(NA,palette(gray(seq(0,0.5,length.out = (length(quantiles)-2))))) #darker lines = younger ages, lighter lines = older ages

plot(c(0,1)~c(0,35),
     ylab="Prob. FR",
     xlab="Age",
     main="Model with wealth variability: Quantile 25%",
     type="n")
#create matrix to store the data
p3_real_25 <- matrix(nrow=nrow(post3_real$mu),ncol=ncol(post3_real$mu))
p3_real_25
#fill it in with values for quantile 25%
for(j in 1:ncol(post3_real$mu)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real_25[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                   post3_real$mu[i,j] + #age
                                   post3_real$beta_wealth[i,j]*quantiles[2]) #wealth
  }
}
#check data
p3_real_25
#plot it!
#prepare model prediction data
plot_data3_poor <- data.frame(age = 1:ncol(p3_real_25),
                              mean = apply(p3_real_25, 2, mean), 
                              upp = apply(p3_real_25, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real_25, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix3[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_poor$mean~plot_data3_poor$age,col=palette_b[2])
lines(plot_data3_poor$upp~plot_data3_poor$age,col=palette_b[2],lty=2)
lines(plot_data3_poor$low~plot_data3_poor$age,col=palette_b[2],lty=2)
#points(apply(plot_afr3,2,sum,na.rm=T)/sum(is.na(real_data3$afr)==F)~plot_data3_poor$age,col="gold",pch=16)

#### Quantile 50% ----

#simulate wealth values
range(real_list3$diffwealth) #use the range of values of real data as reference for simulation
simwealth_g_real <- seq(from=round(min(range(real_list3$diffwealth)),1),to=round(max(range(real_list3$diffwealth)),1),length.out=nrow(std_absw_matrix3)) #specify according to range and length related to sample size
simwealth_g_real

#colour palette
palette_b<- c(NA,palette(gray(seq(0,0.5,length.out = (length(quantiles)-2))))) #darker lines = younger ages, lighter lines = older ages

plot(c(0,1)~c(0,35),
     ylab="Prob. FR",
     xlab="Age",
     main="Model with wealth variability: Quantile 50%",
     type="n")
#create matrix to store the data
p3_real_50 <- matrix(nrow=nrow(post3_real$mu),ncol=ncol(post3_real$mu))
p3_real_50
#fill it in with values for quantile 25%
for(j in 1:ncol(post3_real$mu)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real_50[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                   post3_real$mu[i,j] + #age
                                   post3_real$beta_wealth[i,j]*quantiles[3]) #wealth
  }
}
#check data
p3_real_50
#plot it!
#prepare model prediction data
plot_data3_medium <- data.frame(age = 1:ncol(p3_real_50),
                                mean = apply(p3_real_50, 2, mean), 
                                upp = apply(p3_real_50, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p3_real_50, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix3[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_medium$mean~plot_data3_medium$age,col=palette_b[3])
lines(plot_data3_medium$upp~plot_data3_medium$age,col=palette_b[3],lty=2)
lines(plot_data3_medium$low~plot_data3_medium$age,col=palette_b[3],lty=2)
#  points(apply(plot_afr3,2,sum,na.rm=T)/sum(is.na(real_data3$afr)==F)~plot_data3_medium$age,col="gold",pch=16)

#### Quantile 75% ----

#simulate wealth values
range(real_list3$diffwealth) #use the range of values of real data as reference for simulation
simwealth_g_real <- seq(from=round(min(range(real_list3$diffwealth)),1),to=round(max(range(real_list3$diffwealth)),1),length.out=nrow(std_absw_matrix3)) #specify according to range and length related to sample size
simwealth_g_real

#colour palette
palette_b<- c(NA,palette(gray(seq(0,0.5,length.out = (length(quantiles)-2))))) #darker lines = younger ages, lighter lines = older ages

plot(c(0,1)~c(0,35),
     ylab="Prob. FR",
     xlab="Age",
     main="Model with wealth variability: Quantile 50%",
     type="n")
#create matrix to store the data
p3_real_75 <- matrix(nrow=nrow(post3_real$mu),ncol=ncol(post3_real$mu))
p3_real_75
#fill it in with values for quantile 75%
for(j in 1:ncol(post3_real$mu)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real_75[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                   post3_real$mu[i,j] + #age
                                   post3_real$beta_wealth[i,j]*quantiles[4]) #wealth
  }
}
#check data
p3_real_75
#plot it!
#prepare model prediction data
plot_data3_high <- data.frame(age = 1:ncol(p3_real_75),
                              mean = apply(p3_real_75, 2, mean), 
                              upp = apply(p3_real_75, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real_75, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix3[,1:35]
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

lines(plot_data3_high$mean~plot_data3_high$age,col=palette_b[3])
lines(plot_data3_high$upp~plot_data3_high$age,col=palette_b[3],lty=2)
lines(plot_data3_high$low~plot_data3_high$age,col=palette_b[3],lty=2)
#  points(apply(plot_afr3,2,sum,na.rm=T)/sum(is.na(real_data3$afr)==F)~plot_data3_high$age,col="gold",pch=16)

