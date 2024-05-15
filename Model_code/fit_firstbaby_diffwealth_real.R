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
std_absw_matrix3 <- matrix(standardize(log(as.vector(absw_matrix3))),ncol=ncol(absw_matrix3),nrow=nrow(absw_matrix3))
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

std_wealth_restricted <- std_absw_matrix3[,round(min(real_data3$afr,na.rm=T)):round(max(real_data3$afr,na.rm=T))+1]
afrs_restricted <- afr_matrix3[,round(min(real_data3$afr,na.rm=T)):round(max(real_data3$afr,na.rm=T))+1]

#put all the data together
#create dataset
real_list3 <- list(N = nrow(afrs_restricted), #population size
                   A = ncol(afrs_restricted), #age
                   wealth = as.vector(t(std_wealth_restricted)), #absolute wealth
                   baby = afrs_restricted, #AFR
                   N_miss = sum((std_wealth_restricted)== -99), # number of missing values that need imputation
                   id_wealth_miss =which(as.vector(t(std_wealth_restricted))== -99)) # provide the indexes for the missing data
#check data
real_list3

# compile model

m3_add <- cmdstan_model("Model_code/firstbaby_diffwealth_additive.stan")

#fit model
fit3_add_real <- m3_add$sample(data = real_list3, 
                       chains = 4, 
                       parallel_chains = 15, 
                       adapt_delta = 0.95,
                       max_treedepth = 13,
                       init = 0)


# save fit 
fit_3_add_real <- rstan::read_stan_csv(fit3_add_real$output_files())
saveRDS(fit_3_add_real, "firstbaby3_add_real.rds")
#load RDS file
rds3_add_real <- readRDS("firstbaby3_add_real.rds")
#extract samples
post3_add_real <- extract.samples(rds3_add_real)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds3_add_real,pars="alpha")
#mu
#traceplot(rds3_add_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds3_add_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds3_add_real,pars="mu_tau")
#mu_kappa
traceplot(rds3_add_real,pars="mu_kappa")
#mu_delta
traceplot(rds3_add_real,pars="mu_delta")
#gamma_wealth
#traceplot(rds3_add_real,pars="gamma_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds3_add_real,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab3_add_real <- precis(rds3_add_real,depth=2,pars=c("alpha",
                                             "mu_raw",
                                             "mu_tau",
                                             "mu_delta"))
#check table
tab3_add_real
#create summary table for mu
tab3_add_real_mu <- precis(rds3_add_real,depth=2,pars="mu")
#check table
tab3_add_real_mu
#create summary table for gamma
tab3_add_real_gamma <- precis(rds3_add_real,depth=2,pars="gamma_wealth")
#check table
tab3_add_real_gamma

# To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
#mu
tab3_add_real_mu[,1]<-round(inv_logit(tab3_add_real_mu[,1]),3)
tab3_add_real_mu[,3]<-round(inv_logit(tab3_add_real_mu[,3]),3)
tab3_add_real_mu[,4]<-round(inv_logit(tab3_add_real_mu[,4]),3)
#gamma_wealth
tab3_add_real_gamma[,1]<-round(inv_logit(tab3_add_real_gamma[,1]),3)
tab3_add_real_gamma[,3]<-round(inv_logit(tab3_add_real_gamma[,3]),3)
tab3_add_real_gamma[,4]<-round(inv_logit(tab3_add_real_gamma[,4]),3)

## Plot the fit of the real data ----

### Age ----

#### All ages ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post3_add_real$diffwealth),1),to=round(max(post3_add_real$diffwealth),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post3_add_real$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"zissou 1") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,7))
plot(c(0,1)~c(min(simwealth_add_real),max(simwealth_add_real)),
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
  p3_add_real <- matrix(nrow=nrow(post3_add_real$mu),ncol=length(simwealth_add_real))
  p3_add_real
  #fill it in with values for age 25
  for(j in 1:length(simwealth_add_real)){
    for(i in 1:nrow(post3_add_real$mu)){
      p3_add_real[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                      post3_add_real$mu[i,age_quantiles[k]] + #age
                                      post3_add_real$gamma_wealth[i,age_quantiles[k]]*simwealth_add_real[j]) #wealth
    }
  }
  #check data
  p3_add_real
  #plot it!
  #prepare model prediction data
  plot_post3_add_real <- data.frame(wealth = simwealth_add_real,
                                    mean = apply(p3_add_real, 2, mean), 
                                    upp = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                    low = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr3 <- afrs_restricted
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
  
  lines(plot_post3_add_real$mean~plot_post3_add_real$wealth,col=palette[k])
}

#### De-couple plot by age ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post3_add_real$diffwealth),1),to=round(max(post3_add_real$diffwealth),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post3_add_real$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"zissou 1") #darker lines = younger ages, lighter lines = older ages

#define layout of plots
layout( matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow=2, byrow=TRUE) )

#### Age 13 ----

plot(c(0,1)~c(min(simwealth_add_real),max(simwealth_add_real)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 13",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p3_add_real <- matrix(nrow=nrow(post3_add_real$mu),ncol=length(simwealth_add_real))
p3_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                    post3_add_real$mu[i,age_quantiles[1]+1] + #age
                                    post3_add_real$gamma_wealth[i,age_quantiles[1]]*simwealth_add_real[j]) #wealth
  }
}
#check data
p3_add_real
#plot it!
#prepare model prediction data
plot_post3_add_real <- data.frame(wealth = simwealth_add_real,
                                  mean = apply(p3_add_real, 2, mean), 
                                  upp = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afrs_restricted
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

lines(plot_post3_add_real$mean~plot_post3_add_real$wealth,col=palette[1])
lines(plot_post3_add_real$upp~plot_post3_add_real$wealth,col=palette[1],lty=2)
lines(plot_post3_add_real$low~plot_post3_add_real$wealth,col=palette[1],lty=2)
points(plot_afr3[,age_quantiles[1]]~plot_post3_add_real$wealth,col=alpha(palette[1],0.25),pch=16)

#### Age 18 ----

plot(c(0,1)~c(min(simwealth_add_real),max(simwealth_add_real)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 18",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p3_add_real <- matrix(nrow=nrow(post3_add_real$mu),ncol=length(simwealth_add_real))
p3_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                    post3_add_real$mu[i,age_quantiles[2]] + #age
                                    post3_add_real$gamma_wealth[i,age_quantiles[2]]*simwealth_add_real[j]) #wealth
  }
}
#check data
p3_add_real
#plot it!
#prepare model prediction data
plot_post3_add_real <- data.frame(wealth = simwealth_add_real,
                                  mean = apply(p3_add_real, 2, mean), 
                                  upp = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afrs_restricted
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

lines(plot_post3_add_real$mean~plot_post3_add_real$wealth,col=palette[2])
lines(plot_post3_add_real$upp~plot_post3_add_real$wealth,col=palette[2],lty=2)
lines(plot_post3_add_real$low~plot_post3_add_real$wealth,col=palette[2],lty=2)
points(plot_afr3[,age_quantiles[2]]~plot_post3_add_real$wealth,col=alpha(palette[2],0.25),pch=16)

#### Age 23 ----

plot(c(0,1)~c(min(simwealth_add_real),max(simwealth_add_real)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 23",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p3_add_real <- matrix(nrow=nrow(post3_add_real$mu),ncol=length(simwealth_add_real))
p3_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                    post3_add_real$mu[i,age_quantiles[3]] + #age
                                    post3_add_real$gamma_wealth[i,age_quantiles[3]]*simwealth_add_real[j]) #wealth
  }
}
#check data
p3_add_real
#plot it!
#prepare model prediction data
plot_post3_add_real <- data.frame(wealth = simwealth_add_real,
                                  mean = apply(p3_add_real, 2, mean), 
                                  upp = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afrs_restricted
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

lines(plot_post3_add_real$mean~plot_post3_add_real$wealth,col=palette[3])
lines(plot_post3_add_real$upp~plot_post3_add_real$wealth,col=palette[3],lty=2)
lines(plot_post3_add_real$low~plot_post3_add_real$wealth,col=palette[3],lty=2)
points(plot_afr3[,age_quantiles[3]]~plot_post3_add_real$wealth,col=alpha(palette[3],0.25),pch=16)

#### Age 27 ----

plot(c(0,1)~c(min(simwealth_add_real),max(simwealth_add_real)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 27",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p3_add_real <- matrix(nrow=nrow(post3_add_real$mu),ncol=length(simwealth_add_real))
p3_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                    post3_add_real$mu[i,age_quantiles[4]] + #age
                                    post3_add_real$gamma_wealth[i,age_quantiles[4]]*simwealth_add_real[j]) #wealth
  }
}
#check data
p3_add_real
#plot it!
#prepare model prediction data
plot_post3_add_real <- data.frame(wealth = simwealth_add_real,
                                  mean = apply(p3_add_real, 2, mean), 
                                  upp = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afrs_restricted
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

lines(plot_post3_add_real$mean~plot_post3_add_real$wealth,col=palette[4])
lines(plot_post3_add_real$upp~plot_post3_add_real$wealth,col=palette[4],lty=2)
lines(plot_post3_add_real$low~plot_post3_add_real$wealth,col=palette[4],lty=2)
points(plot_afr3[,age_quantiles[4]]~plot_post3_add_real$wealth,col=alpha(palette[4],0.25),pch=16)

#### Age 32 ----

plot(c(0,1)~c(min(simwealth_add_real),max(simwealth_add_real)),
     ylab="Probability of first reproduction",
     xlab="Std. short-term variability",
     main="Age 32",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)

#create matrix to store the data
p3_add_real <- matrix(nrow=nrow(post3_add_real$mu),ncol=length(simwealth_add_real))
p3_add_real
#fill it in with values for age 25
for(j in 1:length(simwealth_add_real)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                    post3_add_real$mu[i,age_quantiles[5]] + #age
                                    post3_add_real$gamma_wealth[i,age_quantiles[5]]*simwealth_add_real[j]) #wealth
  }
}
#check data
p3_add_real
#plot it!
#prepare model prediction data
plot_post3_add_real <- data.frame(wealth = simwealth_add_real,
                                  mean = apply(p3_add_real, 2, mean), 
                                  upp = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p3_add_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afrs_restricted
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

lines(plot_post3_add_real$mean~plot_post3_add_real$wealth,col=palette[5])
lines(plot_post3_add_real$upp~plot_post3_add_real$wealth,col=palette[5],lty=2)
lines(plot_post3_add_real$low~plot_post3_add_real$wealth,col=palette[5],lty=2)
points(plot_afr3[,age_quantiles[5]]~plot_post3_add_real$wealth,col=alpha(palette[5],0.25),pch=16)

### Wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post3_add_real$diffwealth),1),to=round(max(post3_add_real$diffwealth),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real

#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"berlin") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,7))
plot(c(0,1)~c(0,ncol(post3_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post3_add_real$mu),by=1),labels=(round(min(real_data3$afr,na.rm=T))-1):round(max(real_data3$afr,na.rm=T)),cex.axis=1.5)
axis(2,cex.axis=1.5)
legend(21,0.75,c("Loss","Stable","Gain"),lty=1,col=palette_b,title="Wealth")

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p3_add_real_b <- matrix(nrow=nrow(post3_add_real$mu),ncol=ncol(post3_add_real$mu))
  p3_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post3_add_real$mu)){
    for(i in 1:nrow(post3_add_real$mu)){
      p3_add_real_b[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                        post3_add_real$mu[i,j] + #age
                                        post3_add_real$gamma_wealth[i,j]*deciles[k]) #wealth
    }
  }
  #check data
  p3_add_real_b
  #plot it!
  #prepare model prediction data
  plot_post3_add_real_b <- data.frame(age = 1:ncol(p3_add_real_b),
                                      mean = apply(p3_add_real_b, 2, mean), 
                                      upp = apply(p3_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p3_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
  
  points(plot_post3_add_real_b$mean~plot_post3_add_real_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_post3_add_real_b$mean~plot_post3_add_real_b$age,col=palette_b[k])
}

#### De-couple plot by wealth quantile ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post3_add_real$wealth_full),1),to=round(max(post3_add_real$wealth_full),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length related to sample size
simwealth_add_real

#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"berlin") #darker lines = younger ages, lighter lines = older ages

#define layout of plots
par(mfrow=c(1,3))

#### Minimum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post3_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Loss",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post3_add_real$mu),by=1),labels=(round(min(real_data3$afr,na.rm=T))-1):round(max(real_data3$afr,na.rm=T)),cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p3_add_real_0_b <- matrix(nrow=nrow(post3_add_real$mu),ncol=ncol(post3_add_real$mu))
p3_add_real_0_b
#fill it in with values for age 25
for(j in 1:ncol(post3_add_real$mu)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real_0_b[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                        post3_add_real$mu[i,j] + #age
                                        post3_add_real$gamma_wealth[i,j]*deciles[1]) #wealth
  }
}
#check data
p3_add_real_0_b
#plot it!
#prepare model prediction data
plot_post3_add_real_0_b <- data.frame(age = 1:ncol(p3_add_real_0_b),
                                      mean = apply(p3_add_real_0_b, 2, mean), 
                                      upp = apply(p3_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p3_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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

points(plot_post3_add_real_0_b$mean~plot_post3_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=15)
lines(plot_post3_add_real_0_b$mean~plot_post3_add_real_0_b$age,col=palette_b[1])
points(plot_post3_add_real_0_b$upp~plot_post3_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=4)
lines(plot_post3_add_real_0_b$upp~plot_post3_add_real_0_b$age,col=palette_b[1],lty=2)
points(plot_post3_add_real_0_b$low~plot_post3_add_real_0_b$age,col=alpha(palette_b[1],0.75),pch=4)
lines(plot_post3_add_real_0_b$low~plot_post3_add_real_0_b$age,col=palette_b[1],lty=2)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post3_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Stable",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post3_add_real$mu),by=1),labels=(round(min(real_data3$afr,na.rm=T))-1):round(max(real_data3$afr,na.rm=T)),cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p3_add_real_50_b <- matrix(nrow=nrow(post3_add_real$mu),ncol=ncol(post3_add_real$mu))
p3_add_real_50_b
#fill it in with values for age 25
for(j in 1:ncol(post3_add_real$mu)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real_50_b[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                         post3_add_real$mu[i,j] + #age
                                         post3_add_real$gamma_wealth[i,j]*deciles[2]) #wealth
  }
}
#check data
p3_add_real_50_b
#plot it!
#prepare model prediction data
plot_post3_add_real_50_b <- data.frame(age = 1:ncol(p3_add_real_50_b),
                                       mean = apply(p3_add_real_50_b, 2, mean), 
                                       upp = apply(p3_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p3_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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

points(plot_post3_add_real_50_b$mean~plot_post3_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=15)
lines(plot_post3_add_real_50_b$mean~plot_post3_add_real_50_b$age,col=palette_b[2])
points(plot_post3_add_real_50_b$upp~plot_post3_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=4)
lines(plot_post3_add_real_50_b$upp~plot_post3_add_real_50_b$age,col=palette_b[2],lty=2)
points(plot_post3_add_real_50_b$low~plot_post3_add_real_50_b$age,col=alpha(palette_b[2],0.75),pch=4)
lines(plot_post3_add_real_50_b$low~plot_post3_add_real_50_b$age,col=palette_b[2],lty=2)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post3_add_real$mu)),
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Gain",
     xaxt="n",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(1,at=seq(0,ncol(post3_add_real$mu),by=1),labels=(round(min(real_data3$afr,na.rm=T))-1):round(max(real_data3$afr,na.rm=T)),cex.axis=1.5)
axis(2,cex.axis=1.5)

#create matrix to store the data
p3_add_real_100_b <- matrix(nrow=nrow(post3_add_real$mu),ncol=ncol(post3_add_real$mu))
p3_add_real_100_b
#fill it in with values for age 25
for(j in 1:ncol(post3_add_real$mu)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real_100_b[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                          post3_add_real$mu[i,j] + #age
                                          post3_add_real$gamma_wealth[i,j]*deciles[3]) #wealth
  }
}
#check data
p3_add_real_100_b
#plot it!
#prepare model prediction data
plot_post3_add_real_100_b <- data.frame(age = 1:ncol(p3_add_real_100_b),
                                        mean = apply(p3_add_real_100_b, 2, mean), 
                                        upp = apply(p3_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                        low = apply(p3_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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

points(plot_post3_add_real_100_b$mean~plot_post3_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=15)
lines(plot_post3_add_real_100_b$mean~plot_post3_add_real_100_b$age,col=palette_b[3])
points(plot_post3_add_real_100_b$upp~plot_post3_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=4)
lines(plot_post3_add_real_100_b$upp~plot_post3_add_real_100_b$age,col=palette_b[3],lty=2)
points(plot_post3_add_real_100_b$low~plot_post3_add_real_100_b$age,col=alpha(palette_b[3],0.75),pch=4)
lines(plot_post3_add_real_100_b$low~plot_post3_add_real_100_b$age,col=palette_b[3],lty=2)

