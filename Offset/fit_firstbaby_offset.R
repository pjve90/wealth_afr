# Model with absolute levels, wealth change, and moving variance ----

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
#install.packages("scales")
library(scales)

## Data wrangling of real data ----

#Load data
real_data6 <- read.csv("Data/dataf.csv")[,-1]
head(real_data6)

# Age at first reproduction ----

#create a matrix to store the age-specific age of censor
afr_matrix6 <- matrix(nrow=nrow(real_data6),ncol=max(real_data6$aoc)+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix6)){
  afr <- real_data6$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data6$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix6[i,1:(afr-1)] <- 0
    afr_matrix6[i,afr] <- 1
  } else{
    afr_matrix6[i,1:aoc] <- rep(0,length(afr_matrix6[i,1:aoc]))
  }
}
#check the data
afr_matrix6
#check the age-specific probability of FR
apply(afr_matrix6,2,sum,na.rm=T)/apply(afr_matrix6,2,function(x)sum(!is.na(x)))
#plot it
plot(cumprod(1-apply(afr_matrix6,2,sum,na.rm=T)/apply(afr_matrix6,2,function(x)sum(!is.na(x))))~c(1:(max(real_data6$aoc)+1)),xlab="Age",ylab="Cumulative probability of first birth",ylim=c(0,1))

#Age-specific absolute wealth ----

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix6 <- matrix(nrow = nrow(real_data6),ncol=max(real_data6$aoc)+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix6)){
  absw <- real_data6$absw95[i]
  age_absw <- real_data6$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data6$aoc[i]+1)){
    absw_matrix6[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data6$aoc[i]+1)){
      absw_matrix6[i,(real_data6$aoc[i]+1)] <- NA
    } else{
      absw_matrix6[i,age_absw] <- NA
    }
}
#check data
absw_matrix6
#98
for(i in 1:nrow(absw_matrix6)){
  absw <- real_data6$absw98[i]
  age_absw <- real_data6$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data6$aoc[i]){
    absw_matrix6[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data6$aoc[i]+1)){
      absw_matrix6[i,(real_data6$aoc[i]+1)] <- NA
    } else{
      absw_matrix6[i,age_absw] <- NA
    }
}
#check data
absw_matrix6
#00
for(i in 1:nrow(absw_matrix6)){
  absw <- real_data6$absw00[i]
  age_absw <- real_data6$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data6$aoc[i]){
    absw_matrix6[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data6$aoc[i]+1)){
      absw_matrix6[i,(real_data6$aoc[i]+1)] <- NA
    } else{
      absw_matrix6[i,age_absw] <- NA
    }
}
#check data
absw_matrix6
#02
for(i in 1:nrow(absw_matrix6)){
  absw <- real_data6$absw02[i]
  age_absw <- real_data6$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data6$aoc[i]){
    absw_matrix6[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data6$aoc[i]+1)){
      absw_matrix6[i,(real_data6$aoc[i]+1)] <- NA
    } else{
      absw_matrix6[i,age_absw] <- NA
    }
}
#check data
absw_matrix6
#04
for(i in 1:nrow(absw_matrix6)){
  absw <- real_data6$absw04[i]
  age_absw <- real_data6$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data6$aoc[i]){
    absw_matrix6[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data6$aoc[i]+1)){
      absw_matrix6[i,(real_data6$aoc[i]+1)] <- NA
    } else{
      absw_matrix6[i,age_absw] <- NA
    }
}
#check data
absw_matrix6
#06
for(i in 1:nrow(absw_matrix6)){
  absw <- real_data6$absw06[i]
  age_absw <- real_data6$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data6$aoc[i]){
    absw_matrix6[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data6$aoc[i]+1)){
      absw_matrix6[i,(real_data6$aoc[i]+1)] <- NA
    } else{
      absw_matrix6[i,age_absw] <- NA
    }
}
#check data
absw_matrix6
#10
for(i in 1:nrow(absw_matrix6)){
  absw <- real_data6$absw10[i]
  age_absw <- real_data6$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data6$aoc[i]+1)){
    absw_matrix6[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data6$aoc[i]+1)){
      absw_matrix6[i,(real_data6$aoc[i]+1)] <- NA
    } else{
      absw_matrix6[i,age_absw] <- NA
    }
}
#check data
absw_matrix6
#check the age-specific average of absolute wealth
apply(absw_matrix6,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix6,2,mean,na.rm=T)~c(1:ncol(absw_matrix6)),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix6 <- matrix(standardize(log(as.vector(absw_matrix6))),ncol=ncol(absw_matrix6),nrow=nrow(absw_matrix6))
#check the data
std_absw_matrix6
#check the age-specific average of absolute wealth
apply(std_absw_matrix6,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix6,2,mean,na.rm=T)~c(1:(max(real_data6$aoc)+1)),xlab="Age",ylab="Average absolute wealth")
hist(std_absw_matrix6)

# Fit real data ----

##Prepare data ----

#Age at first birth
#replace NAs with -99
for(j in 1:ncol(afr_matrix6)){
  for(i in 1:nrow(afr_matrix6)){
    if(is.na(afr_matrix6[i,j])){
      afr_matrix6[i,j] <- -99
    } else{
      afr_matrix6[i,j] <- afr_matrix6[i,j]
    }
  }
}
#check the data
afr_matrix6

#Wealth
#matrix identifying missing wealth data
wealth_miss6 <- which(is.na(std_absw_matrix6),arr.ind = T)
#check data
wealth_miss6

#replace NAs with -99
for(j in 1:ncol(std_absw_matrix6)){
  for(i in 1:nrow(std_absw_matrix6)){
    if(is.na(std_absw_matrix6[i,j])){
      std_absw_matrix6[i,j] <- -99
    } else{
      std_absw_matrix6[i,j] <- std_absw_matrix6[i,j]
    }
  }
}
#check the data
std_absw_matrix6

#Subset the data for realistic ages
#Subset wealth and AFB for those between zero years old and 50 years old.
#wealth
std_absw_restricted_b <- std_absw_matrix6[,1:51] #Adding 1, since first column in the matrix is year 0
#AFB
afrs_restricted_b <- afr_matrix6[,1:51] #Adding 1, since first column in the matrix is year 0
afrs_restricted_b[,1:10] <- -99 #turning the first 10 years to NAs because we do not need to model such ages for age at first birth
afrs_restricted_b
#missing wealth data
wealth_miss_restricted_b <- wealth_miss6[wealth_miss6[,2] <= 51,] #Adding 1, since first column in the matrix is year 0

#put all the data together
#create dataset
real_list6 <- list(N = nrow(afrs_restricted_b), #population size
                   A = ncol(afrs_restricted_b), #age
                   wealth = std_absw_restricted_b, #absolute wealth
                   baby = afrs_restricted_b, #AFR
                   N_miss = nrow(wealth_miss_restricted_b), # number of missing values that need imputation
                   wealth_miss=wealth_miss_restricted_b) # matrix indicating missing wealth data
#check data
real_list6

## Compile and fit model ----

# compile model

m6_add <- cmdstan_model("Offset/firstbaby_offset.stan")

#fit model
fit6_add_real <- m6_add$sample(data = real_list6, 
                               chains = 4, 
                               parallel_chains = 15, 
                               adapt_delta = 0.95,
                               max_treedepth = 13,
                               init = 0)


# save fit 
fit_6_add_real <- rstan::read_stan_csv(fit6_add_real$output_files())
saveRDS(fit_6_add_real, "firstbaby6_add_real.rds")
#load RDS file
rds6_add_real <- readRDS("firstbaby6_add_real.rds")
#extract samples
post6_add_real <- extract.samples(rds6_add_real)

#check the model
#check trace of all main parameters
#alpha
rstan::traceplot(rds6_add_real,pars="alpha")
#mu
traceplot(rds6_add_real,pars="mu") 
#mu_raw
traceplot(rds6_add_real,pars="mu_raw")
#mu_tau
rstan::traceplot(rds6_add_real,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds6_add_real,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds6_add_real,pars="mu_delta")
#beta_wealth_z
traceplot(rds6_add_real,pars="beta_wealth_z") 
#beta_wealth_sigma
traceplot(rds6_add_real,pars="beta_wealth_sigma") 
#gamma_wealth
traceplot(rds6_add_real,pars="gamma_wealth_z") 
#gamma_wealth
traceplot(rds6_add_real,pars="gamma_wealth_sigma") 
#delta_wealth
traceplot(rds6_add_real,pars="delta_wealth_z") 
#delta_wealth
traceplot(rds6_add_real,pars="delta_wealth_sigma") 

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab6_add_real <- precis(rds6_add_real,depth=2,pars=c("alpha",
                                                     "mu_raw",
                                                     "mu_tau",
                                                     "mu_delta"))
#check table
tab6_add_real
#create summary table for mu
tab6_add_real_mu <- precis(rds6_add_real,depth=2,pars="mu")
#check table
tab6_add_real_mu
plot(tab6_add_real_mu)
#create summary table for beta_z
tab6_add_real_beta_z <- precis(rds6_add_real,depth=2,pars="beta_wealth_z")
#check table
tab6_add_real_beta_z
plot(tab6_add_real_beta_z)
#create summary table for beta_sigma
tab6_add_real_beta_sigma <- precis(rds6_add_real,depth=2,pars="beta_wealth_sigma")
#check table
tab6_add_real_beta_sigma
#plot(tab6_add_real_beta_sigma)
#create summary table for gamma_z
tab6_add_real_gamma_z <- precis(rds6_add_real,depth=2,pars="gamma_wealth_z")
#check table
tab6_add_real_gamma_z
plot(tab6_add_real_gamma_z)
#create summary table for gamma_sigma
tab6_add_real_gamma_sigma <- precis(rds6_add_real,depth=2,pars="gamma_wealth_sigma")
#check table
tab6_add_real_gamma_sigma
#plot(tab3_add_real_gamma_sigma)
#create summary table for delta_z
tab6_add_real_delta_z <- precis(rds6_add_real,depth=2,pars="delta_wealth_z")
#check table
tab6_add_real_delta_z
plot(tab6_add_real_delta_z)
#create summary table for delta_sigma
tab6_add_real_delta_sigma <- precis(rds6_add_real,depth=2,pars="delta_wealth_sigma")
#check table
tab6_add_real_delta_sigma
#plot(tab4_add_real_delta_sigma)

# # To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
# #mu
# tab6_add_real_mu[,1]<-round(inv_logit(tab6_add_real_mu[,1]),3)
# tab6_add_real_mu[,3]<-round(inv_logit(tab6_add_real_mu[,3]),3)
# tab6_add_real_mu[,4]<-round(inv_logit(tab6_add_real_mu[,4]),3)
# #delta_wealth
# tab6_add_real_delta[,1]<-round(inv_logit(tab6_add_real_delta[,1]),3)
# tab6_add_real_delta[,3]<-round(inv_logit(tab6_add_real_delta[,3]),3)
# tab6_add_real_delta[,4]<-round(inv_logit(tab6_add_real_delta[,4]),3)

# Plot the fit of the real data ----

### Absolute Wealth ----

#simulate wealth values
simwealth_add_real_b <- seq(from=round(min(post6_add_real$wealth_full),1),to=round(max(post6_add_real$wealth_full),1),length.out=nrow(std_absw_restricted_b)) #specify according to range and length related to sample size
simwealth_add_real_b
#get the deciles_b
deciles_b <- as.numeric(quantile(simwealth_add_real_b,seq(0,1,0.5)))
deciles_b

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_a<-palette[1:length(deciles_b)]
palette_a

#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,12))

#plot empty plot
plot(c(0,1)~c(10,ncol(post6_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Current levels\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("Min.","Med.", "Max."),col=palette_a,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2,box.col = NA)

#add lines
for(k in 1:(length(deciles_b))){
  #create matrix to store the data
  p6_add_real_b <- matrix(nrow=nrow(post6_add_real$mu),ncol=ncol(post6_add_real$mu))
  p6_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post6_add_real$mu)){
    for(i in 1:nrow(post6_add_real$mu)){
      p6_add_real_b[i,j] <- inv_logit(post6_add_real$alpha[i] + #inv logit because originally is logit
                                        post6_add_real$mu[i,j] + #age
                                        (post6_add_real$beta_wealth_z[i,j]*post6_add_real$beta_wealth_sigma[i])*deciles_b[k] + #absolute wealth
                                        (post6_add_real$gamma_wealth_z[i,j]*post6_add_real$gamma_wealth_sigma[i])*0 + #wealth change
                                        (post6_add_real$delta_wealth_z[i,j]*post6_add_real$delta_wealth_sigma[i])*0) #moving variance
    }
  }
  #check data
  p6_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data6_add_real_b <- data.frame(age = 1:ncol(p6_add_real_b),
                                      median = apply(p6_add_real_b, 2, median), 
                                      upp = apply(p6_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p6_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_absw_b <- numeric(length(plot_data6_add_real_b$median))
  cumulative_low_absw_b <- numeric(length(plot_data6_add_real_b$low))
  cumulative_upp_absw_b <- numeric(length(plot_data6_add_real_b$upp))
  #set the first probability
  cumulative_median_absw_b[1] <- plot_data6_add_real_b$median[1]
  cumulative_low_absw_b[1] <- plot_data6_add_real_b$low[1]
  cumulative_upp_absw_b[1] <- plot_data6_add_real_b$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_data6_add_real_b$median)) {
    cumulative_median_absw_b[a] <- cumulative_median_absw_b[a-1] + (1 - cumulative_median_absw_b[a-1]) * plot_data6_add_real_b$median[a]
    cumulative_low_absw_b[a] <- cumulative_low_absw_b[a-1] + (1 - cumulative_low_absw_b[a-1]) * plot_data6_add_real_b$low[a]
    cumulative_upp_absw_b[a] <- cumulative_upp_absw_b[a-1] + (1 - cumulative_upp_absw_b[a-1]) * plot_data6_add_real_b$upp[a]
  }
  
  #add median
  #add points
  points(cumulative_median_absw_b[11:51] ~ plot_data6_add_real_b$age[11:51], col=palette_a[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_absw_b[11:51] ~ plot_data6_add_real_b$age[11:51], col=palette_a[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_data6_add_real_b$age[11:51], rev(plot_data6_add_real_b$age[11:51])), c(cumulative_low_absw_b[11:51], rev(cumulative_upp_absw_b[11:51])), col=alpha(palette_a[k], 0.25), border=NA)
  
}

### Short-term wealth variability ----

#simulate wealth values
simwealth_change_real <- seq(from=round(min(post6_add_real$wealth_change),1),to=round(max(post6_add_real$wealth_change),1),length.out=nrow(std_absw_restricted_b)) #specify according to range and length related to sample size
simwealth_change_real
#get the deciles_b
deciles_b <- as.numeric(quantile(simwealth_change_real,seq(0,1,0.5)))
deciles_b

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_b<-palette[4:(length(deciles_b)+3)]
palette_b

#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,12))

#plot empty plot
plot(c(0,1)~c(10,ncol(post6_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Short-term variability\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("Min.","Med.", "Max."),col=palette_b,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2,box.col = NA)

#add lines
for(k in 1:(length(deciles_b))){
  #create matrix to store the data
  p6_add_real_b <- matrix(nrow=nrow(post6_add_real$mu),ncol=ncol(post6_add_real$mu))
  p6_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post6_add_real$mu)){
    for(i in 1:nrow(post6_add_real$mu)){
      p6_add_real_b[i,j] <- inv_logit(post6_add_real$alpha[i] + #inv logit because originally is logit
                                        post6_add_real$mu[i,j] + #age
                                        (post6_add_real$beta_wealth_z[i,j]*post6_add_real$beta_wealth_sigma[i])*0 + #absolute wealth
                                        (post6_add_real$gamma_wealth_z[i,j]*post6_add_real$gamma_wealth_sigma[i])*deciles_b[k] + #wealth change
                                        (post6_add_real$delta_wealth_z[i,j]*post6_add_real$delta_wealth_sigma[i])*0) #moving variance
    }
  }
  #check data
  p6_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data6_add_real_b <- data.frame(age = 1:ncol(p6_add_real_b),
                                      median = apply(p6_add_real_b, 2, median), 
                                      upp = apply(p6_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p6_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diffw_b <- numeric(length(plot_data6_add_real_b$median))
  cumulative_low_diffw_b <- numeric(length(plot_data6_add_real_b$low))
  cumulative_upp_diffw_b <- numeric(length(plot_data6_add_real_b$upp))
  #set the first probability
  cumulative_median_diffw_b[1] <- plot_data6_add_real_b$median[1]
  cumulative_low_diffw_b[1] <- plot_data6_add_real_b$low[1]
  cumulative_upp_diffw_b[1] <- plot_data6_add_real_b$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_data6_add_real_b$median)) {
    cumulative_median_diffw_b[a] <- cumulative_median_diffw_b[a-1] + (1 - cumulative_median_diffw_b[a-1]) * plot_data6_add_real_b$median[a]
    cumulative_low_diffw_b[a] <- cumulative_low_diffw_b[a-1] + (1 - cumulative_low_diffw_b[a-1]) * plot_data6_add_real_b$low[a]
    cumulative_upp_diffw_b[a] <- cumulative_upp_diffw_b[a-1] + (1 - cumulative_upp_diffw_b[a-1]) * plot_data6_add_real_b$upp[a]
  }
  
  #add median
  #add points
  points(cumulative_median_diffw_b[11:51] ~ plot_data6_add_real_b$age[11:51], col=palette_b[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_diffw_b[11:51] ~ plot_data6_add_real_b$age[11:51], col=palette_b[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_data6_add_real_b$age[11:51], rev(plot_data6_add_real_b$age[11:51])), c(cumulative_low_diffw_b[11:51], rev(cumulative_upp_diffw_b[11:51])), col=alpha(palette_b[k], 0.25), border=NA)
}

### Long-term variability of wealth ----

#simulate wealth values
simwealth_msd_real <- seq(from=round(min(post6_add_real$wealth_msd),1),to=round(max(post6_add_real$wealth_msd),1),length.out=nrow(std_absw_restricted_b)) #specify according to range and length related to sample size
simwealth_msd_real
#get the deciles_b
deciles_b <- as.numeric(quantile(simwealth_msd_real,seq(0,1,0.5)))
deciles_b

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_c<-palette[7:(length(deciles_b)+6)]
palette_c

#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,12))

#plot empty plot
plot(c(0,1)~c(10,ncol(post6_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Long-term variability\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("Min.","Med.", "Max."),col=palette_c,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2,box.col=NA)

#add lines
for(k in 1:(length(deciles_b))){
  #create matrix to store the data
  p6_add_real_b <- matrix(nrow=nrow(post6_add_real$mu),ncol=ncol(post6_add_real$mu))
  p6_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post6_add_real$mu)){
    for(i in 1:nrow(post6_add_real$mu)){
      p6_add_real_b[i,j] <- inv_logit(post6_add_real$alpha[i] + #inv logit because originally is logit
                                        post6_add_real$mu[i,j] + #age
                                        (post6_add_real$beta_wealth_z[i,j]*post6_add_real$beta_wealth_sigma[i])*0 + #absolute wealth
                                        (post6_add_real$gamma_wealth_z[i,j]*post6_add_real$gamma_wealth_sigma[i])*0 + #wealth change
                                        (post6_add_real$delta_wealth_z[i,j]*post6_add_real$delta_wealth_sigma[i])*deciles_b[k]) #moving variance
    }
  }
  #check data
  p6_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data6_add_real_b <- data.frame(age = 1:ncol(p6_add_real_b),
                                      median = apply(p6_add_real_b, 2, median), 
                                      upp = apply(p6_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p6_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_msdw_b <- numeric(length(plot_data6_add_real_b$median))
  cumulative_low_msdw_b <- numeric(length(plot_data6_add_real_b$low))
  cumulative_upp_msdw_b <- numeric(length(plot_data6_add_real_b$upp))
  #set the first probability
  cumulative_median_msdw_b[1] <- plot_data6_add_real_b$median[1]
  cumulative_low_msdw_b[1] <- plot_data6_add_real_b$low[1]
  cumulative_upp_msdw_b[1] <- plot_data6_add_real_b$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_data6_add_real_b$median)) {
    cumulative_median_msdw_b[a] <- cumulative_median_msdw_b[a-1] + (1 - cumulative_median_msdw_b[a-1]) * plot_data6_add_real_b$median[a]
    cumulative_low_msdw_b[a] <- cumulative_low_msdw_b[a-1] + (1 - cumulative_low_msdw_b[a-1]) * plot_data6_add_real_b$low[a]
    cumulative_upp_msdw_b[a] <- cumulative_upp_msdw_b[a-1] + (1 - cumulative_upp_msdw_b[a-1]) * plot_data6_add_real_b$upp[a]
  }
  
  #add median
  #add points
  points(cumulative_median_msdw_b[11:51] ~ plot_data6_add_real_b$age[11:51], col=palette_c[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_msdw_b[11:51] ~ plot_data6_add_real_b$age[11:51], col=palette_c[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_data6_add_real_b$age[11:51], rev(plot_data6_add_real_b$age[11:51])), c(cumulative_low_msdw_b[11:51], rev(cumulative_upp_msdw_b[11:51])), col=alpha(palette_c[k], 0.25), border=NA)
}

# Relative importance ----

relative <- precis(rds6_add_real,
                   depth=2,
                   pars=c("beta_wealth_sigma","gamma_wealth_sigma","delta_wealth_sigma"))

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,5,5))


plot(c(0,2),c(0,3),
     main="Relative importance\nof wealth predictors",
     type="n",
     xlab="Value",
     ylab="Wealth predictor",
     yaxt="n",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5
) 
axis(2,at=seq(2.5,0.5,by=-1),
     labels=c(expression(sigma[beta]),
              expression(sigma[gamma]),
              expression(sigma[delta])
     ),
     las=1,
     tick=F,
     cex.axis=1.2
)
segments(0,-0.1,0,3.1,lty="dashed",col="lightgrey")
segments(-0.01,0.5,2,0.5,lty="dashed",col="lightgrey")
segments(-0.01,1.5,2,1.5,lty="dashed",col="lightgrey")
segments(-0.01,2.5,2,2.5,lty="dashed",col="lightgrey")
points(relative[,1],seq(2.5,0.5,by=-1),cex=2,pch=16,col=hcl.colors(3,"berlin"))
segments(relative[1,1]-relative[1,2],2.5,relative[1,1]+relative[1,2],2.5,lwd=3,col=hcl.colors(3,"berlin")[1])
segments(relative[2,1]-relative[2,2],1.5,relative[2,1]+relative[2,2],1.5,lwd=3,col=hcl.colors(3,"berlin")[2])
segments(relative[3,1]-relative[3,2],0.5,relative[3,1]+relative[3,2],0.5,lwd=3,col=hcl.colors(3,"berlin")[3])
