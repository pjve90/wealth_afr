# Model with current absolute levels, wealth change, and moving variance ----

#The code in this script is meant to fit a Bayesian model that aims to predict the probability of first birth by the amount of wealth a woman has, together with short-term and long-term wealth variability.

#Load packages
#install.packages("cmdstanr")
library(cmdstanr)
#install.packages("rethinking")
library(rethinking)
#install.packages("scales")
library(scales)
#install.packages("corrplot")
library(corrplot)

## Data wrangling of real data ----

#Load data
real_data <- read.csv("dataf.csv")[,-1]
head(real_data)

# Age at first reproduction ----

#create a matrix to store the age-specific age of censor
afr_matrix <- matrix(nrow=nrow(real_data),ncol=max(real_data$aoc)+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix)){
  afr <- real_data$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix[i,1:(afr-1)] <- 0
    afr_matrix[i,afr] <- 1
  } else{
    afr_matrix[i,1:aoc] <- rep(0,length(afr_matrix[i,1:aoc]))
  }
}
#check the data
afr_matrix
#check the age-specific probability of FR
apply(afr_matrix,2,sum,na.rm=T)/apply(afr_matrix,2,function(x)sum(!is.na(x)))
#plot the CCDF of first birth
plot(cumprod(1-apply(afr_matrix,2,sum,na.rm=T)/apply(afr_matrix,2,function(x)sum(!is.na(x))))~c(1:(max(real_data$aoc)+1)),
     xlab="Age",
     ylab="CCDF of first birth",
     ylim=c(0,1))

#Current absolute wealth ----

#Current absolute wealth
#create matrix to store the amount of wealth at each age
absw_matrix <- matrix(nrow = nrow(real_data),ncol=max(real_data$aoc)+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw95[i]
  age_absw <- real_data$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data$aoc[i]+1)){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}
#check data
absw_matrix
#98
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw98[i]
  age_absw <- real_data$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}
#check data
absw_matrix
#00
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw00[i]
  age_absw <- real_data$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}
#check data
absw_matrix
#02
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw02[i]
  age_absw <- real_data$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}
#check data
absw_matrix
#04
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw04[i]
  age_absw <- real_data$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}
#check data
absw_matrix
#06
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw06[i]
  age_absw <- real_data$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}
#check data
absw_matrix
#10
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw10[i]
  age_absw <- real_data$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data$aoc[i]+1)){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}
#check data
absw_matrix
#check the average of current absolute wealth at each age
apply(absw_matrix,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix,2,mean,na.rm=T)~c(1:ncol(absw_matrix)),
     xlab="Age",
     ylab="Average current absolute wealth",
     type="b"
)

#standardise the log-transformed current absolute wealth
std_absw_matrix <- matrix(standardize(log(as.vector(absw_matrix))),ncol=ncol(absw_matrix),nrow=nrow(absw_matrix))
#check the data
std_absw_matrix
#check the age-specific average of standardised current absolute wealth
apply(std_absw_matrix,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix,2,mean,na.rm=T)~c(1:(max(real_data$aoc)+1)),
     xlab="Age",
     ylab="Average absolute wealth",
     type="b"
)

# Fit real data ----

##Prepare data ----

#Age at first birth
#replace NAs with -99
for(j in 1:ncol(afr_matrix)){
  for(i in 1:nrow(afr_matrix)){
    if(is.na(afr_matrix[i,j])){
      afr_matrix[i,j] <- -99
    } else{
      afr_matrix[i,j] <- afr_matrix[i,j]
    }
  }
}
#check the data
afr_matrix

#Wealth
#matrix identifying missing wealth data
wealth_miss <- which(is.na(std_absw_matrix),arr.ind = T)
#check data
wealth_miss

#replace NAs with -99
for(j in 1:ncol(std_absw_matrix)){
  for(i in 1:nrow(std_absw_matrix)){
    if(is.na(std_absw_matrix[i,j])){
      std_absw_matrix[i,j] <- -99
    } else{
      std_absw_matrix[i,j] <- std_absw_matrix[i,j]
    }
  }
}
#check the data
std_absw_matrix

#Subset the data for realistic ages
#Subset wealth and AFB for those between zero years old and 50 years old.
#wealth
std_absw_restricted <- std_absw_matrix[,1:51] #Adding 1, since first column in the matrix is year 0
#AFB
afrs_restricted <- afr_matrix[,1:51] #Adding 1, since first column in the matrix is year 0
afrs_restricted[,1:10] <- -99 #turning the first 10 years to NAs because we do not need to model such ages for age at first birth
afrs_restricted
#missing wealth data
wealth_miss_restricted <- wealth_miss[wealth_miss[,2] <= 51,] #Adding 1, since first column in the matrix is year 0

#put all the data together
#create dataset
real_list <- list(N = nrow(afrs_restricted), #population size
                  A = ncol(afrs_restricted), #age
                  wealth = std_absw_restricted, #current absolute wealth
                  baby = afrs_restricted, #AFR
                  N_miss = nrow(wealth_miss_restricted), # number of missing values that need imputation
                  wealth_miss=wealth_miss_restricted) # matrix indicating missing wealth data
#check data
real_list

## Compile and fit model ----

# compile model

model_int1 <- cmdstan_model("Misc/firstbaby_interaction_abs_msd.stan")

#fit model
fit_int1 <- model_int1$sample(data = real_list, 
                            chains = 4, 
                            parallel_chains = 15, 
                            adapt_delta = 0.95,
                            max_treedepth = 13,
                            init = 0)


# save fit 
fit_int1_csv <- rstan::read_stan_csv(fit_int1$output_files())
saveRDS(fit_int1_csv, "fit_int1_output.rds")
#load RDS file
rds_int1 <- readRDS("fit_int1_output.rds")
#extract samples
post_int1 <- extract.samples(rds_int1)

## Model diagnostics ----

#check trace of all parameters
#alpha
rstan::traceplot(rds_int1,pars="alpha")
#mu
traceplot(rds_int1,pars="mu") 
#mu_raw
traceplot(rds_int1,pars="mu_raw")
#mu_tau
rstan::traceplot(rds_int1,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds_int1,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds_int1,pars="mu_delta")
#beta_wealth_z
traceplot(rds_int1,pars="beta_wealth_z") 
#beta_wealth_sigma
traceplot(rds_int1,pars="beta_wealth_sigma") 
#delta_wealth
traceplot(rds_int1,pars="delta_wealth_z") 
#delta_wealth
traceplot(rds_int1,pars="delta_wealth_sigma") 
#epsilon_wealth
traceplot(rds_int1,pars="epsilon_wealth_z") 
#epsilon_wealth
traceplot(rds_int1,pars="epsilon_wealth_sigma") 

#summary of the model
#create summary tables of the different parameters

#alpha and hiper priors of Gaussian process
#create summary table for alpha and hiper priors of Gaussian process
tab_int1_alphagp <- precis(rds_int1,depth=2,pars=c("alpha",
                                                   "mu_raw",
                                                   "mu_tau",
                                                   "mu_delta"))
#check table
tab_int1_alphagp

#mu
#create summary table for mu
tab_int1_mu <- precis(rds_int1,depth=2,pars="mu")
#check table
tab_int1_mu
#plot it!
plot(tab_int1_mu)

#beta z
#create summary table for beta_z
tab_int1_beta_z <- precis(rds_int1,depth=2,pars="beta_wealth_z")
#check table
tab_int1_beta_z
#plot it!
plot(tab_int1_beta_z)

#beta sigma
#create summary table for beta_sigma
tab_int1_beta_sigma <- precis(rds_int1,depth=2,pars="beta_wealth_sigma")
#check table
tab_int1_beta_sigma

#delta z
#create summary table for delta_z
tab_int1_delta_z <- precis(rds_int1,depth=2,pars="delta_wealth_z")
#check table
tab_int1_delta_z
#plot it!
plot(tab_int1_delta_z)

#delta sigma
#create summary table for delta_sigma
tab_int1_delta_sigma <- precis(rds_int1,depth=2,pars="delta_wealth_sigma")
#check table
tab_int1_delta_sigma

#epsilon z
#create summary table for epsilon_z
tab_int1_epsilon_z <- precis(rds_int1,depth=2,pars="epsilon_wealth_z")
#check table
tab_int1_epsilon_z
#plot it!
plot(tab_int1_epsilon_z)

#epsilon sigma
#create summary table for epsilon_sigma
tab_int1_epsilon_sigma <- precis(rds_int1,depth=2,pars="epsilon_wealth_sigma")
#check table
tab_int1_epsilon_sigma

#Check correlation between wealth predictors

#beta versus delta
#create correlation matrix
cor1 <- round(cor(post_int1$beta_wealth_z,post_int1$delta_wealth_z),3)
#plot it!
corrplot(cor1, "color", tl.col="black")

#beta versus epsion
#create correlation matrix
cor2 <- round(cor(post_int1$beta_wealth_z,post_int1$epsilon_wealth_z),3)
#plot it!
corrplot(cor2, "color", tl.col="black")

#delta versus epsilon
#create correlation matrix
cor3 <- round(cor(post_int1$delta_wealth_z,post_int1$epsilon_wealth_z),3)
#plot it!
corrplot(cor3, "color", tl.col="black")

# Plot the output of the model ----

## Current absolute Wealth ----

#simulate wealth values
simwealth_absw_int1 <- seq(from=round(min(post_int1$wealth_full),1),to=round(max(post_int1$wealth_full),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_absw_int1
#get the deciles
deciles_absw <- as.numeric(quantile(simwealth_absw_int1,seq(0,1,0.5)))
deciles_absw

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_a<-palette[1:length(deciles_absw)]
palette_a

#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,12))

#plot empty plot
plot(c(0,1)~c(10,ncol(post_int1$mu)),
     ylab="CCDF of first birth",
     xlab="Age",
     main="Current absolute levels\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("Poor","Middle", "Rich"),col=palette_a,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2)

#add lines
for(k in 1:(length(deciles_absw))){
  #create matrix to store the data
  p_absw_int1 <- matrix(nrow=nrow(post_int1$mu),ncol=ncol(post_int1$mu))
  p_absw_int1
  #fill it in with values for age 25
  for(j in 1:ncol(post_int1$mu)){
    for(i in 1:nrow(post_int1$mu)){
      p_absw_int1[i,j] <- inv_logit(post_int1$alpha[i] + #inv logit because originally is logit
                                      post_int1$mu[i,j] + #age
                                      (post_int1$beta_wealth_z[i,j]*post_int1$beta_wealth_sigma[i])*deciles_absw[k] + #absolute wealth
                                      (post_int1$gamma_wealth_z[i,j]*post_int1$gamma_wealth_sigma[i])*0 + #wealth change
                                      (post_int1$delta_wealth_z[i,j]*post_int1$delta_wealth_sigma[i])*0) #moving variance
    }
  }
  #check data
  p_absw_int1
  #plot it!
  #prepare model prediction data
  plot_absw_int1 <- data.frame(age = 1:ncol(p_absw_int1),
                               median = apply(p_absw_int1, 2, median), 
                               upp = apply(p_absw_int1, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                               low = apply(p_absw_int1, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("absw_",i),plot_absw_int1)
  #add median
  #add points
  points(cumprod(1-plot_absw_int1$median[11:51])~plot_absw_int1$age[11:51],col=palette_a[k],pch=shape[k],cex=1.5)
  #add lines
  lines(cumprod(1-plot_absw_int1$median[11:51])~plot_absw_int1$age[11:51],col=palette_a[k],lwd=3,lty=type[k])
  #add confidence intervals
  polygon(c(plot_absw_int1$age[11:51],rev(plot_absw_int1$age[11:51])),c(cumprod(1-plot_absw_int1$low[11:51]),rev(cumprod(1-plot_absw_int1$upp[11:51]))),col=alpha(palette_a[k],0.25),border=NA)
}

## Short-term wealth variability ----

#simulate wealth values
simwealth_change_int1 <- seq(from=round(min(post_int1$wealth_change),1),to=round(max(post_int1$wealth_change),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_change_int1
#get the deciles
deciles_diffw <- as.numeric(quantile(simwealth_change_int1,seq(0,1,0.5)))
deciles_diffw

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_b<-palette[4:(length(deciles_diffw)+3)]
palette_b

#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,12))

#plot empty plot
plot(c(0,1)~c(10,ncol(post_int1$mu)),
     ylab="CCDF of first birth",
     xlab="Age",
     main="Short-term variability\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("No var.","Mid. var.", "Max. var."),col=palette_b,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2)

#add lines
for(k in 1:(length(deciles_diffw))){
  #create matrix to store the data
  p_diffw_int1 <- matrix(nrow=nrow(post_int1$mu),ncol=ncol(post_int1$mu))
  p_diffw_int1
  #fill it in with values for age 25
  for(j in 1:ncol(post_int1$mu)){
    for(i in 1:nrow(post_int1$mu)){
      p_diffw_int1[i,j] <- inv_logit(post_int1$alpha[i] + #inv logit because originally is logit
                                       post_int1$mu[i,j] + #age
                                       (post_int1$beta_wealth_z[i,j]*post_int1$beta_wealth_sigma[i])*0 + #absolute wealth
                                       (post_int1$gamma_wealth_z[i,j]*post_int1$gamma_wealth_sigma[i])*deciles_diffw[k] + #wealth change
                                       (post_int1$delta_wealth_z[i,j]*post_int1$delta_wealth_sigma[i])*0) #moving variance
    }
  }
  #check data
  p_diffw_int1
  #plot it!
  #prepare model prediction data
  plot_diffw_int1 <- data.frame(age = 1:ncol(p_diffw_int1),
                                mean = apply(p_diffw_int1, 2, median), 
                                upp = apply(p_diffw_int1, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p_diffw_int1, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("diffw_",i),plot_diffw_int1)
  #add median
  #add points
  points(cumprod(1-plot_diffw_int1$mean[11:51])~plot_diffw_int1$age[11:51],col=palette_b[k],pch=shape[k],cex=1.5)
  #add lines
  lines(cumprod(1-plot_diffw_int1$mean[11:51])~plot_diffw_int1$age[11:51],col=palette_b[k],lwd=3,lty=type[k])
  #add confidence intervals
  polygon(c(plot_diffw_int1$age[11:51],rev(plot_diffw_int1$age[11:51])),c(cumprod(1-plot_diffw_int1$low[11:51]),rev(cumprod(1-plot_diffw_int1$upp[11:51]))),col=alpha(palette_b[k],0.25),border=NA)
}

## Long-term variability of wealth ----

#simulate wealth values
simwealth_msd_int1 <- seq(from=round(min(post_int1$wealth_msd),1),to=round(max(post_int1$wealth_msd),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_msd_int1
#get the deciles
deciles_msd <- as.numeric(quantile(simwealth_msd_int1,seq(0,1,0.5)))
deciles_msd

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_c<-palette[7:(length(deciles_msd)+6)]
palette_c

#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,12))

#plot empty plot
plot(c(0,1)~c(10,ncol(post_int1$mu)),
     ylab="CCDF of first birth",
     xlab="Age",
     main="Long-term variability\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("No var.","Mid. var.", "Max. var."),col=palette_c,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2)

#add lines
for(k in 1:(length(deciles_msd))){
  #create matrix to store the data
  p_msd_int1 <- matrix(nrow=nrow(post_int1$mu),ncol=ncol(post_int1$mu))
  p_msd_int1
  #fill it in with values for age 25
  for(j in 1:ncol(post_int1$mu)){
    for(i in 1:nrow(post_int1$mu)){
      p_msd_int1[i,j] <- inv_logit(post_int1$alpha[i] + #inv logit because originally is logit
                                     post_int1$mu[i,j] + #age
                                     (post_int1$beta_wealth_z[i,j]*post_int1$beta_wealth_sigma[i])*0 + #absolute wealth
                                     (post_int1$gamma_wealth_z[i,j]*post_int1$gamma_wealth_sigma[i])*0 + #wealth change
                                     (post_int1$delta_wealth_z[i,j]*post_int1$delta_wealth_sigma[i])*deciles_msd[k]) #moving variance
    }
  }
  #check data
  p_msd_int1
  #plot it!
  #prepare model prediction data
  plot_msd_int1 <- data.frame(age = 1:ncol(p_msd_int1),
                              mean = apply(p_msd_int1, 2, median), 
                              upp = apply(p_msd_int1, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p_msd_int1, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msdw_",i),plot_msd_int1)
  #add median
  #add points
  points(cumprod(1-plot_msd_int1$mean[11:51])~plot_msd_int1$age[11:51],col=palette_c[k],pch=shape[k],cex=1.5)
  #add lines
  lines(cumprod(1-plot_msd_int1$mean[11:51])~plot_msd_int1$age[11:51],col=palette_c[k],lwd=3,lty=type[k])
  #add confidence intervals
  polygon(c(plot_msd_int1$age[11:51],rev(plot_msd_int1$age[11:51])),c(cumprod(1-plot_msd_int1$low[11:51]),rev(cumprod(1-plot_msd_int1$upp[11:51]))),col=alpha(palette_c[k],0.25),border=NA)
}

## Interaction absolute and long-term ----

#simulate wealth values
simwealth_absw_int1 <- seq(from=round(min(post_int1$wealth_full),1),to=round(max(post_int1$wealth_full),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_absw_int1
#get the deciles
deciles_absw_int1 <- as.numeric(quantile(simwealth_absw_int1,seq(0,1,0.5)))
deciles_absw_int1

#simulate wealth values
simwealth_msd_int1 <- seq(from=round(min(post_int1$wealth_msd),1),to=round(max(post_int1$wealth_msd),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_msd_int1
#get the deciles
deciles_msd_int1 <- as.numeric(quantile(simwealth_msd_int1,seq(0,1,0.5)))
deciles_msd_int1

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_d<-palette[7:(length(deciles_msd)+6)]
palette_d

#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,12))
#plot empty plot
plot(c(0,1)~c(10,ncol(post_int1$mu)),
     ylab="CCDF of first birth",
     xlab="Age",
     main="Long-term variability\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("No var.","Mid. var.", "Max. var."),col=palette_d,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2)

#add lines
for(k in 1:(length(deciles_absw_int1))){
  #create matrix to store the data
  p_int1_real <- matrix(nrow=nrow(post_int1$mu),ncol=ncol(post_int1$mu))
  p_int1_real
  #fill it in with values for age 25
  for(j in 1:ncol(post_int1$mu)){
    for(i in 1:nrow(post_int1$mu)){
      p_int1_real[i,j] <- inv_logit(post_int1$alpha[i] + #inv logit because originally is logit
                                     post_int1$mu[i,j] + #age
                                     (post_int1$beta_wealth_z[i,j]*post_int1$beta_wealth_sigma[i])*0 + #absolute wealth
                                     (post_int1$delta_wealth_z[i,j]*post_int1$delta_wealth_sigma[i])*0 + #moving variance
                                     (post_int1$epsilon_wealth_z[i,j]*post_int1$epsilon_wealth_sigma[i])*(deciles_absw_int1[k]*deciles_msd_int1[k]) #interaction
      )  
    }
  }
  #check data
  p_int1_real
  #plot it!
  #prepare model prediction data
  plot_int1_real <- data.frame(age = 1:ncol(p_int1_real),
                              median = apply(p_int1_real, 2, median), 
                              upp = apply(p_int1_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p_int1_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("int1w_",i),plot_int1_real)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_int1 <- numeric(length(plot_int1_real$median))
  cumulative_low_int1 <- numeric(length(plot_int1_real$low))
  cumulative_upp_int1 <- numeric(length(plot_int1_real$upp))
  #set the first probability
  cumulative_median_int1[1] <- plot_int1_real$median[1]
  cumulative_low_int1[1] <- plot_int1_real$low[1]
  cumulative_upp_int1[1] <- plot_int1_real$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_int1_real$median)) {
    cumulative_median_int1[a] <- cumulative_median_int1[a-1] + (1 - cumulative_median_int1[a-1]) * plot_int1_real$median[a]
    cumulative_low_int1[a] <- cumulative_low_int1[a-1] + (1 - cumulative_low_int1[a-1]) * plot_int1_real$low[a]
    cumulative_upp_int1[a] <- cumulative_upp_int1[a-1] + (1 - cumulative_upp_int1[a-1]) * plot_int1_real$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_int1_",i),cumulative_median_int1)
  assign(paste0("cumulative_low_int1_",i),cumulative_low_int1)
  assign(paste0("cumulative_upp_int1_",i),cumulative_upp_int1)
  
  #add median
  #add points
  points(cumulative_median_int1[11:51] ~ plot_int1_real$age[11:51], col=palette_d[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_int1[11:51] ~ plot_int1_real$age[11:51], col=palette_d[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_int1_real$age[11:51], rev(plot_int1_real$age[11:51])), c(cumulative_low_int1[11:51], rev(cumulative_upp_int1[11:51])), col=alpha(palette_d[k], 0.25), border=NA)
}

# Expected median age ----

### Current absolute wealth ----

#Minimum current absolute wealth
#check which cumulative probability of 0.5
which(round(cumsum(absw_1$median),1) == 0.5)
#check specific age, since columns are age+1
which(round(cumsum(absw_1$median),1) == 0.5) - 1
#median probability and hpdi
absw_1[which(round(cumsum(absw_1$median),1) == 0.5),]

#Median current absolute wealth
#check which cumulative probability of 0.5
which(round(cumsum(absw_2$median),1) == 0.5)
#check which cumulative probability between 0.4 and 0.6
which(round(cumsum(absw_2$median),1) >= 0.4 & round(cumsum(absw_2$median),1) <= 0.6)
#check specific age, since columns are age+1
which(round(cumsum(absw_2$median),1) >= 0.4 & round(cumsum(absw_2$median),1) <= 0.6) - 1
#median probability and hpdi
absw_2[which(round(cumsum(absw_2$median),1) >= 0.4 & round(cumsum(absw_2$median),1) <= 0.6),]

#Maximum current absolute wealth
#check which cumulative probability of 0.5
which(round(cumsum(absw_3$median),1) == 0.5)
#check specific age, since columns are age+1
which(round(cumsum(absw_3$median),1) == 0.5) - 1
#median probability and hpdi
absw_3[which(round(cumsum(absw_3$median),1) == 0.5),]

### Short-term variability ----

#Minimum short-term variability
#check which cumulative probability of 0.5
which(round(cumsum(diffw_1$median),1) == 0.5)
#check which cumulative probability between 0.4 and 0.6
which(round(cumsum(diffw_1$median),1) >= 0.4 & round(cumsum(diffw_1$median),1) <= 0.6)
#check specific age, since columns are age+1
which(round(cumsum(diffw_1$median),1) >= 0.4 & round(cumsum(diffw_1$median),1) <= 0.6) - 1
#median probability and hpdi
diffw_1[which(round(cumsum(diffw_1$median),1) >= 0.4 & round(cumsum(diffw_1$median),1) <= 0.6),]

#Median short-term variability
#check which cumulative probability of 0.5
which(round(cumsum(diffw_2$median),1) == 0.5)
#check which cumulative probability between 0.4 and 0.6
which(round(cumsum(diffw_2$median),1) >= 0.4 & round(cumsum(diffw_2$median),1) <= 0.6)
#check specific age, since columns are age+1
which(round(cumsum(diffw_2$median),1) >= 0.4 & round(cumsum(diffw_2$median),1) <= 0.6) - 1
#median probability and hpdi
diffw_2[which(round(cumsum(diffw_2$median),1) >= 0.4 & round(cumsum(diffw_2$median),1) <= 0.6),]

#Maximum short-term variability
#check which cumulative probability of 0.5
which(round(cumsum(diffw_3$median),1) == 0.5)
#check specific age, since columns are age+1
which(round(cumsum(diffw_3$median),1) == 0.5) - 1
#median probability and hpdi
diffw_3[which(round(cumsum(diffw_3$median),1) == 0.5),]

### Long-term variability ----

#Minimum long-term variability
#check which cumulative probability of 0.5
which(round(cumsum(msdw_1$median),1) == 0.5)
#check which cumulative probability between 0.4 and 0.6
which(round(cumsum(msdw_1$median),1) >= 0.4 & round(cumsum(msdw_1$median),1) <= 0.6)
#check specific age, since columns are age+1
which(round(cumsum(msdw_1$median),1) >= 0.4 & round(cumsum(msdw_1$median),1) <= 0.6) - 1
#median probability and hpdi
msdw_1[which(round(cumsum(msdw_1$median),1) >= 0.4 & round(cumsum(msdw_1$median),1) <= 0.6),]

#Median long-term variability
#check which cumulative probability of 0.5
which(round(cumsum(msdw_2$median),1) == 0.5)
#check which cumulative probability between 0.4 and 0.6
which(round(cumsum(msdw_2$median),1) >= 0.4 & round(cumsum(msdw_2$median),1) <= 0.6)
#check specific age, since columns are age+1
which(round(cumsum(msdw_2$median),1) >= 0.4 & round(cumsum(msdw_2$median),1) <= 0.6) - 1
#median probability and hpdi
msdw_2[which(round(cumsum(msdw_2$median),1) >= 0.4 & round(cumsum(msdw_2$median),1) <= 0.6),]

#Maximum long-term variability
#check which cumulative probability of 0.5
which(round(cumsum(msdw_3$median),1) == 0.5)
#check specific age, since columns are age+1
which(round(cumsum(msdw_3$median),1) == 0.5) - 1
#median probability and hpdi
msdw_3[which(round(cumsum(msdw_3$median),1) == 0.5),]

# Differences within wealth classes ----

## Current absolute wealth -----

#Minimum current absolute wealth 
#largest absolute difference
max(abs(diff(cumprod(1-absw_1$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-absw_1$median[11:51]))) == max(abs(diff(cumprod(1-absw_1$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-absw_1$median[11:51]))) == max(abs(diff(cumprod(1-absw_1$median[11:51]))))) + 9

#Median current absolute wealth 
#largest absolute difference
max(abs(diff(cumprod(1-absw_2$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-absw_2$median[11:51]))) == max(abs(diff(cumprod(1-absw_2$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-absw_2$median[11:51]))) == max(abs(diff(cumprod(1-absw_2$median[11:51]))))) + 9

#Maximum current absolute wealth 
#largest absolute difference
max(abs(diff(cumprod(1-absw_3$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-absw_3$median[11:51]))) == max(abs(diff(cumprod(1-absw_3$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-absw_3$median[11:51]))) == max(abs(diff(cumprod(1-absw_3$median[11:51]))))) + 9

## Short-term wealth variability -----

#Minimum short-term variability 
#largest absolute difference
max(abs(diff(cumprod(1-diffw_1$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-diffw_1$median[11:51]))) == max(abs(diff(cumprod(1-diffw_1$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-diffw_1$median[11:51]))) == max(abs(diff(cumprod(1-diffw_1$median[11:51]))))) + 9

#Median short-term variability 
#largest absolute difference
max(abs(diff(cumprod(1-diffw_2$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-diffw_2$median[11:51]))) == max(abs(diff(cumprod(1-diffw_2$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-diffw_2$median[11:51]))) == max(abs(diff(cumprod(1-diffw_2$median[11:51]))))) + 9

#Maximum short-term variability 
#largest absolute difference
max(abs(diff(cumprod(1-diffw_3$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-diffw_3$median[11:51]))) == max(abs(diff(cumprod(1-diffw_3$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-diffw_3$median[11:51]))) == max(abs(diff(cumprod(1-diffw_3$median[11:51]))))) + 9

## Long-term wealth variability -----

#Minimum long-term variability 
#largest absolute difference
max(abs(diff(cumprod(1-msdw_1$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-msdw_1$median[11:51]))) == max(abs(diff(cumprod(1-msdw_1$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-msdw_1$median[11:51]))) == max(abs(diff(cumprod(1-msdw_1$median[11:51]))))) + 9

#Median long-term variability 
#largest absolute difference
max(abs(diff(cumprod(1-msdw_2$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-msdw_2$median[11:51]))) == max(abs(diff(cumprod(1-msdw_2$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-msdw_2$median[11:51]))) == max(abs(diff(cumprod(1-msdw_2$median[11:51]))))) + 9

#Maximum long-term variability
#largest absolute difference
max(abs(diff(cumprod(1-msdw_3$median[11:51]))))
#position of largest absolute difference
which(abs(diff(cumprod(1-msdw_3$median[11:51]))) == max(abs(diff(cumprod(1-msdw_3$median[11:51])))))
#age of largest absolute difference (10 (columns) - 1 (age) = 9)
which(abs(diff(cumprod(1-msdw_3$median[11:51]))) == max(abs(diff(cumprod(1-msdw_3$median[11:51]))))) + 9

#Differences between wealth classes ----

## Current absolute wealth ----

#Minimum versus median
which(abs(diff(rbind(cumprod(1-absw_1$median[11:51]),cumprod(1-absw_2$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-absw_1$median[11:51]),cumprod(1-absw_2$median[11:51])))))) + 10
#Median versus maximum
which(abs(diff(rbind(cumprod(1-absw_2$median[11:51]),cumprod(1-absw_3$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-absw_2$median[11:51]),cumprod(1-absw_3$median[11:51])))))) + 10
#Minimum versus maximum
which(abs(diff(rbind(cumprod(1-absw_1$median[11:51]),cumprod(1-absw_3$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-absw_1$median[11:51]),cumprod(1-absw_3$median[11:51])))))) + 10

## Short-term variability ----

#Minimum versus Median
which(abs(diff(rbind(cumprod(1-diffw_1$median[11:51]),cumprod(1-diffw_2$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-diffw_1$median[11:51]),cumprod(1-diffw_2$median[11:51])))))) + 10
#Median versus Maximum
which(abs(diff(rbind(cumprod(1-diffw_2$median[11:51]),cumprod(1-diffw_3$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-diffw_2$median[11:51]),cumprod(1-diffw_3$median[11:51])))))) + 10
#Minimum versus Maximum
which(abs(diff(rbind(cumprod(1-diffw_1$median[11:51]),cumprod(1-diffw_3$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-diffw_1$median[11:51]),cumprod(1-diffw_3$median[11:51])))))) + 10

#Long-term variability ----

#Minimum versus Median
which(abs(diff(rbind(cumprod(1-msdw_1$median[11:51]),cumprod(1-msdw_2$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-msdw_1$median[11:51]),cumprod(1-msdw_2$median[11:51])))))) + 10
#Median versus Maximum
which(abs(diff(rbind(cumprod(1-msdw_2$median[11:51]),cumprod(1-msdw_3$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-msdw_2$median[11:51]),cumprod(1-msdw_3$median[11:51])))))) + 10
#Minimum versus Maximum
which(abs(diff(rbind(cumprod(1-msdw_1$median[11:51]),cumprod(1-msdw_3$median[11:51])))) 
      == max(abs(diff(rbind(cumprod(1-msdw_1$median[11:51]),cumprod(1-msdw_3$median[11:51])))))) + 10

# Relative importance ----

#prepare data
relative <- precis(rds_int1,
                   depth=2,
                   pars=c("beta_wealth_sigma","gamma_wealth_sigma","delta_wealth_sigma"))

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,5,5))
#plot it!
plot(c(0,0.25),c(0,3),
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
segments(-0.01,0.5,0.26,0.5,lty="dashed",col="lightgrey")
segments(-0.01,1.5,0.26,1.5,lty="dashed",col="lightgrey")
segments(-0.01,2.5,0.26,2.5,lty="dashed",col="lightgrey")
points(relative[,1],seq(2.5,0.5,by=-1),cex=2,pch=16,col=hcl.colors(3,"berlin"))
segments(relative[1,1]-relative[1,2],2.5,relative[1,1]+relative[1,2],2.5,lwd=3,col=hcl.colors(3,"berlin")[1])
segments(relative[2,1]-relative[2,2],1.5,relative[2,1]+relative[2,2],1.5,lwd=3,col=hcl.colors(3,"berlin")[2])
segments(relative[3,1]-relative[3,2],0.5,relative[3,1]+relative[3,2],0.5,lwd=3,col=hcl.colors(3,"berlin")[3])
