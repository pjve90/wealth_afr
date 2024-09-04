# Model with current absolute levels of wealth ----

#The code in this script is meant to fit a Bayesian model that aims to predict the probability of first birth by the amount of wealth a woman has.

#Load packages
#install.packages("cmdstanr")
library(cmdstanr)
#install.packages("rethinking")
library(rethinking)
#install.packages("scales")
library(scales)
#install.packages("corrplot")
library(corrplot)

# Data wrangling of real data ----

#Load data
real_data <- read.csv("Data/dataf.csv")[,-1]
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
# Calculate the cumulative probabilities of first birth
cumulative_probs <- rep(NA,length.out=ncol(afr_matrix))
for (j in 1:ncol(afr_matrix)) {
  if (j == 1) {
    cumulative_probs[j] <- sum(afr_matrix[, j], na.rm = TRUE) / colSums(!is.na(afr_matrix))[j]
  } else {
    cumulative_probs[j] <- cumulative_probs[j - 1] + (sum(afr_matrix[, j], na.rm = TRUE) / colSums(!is.na(afr_matrix))[j]) * (1 - cumulative_probs[j - 1])
  }
}
#check data
cumulative_probs
#plot the CCDF of first birth
plot(cumulative_probs~c(1:length(cumulative_probs)),
     xlab="Age",
     ylab="Cumulative probability of first birth",
     ylim=c(0,1),
     type="b",
     col="black",
     pch=16
)

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
plot(NA,
     xlim=c(1,ncol(absw_matrix)),
     ylim=range(absw_matrix,na.rm=T),
     xlab="Age",
     ylab="Current absolute wealth"
)
for(i in 1:nrow(absw_matrix)){
  row_data <- absw_matrix[i, ]
  col_indices <- which(!is.na(row_data))
  points(col_indices, row_data[col_indices],col=alpha("black",0.5), pch = 16)
}

#standardise the log-transformed current absolute wealth
std_absw_matrix <- matrix(standardize(log(as.vector(absw_matrix))),ncol=ncol(absw_matrix),nrow=nrow(absw_matrix))
#check the data
std_absw_matrix
#check the age-specific average of standardised current absolute wealth
apply(std_absw_matrix,2,mean,na.rm=T)
#plot it
plot(NA,
     xlim=c(1,ncol(std_absw_matrix)),
     ylim=range(std_absw_matrix,na.rm=T),
     xlab="Age",
     ylab="Std. current absolute wealth"
)
for(i in 1:nrow(std_absw_matrix)){
  row_data <- std_absw_matrix[i, ]
  col_indices <- which(!is.na(row_data))
  points(col_indices, row_data[col_indices],col=alpha("black",0.5), pch = 16)
}

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
absolute_list <- list(N = nrow(afrs_restricted), #population size
                  A = ncol(afrs_restricted), #age
                  wealth = std_absw_restricted, #current absolute wealth
                  baby = afrs_restricted, #AFR
                  N_miss = nrow(wealth_miss_restricted), # number of missing values that need imputation
                  wealth_miss=wealth_miss_restricted) # matrix indicating missing wealth data
#check data
absolute_list

## Compile and fit model ----

# compile model

model_absolute <- cmdstan_model("Univariate/firstbaby_absonly.stan")

#fit model
fit_absolute <- model_absolute$sample(data = absolute_list, 
                            chains = 4, 
                            parallel_chains = 15, 
                            adapt_delta = 0.95,
                            max_treedepth = 13,
                            init = 0)
# save fit 
fit_absolute_csv <- rstan::read_stan_csv(fit_absolute$output_files())
saveRDS(fit_absolute_csv, "fit_absolute_output.rds")
#load RDS file
rds_absolute <- readRDS("fit_absolute_output.rds")
#extract samples
post_absolute <- extract.samples(rds_absolute)

## Model diagnostics ----

#check trace of all parameters
#alpha
rstan::traceplot(rds_absolute,pars="alpha")
#mu
traceplot(rds_absolute,pars="mu") 
#mu_raw
traceplot(rds_absolute,pars="mu_raw")
#mu_tau
rstan::traceplot(rds_absolute,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds_absolute,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds_absolute,pars="mu_delta")
#beta_wealth_z
traceplot(rds_absolute,pars="beta_wealth_z") 
#beta_wealth_sigma
traceplot(rds_absolute,pars="beta_wealth_sigma") 

#summary of the model
#create summary tables of the different parameters

#alpha and hiper priors of Gaussian process
#create summary table for alpha and hiper priors of Gaussian process
tab_absolute_alphagp <- precis(rds_absolute,depth=2,pars=c("alpha",
                                                   "mu_raw",
                                                   "mu_tau",
                                                   "mu_delta"))
#check table
tab_absolute_alphagp

#mu
#create summary table for mu
tab_absolute_mu <- precis(rds_absolute,depth=2,pars="mu")
#check table
tab_absolute_mu
#plot it!
plot(tab_absolute_mu)

#beta z
#create summary table for beta_z
tab_absolute_beta_z <- precis(rds_absolute,depth=2,pars="beta_wealth_z")
#check table
tab_absolute_beta_z
#plot it!
plot(tab_absolute_beta_z)

#beta sigma
#create summary table for beta_sigma
tab_absolute_beta_sigma <- precis(rds_absolute,depth=2,pars="beta_wealth_sigma")
#check table
tab_absolute_beta_sigma

#Distribution of wealth data

hist(post_absolute$wealth_full,
     freq=F,
     col=hcl.colors(3,"ag_Sunset")[1],
     main="Current wealth",
     xlab="Std. current wealth"
)

## Plot it ----

#simulate wealth values
simwealth_absw_absolute <- seq(from=round(min(post_absolute$wealth_full),1),to=round(max(post_absolute$wealth_full),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_absw_absolute
#get the deciles
deciles_absw <- as.numeric(quantile(simwealth_absw_absolute,seq(0,1,0.5)))
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
plot(c(0,1)~c(10,ncol(post_absolute$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Current levels\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("Min.","Med.", "Max."),col=palette_a,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2,box.col = NA)

#add lines
for(k in 1:(length(deciles_absw))){
  #create matrix to store the data
  p_absw_absolute <- matrix(nrow=nrow(post_absolute$mu),ncol=ncol(post_absolute$mu))
  p_absw_absolute
  #fill it in with values for age 25
  for(j in 1:ncol(post_absolute$mu)){
    for(i in 1:nrow(post_absolute$mu)){
      p_absw_absolute[i,j] <- inv_logit(post_absolute$alpha[i] + #inv logit because originally is logit
                                      post_absolute$mu[i,j] + #age
                                      (post_absolute$beta_wealth_z[i,j]*post_absolute$beta_wealth_sigma[i])*deciles_absw[k] ) #absolute wealth
    }
  }
  #check data
  p_absw_absolute
  #plot it!
  #prepare model prediction data
  plot_absw_absolute <- data.frame(age = 1:ncol(p_absw_absolute),
                               median = apply(p_absw_absolute, 2, median), 
                               upp = apply(p_absw_absolute, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                               low = apply(p_absw_absolute, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("absw_",k),plot_absw_absolute)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_absw <- numeric(length(plot_absw_absolute$median))
  cumulative_low_absw <- numeric(length(plot_absw_absolute$low))
  cumulative_upp_absw <- numeric(length(plot_absw_absolute$upp))
  #set the first probability
  cumulative_median_absw[1] <- plot_absw_absolute$median[1]
  cumulative_low_absw[1] <- plot_absw_absolute$low[1]
  cumulative_upp_absw[1] <- plot_absw_absolute$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_absw_absolute$median)) {
    cumulative_median_absw[a] <- cumulative_median_absw[a-1] + (1 - cumulative_median_absw[a-1]) * plot_absw_absolute$median[a]
    cumulative_low_absw[a] <- cumulative_low_absw[a-1] + (1 - cumulative_low_absw[a-1]) * plot_absw_absolute$low[a]
    cumulative_upp_absw[a] <- cumulative_upp_absw[a-1] + (1 - cumulative_upp_absw[a-1]) * plot_absw_absolute$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_absw_",k),cumulative_median_absw)
  assign(paste0("cumulative_low_absw_",k),cumulative_low_absw)
  assign(paste0("cumulative_upp_absw_",k),cumulative_upp_absw)
  
  #add median
  #add points
  points(cumulative_median_absw[11:51] ~ plot_absw_absolute$age[11:51], col=palette_a[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_absw[11:51] ~ plot_absw_absolute$age[11:51], col=palette_a[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_absw_absolute$age[11:51], rev(plot_absw_absolute$age[11:51])), c(cumulative_low_absw[11:51], rev(cumulative_upp_absw[11:51])), col=alpha(palette_a[k], 0.25), border=NA)
}
