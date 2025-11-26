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

# Data wrangling of real data ----

#Load data
#real_data <- read.csv("Data/dataf.csv")[,-1]
real_data <- read.csv(url("https://raw.githubusercontent.com/pjve90/wealth_afr/refs/heads/master/Data/dataf.csv"), header=T, sep=",", stringsAsFactors=F)[,-1] 

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

#Calculate the short-term and long-term wealth variability from the data ----

#short-term wealth variability
#create matrix
change_matrix <-  matrix(nrow = nrow(std_absw_matrix),ncol=ncol(std_absw_matrix))
#check matrix
change_matrix
#calculate the short-term wealth variability
for(i in 1:nrow(change_matrix)){
  for(j in 1:2){
    change_matrix[i,j] <- 0 #setting zero change at birth and first year, since wealth change is calculated with a 2-years lag
  }
  for(j in 3:ncol(change_matrix)){
    change_matrix[i,j] = abs(std_absw_matrix[i,j] - std_absw_matrix[i,j-2]) #calculating the 2-years lagged wealth change
  }
}
#check matrix
change_matrix
#check the age-specific average of standardised short-term wealth variability
apply(change_matrix,2,mean,na.rm=T)
#plot it
plot(NA,
     xlim=c(1,ncol(change_matrix)),
     ylim=range(change_matrix,na.rm=T),
     xlab="Age",
     ylab="Absolute 2-year lagged wealth change"
)
for(i in 1:nrow(change_matrix)){
  row_data <- change_matrix[i, ]
  col_indices <- which(!is.na(row_data))
  points(col_indices, row_data[col_indices],col=alpha("black",0.5), pch = 16)
}

#long-term wealth variability
#create matrix
msdw_matrix <-  matrix(nrow = nrow(std_absw_matrix),ncol=ncol(std_absw_matrix))
#check matrix
msdw_matrix
#calculate the long-term wealth variability
for(i in 1:nrow(msdw_matrix)){
  for(j in 1:10){
    msdw_matrix[i,j] <- 0 #setting zero standard deviation from birth until age 10 at birth and first year, since wealth change is calculated with a 10-years window
  }
  for(j in 11:ncol(msdw_matrix)){
    msdw_matrix[i,j] = sd(std_absw_matrix[i,(j-10):j],na.rm=T) #calculating the moving standard deviation with a 10-years window
  }
}
#check matrix
msdw_matrix
#check the age-specific average of standardised long-term wealth variability
apply(msdw_matrix,2,mean,na.rm=T)
#plot it
plot(NA,
     xlim=c(1,ncol(msdw_matrix)),
     ylim=range(msdw_matrix,na.rm=T),
     xlab="Age",
     ylab="Long-term wealth variability"
)
for(i in 1:nrow(msdw_matrix)){
  row_data <- msdw_matrix[i, ]
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
#check data
dim(wealth_miss)
#number of missing values
n_miss <- nrow(wealth_miss)
#check data
n_miss

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

#Calculate the median wealth per individual
median_wealth<-NA
for(i in 1:nrow(std_absw_matrix)){
  median_wealth[i]<-median(std_absw_matrix[i,which(std_absw_matrix[i,]!=-99)])
}
#check the data
median_wealth
#If there are individuals without wealth data, sample random values from a normal(0,1) distribution since the data is standardised
median_wealth[is.na(median_wealth)]<-rnorm(sum(is.na(median_wealth)),0,1)
#check the data
median_wealth
#standardise median wealth
std_median_wealth <- standardize(median_wealth)

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
wealth_miss_restricted
#number of missing values
n_miss_restricted <- nrow(wealth_miss_restricted)
n_miss_restricted

#put all the data together
#create dataset
real_list <- list(N = nrow(afrs_restricted), #population size
                  A = ncol(afrs_restricted), #age
                  wealth = std_absw_restricted, #current absolute wealth
                  baby = afrs_restricted, #AFR
                  N_miss = n_miss_restricted, # number of missing values that need imputation
                  wealth_miss=wealth_miss_restricted, # matrix indicating missing wealth data
                  median_wealth=std_median_wealth
) 
#check data
real_list

## Compile and fit model ----

# compile model

model_3w <- cmdstan_model("~/wealth_afr/Model_code/firstbaby_threewealth.stan")

#fit model
fit_real <- model_3w$sample(data = real_list,
                            chains = 4, 
                            parallel_chains = 15, 
                            adapt_delta = 0.99,
                            max_treedepth = 13,
                            iter_warmup = 2000,
                            iter_sampling = 2000,
                            init = 0)

# save fit 
fit_real_csv <- rstan::read_stan_csv(fit_real$output_files())
saveRDS(fit_real_csv, "fit_real_output.rds")
#load RDS file
rds_real <- readRDS("fit_real_output.rds")
#extract samples
post_real <- extract.samples(rds_real)

## Model diagnostics ----

#check trace of all parameters
#alpha
rstan::traceplot(rds_real,pars="alpha")
#mu
traceplot(rds_real,pars="mu") 
#mu_raw
traceplot(rds_real,pars="mu_raw")
#mu_tau
rstan::traceplot(rds_real,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds_real,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds_real,pars="mu_delta")
#beta_wealth_z
rstan::traceplot(rds_real,pars="beta_wealth_z") 
#beta_wealth_sigma
rstan::traceplot(rds_real,pars="beta_wealth_sigma") 
#gamma_wealth
rstan::traceplot(rds_real,pars="gamma_wealth_z") 
#gamma_wealth
rstan::traceplot(rds_real,pars="gamma_wealth_sigma") 
#delta_wealth
rstan::traceplot(rds_real,pars="delta_wealth_z") 
#delta_wealth
rstan::traceplot(rds_real,pars="delta_wealth_sigma") 

#summary of the model
#create summary tables of the different parameters

#alpha and hiper priors of Gaussian process
#create summary table for alpha and hiper priors of Gaussian process
tab_real_alphagp <- precis(rds_real,depth=2,pars=c("alpha",
                                                   "mu_raw",
                                                   "mu_tau",
                                                   "mu_delta"))
#check table
tab_real_alphagp

#mu
#create summary table for mu
tab_real_mu <- precis(rds_real,depth=2,pars="mu")
#check table
tab_real_mu
#plot it!
plot(tab_real_mu)

#beta z
#create summary table for beta_z
tab_real_beta_z <- precis(rds_real,depth=2,pars="beta_wealth_z")
#check table
tab_real_beta_z
#plot it!
plot(tab_real_beta_z)

#beta sigma
#create summary table for beta_sigma
tab_real_beta_sigma <- precis(rds_real,depth=2,pars="beta_wealth_sigma")
#check table
tab_real_beta_sigma

#gamma z
#create summary table for gamma_z
tab_real_gamma_z <- precis(rds_real,depth=2,pars="gamma_wealth_z")
#check table
tab_real_gamma_z
#plot it!
plot(tab_real_gamma_z)

#gamma sigma
#create summary table for gamma_sigma
tab_real_gamma_sigma <- precis(rds_real,depth=2,pars="gamma_wealth_sigma")
#check table
tab_real_gamma_sigma

#delta z
#create summary table for delta_z
tab_real_delta_z <- precis(rds_real,depth=2,pars="delta_wealth_z")
#check table
tab_real_delta_z
#plot it!
plot(tab_real_delta_z)

#delta sigma
#create summary table for delta_sigma
tab_real_delta_sigma <- precis(rds_real,depth=2,pars="delta_wealth_sigma")
#check table
tab_real_delta_sigma

#Check correlation between wealth predictors

#beta versus gamma
#create correlation matrix
cor1 <- round(cor(post_real$beta_wealth_z,post_real$gamma_wealth_z),3)
#plot it!
corrplot(cor1, "color", tl.col="black")

#beta versus delta
#create correlation matrix
cor2 <- round(cor(post_real$beta_wealth_z,post_real$delta_wealth_z),3)
#plot it!
corrplot(cor2, "color", tl.col="black")

#gamma versus delta
#create correlation matrix
cor3 <- round(cor(post_real$gamma_wealth_z,post_real$delta_wealth_z),3)
#plot it!
corrplot(cor3, "color", tl.col="black")

#Distribution of wealth data

par(mfrow=c(1,3))
hist(post_real$wealth_full,
     freq=F,
     col=hcl.colors(3,"ag_Sunset")[1],
     main="Current wealth",
     xlab="Std. current wealth"
)
hist(post_real$wealth_change_std,
     freq=F,
     col=hcl.colors(3,"ag_Sunset")[2],
     main="Short-term wealth variability",
     xlab="Absolute wealth difference"
)
hist(post_real$wealth_msd_std,
     freq=F,
     col=hcl.colors(3,"ag_Sunset")[3],
     main="Long-term wealth variability",
     xlab="Moving standard deviation"
)

# Plot the output of the model ----

## Current Wealth ----

#simulate wealth values
simwealth_absw_real <- seq(from=round(min(post_real$wealth_full),1),to=round(max(post_real$wealth_full),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_absw_real
#get the deciles
deciles_absw <- as.numeric(quantile(simwealth_absw_real,seq(0,1,0.5)))
deciles_absw

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_a<-palette[1:length(deciles_absw)]
palette_a

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,15))

#plot empty plot
plot(c(0,1)~c(11,ncol(post_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Current levels\nof material wealth",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_real$mu), by = 5), 
     labels = seq(10, ncol(post_real$mu)-1, by = 5),
     cex.axis=1.5
)
# Combined legend
legend(53,1,  # or specify x/y coordinates
       legend = c("Cumulative\nprobabilities","", "Min.", "Med.", "Max.",  # First group
                  "",
                  "Expected age\nat first birth","", "Min.", "Med.", "Max."), # Second group
       col = c(NA,NA, palette_a,NA,NA,NA,palette_a),  # NA for section headers
       lwd = 3,
       lty = c(NA,NA, c(1,1,1),NA,NA, NA, c(2,2,2)),  # NA for headers
       pch = c(NA,NA, rep(16, 3),NA,NA, NA, rep(NA, 3)),    # Symbols for Estimated only
       pt.cex = 1.5,
       cex = 1.2,
       box.col = NA,
       ncol = 1,  # Optional: arrange in 2 columns
       title = NULL)

#add lines
for(k in 1:(length(deciles_absw))){
  #create matrix to store the data
  p_absw_real <- matrix(nrow=nrow(post_real$mu),ncol=ncol(post_real$mu))
  p_absw_real
  #fill it in with values for age 25
  for(j in 1:ncol(post_real$mu)){
    for(i in 1:nrow(post_real$mu)){
      p_absw_real[i,j] <- inv_logit(post_real$alpha[i] + #inv logit because originally is logit
                                      post_real$mu[i,j] + #age
                                      (post_real$beta_wealth_z[i,j]*post_real$beta_wealth_sigma[i])*deciles_absw[k] + #absolute wealth
                                      (post_real$gamma_wealth_z[i,j]*post_real$gamma_wealth_sigma[i])*0 + #wealth change
                                      (post_real$delta_wealth_z[i,j]*post_real$delta_wealth_sigma[i])*0) #moving variance
    }
  }
  #check data
  p_absw_real
  #store data per decile
  assign(paste0("post_absw_real_", k), p_absw_real)
  
  #get cumulative probabilities for later comparison
  cumulative_posterior <- matrix(nrow = nrow(p_absw_real), ncol = ncol(p_absw_real))
  cumulative_posterior[,1] <- p_absw_real[,1]
  for (a in 2:ncol(p_absw_real)) {
    cumulative_posterior[,a] <- cumulative_posterior[,a-1] + 
      (1 - cumulative_posterior[,a-1]) * p_absw_real[,a]
  }
  #store data per decile
  assign(paste0("cumulative_posterior_absw_", k), cumulative_posterior)
  
  #plot it!
  #prepare model prediction data
  plot_absw_real <- data.frame(age = 1:ncol(p_absw_real),
                               median = apply(p_absw_real, 2, median), 
                               upp = apply(p_absw_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                               low = apply(p_absw_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("absw_",k),plot_absw_real)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_absw <- numeric(length(plot_absw_real$median))
  cumulative_low_absw <- numeric(length(plot_absw_real$low))
  cumulative_upp_absw <- numeric(length(plot_absw_real$upp))
  #set the first probability
  cumulative_median_absw[1] <- plot_absw_real$median[1]
  cumulative_low_absw[1] <- plot_absw_real$low[1]
  cumulative_upp_absw[1] <- plot_absw_real$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_absw_real$median)) {
    cumulative_median_absw[a] <- cumulative_median_absw[a-1] + (1 - cumulative_median_absw[a-1]) * plot_absw_real$median[a]
    cumulative_low_absw[a] <- cumulative_low_absw[a-1] + (1 - cumulative_low_absw[a-1]) * plot_absw_real$low[a]
    cumulative_upp_absw[a] <- cumulative_upp_absw[a-1] + (1 - cumulative_upp_absw[a-1]) * plot_absw_real$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_absw_",k),cumulative_median_absw)
  assign(paste0("cumulative_low_absw_",k),cumulative_low_absw)
  assign(paste0("cumulative_upp_absw_",k),cumulative_upp_absw)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_absw < 0.5))
  age_after <- min(which(cumulative_median_absw >= 0.5))
  prob_before <- cumulative_median_absw[age_before]
  prob_after <- cumulative_median_absw[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_absw_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_absw[11:51] ~ plot_absw_real$age[11:51], col=palette_a[k], pch=16, cex=1.5)
  #add lines
  lines(cumulative_median_absw[11:51] ~ plot_absw_real$age[11:51], col=palette_a[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_absw_real$age[11:51], rev(plot_absw_real$age[11:51])), c(cumulative_low_absw[11:51], rev(cumulative_upp_absw[11:51])), col=alpha(palette_a[k], 0.1), border=alpha(palette_a[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_a[k], lwd=3, lty=2)
}

## Short-term wealth variability ----

#simulate wealth values
simwealth_change_real <- seq(from=round(min(post_real$wealth_change_std),1),to=round(max(post_real$wealth_change_std),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_change_real
#get the deciles
deciles_diffw <- as.numeric(quantile(simwealth_change_real,seq(0,1,0.5)))
deciles_diffw

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_b<-palette[4:(length(deciles_diffw)+3)]
palette_b

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,15))

#plot empty plot
plot(c(0,1)~c(11,ncol(post_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Short-term\nwealth variability",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_real$mu), by = 5), 
     labels = seq(10, ncol(post_real$mu)-1, by = 5),
     cex.axis=1.5
)
# Combined legend
legend(53,1,  # or specify x/y coordinates
       legend = c("Cumulative\nprobabilities","", "Min.", "Med.", "Max.",  # First group
                  "",
                  "Expected age\nat first birth","", "Min.", "Med.", "Max."), # Second group
       col = c(NA,NA, palette_b,NA,NA,NA,palette_b),  # NA for section headers
       lwd = 3,
       lty = c(NA,NA, rep(1, 3),NA,NA, NA, rep(2, 3)),  # NA for headers
       pch = c(NA,NA, rep(16, 3),NA,NA, NA, rep(NA, 3)),    # Symbols for Estimated only
       pt.cex = 1.5,
       cex = 1.2,
       box.col = NA,
       ncol = 1,  # Optional: arrange in 2 columns
       title = NULL)

#add lines
for(k in 1:(length(deciles_diffw))){
  #create matrix to store the data
  p_diffw_real <- matrix(nrow=nrow(post_real$mu),ncol=ncol(post_real$mu))
  p_diffw_real
  #fill it in with values for age 25
  for(j in 1:ncol(post_real$mu)){
    for(i in 1:nrow(post_real$mu)){
      p_diffw_real[i,j] <- inv_logit(post_real$alpha[i] + #inv logit because originally is logit
                                       post_real$mu[i,j] + #age
                                       (post_real$beta_wealth_z[i,j]*post_real$beta_wealth_sigma[i])*0+ #absolute wealth
                                       (post_real$gamma_wealth_z[i,j]*post_real$gamma_wealth_sigma[i])*deciles_diffw[k] + #wealth change
                                       (post_real$delta_wealth_z[i,j]*post_real$delta_wealth_sigma[i])*0) #moving variance
    }
  }
  #check data
  p_diffw_real
  #store data per decile
  assign(paste0("post_diffw_real_", k), p_diffw_real)
  
  #get cumulative probabilities for later comparison
  cumulative_posterior <- matrix(nrow = nrow(p_diffw_real), ncol = ncol(p_diffw_real))
  cumulative_posterior[,1] <- p_diffw_real[,1]
  for (a in 2:ncol(p_diffw_real)) {
    cumulative_posterior[,a] <- cumulative_posterior[,a-1] + 
      (1 - cumulative_posterior[,a-1]) * p_diffw_real[,a]
  }
  #store data per decile
  assign(paste0("cumulative_posterior_diffw_", k), cumulative_posterior)
  
  #plot it!
  #prepare model prediction data
  plot_diffw_real <- data.frame(age = 1:ncol(p_diffw_real),
                                median = apply(p_diffw_real, 2, median), 
                                upp = apply(p_diffw_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p_diffw_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("diffw_",k),plot_diffw_real)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diffw <- numeric(length(plot_diffw_real$median))
  cumulative_low_diffw <- numeric(length(plot_diffw_real$low))
  cumulative_upp_diffw <- numeric(length(plot_diffw_real$upp))
  #set the first probability
  cumulative_median_diffw[1] <- plot_diffw_real$median[1]
  cumulative_low_diffw[1] <- plot_diffw_real$low[1]
  cumulative_upp_diffw[1] <- plot_diffw_real$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diffw_real$median)) {
    cumulative_median_diffw[a] <- cumulative_median_diffw[a-1] + (1 - cumulative_median_diffw[a-1]) * plot_diffw_real$median[a]
    cumulative_low_diffw[a] <- cumulative_low_diffw[a-1] + (1 - cumulative_low_diffw[a-1]) * plot_diffw_real$low[a]
    cumulative_upp_diffw[a] <- cumulative_upp_diffw[a-1] + (1 - cumulative_upp_diffw[a-1]) * plot_diffw_real$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diffw_",k),cumulative_median_diffw)
  assign(paste0("cumulative_low_diffw_",k),cumulative_low_diffw)
  assign(paste0("cumulative_upp_diffw_",k),cumulative_upp_diffw)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_diffw < 0.5))
  age_after <- min(which(cumulative_median_diffw >= 0.5))
  prob_before <- cumulative_median_diffw[age_before]
  prob_after <- cumulative_median_diffw[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_diffw_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_diffw[11:51] ~ plot_diffw_real$age[11:51], col=palette_b[k], pch=16, cex=1.5)
  #add lines
  lines(cumulative_median_diffw[11:51] ~ plot_diffw_real$age[11:51], col=palette_b[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_diffw_real$age[11:51], rev(plot_diffw_real$age[11:51])), c(cumulative_low_diffw[11:51], rev(cumulative_upp_diffw[11:51])), col=alpha(palette_b[k], 0.1), border=alpha(palette_b[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_b[k], lwd=3, lty=2)
}

## Long-term variability of wealth ----

#simulate wealth values
simwealth_msd_real <- seq(from=round(min(post_real$wealth_msd_std),1),to=round(max(post_real$wealth_msd_std),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_msd_real
#get the deciles
deciles_msd <- as.numeric(quantile(simwealth_msd_real,seq(0,1,0.5)))
deciles_msd

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_c<-palette[7:(length(deciles_msd)+6)]
palette_c

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,15))

#plot empty plot
plot(c(0,1)~c(11,ncol(post_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Long-term\nwealth variability",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_real$mu), by = 5), 
     labels = seq(10, ncol(post_real$mu)-1, by = 5),
     cex.axis=1.5
)

# Combined legend
legend(53,1,  # or specify x/y coordinates
       legend = c("Cumulative\nprobabilities","", "Min.", "Med.", "Max.",  # First group
                  "",
                  "Expected age\nat first birth","", "Min.", "Med.", "Max."), # Second group
       col = c(NA,NA, palette_c,NA,NA,NA,palette_c),  # NA for section headers
       lwd = 3,
       lty = c(NA,NA, rep(1, 3),NA,NA, NA, rep(2, 3)),  # NA for headers
       pch = c(NA,NA, rep(16, 3),NA,NA, NA, rep(NA, 3)),    # Symbols for Estimated only
       pt.cex = 1.5,
       cex = 1.2,
       box.col = NA,
       ncol = 1,  # Optional: arrange in 2 columns
       title = NULL)

#add lines
for(k in 1:(length(deciles_msd))){
  #create matrix to store the data
  p_msd_real <- matrix(nrow=nrow(post_real$mu),ncol=ncol(post_real$mu))
  p_msd_real
  #fill it in with values for age 25
  for(j in 1:ncol(post_real$mu)){
    for(i in 1:nrow(post_real$mu)){
      p_msd_real[i,j] <- inv_logit(post_real$alpha[i] + #inv logit because originally is logit
                                     post_real$mu[i,j] + #age
                                     (post_real$beta_wealth_z[i,j]*post_real$beta_wealth_sigma[i])*0+ #absolute wealth
                                     (post_real$gamma_wealth_z[i,j]*post_real$gamma_wealth_sigma[i])*0 + #wealth change
                                     (post_real$delta_wealth_z[i,j]*post_real$delta_wealth_sigma[i])*deciles_msd[k]) #moving variance
    }
  }
  #check data
  p_msd_real
  #store data per decile
  assign(paste0("post_msd_real_", k), p_msd_real)
  
  #get cumulative probabilities for later comparison
  cumulative_posterior <- matrix(nrow = nrow(p_msd_real), ncol = ncol(p_msd_real))
  cumulative_posterior[,1] <- p_msd_real[,1]
  for (a in 2:ncol(p_msd_real)) {
    cumulative_posterior[,a] <- cumulative_posterior[,a-1] + 
      (1 - cumulative_posterior[,a-1]) * p_msd_real[,a]
  }
  #store data per decile
  assign(paste0("cumulative_posterior_msd_", k), cumulative_posterior)
  
  #plot it!
  #prepare model prediction data
  plot_msd_real <- data.frame(age = 1:ncol(p_msd_real),
                              median = apply(p_msd_real, 2, median), 
                              upp = apply(p_msd_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p_msd_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_msd_real)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_msd <- numeric(length(plot_msd_real$median))
  cumulative_low_msd <- numeric(length(plot_msd_real$low))
  cumulative_upp_msd <- numeric(length(plot_msd_real$upp))
  #set the first probability
  cumulative_median_msd[1] <- plot_msd_real$median[1]
  cumulative_low_msd[1] <- plot_msd_real$low[1]
  cumulative_upp_msd[1] <- plot_msd_real$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_msd_real$median)) {
    cumulative_median_msd[a] <- cumulative_median_msd[a-1] + (1 - cumulative_median_msd[a-1]) * plot_msd_real$median[a]
    cumulative_low_msd[a] <- cumulative_low_msd[a-1] + (1 - cumulative_low_msd[a-1]) * plot_msd_real$low[a]
    cumulative_upp_msd[a] <- cumulative_upp_msd[a-1] + (1 - cumulative_upp_msd[a-1]) * plot_msd_real$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_msd_",k),cumulative_median_msd)
  assign(paste0("cumulative_low_msd_",k),cumulative_low_msd)
  assign(paste0("cumulative_upp_msd_",k),cumulative_upp_msd)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_msd < 0.5))
  age_after <- min(which(cumulative_median_msd >= 0.5))
  prob_before <- cumulative_median_msd[age_before]
  prob_after <- cumulative_median_msd[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_msd_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_msd[11:51] ~ plot_msd_real$age[11:51], col=palette_c[k], pch=16, cex=1.5)
  #add lines
  lines(cumulative_median_msd[11:51] ~ plot_msd_real$age[11:51], col=palette_c[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_msd_real$age[11:51], rev(plot_msd_real$age[11:51])), c(cumulative_low_msd[11:51], rev(cumulative_upp_msd[11:51])), col=alpha(palette_c[k], 0.1), border=alpha(palette_c[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_c[k], lwd=3, lty=2)
}

# Expected median age ----

### Current absolute wealth ----

#Minimum current absolute wealth

#Expected median age at first birth
exact_age_median_absw_1 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_absw_1 <- floor(exact_age_median_absw_1)
age_after_absw_1 <- ceiling(exact_age_median_absw_1)
# Extract full posterior distributions (not just medians)
#beta
beta_before_samples <- post_real$beta_wealth_z[, age_before_absw_1] * post_real$beta_wealth_sigma
beta_after_samples <- post_real$beta_wealth_z[, age_after_absw_1] * post_real$beta_wealth_sigma
# Interpolate each MCMC sample
#beta
beta_interp_samples <- beta_before_samples + 
  (exact_age_median_absw_1 - age_before_absw_1) * 
  (beta_after_samples - beta_before_samples) / 
  (age_after_absw_1 - age_before_absw_1)
# Calculate medians and HPDI
#beta
beta_median_absw_1 <- median(beta_interp_samples)
beta_median_absw_1
beta_hpdi_absw_1 <- HPDI(beta_interp_samples, prob = 0.9)
beta_hpdi_absw_1

#Median current absolute wealth
exact_age_median_absw_2 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_absw_2 <- floor(exact_age_median_absw_2)
age_after_absw_2 <- ceiling(exact_age_median_absw_2)
# Extract full posterior distributions (not just medians)
#beta
beta_before_samples <- post_real$beta_wealth_z[, age_before_absw_2] * post_real$beta_wealth_sigma
beta_after_samples <- post_real$beta_wealth_z[, age_after_absw_2] * post_real$beta_wealth_sigma
# Interpolate each MCMC sample
#beta
beta_interp_samples <- beta_before_samples + 
  (exact_age_median_absw_2 - age_before_absw_2) * 
  (beta_after_samples - beta_before_samples) / 
  (age_after_absw_2 - age_before_absw_2)
# Calculate medians and HPDI
#beta
beta_median_absw_2 <- median(beta_interp_samples)
beta_median_absw_2
beta_hpdi_absw_2 <- HPDI(beta_interp_samples, prob = 0.9)
beta_hpdi_absw_2

#Maximum current absolute wealth
exact_age_median_absw_3 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_absw_3 <- floor(exact_age_median_absw_3)
age_after_absw_3 <- ceiling(exact_age_median_absw_3)
# Extract full posterior distributions (not just medians)
#beta
beta_before_samples <- post_real$beta_wealth_z[, age_before_absw_3] * post_real$beta_wealth_sigma
beta_after_samples <- post_real$beta_wealth_z[, age_after_absw_3] * post_real$beta_wealth_sigma
# Interpolate each MCMC sample
#beta
beta_interp_samples <- beta_before_samples + 
  (exact_age_median_absw_3 - age_before_absw_3) * 
  (beta_after_samples - beta_before_samples) / 
  (age_after_absw_3 - age_before_absw_3)
# Calculate medians and HPDI
#beta
beta_median_absw_3 <- median(beta_interp_samples)
beta_median_absw_3
beta_hpdi_absw_3 <- HPDI(beta_interp_samples, prob = 0.9)
beta_hpdi_absw_3

### Short-term variability ----

#Minimum short-term wealth variability
exact_age_median_diffw_1 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_diffw_1 <- floor(exact_age_median_diffw_1)
age_after_diffw_1 <- ceiling(exact_age_median_diffw_1)
# Extract full posterior distributions (not just medians)
#gamma
gamma_before_samples <- post_real$gamma_wealth_z[, age_before_diffw_1] * post_real$gamma_wealth_sigma
gamma_after_samples <- post_real$gamma_wealth_z[, age_after_diffw_1] * post_real$gamma_wealth_sigma
# Interpolate each MCMC sample
#gamma
gamma_interp_samples <- gamma_before_samples + 
  (exact_age_median_diffw_1 - age_before_diffw_1) * 
  (gamma_after_samples - gamma_before_samples) / 
  (age_after_diffw_1 - age_before_diffw_1)
# Calculate medians and HPDI
#gamma
gamma_median_diffw_1 <- median(gamma_interp_samples)
gamma_median_diffw_1
gamma_hpdi_diffw_1 <- HPDI(gamma_interp_samples, prob = 0.9)
gamma_hpdi_diffw_1

#Median short-term wealth variability
exact_age_median_diffw_2 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_diffw_2 <- floor(exact_age_median_diffw_2)
age_after_diffw_2 <- ceiling(exact_age_median_diffw_2)
# Extract full posterior distributions (not just medians)
#gamma
gamma_before_samples <- post_real$gamma_wealth_z[, age_before_diffw_2] * post_real$gamma_wealth_sigma
gamma_after_samples <- post_real$gamma_wealth_z[, age_after_diffw_2] * post_real$gamma_wealth_sigma
# Interpolate each MCMC sample
#gamma
gamma_interp_samples <- gamma_before_samples + 
  (exact_age_median_diffw_2 - age_before_diffw_2) * 
  (gamma_after_samples - gamma_before_samples) / 
  (age_after_diffw_2 - age_before_diffw_2)
# Calculate medians and HPDI
#gamma
gamma_median_diffw_2 <- median(gamma_interp_samples)
gamma_median_diffw_2
gamma_hpdi_diffw_2 <- HPDI(gamma_interp_samples, prob = 0.9)
gamma_hpdi_diffw_2

#Maximum short-term wealth variability
exact_age_median_diffw_3 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_diffw_3 <- floor(exact_age_median_diffw_3)
age_after_diffw_3 <- ceiling(exact_age_median_diffw_3)
# Extract full posterior distributions (not just medians)
#gamma
gamma_before_samples <- post_real$gamma_wealth_z[, age_before_diffw_3] * post_real$gamma_wealth_sigma
gamma_after_samples <- post_real$gamma_wealth_z[, age_after_diffw_3] * post_real$gamma_wealth_sigma
# Interpolate each MCMC sample
#gamma
gamma_interp_samples <- gamma_before_samples + 
  (exact_age_median_diffw_3 - age_before_diffw_3) * 
  (gamma_after_samples - gamma_before_samples) / 
  (age_after_diffw_3 - age_before_diffw_3)
# Calculate medians and HPDI
#gamma
gamma_median_diffw_3 <- median(gamma_interp_samples)
gamma_median_diffw_3
gamma_hpdi_diffw_3 <- HPDI(gamma_interp_samples, prob = 0.9)
gamma_hpdi_diffw_3

### Long-term variability ----

#Minimum long-term wealth variability
exact_age_median_msd_1 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_msd_1 <- floor(exact_age_median_msd_1)
age_after_msd_1 <- ceiling(exact_age_median_msd_1)
# Extract full posterior distributions (not just medians)
#delta
delta_before_samples <- post_real$delta_wealth_z[, age_before_msd_1] * post_real$delta_wealth_sigma
delta_after_samples <- post_real$delta_wealth_z[, age_after_msd_1] * post_real$delta_wealth_sigma
# Interpolate each MCMC sample
#delta
delta_interp_samples <- delta_before_samples + 
  (exact_age_median_msd_1 - age_before_msd_1) * 
  (delta_after_samples - delta_before_samples) / 
  (age_after_msd_1 - age_before_msd_1)
# Calculate medians and HPDI
#delta
delta_median_msd_1 <- median(delta_interp_samples)
delta_median_msd_1
delta_hpdi_msd_1 <- HPDI(delta_interp_samples, prob = 0.9)
delta_hpdi_msd_1

#Median long-term wealth variability
exact_age_median_msd_2 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_msd_2 <- floor(exact_age_median_msd_2)
age_after_msd_2 <- ceiling(exact_age_median_msd_2)
# Extract full posterior distributions (not just medians)
#delta
delta_before_samples <- post_real$delta_wealth_z[, age_before_msd_2] * post_real$delta_wealth_sigma
delta_after_samples <- post_real$delta_wealth_z[, age_after_msd_2] * post_real$delta_wealth_sigma
# Interpolate each MCMC sample
#delta
delta_interp_samples <- delta_before_samples + 
  (exact_age_median_msd_2 - age_before_msd_2) * 
  (delta_after_samples - delta_before_samples) / 
  (age_after_msd_2 - age_before_msd_2)
# Calculate medians and HPDI
#delta
delta_median_msd_2 <- median(delta_interp_samples)
delta_median_msd_2
delta_hpdi_msd_2 <- HPDI(delta_interp_samples, prob = 0.9)
delta_hpdi_msd_2

#Maximum long-term wealth variability
exact_age_median_msd_3 -1

#Coefficients
# Get the bounding ages in R indexing
age_before_msd_3 <- floor(exact_age_median_msd_3)
age_after_msd_3 <- ceiling(exact_age_median_msd_3)
# Extract full posterior distributions (not just medians)
#delta
delta_before_samples <- post_real$delta_wealth_z[, age_before_msd_3] * post_real$delta_wealth_sigma
delta_after_samples <- post_real$delta_wealth_z[, age_after_msd_3] * post_real$delta_wealth_sigma
# Interpolate each MCMC sample
#delta
delta_interp_samples <- delta_before_samples + 
  (exact_age_median_msd_3 - age_before_msd_3) * 
  (delta_after_samples - delta_before_samples) / 
  (age_after_msd_3 - age_before_msd_3)
# Calculate medians and HPDI
#delta
delta_median_msd_3 <- median(delta_interp_samples)
delta_median_msd_3
delta_hpdi_msd_3 <- HPDI(delta_interp_samples, prob = 0.9)
delta_hpdi_msd_3

#Difference between ages at first births ----

#Current wealth
#minimum versus medium
abs(diff(c(exact_age_median_absw_1 -1,exact_age_median_absw_2 -1,exact_age_median_absw_3 -1,exact_age_median_absw_1 -1,exact_age_median_absw_3 -1))[1])
#medium versus maximum
abs(diff(c(exact_age_median_absw_1 -1,exact_age_median_absw_2 -1,exact_age_median_absw_3 -1,exact_age_median_absw_1 -1,exact_age_median_absw_3 -1))[2])
#minimum versus maximum
abs(diff(c(exact_age_median_absw_1 -1,exact_age_median_absw_2 -1,exact_age_median_absw_3 -1,exact_age_median_absw_1 -1,exact_age_median_absw_3 -1))[4])

#Short-term wealth variability
#minimum versus medium
abs(diff(c(exact_age_median_diffw_1 -1,exact_age_median_diffw_2 -1,exact_age_median_diffw_3 -1,exact_age_median_diffw_1 -1,exact_age_median_diffw_3 -1))[1])
#medium versus maximum
abs(diff(c(exact_age_median_diffw_1 -1,exact_age_median_diffw_2 -1,exact_age_median_diffw_3 -1,exact_age_median_diffw_1 -1,exact_age_median_diffw_3 -1))[2])
#minimum versus maximum
abs(diff(c(exact_age_median_diffw_1 -1,exact_age_median_diffw_2 -1,exact_age_median_diffw_3 -1,exact_age_median_diffw_1 -1,exact_age_median_diffw_3 -1))[4])

#Long-term wealth variability
#minimum versus medium
abs(diff(c(exact_age_median_msd_1 -1,exact_age_median_msd_2 -1,exact_age_median_msd_3 -1,exact_age_median_msd_1 -1,exact_age_median_msd_3 -1))[1])
#medium versus maximum
abs(diff(c(exact_age_median_msd_1 -1,exact_age_median_msd_2 -1,exact_age_median_msd_3 -1,exact_age_median_msd_1 -1,exact_age_median_msd_3 -1))[2])
#minimum versus maximum
abs(diff(c(exact_age_median_msd_1 -1,exact_age_median_msd_2 -1,exact_age_median_msd_3 -1,exact_age_median_msd_1 -1,exact_age_median_msd_3 -1))[4])

# Differences within wealth classes ----

## Current absolute wealth -----

# Function to calculate within-wealth differences with proper HPDI
calculate_within_differences <- function(wealth_level) {
  cumulative_posterior <- get(paste0("cumulative_posterior_absw_", wealth_level))
  
  # Calculate absolute differences for each posterior sample (ages 11:51)
  abs_diffs_posterior <- t(apply(cumulative_posterior[,11:51], 1, diff))
  abs_diffs_posterior <- abs(abs_diffs_posterior)
  
  # Find largest absolute difference AND its position for each posterior sample
  max_diffs <- apply(abs_diffs_posterior, 1, max, na.rm = TRUE)
  max_positions <- apply(abs_diffs_posterior, 1, which.max)
  
  # Convert positions to actual ages (age interval is between age and age+1)
  max_ages <- max_positions + 10  # +10 because we started at age 11
  
  # Summary statistics
  median_max_diff <- median(max_diffs)
  max_diff_hpdi <- HPDI(max_diffs, prob = 0.9)
  
  median_max_age <- median(max_ages)
  max_age_hpdi <- HPDI(max_ages, prob = 0.9)
  
  return(list(
    median_largest_diff = median_max_diff,
    hpdi_largest_diff = max_diff_hpdi,
    median_age_of_largest_diff = median_max_age,
    hpdi_age_of_largest_diff = max_age_hpdi,
    all_max_differences = max_diffs,
    all_max_ages = max_ages
  ))
}

# Minimum current absolute wealth 
min_absw <- calculate_within_differences(1)
cat("Minimum wealth:\n")
cat("Largest absolute difference:", min_absw$median_largest_diff, "\n")
cat("HPDI of difference:", min_absw$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", min_absw$median_age_of_largest_diff, 
    "and", min_absw$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", min_absw$hpdi_age_of_largest_diff, "\n\n")

# Median current absolute wealth 
med_absw <- calculate_within_differences(2)
cat("Median wealth:\n")
cat("Largest absolute difference:", med_absw$median_largest_diff, "\n")
cat("HPDI of difference:", med_absw$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", med_absw$median_age_of_largest_diff, 
    "and", med_absw$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", med_absw$hpdi_age_of_largest_diff, "\n\n")

# Maximum current absolute wealth 
max_absw <- calculate_within_differences(3)
cat("Maximum wealth:\n")
cat("Largest absolute difference:", max_absw$median_largest_diff, "\n")
cat("HPDI of difference:", max_absw$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", max_absw$median_age_of_largest_diff, 
    "and", max_absw$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", max_absw$hpdi_age_of_largest_diff, "\n")

## Short-term wealth variability -----

calculate_within_differences <- function(wealth_level) {
  cumulative_posterior <- get(paste0("cumulative_posterior_diffw_", wealth_level))
  
  # Calculate absolute differences for each posterior sample (ages 11:51)
  abs_diffs_posterior <- t(apply(cumulative_posterior[,11:51], 1, diff))
  abs_diffs_posterior <- abs(abs_diffs_posterior)
  
  # Find largest absolute difference AND its position for each posterior sample
  max_diffs <- apply(abs_diffs_posterior, 1, max, na.rm = TRUE)
  max_positions <- apply(abs_diffs_posterior, 1, which.max)
  
  # Convert positions to actual ages (age interval is between age and age+1)
  max_ages <- max_positions + 10  # +10 because we started at age 11
  
  # Summary statistics
  median_max_diff <- median(max_diffs)
  max_diff_hpdi <- HPDI(max_diffs, prob = 0.9)
  
  median_max_age <- median(max_ages)
  max_age_hpdi <- HPDI(max_ages, prob = 0.9)
  
  return(list(
    median_largest_diff = median_max_diff,
    hpdi_largest_diff = max_diff_hpdi,
    median_age_of_largest_diff = median_max_age,
    hpdi_age_of_largest_diff = max_age_hpdi,
    all_max_differences = max_diffs,
    all_max_ages = max_ages
  ))
}

# Minimum current absolute wealth 
min_diffw <- calculate_within_differences(1)
cat("Minimum wealth:\n")
cat("Largest absolute difference:", min_diffw$median_largest_diff, "\n")
cat("HPDI of difference:", min_diffw$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", min_diffw$median_age_of_largest_diff, 
    "and", min_diffw$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", min_diffw$hpdi_age_of_largest_diff, "\n\n")

# Median current absolute wealth 
med_diffw <- calculate_within_differences(2)
cat("Median wealth:\n")
cat("Largest absolute difference:", med_diffw$median_largest_diff, "\n")
cat("HPDI of difference:", med_diffw$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", med_diffw$median_age_of_largest_diff, 
    "and", med_diffw$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", med_diffw$hpdi_age_of_largest_diff, "\n\n")

# Maximum current absolute wealth 
max_diffw <- calculate_within_differences(3)
cat("Maximum wealth:\n")
cat("Largest absolute difference:", max_diffw$median_largest_diff, "\n")
cat("HPDI of difference:", max_diffw$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", max_diffw$median_age_of_largest_diff, 
    "and", max_diffw$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", max_diffw$hpdi_age_of_largest_diff, "\n")

## Long-term wealth variability -----

calculate_within_differences <- function(wealth_level) {
  cumulative_posterior <- get(paste0("cumulative_posterior_msd_", wealth_level))
  
  # Calculate absolute differences for each posterior sample (ages 11:51)
  abs_diffs_posterior <- t(apply(cumulative_posterior[,11:51], 1, diff))
  abs_diffs_posterior <- abs(abs_diffs_posterior)
  
  # Find largest absolute difference AND its position for each posterior sample
  max_diffs <- apply(abs_diffs_posterior, 1, max, na.rm = TRUE)
  max_positions <- apply(abs_diffs_posterior, 1, which.max)
  
  # Convert positions to actual ages (age interval is between age and age+1)
  max_ages <- max_positions + 10  # +10 because we started at age 11
  
  # Summary statistics
  median_max_diff <- median(max_diffs)
  max_diff_hpdi <- HPDI(max_diffs, prob = 0.9)
  
  median_max_age <- median(max_ages)
  max_age_hpdi <- HPDI(max_ages, prob = 0.9)
  
  return(list(
    median_largest_diff = median_max_diff,
    hpdi_largest_diff = max_diff_hpdi,
    median_age_of_largest_diff = median_max_age,
    hpdi_age_of_largest_diff = max_age_hpdi,
    all_max_differences = max_diffs,
    all_max_ages = max_ages
  ))
}

# Minimum current absolute wealth 
min_msd <- calculate_within_differences(1)
cat("Minimum wealth:\n")
cat("Largest absolute difference:", min_msd$median_largest_diff, "\n")
cat("HPDI of difference:", min_msd$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", min_msd$median_age_of_largest_diff, 
    "and", min_msd$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", min_msd$hpdi_age_of_largest_diff, "\n\n")

# Median current absolute wealth 
med_msd <- calculate_within_differences(2)
cat("Median wealth:\n")
cat("Largest absolute difference:", med_msd$median_largest_diff, "\n")
cat("HPDI of difference:", med_msd$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", med_msd$median_age_of_largest_diff, 
    "and", med_msd$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", med_msd$hpdi_age_of_largest_diff, "\n\n")

# Maximum current absolute wealth 
max_msd <- calculate_within_differences(3)
cat("Maximum wealth:\n")
cat("Largest absolute difference:", max_msd$median_largest_diff, "\n")
cat("HPDI of difference:", max_msd$hpdi_largest_diff, "\n")
cat("Largest jump occurs between age", max_msd$median_age_of_largest_diff, 
    "and", max_msd$median_age_of_largest_diff + 1, "\n")
cat("HPDI of age:", max_msd$hpdi_age_of_largest_diff, "\n")

#Differences between wealth classes ----

## Current absolute wealth ----

# Function to calculate between-wealth differences with proper HPDI
calculate_between_differences <- function(wealth_level_high, wealth_level_low, comparison_name) {
  cumulative_posterior_high <- get(paste0("cumulative_posterior_absw_", wealth_level_high))
  cumulative_posterior_low <- get(paste0("cumulative_posterior_absw_", wealth_level_low))
  
  # Calculate absolute differences between wealth levels for each posterior sample (ages 11:51)
  wealth_diffs_posterior <- cumulative_posterior_high[,11:51] - cumulative_posterior_low[,11:51]
  abs_wealth_diffs_posterior <- abs(wealth_diffs_posterior)
  
  # Find largest absolute difference AND its position for each posterior sample
  max_diffs <- apply(abs_wealth_diffs_posterior, 1, max, na.rm = TRUE)
  max_positions <- apply(abs_wealth_diffs_posterior, 1, which.max)
  
  # Convert positions to actual ages
  max_ages <- max_positions + 10  # +10 because we started at age 11
  
  # Summary statistics
  median_max_diff <- median(max_diffs)
  max_diff_hpdi <- HPDI(max_diffs, prob = 0.9)
  
  median_max_age <- median(max_ages)
  max_age_hpdi <- HPDI(max_ages, prob = 0.9)
  
  return(list(
    comparison = comparison_name,
    median_largest_diff = median_max_diff,
    hpdi_largest_diff = max_diff_hpdi,
    median_age_of_largest_diff = median_max_age,
    hpdi_age_of_largest_diff = max_age_hpdi,
    all_max_differences = max_diffs,
    all_max_ages = max_ages
  ))
}

# Compare maximum vs minimum wealth
max_vs_min_absw <- calculate_between_differences(3, 1, "Maximum vs Minimum wealth")
cat("Comparison:", max_vs_min_absw$comparison, "\n")
cat("Largest absolute difference:", max_vs_min_absw$median_largest_diff, "\n")
cat("HPDI of difference:", max_vs_min_absw$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", max_vs_min_absw$median_age_of_largest_diff, "\n")
cat("HPDI of age:", max_vs_min_absw$hpdi_age_of_largest_diff, "\n\n")

# Compare maximum vs median wealth
max_vs_med_absw <- calculate_between_differences(3, 2, "Maximum vs Median wealth")
cat("Comparison:", max_vs_med_absw$comparison, "\n")
cat("Largest absolute difference:", max_vs_med_absw$median_largest_diff, "\n")
cat("HPDI of difference:", max_vs_med_absw$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", max_vs_med_absw$median_age_of_largest_diff, "\n")
cat("HPDI of age:", max_vs_med_absw$hpdi_age_of_largest_diff, "\n\n")

# Compare median vs minimum wealth
med_vs_min_absw <- calculate_between_differences(2, 1, "Median vs Minimum wealth")
cat("Comparison:", med_vs_min_absw$comparison, "\n")
cat("Largest absolute difference:", med_vs_min_absw$median_largest_diff, "\n")
cat("HPDI of difference:", med_vs_min_absw$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", med_vs_min_absw$median_age_of_largest_diff, "\n")
cat("HPDI of age:", med_vs_min_absw$hpdi_age_of_largest_diff, "\n")

## Short-term variability ----

# Function to calculate between-wealth differences with proper HPDI
calculate_between_differences <- function(wealth_level_high, wealth_level_low, comparison_name) {
  cumulative_posterior_high <- get(paste0("cumulative_posterior_diffw_", wealth_level_high))
  cumulative_posterior_low <- get(paste0("cumulative_posterior_diffw_", wealth_level_low))
  
  # Calculate absolute differences between wealth levels for each posterior sample (ages 11:51)
  wealth_diffs_posterior <- cumulative_posterior_high[,11:51] - cumulative_posterior_low[,11:51]
  abs_wealth_diffs_posterior <- abs(wealth_diffs_posterior)
  
  # Find largest absolute difference AND its position for each posterior sample
  max_diffs <- apply(abs_wealth_diffs_posterior, 1, max, na.rm = TRUE)
  max_positions <- apply(abs_wealth_diffs_posterior, 1, which.max)
  
  # Convert positions to actual ages
  max_ages <- max_positions + 10  # +10 because we started at age 11
  
  # Summary statistics
  median_max_diff <- median(max_diffs)
  max_diff_hpdi <- HPDI(max_diffs, prob = 0.9)
  
  median_max_age <- median(max_ages)
  max_age_hpdi <- HPDI(max_ages, prob = 0.9)
  
  return(list(
    comparison = comparison_name,
    median_largest_diff = median_max_diff,
    hpdi_largest_diff = max_diff_hpdi,
    median_age_of_largest_diff = median_max_age,
    hpdi_age_of_largest_diff = max_age_hpdi,
    all_max_differences = max_diffs,
    all_max_ages = max_ages
  ))
}

# Compare maximum vs minimum wealth
max_vs_min_diffw <- calculate_between_differences(3, 1, "Maximum vs Minimum wealth")
cat("Comparison:", max_vs_min_diffw$comparison, "\n")
cat("Largest absolute difference:", max_vs_min_diffw$median_largest_diff, "\n")
cat("HPDI of difference:", max_vs_min_diffw$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", max_vs_min_diffw$median_age_of_largest_diff, "\n")
cat("HPDI of age:", max_vs_min_diffw$hpdi_age_of_largest_diff, "\n\n")

# Compare maximum vs median wealth
max_vs_med_diffw <- calculate_between_differences(3, 2, "Maximum vs Median wealth")
cat("Comparison:", max_vs_med_diffw$comparison, "\n")
cat("Largest absolute difference:", max_vs_med_diffw$median_largest_diff, "\n")
cat("HPDI of difference:", max_vs_med_diffw$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", max_vs_med_diffw$median_age_of_largest_diff, "\n")
cat("HPDI of age:", max_vs_med_diffw$hpdi_age_of_largest_diff, "\n\n")

# Compare median vs minimum wealth
med_vs_min_diffw <- calculate_between_differences(2, 1, "Median vs Minimum wealth")
cat("Comparison:", med_vs_min_diffw$comparison, "\n")
cat("Largest absolute difference:", med_vs_min_diffw$median_largest_diff, "\n")
cat("HPDI of difference:", med_vs_min_diffw$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", med_vs_min_diffw$median_age_of_largest_diff, "\n")
cat("HPDI of age:", med_vs_min_diffw$hpdi_age_of_largest_diff, "\n")

##Long-term variability ----

# Function to calculate between-wealth differences with proper HPDI
calculate_between_differences <- function(wealth_level_high, wealth_level_low, comparison_name) {
  cumulative_posterior_high <- get(paste0("cumulative_posterior_msd_", wealth_level_high))
  cumulative_posterior_low <- get(paste0("cumulative_posterior_msd_", wealth_level_low))
  
  # Calculate absolute differences between wealth levels for each posterior sample (ages 11:51)
  wealth_diffs_posterior <- cumulative_posterior_high[,11:51] - cumulative_posterior_low[,11:51]
  abs_wealth_diffs_posterior <- abs(wealth_diffs_posterior)
  
  # Find largest absolute difference AND its position for each posterior sample
  max_diffs <- apply(abs_wealth_diffs_posterior, 1, max, na.rm = TRUE)
  max_positions <- apply(abs_wealth_diffs_posterior, 1, which.max)
  
  # Convert positions to actual ages
  max_ages <- max_positions + 10  # +10 because we started at age 11
  
  # Summary statistics
  median_max_diff <- median(max_diffs)
  max_diff_hpdi <- HPDI(max_diffs, prob = 0.9)
  
  median_max_age <- median(max_ages)
  max_age_hpdi <- HPDI(max_ages, prob = 0.9)
  
  return(list(
    comparison = comparison_name,
    median_largest_diff = median_max_diff,
    hpdi_largest_diff = max_diff_hpdi,
    median_age_of_largest_diff = median_max_age,
    hpdi_age_of_largest_diff = max_age_hpdi,
    all_max_differences = max_diffs,
    all_max_ages = max_ages
  ))
}

# Compare maximum vs minimum wealth
max_vs_min_msd <- calculate_between_differences(3, 1, "Maximum vs Minimum wealth")
cat("Comparison:", max_vs_min_msd$comparison, "\n")
cat("Largest absolute difference:", max_vs_min_msd$median_largest_diff, "\n")
cat("HPDI of difference:", max_vs_min_msd$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", max_vs_min_msd$median_age_of_largest_diff, "\n")
cat("HPDI of age:", max_vs_min_msd$hpdi_age_of_largest_diff, "\n\n")

# Compare maximum vs median wealth
max_vs_med_msd <- calculate_between_differences(3, 2, "Maximum vs Median wealth")
cat("Comparison:", max_vs_med_msd$comparison, "\n")
cat("Largest absolute difference:", max_vs_med_msd$median_largest_diff, "\n")
cat("HPDI of difference:", max_vs_med_msd$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", max_vs_med_msd$median_age_of_largest_diff, "\n")
cat("HPDI of age:", max_vs_med_msd$hpdi_age_of_largest_diff, "\n\n")

# Compare median vs minimum wealth
med_vs_min_msd <- calculate_between_differences(2, 1, "Median vs Minimum wealth")
cat("Comparison:", med_vs_min_msd$comparison, "\n")
cat("Largest absolute difference:", med_vs_min_msd$median_largest_diff, "\n")
cat("HPDI of difference:", med_vs_min_msd$hpdi_largest_diff, "\n")
cat("Largest difference occurs at age", med_vs_min_msd$median_age_of_largest_diff, "\n")
cat("HPDI of age:", med_vs_min_msd$hpdi_age_of_largest_diff, "\n")

# Coefficient plots ----

# Set up plotting area with space at bottom
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.5, oma = c(1, 0, 0, 0), xpd = NA)

## Current wealth ----

plot(c(30:1)~c(tab_real_beta_z[11:40,1]*tab_real_beta_sigma[1,1]),
     xlim=c(-0.5,0.5),
     main="Current\nwealth",
     yaxt="n",
     xlab=expression(beta~coefficients),
     ylab="Ages",
     pch=16,
     col="#648FFF",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     mgp = c(2.5, 0.7, 0)
)
axis(2,c(30:1),c(10:39),cex.axis=1.5)
clip(-1, 1, 0, 31)
abline(v=0,lty=2)
for (i in 11:40){
  segments(
    c(tab_real_beta_z[i,1]*tab_real_beta_sigma[1,1])-c(tab_real_beta_z[i,2]*tab_real_beta_sigma[1,1]),
    41-i,
    c(tab_real_beta_z[i,1]*tab_real_beta_sigma[1,1])+c(tab_real_beta_z[i,2]*tab_real_beta_sigma[1,1]),
    41-i,
    lwd=2,col="#648FFF") 
}

## Short-term wealth variability ----

plot(c(30:1)~c(tab_real_gamma_z[11:40,1]*tab_real_gamma_sigma[1,1]),
     xlim=c(-0.5,0.5),
     main="Short-term\nwealth variability",
     yaxt="n",
     xlab=expression(gamma~coefficients),
     ylab="Ages",
     pch=16,
     col="#785EF0",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     mgp = c(2.5, 0.7, 0)
)
axis(2,c(30:1),c(10:39),cex.axis=1.5)
clip(-1, 1, 0, 31)
abline(v=0,lty=2)
for (i in 11:40){
  segments(
    c(tab_real_gamma_z[i,1]*tab_real_gamma_sigma[1,1])-c(tab_real_gamma_z[i,2]*tab_real_gamma_sigma[1,1]),
    41-i,
    c(tab_real_gamma_z[i,1]*tab_real_gamma_sigma[1,1])+c(tab_real_gamma_z[i,2]*tab_real_gamma_sigma[1,1]),
    41-i,
    lwd=2,col="#785EF0") 
}

## Long-term wealth variability ----

plot(c(30:1)~c(tab_real_delta_z[11:40,1]*tab_real_delta_sigma[1,1]),
     xlim=c(-0.5,0.5),
     main="Long-term\nwealth variability",
     yaxt="n",
     xlab=expression(delta~coefficients),
     ylab="Ages",
     pch=16,col="#DC267F",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     mgp = c(2.5, 0.7, 0)
)
axis(2,c(30:1),c(10:39),cex.axis=1.5)
clip(-1, 1, 0, 31)
abline(v=0,lty=2)
for (i in 11:40){
  segments(
    c(tab_real_delta_z[i,1]*tab_real_delta_sigma[1,1])-c(tab_real_delta_z[i,2]*tab_real_delta_sigma[1,1]),
    41-i,
    c(tab_real_delta_z[i,1]*tab_real_delta_sigma[1,1])+c(tab_real_delta_z[i,2]*tab_real_delta_sigma[1,1]),
    41-i,
    lwd=2,col="#DC267F") 
}

# Add common legend at bottom
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0.5, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlim = c(0, 1), ylim = c(0, 1))

# Create custom legend with proper spacing
legend("bottom", 
       legend = c("Wealth parameters:",expression(beta), expression(gamma), expression(delta)),
       col = c(NA,"#648FFF", "#785EF0", "#DC267F"),
       lwd = c(NA,2, 2, 2),
       lty = c(NA,1, 1, 1),
       pch = c(NA,16, 16, 16),
       ncol = 4,
       bty = "n",
       xjust = 0.5,
       text.width = c(0.13, 0.01, 0.01, 0.01),
       pt.cex = 1.5,
       cex = 1.5
)

