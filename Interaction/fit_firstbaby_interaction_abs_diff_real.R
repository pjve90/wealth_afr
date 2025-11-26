# Model with current absolute levels, wealth change, and wealth change ----

# The code in this script is meant to fit a Bayesian model that aims to predict the probability of first birth by  the interaction between absolute wealth and Short-term wealth variability.

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

#Calculate the short-term and Short-term wealth variability from the data ----

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

#Short-term wealth variability
#create matrix
msdw_matrix <-  matrix(nrow = nrow(std_absw_matrix),ncol=ncol(std_absw_matrix))
#check matrix
msdw_matrix
#calculate the Short-term wealth variability
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
#check the age-specific average of standardised Short-term wealth variability
apply(msdw_matrix,2,mean,na.rm=T)
#plot it
plot(NA,
     xlim=c(1,ncol(msdw_matrix)),
     ylim=range(msdw_matrix,na.rm=T),
     xlab="Age",
     ylab="Short-term wealth variability"
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
int_abs_diff_list <- list(N = nrow(afrs_restricted), #population size
                          A = ncol(afrs_restricted), #age
                          wealth = std_absw_restricted, #current absolute wealth
                          baby = afrs_restricted, #AFR
                          N_miss = n_miss_restricted, # number of missing values that need imputation
                          wealth_miss=wealth_miss_restricted, # matrix indicating missing wealth data
                          median_wealth=std_median_wealth
) 
#check data
int_abs_diff_list

## Compile and fit model ----

# compile model

model_int_abs_diff <- cmdstan_model("~/wealth_afr/Interaction/firstbaby_interaction_abs_diff.stan")

#fit model
fit_int_abs_diff <- model_int_abs_diff$sample(data = int_abs_diff_list, 
                                              chains = 4, 
                                              parallel_chains = 15, 
                                              adapt_delta = 0.99,
                                              max_treedepth = 13,
                                              iter_warmup = 2000,
                                              iter_sampling = 2000,
                                              init = 0)

# save fit 
fit_int_abs_diff_csv <- rstan::read_stan_csv(fit_int_abs_diff$output_files())
saveRDS(fit_int_abs_diff_csv, "fit_int_abs_diff_output.rds")
#load RDS file
rds_int_abs_diff <- readRDS("fit_int_abs_diff_output.rds")
#extract samples
post_int_abs_diff <- extract.samples(rds_int_abs_diff)

## Model diagnostics ----

#check trace of all parameters
#alpha
rstan::traceplot(rds_int_abs_diff,pars="alpha")
#mu
traceplot(rds_int_abs_diff,pars="mu") 
#mu_raw
traceplot(rds_int_abs_diff,pars="mu_raw")
#mu_tau
rstan::traceplot(rds_int_abs_diff,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds_int_abs_diff,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds_int_abs_diff,pars="mu_delta")
#beta_wealth_z
traceplot(rds_int_abs_diff,pars="beta_wealth_z") 
#beta_wealth_sigma
traceplot(rds_int_abs_diff,pars="beta_wealth_sigma") 
#gamma_wealth
traceplot(rds_int_abs_diff,pars="gamma_wealth_z") 
#gamma_wealth
traceplot(rds_int_abs_diff,pars="gamma_wealth_sigma") 
#zeta_wealth
traceplot(rds_int_abs_diff,pars="zeta_wealth_z") 
#zeta_wealth
traceplot(rds_int_abs_diff,pars="zeta_wealth_sigma") 

#summary of the model
#create summary tables of the different parameters

#alpha and hiper priors of Gaussian process
#create summary table for alpha and hiper priors of Gaussian process
tab_int_abs_diff_alphagp <- precis(rds_int_abs_diff,depth=2,pars=c("alpha",
                                                                   "mu_raw",
                                                                   "mu_tau",
                                                                   "mu_delta"))
#check table
tab_int_abs_diff_alphagp

#mu
#create summary table for mu
tab_int_abs_diff_mu <- precis(rds_int_abs_diff,depth=2,pars="mu")
#check table
tab_int_abs_diff_mu
#plot it!
plot(tab_int_abs_diff_mu)

#beta z
#create summary table for beta_z
tab_int_abs_diff_beta_z <- precis(rds_int_abs_diff,depth=2,pars="beta_wealth_z")
#check table
tab_int_abs_diff_beta_z
#plot it!
plot(tab_int_abs_diff_beta_z)

#beta sigma
#create summary table for beta_sigma
tab_int_abs_diff_beta_sigma <- precis(rds_int_abs_diff,depth=2,pars="beta_wealth_sigma")
#check table
tab_int_abs_diff_beta_sigma

#gamma z
#create summary table for gamma_z
tab_int_abs_diff_gamma_z <- precis(rds_int_abs_diff,depth=2,pars="gamma_wealth_z")
#check table
tab_int_abs_diff_gamma_z
#plot it!
plot(tab_int_abs_diff_gamma_z)

#gamma sigma
#create summary table for gamma_sigma
tab_int_abs_diff_gamma_sigma <- precis(rds_int_abs_diff,depth=2,pars="gamma_wealth_sigma")
#check table
tab_int_abs_diff_gamma_sigma

#zeta z
#create summary table for zeta_z
tab_int_abs_diff_zeta_z <- precis(rds_int_abs_diff,depth=2,pars="zeta_wealth_z")
#check table
tab_int_abs_diff_zeta_z
#plot it!
plot(tab_int_abs_diff_zeta_z)

#zeta sigma
#create summary table for zeta_sigma
tab_int_abs_diff_zeta_sigma <- precis(rds_int_abs_diff,depth=2,pars="zeta_wealth_sigma")
#check table
tab_int_abs_diff_zeta_sigma

#Check correlation between wealth predictors

#beta versus delta
#create correlation matrix
cor1 <- round(cor(post_int_abs_diff$beta_wealth_z,post_int_abs_diff$gamma_wealth_z),3)
#plot it!
corrplot(cor1, "color", tl.col="black")

#beta versus epsion
#create correlation matrix
cor2 <- round(cor(post_int_abs_diff$beta_wealth_z,post_int_abs_diff$zeta_wealth_z),3)
#plot it!
corrplot(cor2, "color", tl.col="black")

#delta versus zeta
#create correlation matrix
cor3 <- round(cor(post_int_abs_diff$gamma_wealth_z,post_int_abs_diff$zeta_wealth_z),3)
#plot it!
corrplot(cor3, "color", tl.col="black")

# Plot the output of the model ----

## Current Wealth ----

#simulate wealth values
simwealth_int_abs_diff <- seq(from=round(min(post_int_abs_diff$wealth_full),1),to=round(max(post_int_abs_diff$wealth_full),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_int_abs_diff
#get the deciles
deciles_absw <- as.numeric(quantile(simwealth_int_abs_diff,seq(0,1,0.5)))
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
plot(c(0,1)~c(11,ncol(post_int_abs_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Current levels\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_int_abs_diff$mu), by = 5), 
     labels = seq(10, ncol(post_int_abs_diff$mu)-1, by = 5))
# Combined legend
legend(53,1,  # or specify x/y coordinates
       legend = c("Cumulative\nprobabilities", "", "Min.", "Med.", "Max.",  # First group
                  "",
                  "Expected age\nat first birth", "", "Min.", "Med.", "Max."), # Second group
       col = c(NA,NA, palette_a,NA,NA,NA,palette_a),  # NA for section headers
       lwd = 3,
       lty = c(NA,NA, type[1:3],NA,NA, NA, c(4,4,4)),  # NA for headers
       pch = c(NA,NA, shape,NA,NA, NA, rep(NA, 3)),    # Symbols for Estimated only
       pt.cex = 1.5,
       cex = 1.2,
       box.col = NA,
       ncol = 1,  # Optional: arrange in 2 columns
       title = NULL)

#add lines
for(k in 1:(length(deciles_absw))){
  #create matrix to store the data
  p_int_abs_diff <- matrix(nrow=nrow(post_int_abs_diff$mu),ncol=ncol(post_int_abs_diff$mu))
  p_int_abs_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_int_abs_diff$mu)){
    for(i in 1:nrow(post_int_abs_diff$mu)){
      p_int_abs_diff[i,j] <- inv_logit(post_int_abs_diff$alpha[i] + #inv logit because originally is logit
                                         post_int_abs_diff$mu[i,j] + # age
                                         (post_int_abs_diff$beta_wealth_z[i,j]*post_int_abs_diff$beta_wealth_sigma[i])*deciles_absw[k] + # absolute wealth
                                         (post_int_abs_diff$gamma_wealth_z[i,j]*post_int_abs_diff$gamma_wealth_sigma[i])*0 + # absolute wealth change
                                         (post_int_abs_diff$zeta_wealth_z[i,j]*post_int_abs_diff$zeta_wealth_sigma[i])*(0*0) # interaction
      )
    }
  }
  #check data
  p_int_abs_diff
  #plot it!
  #prepare model prediction data
  plot_int_abs_diff <- data.frame(age = 1:ncol(p_int_abs_diff),
                                  median = apply(p_int_abs_diff, 2, median), 
                                  upp = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("absw_",k),plot_int_abs_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_absw <- numeric(length(plot_int_abs_diff$median))
  cumulative_low_absw <- numeric(length(plot_int_abs_diff$low))
  cumulative_upp_absw <- numeric(length(plot_int_abs_diff$upp))
  #set the first probability
  cumulative_median_absw[1] <- plot_int_abs_diff$median[1]
  cumulative_low_absw[1] <- plot_int_abs_diff$low[1]
  cumulative_upp_absw[1] <- plot_int_abs_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_int_abs_diff$median)) {
    cumulative_median_absw[a] <- cumulative_median_absw[a-1] + (1 - cumulative_median_absw[a-1]) * plot_int_abs_diff$median[a]
    cumulative_low_absw[a] <- cumulative_low_absw[a-1] + (1 - cumulative_low_absw[a-1]) * plot_int_abs_diff$low[a]
    cumulative_upp_absw[a] <- cumulative_upp_absw[a-1] + (1 - cumulative_upp_absw[a-1]) * plot_int_abs_diff$upp[a]
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
  points(cumulative_median_absw[11:51] ~ plot_int_abs_diff$age[11:51], col=palette_a[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_absw[11:51] ~ plot_int_abs_diff$age[11:51], col=palette_a[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_int_abs_diff$age[11:51], rev(plot_int_abs_diff$age[11:51])), c(cumulative_low_absw[11:51], rev(cumulative_upp_absw[11:51])), col=alpha(palette_a[k], 0.1), border=palette_a[k])
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_a[k], lwd=3, lty=4)
}

## Short-term variability of wealth ----

#simulate wealth values
simwealth_diff_int_abs_diff <- seq(from=round(min(post_int_abs_diff$wealth_change_std),1),to=round(max(post_int_abs_diff$wealth_change_std),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_diff_int_abs_diff
#get the deciles
deciles_diffw <- as.numeric(quantile(simwealth_diff_int_abs_diff,seq(0,1,0.5)))
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
plot(c(0,1)~c(11,ncol(post_int_abs_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Short-term\nwealth variability",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_int_abs_diff$mu), by = 5), 
     labels = seq(10, ncol(post_int_abs_diff$mu)-1, by = 5))

# Combined legend
legend(53,1,  # or specify x/y coordinates
       legend = c("Cumulative\nprobabilities", "", "Min.", "Med.", "Max.",  # First group
                  "",
                  "Expected age\nat first birth", "", "Min.", "Med.", "Max."), # Second group
       col = c(NA,NA, palette_b,NA,NA,NA,palette_b),  # NA for section headers
       lwd = 3,
       lty = c(NA,NA, type[1:3],NA,NA, NA, c(4,4,4)),  # NA for headers
       pch = c(NA,NA, shape,NA,NA, NA, rep(NA, 3)),    # Symbols for Estimated only
       pt.cex = 1.5,
       cex = 1.2,
       box.col = NA,
       ncol = 1,  # Optional: arrange in 2 columns
       title = NULL)

#add lines
for(k in 1:(length(deciles_diffw))){
  #create matrix to store the data
  p_diff_int_abs_diff <- matrix(nrow=nrow(post_int_abs_diff$mu),ncol=ncol(post_int_abs_diff$mu))
  p_diff_int_abs_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_int_abs_diff$mu)){
    for(i in 1:nrow(post_int_abs_diff$mu)){
      p_diff_int_abs_diff[i,j] <- inv_logit(post_int_abs_diff$alpha[i] + #inv logit because originally is logit
                                              post_int_abs_diff$mu[i,j] + # age
                                              (post_int_abs_diff$beta_wealth_z[i,j]*post_int_abs_diff$beta_wealth_sigma[i])*0 + # absolute wealth
                                              (post_int_abs_diff$gamma_wealth_z[i,j]*post_int_abs_diff$gamma_wealth_sigma[i])*deciles_diffw[k] + # absolute wealth change
                                              (post_int_abs_diff$zeta_wealth_z[i,j]*post_int_abs_diff$zeta_wealth_sigma[i])*(0*0) # interaction
      )
    }
  }
  #check data
  p_diff_int_abs_diff
  #plot it!
  #prepare model prediction data
  plot_diff_int_abs_diff <- data.frame(age = 1:ncol(p_diff_int_abs_diff),
                                       median = apply(p_diff_int_abs_diff, 2, median), 
                                       upp = apply(p_diff_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p_diff_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_diff_int_abs_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diff <- numeric(length(plot_diff_int_abs_diff$median))
  cumulative_low_diff <- numeric(length(plot_diff_int_abs_diff$low))
  cumulative_upp_diff <- numeric(length(plot_diff_int_abs_diff$upp))
  #set the first probability
  cumulative_median_diff[1] <- plot_diff_int_abs_diff$median[1]
  cumulative_low_diff[1] <- plot_diff_int_abs_diff$low[1]
  cumulative_upp_diff[1] <- plot_diff_int_abs_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diff_int_abs_diff$median)) {
    cumulative_median_diff[a] <- cumulative_median_diff[a-1] + (1 - cumulative_median_diff[a-1]) * plot_diff_int_abs_diff$median[a]
    cumulative_low_diff[a] <- cumulative_low_diff[a-1] + (1 - cumulative_low_diff[a-1]) * plot_diff_int_abs_diff$low[a]
    cumulative_upp_diff[a] <- cumulative_upp_diff[a-1] + (1 - cumulative_upp_diff[a-1]) * plot_diff_int_abs_diff$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diff_",k),cumulative_median_diff)
  assign(paste0("cumulative_low_diff_",k),cumulative_low_diff)
  assign(paste0("cumulative_upp_diff_",k),cumulative_upp_diff)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_diff < 0.5))
  age_after <- min(which(cumulative_median_diff >= 0.5))
  prob_before <- cumulative_median_diff[age_before]
  prob_after <- cumulative_median_diff[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_diff_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_b[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_b[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_diff_int_abs_diff$age[11:51], rev(plot_diff_int_abs_diff$age[11:51])), c(cumulative_low_diff[11:51], rev(cumulative_upp_diff[11:51])), col=alpha(palette_b[k], 0.1), border=palette_b[k])
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_b[k], lwd=3, lty=4)
}


## Interaction absolute and Short-term ----

#simulate wealth values
simwealth_absw_int_abs_diff <- seq(from=round(min(post_int_abs_diff$wealth_full),1),to=round(max(post_int_abs_diff$wealth_full),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_absw_int_abs_diff
#get the deciles
deciles_absw_int_abs_diff <- as.numeric(quantile(simwealth_absw_int_abs_diff,seq(0,1,0.5)))
deciles_absw_int_abs_diff

#simulate wealth values
simwealth_diff_int_abs_diff <- seq(from=round(min(post_int_abs_diff$wealth_change_std),1),to=round(max(post_int_abs_diff$wealth_change_std),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_diff_int_abs_diff
#get the deciles
deciles_diff_int_abs_diff <- as.numeric(quantile(simwealth_diff_int_abs_diff,seq(0,1,0.5)))
deciles_diff_int_abs_diff

### Short-term wealth variability ----

layout(matrix(c(1,2,3,4,4,4), ncol=3,byrow=T),heights=c(4,0.175))

#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#### Min. absolute wealth ----

#set parameters for a legend outside of the plot
par(mar = c(5.1, 4.1, 4.1, 2.1))
#plot empty plot
plot(c(0,1)~c(10,ncol(post_int_abs_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Short-term wealth variability with\nMinimum absolute wealth",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_int_abs_diff$mu), by = 5), 
     labels = seq(10, ncol(post_int_abs_diff$mu)-1, by = 5),
     cex.axis=1.5
)

#add lines
for(k in 1:(length(deciles_diff_int_abs_diff))){
  #create matrix to store the data
  p_int_abs_diff <- matrix(nrow=nrow(post_int_abs_diff$mu),ncol=ncol(post_int_abs_diff$mu))
  p_int_abs_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_int_abs_diff$mu)){
    for(i in 1:nrow(post_int_abs_diff$mu)){
      p_int_abs_diff[i,j] <- inv_logit(post_int_abs_diff$alpha[i] + #inv logit because originally is logit
                                         post_int_abs_diff$mu[i,j] + #age
                                         (post_int_abs_diff$beta_wealth_z[i,j]*post_int_abs_diff$beta_wealth_sigma[i])*0 + #absolute wealth
                                         (post_int_abs_diff$gamma_wealth_z[i,j]*post_int_abs_diff$gamma_wealth_sigma[i])*0 + #wealth change
                                         (post_int_abs_diff$zeta_wealth_z[i,j]*post_int_abs_diff$zeta_wealth_sigma[i])*(deciles_absw_int_abs_diff[1]*deciles_diff_int_abs_diff[k]) #interaction
      )  
    }
  }
  #check data
  p_int_abs_diff
  #plot it!
  #prepare model prediction data
  plot_diff_int_abs_diff <- data.frame(age = 1:ncol(p_int_abs_diff),
                                       median = apply(p_int_abs_diff, 2, median), 
                                       upp = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_diff_int_abs_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diff <- numeric(length(plot_diff_int_abs_diff$median))
  cumulative_low_diff <- numeric(length(plot_diff_int_abs_diff$low))
  cumulative_upp_diff <- numeric(length(plot_diff_int_abs_diff$upp))
  #set the first probability
  cumulative_median_diff[1] <- plot_diff_int_abs_diff$median[1]
  cumulative_low_diff[1] <- plot_diff_int_abs_diff$low[1]
  cumulative_upp_diff[1] <- plot_diff_int_abs_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diff_int_abs_diff$median)) {
    cumulative_median_diff[a] <- cumulative_median_diff[a-1] + (1 - cumulative_median_diff[a-1]) * plot_diff_int_abs_diff$median[a]
    cumulative_low_diff[a] <- cumulative_low_diff[a-1] + (1 - cumulative_low_diff[a-1]) * plot_diff_int_abs_diff$low[a]
    cumulative_upp_diff[a] <- cumulative_upp_diff[a-1] + (1 - cumulative_upp_diff[a-1]) * plot_diff_int_abs_diff$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diff_",k),cumulative_median_diff)
  assign(paste0("cumulative_low_diff_",k),cumulative_low_diff)
  assign(paste0("cumulative_upp_diff_",k),cumulative_upp_diff)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_diff < 0.5))
  age_after <- min(which(cumulative_median_diff >= 0.5))
  prob_before <- cumulative_median_diff[age_before]
  prob_after <- cumulative_median_diff[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_diff_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_b[k], pch=16, cex=1.5)
  #add lines
  lines(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_b[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_diff_int_abs_diff$age[11:51], rev(plot_diff_int_abs_diff$age[11:51])), c(cumulative_low_diff[11:51], rev(cumulative_upp_diff[11:51])), col=alpha(palette_b[k], 0.1), border=alpha(palette_b[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_b[k], lwd=3, lty=2)
}

#### Mid. absolute wealth ----

#set parameters for a legend outside of the plot
par(mar = c(5.1, 4.1, 4.1, 2.1))
#plot empty plot
plot(c(0,1)~c(10,ncol(post_int_abs_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Short-term wealth variability with\nMiddle absolute wealth",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_int_abs_diff$mu), by = 5), 
     labels = seq(10, ncol(post_int_abs_diff$mu)-1, by = 5),
     cex.axis=1.5
)

#add lines
for(k in 1:(length(deciles_diff_int_abs_diff))){
  #create matrix to store the data
  p_int_abs_diff <- matrix(nrow=nrow(post_int_abs_diff$mu),ncol=ncol(post_int_abs_diff$mu))
  p_int_abs_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_int_abs_diff$mu)){
    for(i in 1:nrow(post_int_abs_diff$mu)){
      p_int_abs_diff[i,j] <- inv_logit(post_int_abs_diff$alpha[i] + #inv logit because originally is logit
                                         post_int_abs_diff$mu[i,j] + #age
                                         (post_int_abs_diff$beta_wealth_z[i,j]*post_int_abs_diff$beta_wealth_sigma[i])*0 + #absolute wealth
                                         (post_int_abs_diff$gamma_wealth_z[i,j]*post_int_abs_diff$gamma_wealth_sigma[i])*0 + #wealth change
                                         (post_int_abs_diff$zeta_wealth_z[i,j]*post_int_abs_diff$zeta_wealth_sigma[i])*(deciles_absw_int_abs_diff[2]*deciles_diff_int_abs_diff[k]) #interaction
      )  
    }
  }
  #check data
  p_int_abs_diff
  #plot it!
  #prepare model prediction data
  plot_diff_int_abs_diff <- data.frame(age = 1:ncol(p_int_abs_diff),
                                       median = apply(p_int_abs_diff, 2, median), 
                                       upp = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_diff_int_abs_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diff <- numeric(length(plot_diff_int_abs_diff$median))
  cumulative_low_diff <- numeric(length(plot_diff_int_abs_diff$low))
  cumulative_upp_diff <- numeric(length(plot_diff_int_abs_diff$upp))
  #set the first probability
  cumulative_median_diff[1] <- plot_diff_int_abs_diff$median[1]
  cumulative_low_diff[1] <- plot_diff_int_abs_diff$low[1]
  cumulative_upp_diff[1] <- plot_diff_int_abs_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diff_int_abs_diff$median)) {
    cumulative_median_diff[a] <- cumulative_median_diff[a-1] + (1 - cumulative_median_diff[a-1]) * plot_diff_int_abs_diff$median[a]
    cumulative_low_diff[a] <- cumulative_low_diff[a-1] + (1 - cumulative_low_diff[a-1]) * plot_diff_int_abs_diff$low[a]
    cumulative_upp_diff[a] <- cumulative_upp_diff[a-1] + (1 - cumulative_upp_diff[a-1]) * plot_diff_int_abs_diff$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diff_",k),cumulative_median_diff)
  assign(paste0("cumulative_low_diff_",k),cumulative_low_diff)
  assign(paste0("cumulative_upp_diff_",k),cumulative_upp_diff)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_diff < 0.5))
  age_after <- min(which(cumulative_median_diff >= 0.5))
  prob_before <- cumulative_median_diff[age_before]
  prob_after <- cumulative_median_diff[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_diff_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_b[k], pch=16, cex=1.5)
  #add lines
  lines(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_b[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_diff_int_abs_diff$age[11:51], rev(plot_diff_int_abs_diff$age[11:51])), c(cumulative_low_diff[11:51], rev(cumulative_upp_diff[11:51])), col=alpha(palette_b[k], 0.1), border=alpha(palette_b[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_b[k], lwd=3, lty=2)
}

#### Max. absolute wealth ----

#set parameters for a legend outside of the plot
par(mar = c(5.1, 4.1, 4.1, 2.1))
#plot empty plot
plot(c(0,1)~c(10,ncol(post_int_abs_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Short-term wealth variability with\nMaximum absolute wealth",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_int_abs_diff$mu), by = 5), 
     labels = seq(10, ncol(post_int_abs_diff$mu)-1, by = 5),
     cex.axis=1.5
)

#add lines
for(k in 1:(length(deciles_diff_int_abs_diff))){
  #create matrix to store the data
  p_int_abs_diff <- matrix(nrow=nrow(post_int_abs_diff$mu),ncol=ncol(post_int_abs_diff$mu))
  p_int_abs_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_int_abs_diff$mu)){
    for(i in 1:nrow(post_int_abs_diff$mu)){
      p_int_abs_diff[i,j] <- inv_logit(post_int_abs_diff$alpha[i] + #inv logit because originally is logit
                                         post_int_abs_diff$mu[i,j] + #age
                                         (post_int_abs_diff$beta_wealth_z[i,j]*post_int_abs_diff$beta_wealth_sigma[i])*0 + #absolute wealth
                                         (post_int_abs_diff$gamma_wealth_z[i,j]*post_int_abs_diff$gamma_wealth_sigma[i])*0 + #wealth change
                                         (post_int_abs_diff$zeta_wealth_z[i,j]*post_int_abs_diff$zeta_wealth_sigma[i])*(deciles_absw_int_abs_diff[3]*deciles_diff_int_abs_diff[k]) #interaction
      )  
    }
  }
  #check data
  p_int_abs_diff
  #plot it!
  #prepare model prediction data
  plot_diff_int_abs_diff <- data.frame(age = 1:ncol(p_int_abs_diff),
                                       median = apply(p_int_abs_diff, 2, median), 
                                       upp = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_diff_int_abs_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diff <- numeric(length(plot_diff_int_abs_diff$median))
  cumulative_low_diff <- numeric(length(plot_diff_int_abs_diff$low))
  cumulative_upp_diff <- numeric(length(plot_diff_int_abs_diff$upp))
  #set the first probability
  cumulative_median_diff[1] <- plot_diff_int_abs_diff$median[1]
  cumulative_low_diff[1] <- plot_diff_int_abs_diff$low[1]
  cumulative_upp_diff[1] <- plot_diff_int_abs_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diff_int_abs_diff$median)) {
    cumulative_median_diff[a] <- cumulative_median_diff[a-1] + (1 - cumulative_median_diff[a-1]) * plot_diff_int_abs_diff$median[a]
    cumulative_low_diff[a] <- cumulative_low_diff[a-1] + (1 - cumulative_low_diff[a-1]) * plot_diff_int_abs_diff$low[a]
    cumulative_upp_diff[a] <- cumulative_upp_diff[a-1] + (1 - cumulative_upp_diff[a-1]) * plot_diff_int_abs_diff$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diff_",k),cumulative_median_diff)
  assign(paste0("cumulative_low_diff_",k),cumulative_low_diff)
  assign(paste0("cumulative_upp_diff_",k),cumulative_upp_diff)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_diff < 0.5))
  age_after <- min(which(cumulative_median_diff >= 0.5))
  prob_before <- cumulative_median_diff[age_before]
  prob_after <- cumulative_median_diff[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_diff_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_b[k], pch=16, cex=1.5)
  #add lines
  lines(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_b[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_diff_int_abs_diff$age[11:51], rev(plot_diff_int_abs_diff$age[11:51])), c(cumulative_low_diff[11:51], rev(cumulative_upp_diff[11:51])), col=alpha(palette_b[k], 0.1), border=alpha(palette_b[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_b[k], lwd=3, lty=2)
}

#add legend
# Add the legend in the last row
par(mar = c(0, 0, 0, 0))  # Remove margins for the legend plot
plot.new()  # Create a new empty plot for the legend
legend("center",c("Min.","Med.", "Max."),col=palette_b,lwd=3,pch=16,lty=1,pt.cex = 1.5,cex=1.2,horiz=T,bty = "n")

### Current absolute wealth ----

layout(matrix(c(1,2,3,4,4,4), ncol=3,byrow=T),heights=c(4,0.175))

#### Min. Short-term wealth variability ----

#set parameters for a legend outside of the plot
par(mar = c(5.1, 4.1, 4.1, 2.1))
#plot empty plot
plot(c(0,1)~c(10,ncol(post_int_abs_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Current absolute wealth with\nMinimum Short-term variability",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_int_abs_diff$mu), by = 5), 
     labels = seq(10, ncol(post_int_abs_diff$mu)-1, by = 5),
     cex.axis=1.5
)

#add lines
for(k in 1:(length(deciles_diff_int_abs_diff))){
  #create matrix to store the data
  p_int_abs_diff <- matrix(nrow=nrow(post_int_abs_diff$mu),ncol=ncol(post_int_abs_diff$mu))
  p_int_abs_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_int_abs_diff$mu)){
    for(i in 1:nrow(post_int_abs_diff$mu)){
      p_int_abs_diff[i,j] <- inv_logit(post_int_abs_diff$alpha[i] + #inv logit because originally is logit
                                         post_int_abs_diff$mu[i,j] + #age
                                         (post_int_abs_diff$beta_wealth_z[i,j]*post_int_abs_diff$beta_wealth_sigma[i])*0 + #absolute wealth
                                         (post_int_abs_diff$gamma_wealth_z[i,j]*post_int_abs_diff$gamma_wealth_sigma[i])*0 + #wealth change
                                         (post_int_abs_diff$zeta_wealth_z[i,j]*post_int_abs_diff$zeta_wealth_sigma[i])*(deciles_absw_int_abs_diff[k]*deciles_diff_int_abs_diff[1]) #interaction
      )  
    }
  }
  #check data
  p_int_abs_diff
  #plot it!
  #prepare model prediction data
  plot_diff_int_abs_diff <- data.frame(age = 1:ncol(p_int_abs_diff),
                                       median = apply(p_int_abs_diff, 2, median), 
                                       upp = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_diff_int_abs_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diff <- numeric(length(plot_diff_int_abs_diff$median))
  cumulative_low_diff <- numeric(length(plot_diff_int_abs_diff$low))
  cumulative_upp_diff <- numeric(length(plot_diff_int_abs_diff$upp))
  #set the first probability
  cumulative_median_diff[1] <- plot_diff_int_abs_diff$median[1]
  cumulative_low_diff[1] <- plot_diff_int_abs_diff$low[1]
  cumulative_upp_diff[1] <- plot_diff_int_abs_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diff_int_abs_diff$median)) {
    cumulative_median_diff[a] <- cumulative_median_diff[a-1] + (1 - cumulative_median_diff[a-1]) * plot_diff_int_abs_diff$median[a]
    cumulative_low_diff[a] <- cumulative_low_diff[a-1] + (1 - cumulative_low_diff[a-1]) * plot_diff_int_abs_diff$low[a]
    cumulative_upp_diff[a] <- cumulative_upp_diff[a-1] + (1 - cumulative_upp_diff[a-1]) * plot_diff_int_abs_diff$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diff_",k),cumulative_median_diff)
  assign(paste0("cumulative_low_diff_",k),cumulative_low_diff)
  assign(paste0("cumulative_upp_diff_",k),cumulative_upp_diff)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_diff < 0.5))
  age_after <- min(which(cumulative_median_diff >= 0.5))
  prob_before <- cumulative_median_diff[age_before]
  prob_after <- cumulative_median_diff[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_diff_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_a[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_a[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_diff_int_abs_diff$age[11:51], rev(plot_diff_int_abs_diff$age[11:51])), c(cumulative_low_diff[11:51], rev(cumulative_upp_diff[11:51])), col=alpha(palette_a[k], 0.1), border=palette_a[k])
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_a[k], lwd=3, lty=4)
}

#### Mid. Short-term wealth variability ----

#set parameters for a legend outside of the plot
par(mar = c(5.1, 4.1, 4.1, 2.1))
#plot empty plot
plot(c(0,1)~c(10,ncol(post_int_abs_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Current absolute wealth with\nMiddle Short-term variability",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_int_abs_diff$mu), by = 5), 
     labels = seq(10, ncol(post_int_abs_diff$mu)-1, by = 5),
     cex.axis=1.5
)

#add lines
for(k in 1:(length(deciles_diff_int_abs_diff))){
  #create matrix to store the data
  p_int_abs_diff <- matrix(nrow=nrow(post_int_abs_diff$mu),ncol=ncol(post_int_abs_diff$mu))
  p_int_abs_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_int_abs_diff$mu)){
    for(i in 1:nrow(post_int_abs_diff$mu)){
      p_int_abs_diff[i,j] <- inv_logit(post_int_abs_diff$alpha[i] + #inv logit because originally is logit
                                         post_int_abs_diff$mu[i,j] + #age
                                         (post_int_abs_diff$beta_wealth_z[i,j]*post_int_abs_diff$beta_wealth_sigma[i])*0 + #absolute wealth
                                         (post_int_abs_diff$gamma_wealth_z[i,j]*post_int_abs_diff$gamma_wealth_sigma[i])*0 + #wealth change
                                         (post_int_abs_diff$zeta_wealth_z[i,j]*post_int_abs_diff$zeta_wealth_sigma[i])*(deciles_absw_int_abs_diff[k]*deciles_diff_int_abs_diff[2]) #interaction
      )  
    }
  }
  #check data
  p_int_abs_diff
  #plot it!
  #prepare model prediction data
  plot_diff_int_abs_diff <- data.frame(age = 1:ncol(p_int_abs_diff),
                                       median = apply(p_int_abs_diff, 2, median), 
                                       upp = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_diff_int_abs_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diff <- numeric(length(plot_diff_int_abs_diff$median))
  cumulative_low_diff <- numeric(length(plot_diff_int_abs_diff$low))
  cumulative_upp_diff <- numeric(length(plot_diff_int_abs_diff$upp))
  #set the first probability
  cumulative_median_diff[1] <- plot_diff_int_abs_diff$median[1]
  cumulative_low_diff[1] <- plot_diff_int_abs_diff$low[1]
  cumulative_upp_diff[1] <- plot_diff_int_abs_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diff_int_abs_diff$median)) {
    cumulative_median_diff[a] <- cumulative_median_diff[a-1] + (1 - cumulative_median_diff[a-1]) * plot_diff_int_abs_diff$median[a]
    cumulative_low_diff[a] <- cumulative_low_diff[a-1] + (1 - cumulative_low_diff[a-1]) * plot_diff_int_abs_diff$low[a]
    cumulative_upp_diff[a] <- cumulative_upp_diff[a-1] + (1 - cumulative_upp_diff[a-1]) * plot_diff_int_abs_diff$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diff_",k),cumulative_median_diff)
  assign(paste0("cumulative_low_diff_",k),cumulative_low_diff)
  assign(paste0("cumulative_upp_diff_",k),cumulative_upp_diff)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_diff < 0.5))
  age_after <- min(which(cumulative_median_diff >= 0.5))
  prob_before <- cumulative_median_diff[age_before]
  prob_after <- cumulative_median_diff[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_diff_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_a[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_a[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_diff_int_abs_diff$age[11:51], rev(plot_diff_int_abs_diff$age[11:51])), c(cumulative_low_diff[11:51], rev(cumulative_upp_diff[11:51])), col=alpha(palette_a[k], 0.1), border=palette_a[k])
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_a[k], lwd=3, lty=4)
}

#### Max. Short-term wealth variability ----

#set parameters for a legend outside of the plot
par(mar = c(5.1, 4.1, 4.1, 2.1))
#plot empty plot
plot(c(0,1)~c(10,ncol(post_int_abs_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Current absolute wealth with\nMaximum Short-term variability",
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(11, ncol(post_int_abs_diff$mu), by = 5), 
     labels = seq(10, ncol(post_int_abs_diff$mu)-1, by = 5),
     cex.axis=1.5
)

#add lines
for(k in 1:(length(deciles_diff_int_abs_diff))){
  #create matrix to store the data
  p_int_abs_diff <- matrix(nrow=nrow(post_int_abs_diff$mu),ncol=ncol(post_int_abs_diff$mu))
  p_int_abs_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_int_abs_diff$mu)){
    for(i in 1:nrow(post_int_abs_diff$mu)){
      p_int_abs_diff[i,j] <- inv_logit(post_int_abs_diff$alpha[i] + #inv logit because originally is logit
                                         post_int_abs_diff$mu[i,j] + #age
                                         (post_int_abs_diff$beta_wealth_z[i,j]*post_int_abs_diff$beta_wealth_sigma[i])*0 + #absolute wealth
                                         (post_int_abs_diff$gamma_wealth_z[i,j]*post_int_abs_diff$gamma_wealth_sigma[i])*0 + #wealth change
                                         (post_int_abs_diff$zeta_wealth_z[i,j]*post_int_abs_diff$zeta_wealth_sigma[i])*(deciles_absw_int_abs_diff[k]*deciles_diff_int_abs_diff[3]) #interaction
      )  
    }
  }
  #check data
  p_int_abs_diff
  #plot it!
  #prepare model prediction data
  plot_diff_int_abs_diff <- data.frame(age = 1:ncol(p_int_abs_diff),
                                       median = apply(p_int_abs_diff, 2, median), 
                                       upp = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p_int_abs_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_diff_int_abs_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diff <- numeric(length(plot_diff_int_abs_diff$median))
  cumulative_low_diff <- numeric(length(plot_diff_int_abs_diff$low))
  cumulative_upp_diff <- numeric(length(plot_diff_int_abs_diff$upp))
  #set the first probability
  cumulative_median_diff[1] <- plot_diff_int_abs_diff$median[1]
  cumulative_low_diff[1] <- plot_diff_int_abs_diff$low[1]
  cumulative_upp_diff[1] <- plot_diff_int_abs_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diff_int_abs_diff$median)) {
    cumulative_median_diff[a] <- cumulative_median_diff[a-1] + (1 - cumulative_median_diff[a-1]) * plot_diff_int_abs_diff$median[a]
    cumulative_low_diff[a] <- cumulative_low_diff[a-1] + (1 - cumulative_low_diff[a-1]) * plot_diff_int_abs_diff$low[a]
    cumulative_upp_diff[a] <- cumulative_upp_diff[a-1] + (1 - cumulative_upp_diff[a-1]) * plot_diff_int_abs_diff$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diff_",k),cumulative_median_diff)
  assign(paste0("cumulative_low_diff_",k),cumulative_low_diff)
  assign(paste0("cumulative_upp_diff_",k),cumulative_upp_diff)
  
  # Calculate exact median age at first birth
  age_before <- max(which(cumulative_median_diff < 0.5))
  age_after <- min(which(cumulative_median_diff >= 0.5))
  prob_before <- cumulative_median_diff[age_before]
  prob_after <- cumulative_median_diff[age_after]
  exact_age_median <- age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before)
  
  # Store the exact median age
  assign(paste0("exact_age_median_diff_", k), exact_age_median)
  
  #add median
  #add points
  points(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_a[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_diff[11:51] ~ plot_diff_int_abs_diff$age[11:51], col=palette_a[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_diff_int_abs_diff$age[11:51], rev(plot_diff_int_abs_diff$age[11:51])), c(cumulative_low_diff[11:51], rev(cumulative_upp_diff[11:51])), col=alpha(palette_a[k], 0.1), border=palette_a[k])
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_a[k], lwd=3, lty=4)
}

#add legend
# Add the legend in the last row
par(mar = c(0, 0, 0, 0))  # Remove margins for the legend plot
plot.new()  # Create a new empty plot for the legend
legend("center",c("Min.","Med.", "Max."),col=palette_a,lwd=3,pch=16,lty=1,pt.cex = 1.5,cex=1.2,horiz=T,bty = "n")

# Coefficient plots ----

# Set up plotting area with space at bottom
par(mfrow = c(1, 3), mar = c(4, 4, 4, 2) + 0.1, oma = c(1, 0, 0, 0), xpd = NA)

## Current wealth ----

plot(c(30:1)~c(tab_int_abs_diff_beta_z[11:40,1]*tab_int_abs_diff_beta_sigma[1,1]),xlim=c(-1.3,1.3),main="Current\nwealth",yaxt="n",xlab=expression(beta~coefficients),ylab="Ages",pch=16,col="#648FFF")
axis(2,c(30:1),c(10:39))
clip(-1, 1, 0, 31)
abline(v=0,lty=2)
for (i in 11:40){
  segments(
    c(tab_int_abs_diff_beta_z[i,1]*tab_int_abs_diff_beta_sigma[1,1])-c(tab_int_abs_diff_beta_z[i,2]*tab_int_abs_diff_beta_sigma[1,1]),
    41-i,
    c(tab_int_abs_diff_beta_z[i,1]*tab_int_abs_diff_beta_sigma[1,1])+c(tab_int_abs_diff_beta_z[i,2]*tab_int_abs_diff_beta_sigma[1,1]),
    41-i,
    lwd=2,col="#648FFF") 
}

## Short-term wealth variability ----

plot(c(30:1)~c(tab_int_abs_diff_gamma_z[11:40,1]*tab_int_abs_diff_gamma_sigma[1,1]),xlim=c(-1.3,1.3),main="Short-term\nwealth variability",yaxt="n",xlab=expression(gamma~coefficients),ylab="Ages",pch=16,col="#785EF0")
axis(2,c(30:1),c(10:39))
clip(-1, 1, 0, 31)
abline(v=0,lty=2)
for (i in 11:40){
  segments(
    c(tab_int_abs_diff_gamma_z[i,1]*tab_int_abs_diff_gamma_sigma[1,1])-c(tab_int_abs_diff_gamma_z[i,2]*tab_int_abs_diff_gamma_sigma[1,1]),
    41-i,
    c(tab_int_abs_diff_gamma_z[i,1]*tab_int_abs_diff_gamma_sigma[1,1])+c(tab_int_abs_diff_gamma_z[i,2]*tab_int_abs_diff_gamma_sigma[1,1]),
    41-i,
    lwd=2,col="#785EF0") 
}

## Interaction ----

plot(c(30:1)~c(tab_int_abs_diff_zeta_z[11:40,1]*tab_int_abs_diff_zeta_sigma[1,1]),xlim=c(-1.3,1.3),main="Interaction between\nwealth predictors",yaxt="n",xlab=expression(zeta~coefficients),ylab="Ages",pch=16,col="black")
axis(2,c(30:1),c(10:39))
clip(-1, 1, 0, 31)
abline(v=0,lty=2)
for (i in 11:40){
  segments(
    c(tab_int_abs_diff_zeta_z[i,1]*tab_int_abs_diff_zeta_sigma[1,1])-c(tab_int_abs_diff_zeta_z[i,2]*tab_int_abs_diff_zeta_sigma[1,1]),
    41-i,
    c(tab_int_abs_diff_zeta_z[i,1]*tab_int_abs_diff_zeta_sigma[1,1])+c(tab_int_abs_diff_zeta_z[i,2]*tab_int_abs_diff_zeta_sigma[1,1]),
    41-i,
    lwd=2,col="black") 
}


# Add common legend at bottom
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0.5, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlim = c(0, 1), ylim = c(0, 1))

# Create custom legend with proper spacing
legend("bottom", 
       legend = c("Wealth parameters:",expression(beta), expression(gamma), expression(zeta)),
       col = c(NA,"#648FFF", "#785EF0", "black"),
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
