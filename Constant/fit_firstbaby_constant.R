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
constant_list <- list(N = nrow(afrs_restricted), #population size
                      A = ncol(afrs_restricted), #age
                      wealth = std_absw_restricted, #current absolute wealth
                      baby = afrs_restricted, #AFR
                      N_miss = n_miss_restricted, # number of missing values that need imputation
                      wealth_miss=wealth_miss_restricted, # matrix indicating missing wealth data
                      median_wealth=std_median_wealth
) 
#check data
constant_list

## Compile and fit model ----

# compile model

model_constant <- cmdstan_model("~/wealth_afr/Constant/firstbaby_constant.stan")

#fit model
fit_constant <- model_constant$sample(data = constant_list,
                                      chains = 4, 
                                      parallel_chains = 15, 
                                      adapt_delta = 0.99,
                                      max_treedepth = 13,
                                      iter_warmup = 2000,
                                      iter_sampling = 2000,
                                      init = 0)

# save fit 
fit_constant_csv <- rstan::read_stan_csv(fit_constant$output_files())
saveRDS(fit_constant_csv, "fit_constant_output.rds")
#load RDS file
rds_constant <- readRDS("fit_constant_output.rds")
#extract samples
post_constant <- extract.samples(rds_constant)

## Model diagnostics ----

#check trace of all parameters
#alpha
rstan::traceplot(rds_constant,pars="alpha")
#mu
traceplot(rds_constant,pars="mu") 
#mu_raw
traceplot(rds_constant,pars="mu_raw")
#mu_tau
rstan::traceplot(rds_constant,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds_constant,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds_constant,pars="mu_delta")
#beta_wealth_z
rstan::traceplot(rds_constant,pars="beta_wealth_z") 
#beta_wealth_sigma
rstan::traceplot(rds_constant,pars="beta_wealth_sigma") 
#gamma_wealth
rstan::traceplot(rds_constant,pars="gamma_wealth_z") 
#gamma_wealth
rstan::traceplot(rds_constant,pars="gamma_wealth_sigma") 
#delta_wealth
rstan::traceplot(rds_constant,pars="delta_wealth_z") 
#delta_wealth
rstan::traceplot(rds_constant,pars="delta_wealth_sigma") 

#summary of the model
#create summary tables of the different parameters

#alpha and hiper priors of Gaussian process
#create summary table for alpha and hiper priors of Gaussian process
tab_constant_alphagp <- precis(rds_constant,depth=2,pars=c("alpha"))
#check table
tab_constant_alphagp

#beta z
#create summary table for beta_z
tab_constant_beta_z <- precis(rds_constant,depth=2,pars="beta_wealth_z")
#check table
tab_constant_beta_z

#beta sigma
#create summary table for beta_sigma
tab_constant_beta_sigma <- precis(rds_constant,depth=2,pars="beta_wealth_sigma")
#check table
tab_constant_beta_sigma

#gamma z
#create summary table for gamma_z
tab_constant_gamma_z <- precis(rds_constant,depth=2,pars="gamma_wealth_z")
#check table
tab_constant_gamma_z

#gamma sigma
#create summary table for gamma_sigma
tab_constant_gamma_sigma <- precis(rds_constant,depth=2,pars="gamma_wealth_sigma")
#check table
tab_constant_gamma_sigma

#delta z
#create summary table for delta_z
tab_constant_delta_z <- precis(rds_constant,depth=2,pars="delta_wealth_z")
#check table
tab_constant_delta_z

#delta sigma
#create summary table for delta_sigma
tab_constant_delta_sigma <- precis(rds_constant,depth=2,pars="delta_wealth_sigma")
#check table
tab_constant_delta_sigma

#Check correlation between wealth predictors

#beta versus gamma
#create correlation matrix
cor1 <- round(cor(post_constant$beta_wealth_z,post_constant$gamma_wealth_z),3)
#plot it!
corrplot(cor1, "color", tl.col="black")

#beta versus delta
#create correlation matrix
cor2 <- round(cor(post_constant$beta_wealth_z,post_constant$delta_wealth_z),3)
#plot it!
corrplot(cor2, "color", tl.col="black")

#gamma versus delta
#create correlation matrix
cor3 <- round(cor(post_constant$gamma_wealth_z,post_constant$delta_wealth_z),3)
#plot it!
corrplot(cor3, "color", tl.col="black")

#Distribution of wealth data

par(mfrow=c(1,3))
hist(post_constant$wealth_full,
     freq=F,
     col=hcl.colors(3,"ag_Sunset")[1],
     main="Current wealth",
     xlab="Std. current wealth"
)
hist(post_constant$wealth_change_std,
     freq=F,
     col=hcl.colors(3,"ag_Sunset")[2],
     main="Short-term wealth variability",
     xlab="Absolute wealth difference"
)
hist(post_constant$wealth_msd_std,
     freq=F,
     col=hcl.colors(3,"ag_Sunset")[3],
     main="Long-term wealth variability",
     xlab="Moving standard deviation"
)

# Plot the output of the model ----

## Current Wealth ----

#simulate wealth values
simwealth_absw_constant <- seq(from=round(min(post_constant$wealth_full),1),to=round(max(post_constant$wealth_full),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_absw_constant
#get the deciles
deciles_absw <- as.numeric(quantile(simwealth_absw_constant,seq(0,1,0.5)))
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
plot(c(0,1)~c(0,41),
     ylab="Complementary cumulative probability of first birth",
     xlab="Age",
     main="Current levels\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(0, 41, by = 5), 
     labels = seq(10, 51-1, by = 5),cex.axis=1.2)
# Combined legend
legend(44,1,  # or specify x/y coordinates
       legend = c("Complementary\ncumulative\nprobabilities","", "Min.", "Med.", "Max.",  # First group
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
  p_absw_constant <- matrix(nrow=nrow(post_constant$alpha),ncol=41)
  p_absw_constant
  #fill it in with values for age 25
  for(j in 1:41){
    for(i in 1:nrow(post_constant$alpha)){
      p_absw_constant[i,j] <- inv_logit(post_constant$alpha[i] + #inv logit because originally is logit
                                          (post_constant$beta_wealth_z[i]*post_constant$beta_wealth_sigma[i])*deciles_absw[k] + #absolute wealth
                                          (post_constant$gamma_wealth_z[i]*post_constant$gamma_wealth_sigma[i])*0 + #wealth change
                                          (post_constant$delta_wealth_z[i]*post_constant$delta_wealth_sigma[i])*0) #moving variance
    }
  }
  #check data
  p_absw_constant
  #plot it!
  #prepare model prediction data
  plot_absw_constant <- data.frame(age = 1:ncol(p_absw_constant),
                                   median = apply(p_absw_constant, 2, median), 
                                   upp = apply(p_absw_constant, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                   low = apply(p_absw_constant, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("absw_",k),plot_absw_constant)
  
  # Calculate COMPLEMENTARY cumulative probabilities (survival function)
  #create vectors
  complementary_cumulative_median_absw <- numeric(length(plot_absw_constant$median))
  complementary_cumulative_low_absw <- numeric(length(plot_absw_constant$low))
  complementary_cumulative_upp_absw <- numeric(length(plot_absw_constant$upp))
  
  #Start at age 10 with 100% childless
  complementary_cumulative_median_absw[1] <- 1  # Age 10
  complementary_cumulative_low_absw[1] <- 1
  complementary_cumulative_upp_absw[1] <- 1
  
  #calculate the complementary cumulative probabilities for the other ages
  for (a in 2:length(plot_absw_constant$median)) {
    complementary_cumulative_median_absw[a] <- complementary_cumulative_median_absw[a-1] * (1 - plot_absw_constant$median[a])
    complementary_cumulative_low_absw[a] <- complementary_cumulative_low_absw[a-1] * (1 - plot_absw_constant$low[a])
    complementary_cumulative_upp_absw[a] <- complementary_cumulative_upp_absw[a-1] * (1 - plot_absw_constant$upp[a])
  }
  
  #store data per decile
  assign(paste0("complementary_cumulative_median_absw_",k),complementary_cumulative_median_absw)
  assign(paste0("complementary_cumulative_low_absw_",k),complementary_cumulative_low_absw)
  assign(paste0("complementary_cumulative_upp_absw_",k),complementary_cumulative_upp_absw)
  
  # Calculate exact median age at first birth (age when complementary cumulative probability drops below 0.5)
  age_before <- max(which(complementary_cumulative_median_absw >= 0.5))
  age_after <- min(which(complementary_cumulative_median_absw < 0.5))
  prob_before <- complementary_cumulative_median_absw[age_before]
  prob_after <- complementary_cumulative_median_absw[age_after]
  exact_age_median <- (age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before))
  
  # Store the exact median age
  assign(paste0("exact_age_median_absw_", k), exact_age_median)
  
  #add median
  #add points
  points(complementary_cumulative_median_absw[1:41] ~ plot_absw_constant$age[1:41], col=palette_a[k], pch=16, cex=1.5)
  #add lines
  lines(complementary_cumulative_median_absw[1:41] ~ plot_absw_constant$age[1:41], col=palette_a[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_absw_constant$age[1:41], rev(plot_absw_constant$age[1:41])), c(complementary_cumulative_low_absw[1:41], rev(complementary_cumulative_upp_absw[1:41])), col=alpha(palette_a[k], 0.1), border=alpha(palette_a[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_a[k], lwd=3, lty=2)
}

## Short-term wealth variability ----

#simulate wealth values
simwealth_change_constant <- seq(from=round(min(post_constant$wealth_change_std),1),to=round(max(post_constant$wealth_change_std),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_change_constant
#get the deciles
deciles_diffw <- as.numeric(quantile(simwealth_change_constant,seq(0,1,0.5)))
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
plot(c(0,1)~c(0,41),
     ylab="Complementary cumulative probability of first birth",
     xlab="Age",
     main="Short-term\nwealth variability",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(0, 41, by = 5), 
     labels = seq(10, 51-1, by = 5),cex.axis=1.2)
# Combined legend
legend(44,1,  # or specify x/y coordinates
       legend = c("Complementary\ncumulative\nprobabilities","", "Min.", "Med.", "Max.",  # First group
                  "",
                  "Expected age\nat first birth","", "Min.", "Med.", "Max."), # Second group
       col = c(NA,NA, palette_b,NA,NA,NA,palette_b),  # NA for section headers
       lwd = 3,
       lty = c(NA,NA, c(1,1,1),NA,NA, NA, c(2,2,2)),  # NA for headers
       pch = c(NA,NA, rep(16, 3),NA,NA, NA, rep(NA, 3)),    # Symbols for Estimated only
       pt.cex = 1.5,
       cex = 1.2,
       box.col = NA,
       ncol = 1,  # Optional: arrange in 2 columns
       title = NULL)

#add lines
for(k in 1:(length(deciles_diffw))){
  #create matrix to store the data
  p_diffw_constant <- matrix(nrow=nrow(post_constant$alpha),ncol=41)
  p_diffw_constant
  #fill it in with values for age 25
  for(j in 1:41){
    for(i in 1:nrow(post_constant$alpha)){
      p_diffw_constant[i,j] <- inv_logit(post_constant$alpha[i] + #inv logit because originally is logit
                                           (post_constant$beta_wealth_z[i]*post_constant$beta_wealth_sigma[i])* 0 + #diffolute wealth
                                           (post_constant$gamma_wealth_z[i]*post_constant$gamma_wealth_sigma[i])* deciles_diffw[k] + #wealth change
                                           (post_constant$delta_wealth_z[i]*post_constant$delta_wealth_sigma[i])*0) #moving variance
    }
  }
  #check data
  p_diffw_constant
  #plot it!
  #prepare model prediction data
  plot_diffw_constant <- data.frame(age = 1:ncol(p_diffw_constant),
                                    median = apply(p_diffw_constant, 2, median), 
                                    upp = apply(p_diffw_constant, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                    low = apply(p_diffw_constant, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("diffw_",k),plot_diffw_constant)
  
  # Calculate COMPLEMENTARY cumulative probabilities (survival function)
  #create vectors
  complementary_cumulative_median_diffw <- numeric(length(plot_diffw_constant$median))
  complementary_cumulative_low_diffw <- numeric(length(plot_diffw_constant$low))
  complementary_cumulative_upp_diffw <- numeric(length(plot_diffw_constant$upp))
  
  #Start at age 10 with 100% childless
  complementary_cumulative_median_diffw[1] <- 1  # Age 10
  complementary_cumulative_low_diffw[1] <- 1
  complementary_cumulative_upp_diffw[1] <- 1
  
  #calculate the complementary cumulative probabilities for the other ages
  for (a in 2:length(plot_diffw_constant$median)) {
    complementary_cumulative_median_diffw[a] <- complementary_cumulative_median_diffw[a-1] * (1 - plot_diffw_constant$median[a])
    complementary_cumulative_low_diffw[a] <- complementary_cumulative_low_diffw[a-1] * (1 - plot_diffw_constant$low[a])
    complementary_cumulative_upp_diffw[a] <- complementary_cumulative_upp_diffw[a-1] * (1 - plot_diffw_constant$upp[a])
  }
  
  #store data per decile
  assign(paste0("complementary_cumulative_median_diffw_",k),complementary_cumulative_median_diffw)
  assign(paste0("complementary_cumulative_low_diffw_",k),complementary_cumulative_low_diffw)
  assign(paste0("complementary_cumulative_upp_diffw_",k),complementary_cumulative_upp_diffw)
  
  # Calculate exact median age at first birth (age when complementary cumulative probability drops below 0.5)
  age_before <- max(which(complementary_cumulative_median_diffw >= 0.5))
  age_after <- min(which(complementary_cumulative_median_diffw < 0.5))
  prob_before <- complementary_cumulative_median_diffw[age_before]
  prob_after <- complementary_cumulative_median_diffw[age_after]
  exact_age_median <- (age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before))
  
  # Store the exact median age
  assign(paste0("exact_age_median_diffw_", k), exact_age_median)
  
  #add median
  #add points
  points(complementary_cumulative_median_diffw[1:41] ~ plot_diffw_constant$age[1:41], col=palette_b[k], pch=16, cex=1.5)
  #add lines
  lines(complementary_cumulative_median_diffw[1:41] ~ plot_diffw_constant$age[1:41], col=palette_b[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_diffw_constant$age[1:41], rev(plot_diffw_constant$age[1:41])), c(complementary_cumulative_low_diffw[1:41], rev(complementary_cumulative_upp_diffw[1:41])), col=alpha(palette_b[k], 0.1), border=alpha(palette_b[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_b[k], lwd=3, lty=2)
}

## Long-term variability of wealth ----

#simulate wealth values
simwealth_msd_constant <- seq(from=round(min(post_constant$wealth_msd_std),1),to=round(max(post_constant$wealth_msd_std),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_msd_constant
#get the deciles
deciles_msd <- as.numeric(quantile(simwealth_msd_constant,seq(0,1,0.5)))
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
plot(c(0,1)~c(0,41),
     ylab="Complementary cumulative probability of first birth",
     xlab="Age",
     main="Long-term\nwealth variability",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n",
     xaxt="n")

#x-axis should follow biological ages
axis(1,
     at = seq(0, 41, by = 5), 
     labels = seq(10, 51-1, by = 5),cex.axis=1.2)

# Combined legend
legend(44,1,  # or specify x/y coordinates
       legend = c("Complementary\ncumulative\nprobabilities","", "Min.", "Med.", "Max.",  # First group
                  "",
                  "Expected age\nat first birth","", "Min.", "Med.", "Max."), # Second group
       col = c(NA,NA, palette_c,NA,NA,NA,palette_c),  # NA for section headers
       lwd = 3,
       lty = c(NA,NA, c(1,1,1),NA,NA, NA, c(2,2,2)),  # NA for headers
       pch = c(NA,NA, rep(16, 3),NA,NA, NA, rep(NA, 3)),    # Symbols for Estimated only
       pt.cex = 1.5,
       cex = 1.2,
       box.col = NA,
       ncol = 1,  # Optional: arrange in 2 columns
       title = NULL)

#add lines
for(k in 1:(length(deciles_msd))){
  #create matrix to store the data
  p_msd_constant <- matrix(nrow=nrow(post_constant$alpha),ncol=41)
  p_msd_constant
  #fill it in with values for age 25
  for(j in 1:41){
    for(i in 1:nrow(post_constant$alpha)){
      p_msd_constant[i,j] <- inv_logit(post_constant$alpha[i] + #inv logit because originally is logit
                                         (post_constant$beta_wealth_z[i]*post_constant$beta_wealth_sigma[i])* 0 + #diffolute wealth
                                         (post_constant$gamma_wealth_z[i]*post_constant$gamma_wealth_sigma[i])*0 + #wealth change
                                         (post_constant$delta_wealth_z[i]*post_constant$delta_wealth_sigma[i])* deciles_msd[k]) #moving variance
    }
  }
  #check data
  p_msd_constant
  #plot it!
  #prepare model prediction data
  plot_msd_constant <- data.frame(age = 1:ncol(p_msd_constant),
                                  median = apply(p_msd_constant, 2, median), 
                                  upp = apply(p_msd_constant, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p_msd_constant, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("msd_",k),plot_msd_constant)
  
  # Calculate COMPLEMENTARY cumulative probabilities (survival function)
  #create vectors
  complementary_cumulative_median_msd <- numeric(length(plot_msd_constant$median))
  complementary_cumulative_low_msd <- numeric(length(plot_msd_constant$low))
  complementary_cumulative_upp_msd <- numeric(length(plot_msd_constant$upp))
  
  #Start at age 10 with 100% childless
  complementary_cumulative_median_msd[1] <- 1  # Age 10
  complementary_cumulative_low_msd[1] <- 1
  complementary_cumulative_upp_msd[1] <- 1
  
  #calculate the complementary cumulative probabilities for the other ages
  for (a in 2:length(plot_msd_constant$median)) {
    complementary_cumulative_median_msd[a] <- complementary_cumulative_median_msd[a-1] * (1 - plot_msd_constant$median[a])
    complementary_cumulative_low_msd[a] <- complementary_cumulative_low_msd[a-1] * (1 - plot_msd_constant$low[a])
    complementary_cumulative_upp_msd[a] <- complementary_cumulative_upp_msd[a-1] * (1 - plot_msd_constant$upp[a])
  }
  
  #store data per decile
  assign(paste0("complementary_cumulative_median_msd_",k),complementary_cumulative_median_msd)
  assign(paste0("complementary_cumulative_low_msd_",k),complementary_cumulative_low_msd)
  assign(paste0("complementary_cumulative_upp_msd_",k),complementary_cumulative_upp_msd)
  
  # Calculate exact median age at first birth (age when complementary cumulative probability drops below 0.5)
  age_before <- max(which(complementary_cumulative_median_msd >= 0.5))
  age_after <- min(which(complementary_cumulative_median_msd < 0.5))
  prob_before <- complementary_cumulative_median_msd[age_before]
  prob_after <- complementary_cumulative_median_msd[age_after]
  exact_age_median <- (age_before + (0.5 - prob_before) * (age_after - age_before) / (prob_after - prob_before))
  
  # Store the exact median age
  assign(paste0("exact_age_median_msd_", k), exact_age_median)
  
  #add median
  #add points
  points(complementary_cumulative_median_msd[1:41] ~ plot_msd_constant$age[1:41], col=palette_c[k], pch=16, cex=1.5)
  #add lines
  lines(complementary_cumulative_median_msd[1:41] ~ plot_msd_constant$age[1:41], col=palette_c[k], lwd=3, lty=1)
  #add confidence intervals
  polygon(c(plot_msd_constant$age[1:41], rev(plot_msd_constant$age[1:41])), c(complementary_cumulative_low_msd[1:41], rev(complementary_cumulative_upp_msd[1:41])), col=alpha(palette_c[k], 0.1), border=alpha(palette_c[k],0.25))
  # Add vertical line for median age at first birth
  segments(x0 = exact_age_median, y0 = 0, x1 = exact_age_median, y1 = 0.5, col = palette_c[k], lwd=3, lty=2)
}


# Coefficient plots ----

# Set up plotting area with space at bottom
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.5, oma = c(1, 0, 0, 0), xpd = NA)

#general plot
plot(c(0,4)~c(-1,1),
     xlim=c(-0.5,0.5),
     main="Wealth predictors",
     yaxt="n",
     xlab="Coefficients",
     ylab="Predictors",
     pch=16,
     col="#648FFF",
     type="n",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     mgp = c(2.5, 0.7, 0)
)
axis(2,c(1:3),c(expression(beta), expression(gamma), expression(delta)))
clip(-1, 1, 0, 4)
abline(v=0,lty=2)

## Current wealth ----
points(c(tab_constant_beta_z[1,1]*tab_constant_beta_sigma[1,1]),
       1,
       pch=16,col="#648FFF")
segments(
  c(tab_constant_beta_z[1,1]*tab_constant_beta_sigma[1,1])-c(tab_constant_beta_z[2,1]*tab_constant_beta_sigma[1,1]),
  1,
  c(tab_constant_beta_z[1,1]*tab_constant_beta_sigma[1,1])+c(tab_constant_beta_z[2,1]*tab_constant_beta_sigma[1,1]),
  1,
  lwd=2,col="#648FFF") 

## Short-term wealth variability ----
points(c(tab_constant_gamma_z[1,1]*tab_constant_gamma_sigma[1,1]),
       2,
       pch=16,col="#785EF0")
segments(
  c(tab_constant_gamma_z[1,1]*tab_constant_gamma_sigma[1,1])-c(tab_constant_gamma_z[2,1]*tab_constant_gamma_sigma[1,1]),
  2,
  c(tab_constant_gamma_z[1,1]*tab_constant_gamma_sigma[1,1])+c(tab_constant_gamma_z[2,1]*tab_constant_gamma_sigma[1,1]),
  2,
  lwd=2,col="#785EF0") 

## Long-term wealth variability ----
points(c(tab_constant_delta_z[1,1]*tab_constant_delta_sigma[1,1]),
       3,
       pch=16,col="#DC267F")
segments(
  c(tab_constant_delta_z[1,1]*tab_constant_delta_sigma[1,1])-c(tab_constant_delta_z[2,1]*tab_constant_delta_sigma[1,1]),
  3,
  c(tab_constant_delta_z[1,1]*tab_constant_delta_sigma[1,1])+c(tab_constant_delta_z[2,1]*tab_constant_delta_sigma[1,1]),
  3,
  lwd=2,col="#DC267F") 

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
       text.width = c(0.22, 0.01, 0.01, 0.01),
       pt.cex = 1.5,
       cex = 1.5
)

