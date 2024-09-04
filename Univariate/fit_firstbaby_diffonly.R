# Model with wealth change ----

#The code in this script is meant to fit a Bayesian model that aims to predict the probability of first birth by the amount of short-term wealth variability.

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
diff_list <- list(N = nrow(afrs_restricted), #population size
                  A = ncol(afrs_restricted), #age
                  wealth = std_absw_restricted, #current absolute wealth
                  baby = afrs_restricted, #AFR
                  N_miss = nrow(wealth_miss_restricted), # number of missing values that need imputation
                  wealth_miss=wealth_miss_restricted) # matrix indicating missing wealth data
#check data
diff_list

## Compile and fit model ----

# compile model

model_diff <- cmdstan_model("Univariate/firstbaby_diffonly.stan")

#fit model
fit_diff <- model_diff$sample(data = diff_list, 
                            chains = 4, 
                            parallel_chains = 15, 
                            adapt_delta = 0.95,
                            max_treedepth = 13,
                            init = 0)
# save fit 
fit_diff_csv <- rstan::read_stan_csv(fit_diff$output_files())
saveRDS(fit_diff_csv, "fit_diff_output.rds")
#load RDS file
rds_diff <- readRDS("fit_diff_output.rds")
#extract samples
post_diff <- extract.samples(rds_diff)

## Model diagnostics ----

#check trace of all parameters
#alpha
rstan::traceplot(rds_diff,pars="alpha")
#mu
traceplot(rds_diff,pars="mu") 
#mu_raw
traceplot(rds_diff,pars="mu_raw")
#mu_tau
rstan::traceplot(rds_diff,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds_diff,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds_diff,pars="mu_delta")
#gamma_wealth
traceplot(rds_diff,pars="gamma_wealth_z") 
#gamma_wealth
traceplot(rds_diff,pars="gamma_wealth_sigma") 

#summary of the model
#create summary tables of the different parameters

#alpha and hiper priors of Gaussian process
#create summary table for alpha and hiper priors of Gaussian process
tab_diff_alphagp <- precis(rds_diff,depth=2,pars=c("alpha",
                                                   "mu_raw",
                                                   "mu_tau",
                                                   "mu_delta"))
#check table
tab_diff_alphagp

#mu
#create summary table for mu
tab_diff_mu <- precis(rds_diff,depth=2,pars="mu")
#check table
tab_diff_mu
#plot it!
plot(tab_diff_mu)

#gamma z
#create summary table for gamma_z
tab_diff_gamma_z <- precis(rds_diff,depth=2,pars="gamma_wealth_z")
#check table
tab_diff_gamma_z
#plot it!
plot(tab_diff_gamma_z)

#gamma sigma
#create summary table for gamma_sigma
tab_diff_gamma_sigma <- precis(rds_diff,depth=2,pars="gamma_wealth_sigma")
#check table
tab_diff_gamma_sigma

#Distribution of wealth data

par(mfrow=c(1,3))
hist(post_diff$wealth_change,
     freq=F,
     col=hcl.colors(3,"ag_Sunset")[2],
     main="Short-term wealth variability",
     xlab="Absolute wealth difference"
)


## Plot it ----

#simulate wealth values
simwealth_change_diff <- seq(from=round(min(post_diff$wealth_change),1),to=round(max(post_diff$wealth_change),1),length.out=nrow(std_absw_restricted)) #specify according to range and length related to sample size
simwealth_change_diff
#get the deciles
deciles_diffw <- as.numeric(quantile(simwealth_change_diff,seq(0,1,0.5)))
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
plot(c(0,1)~c(10,ncol(post_diff$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Short-term variability\nof material wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(53,1,c("Min.","Med.", "Max."),col=palette_b,lwd=3,pch=shape,lty=type,pt.cex = 1.5,cex=1.2,box.col = NA)

#add lines
for(k in 1:(length(deciles_diffw))){
  #create matrix to store the data
  p_diffw_diff <- matrix(nrow=nrow(post_diff$mu),ncol=ncol(post_diff$mu))
  p_diffw_diff
  #fill it in with values for age 25
  for(j in 1:ncol(post_diff$mu)){
    for(i in 1:nrow(post_diff$mu)){
      p_diffw_diff[i,j] <- inv_logit(post_diff$alpha[i] + #inv logit because originally is logit
                                       post_diff$mu[i,j] + #age
                                       (post_diff$gamma_wealth_z[i,j]*post_diff$gamma_wealth_sigma[i])*deciles_diffw[k]  #wealth change
                                       ) #moving variance
    }
  }
  #check data
  p_diffw_diff
  #plot it!
  #prepare model prediction data
  plot_diffw_diff <- data.frame(age = 1:ncol(p_diffw_diff),
                                median = apply(p_diffw_diff, 2, median), 
                                upp = apply(p_diffw_diff, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p_diffw_diff, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #store data per decile
  assign(paste0("diffw_",k),plot_diffw_diff)
  
  # Calculate cumulative probabilities
  #create vectors
  cumulative_median_diffw <- numeric(length(plot_diffw_diff$median))
  cumulative_low_diffw <- numeric(length(plot_diffw_diff$low))
  cumulative_upp_diffw <- numeric(length(plot_diffw_diff$upp))
  #set the first probability
  cumulative_median_diffw[1] <- plot_diffw_diff$median[1]
  cumulative_low_diffw[1] <- plot_diffw_diff$low[1]
  cumulative_upp_diffw[1] <- plot_diffw_diff$upp[1]
  #calculate the cumulative probabilities for the other ages
  for (a in 2:length(plot_diffw_diff$median)) {
    cumulative_median_diffw[a] <- cumulative_median_diffw[a-1] + (1 - cumulative_median_diffw[a-1]) * plot_diffw_diff$median[a]
    cumulative_low_diffw[a] <- cumulative_low_diffw[a-1] + (1 - cumulative_low_diffw[a-1]) * plot_diffw_diff$low[a]
    cumulative_upp_diffw[a] <- cumulative_upp_diffw[a-1] + (1 - cumulative_upp_diffw[a-1]) * plot_diffw_diff$upp[a]
  }
  #store data per decile
  assign(paste0("cumulative_median_diffw_",k),cumulative_median_diffw)
  assign(paste0("cumulative_low_diffw_",k),cumulative_low_diffw)
  assign(paste0("cumulative_upp_diffw_",k),cumulative_upp_diffw)
  
  #add median
  #add points
  points(cumulative_median_diffw[11:51] ~ plot_diffw_diff$age[11:51], col=palette_b[k], pch=shape[k], cex=1.5)
  #add lines
  lines(cumulative_median_diffw[11:51] ~ plot_diffw_diff$age[11:51], col=palette_b[k], lwd=3, lty=type[k])
  #add confidence intervals
  polygon(c(plot_diffw_diff$age[11:51], rev(plot_diffw_diff$age[11:51])), c(cumulative_low_diffw[11:51], rev(cumulative_upp_diffw[11:51])), col=alpha(palette_b[k], 0.25), border=NA)
}
