#Code to validate the model using synthetic data ----

# Load the necessary libraries  ----

library(cmdstanr)
library(rethinking)
library(scales)
library(corrplot)
library(ggplot2)
library(scales)

#Import the data ----

# We base the simulation on the real data.  
# We therefore first load these data to calculate the distributions of the key variables in the original data to check whether the simulated data reflects these.
# We first load the data directly from github.
real_data <- read.csv(url("https://raw.githubusercontent.com/pjve90/wealth_afr/refs/heads/master/Data/dataf.csv"), header=T, sep=",", stringsAsFactors=F)[,-1] 

#Data transformation ----

# We then transform the data to calculate the three predictor variables. These transformations are the same that are used to prepare the data for the analyses.

## Age at first reproduction ----
#create a matrix to store the age-specific age of censor
afr_matrix <- matrix(nrow=nrow(real_data),ncol=max(real_data$aoc)+1) #add 1 because column 1 is age 0 (birth)
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
#check data
afr_matrix

#Calculate the age-specific probabilities of first birth
agespecific_probabilities<-apply(afr_matrix,2,sum,na.rm=T)/apply(afr_matrix,2,function(x)sum(!is.na(x)))
#check data
agespecific_probabilities

##Material wealth ----

##Current absolute wealth ----

#create matrix to store the amount of wealth at each age
absw_matrix <- matrix(nrow = nrow(real_data),ncol=max(real_data$aoc)+1) #add 1 because column 1 is age 0 (birth)
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

#standardise the log-transformed current absolute wealth
std_absw_matrix <- matrix(standardize(log(as.vector(absw_matrix))),ncol=ncol(absw_matrix),nrow=nrow(absw_matrix))
#check data
std_absw_matrix

##Short-term wealth variability ----

#create matrix
change_matrix <-  matrix(nrow = nrow(std_absw_matrix),ncol=ncol(std_absw_matrix))

#calculate the short-term wealth variability
for(i in 1:nrow(change_matrix)){
  for(j in 1:2){
    change_matrix[i,j] <- 0 #setting zero change at birth and first year, since wealth change is calculated with a 2-years lag
  }
  for(j in 3:ncol(change_matrix)){
    change_matrix[i,j] = abs(std_absw_matrix[i,j] - std_absw_matrix[i,j-2]) #calculating the 2-years lagged wealth change
  }
}
#check data
change_matrix

#Long-term wealth variability - 
#create matrix
msdw_matrix <-  matrix(nrow = nrow(std_absw_matrix),ncol=ncol(std_absw_matrix))

#calculate the long-term wealth variability
for(i in 1:nrow(msdw_matrix)){
  for(j in 1:10){
    msdw_matrix[i,j] <- 0 #setting zero standard deviation from birth until age 10 at birth and first year, since wealth change is calculated with a 10-years window
  }
  for(j in 11:ncol(msdw_matrix)){
    msdw_matrix[i,j] = sd(std_absw_matrix[i,(j-10):j],na.rm=T) #calculating the moving standard deviation with a 10-years window
  }
}
#check data
msdw_matrix

##Individual median ----

# We use the median wealth of each individual as the starting point for the simulations, to reflect the scale of the interindividual differences.
#Get the median of each individual
medianwealthperindividual<-NA
for(individual in 1:nrow(std_absw_matrix)){
  medianwealthperindividual[individual]<-median(std_absw_matrix[individual,],na.rm=T)
}
#check the data
medianwealthperindividual
#If there are individuals without wealth data, sample random values from a normal(0,1) distribution since the data is standardised
medianwealthperindividual[is.na(medianwealthperindividual)]<-rnorm(sum(is.na(medianwealthperindividual)),0,1)
#check the data
medianwealthperindividual

# Data  simulation ----

# First, we generate the three predictor variables: absolute amount of wealth, short-term variability in wealth, and long-term variability in wealth.
# We start by simulating the absolute wealth of each individual. The wealth distribution of the original dataset is slightly skewed (more individuals with low wealth, some very rich individuals). To reflect this, we set up a process that makes it more likely that an average individual will have slightly less than the average wealth, but some individuals might end up with more. We also include the trend in the original data that wealth increases as individuals get older. 
# We simulate 495 individuals (the same as in the original data), one in each row. For these individuals, we simulate 73 years, one in each column. We chose 73 years since that is the oldest age in the sample

## Simulate material wealth ----

### Current wealth ----

# We set up the data frame to store the data
simwealth<-matrix(NA,ncol=ncol(std_absw_matrix),nrow=nrow(std_absw_matrix))
#check the data
simwealth

# The wealth data we simulate reflect the standardised values. That means that the average will be close to 0 and the standard deviation close to 1.

# We start at the youngest age. To create variation among individuals, we assign them a random value sampled from a normal distribution with average = median individual wealth, and sigma = 1.
for(individual in 1:nrow(simwealth)){
  simwealth[individual,1] <- rnorm(1,medianwealthperindividual[individual],1)
}
#check data
simwealth[,1]

#We now simulate the change each year of age.
#The process we use to reflect the change in absolute wealth from one year to the next matches the process in the imputation of missing data in the analyses. That means that the value of absolute wealth of the following year is similar to the previous year with probability alpha_miss, if it is dissimilar change to be close to value beta_miss, with some noise around these two values indicated by sigma_miss
set.seed(1934)
#define the priors in the autorregresive dynamic
alpha_miss<-runif(1,min=0.5,max=1)
beta_miss<-rnorm(1,0,0.1)
sigma_miss<-rexp(1,3)
#simulate the wealth
for(individual in 1:nrow(simwealth)){
  for(ages in 2:ncol(simwealth)){
    simwealth[individual,ages]<-rnorm(1,(simwealth[individual,ages-1]*alpha_miss+(1-alpha_miss)*(beta_miss)), sigma_miss)
  }
}
#check the data
simwealth

###Short-term wealth variability ----

# Based on the amount of wealth individuals have, we calculate their change in wealth from one year to the next to create the predictor variable of short-term variability in wealth. For this, we only fill in the ages we are interested in, from age 11 to age 60.  
#create matrix
simshorttermwealth<-matrix(NA,ncol=ncol(std_absw_matrix),nrow=nrow(std_absw_matrix))
#calculate the short-term wealth variability
for(individual in 1:nrow(simshorttermwealth)){
  for(ages in 3:ncol(simwealth)){
    simshorttermwealth[individual,ages]<-abs(simwealth[individual,ages]-simwealth[individual,ages-2])
  }
}
#check data
simshorttermwealth

### Long-term wealth variability ----

# Based on the amount of wealth individuals, we calculate the standard deviation over the past 10 years to create the predictor variable of long-term variability in wealth. Again, we only calculate this for the relevant age range
#Create matrix
simlongtermwealth<-matrix(NA,ncol=ncol(std_absw_matrix),nrow=nrow(std_absw_matrix))
#Calculate the long-term wealth variability
for(individual in 1:nrow(simlongtermwealth)){
  for(ages in 11:ncol(simwealth)){
    simlongtermwealth[individual,ages]<-sd(as.numeric(simwealth[individual,(ages-10):(ages)]))
  }
}
#check data
simlongtermwealth

# We now restrict the absolute wealth data frame to the relevant ages
simwealth_restricted<-simwealth[,11:51]

# We can check whether wealth within individuals wealth is correlated from one year to the next
plot(simwealth[,14]~simwealth[,13])

# We can then check the overall distribution of the three predictor values in the simulated data, and compare it to their distribution in the real data
plot(density(simwealth),ylim=c(0,0.45))
lines(density(std_absw_matrix,na.rm=T),col="red")
hist(simshorttermwealth)
hist(simlongtermwealth)

## Simulate age-specific probabilities of first birth ----

### Based on literature ----

# First, need the age-specific probabilities of birth. We base these probabilities on the demographic literature to have a peak at early ages before dropping off again
afr_age<-c(seq(from=0.001, to=0.003,length.out=10),seq(from=0.03, to=0.12,length.out=5),seq(from=0.14, to=0.05,length.out=5),seq(from=0.01, to=0.005,length.out=10),seq(from=0.005, to=0.0001,length.out=21))

# alternatively, we base them directly on the observed age-specific probabilities here. Given the small sample, the observed data is not a distribution but has some peaks. We smooth these to generate a distribution
agespecific_probabilities_rounded<-rep(0,74)
for(i in 2:(ncol(afr_matrix))){
  agespecific_probabilities_rounded[i-1]<-(agespecific_probabilities[i-1]+agespecific_probabilities[i]+agespecific_probabilities[i-1])/3
}
for(i in 2:(ncol(afr_matrix))){
  agespecific_probabilities_rounded[i-1]<-(agespecific_probabilities_rounded[i-1]+agespecific_probabilities_rounded[i]+agespecific_probabilities_rounded[i-1])/3
}
for(i in 2:(ncol(afr_matrix))){
  agespecific_probabilities_rounded[i-1]<-(agespecific_probabilities_rounded[i-1]+agespecific_probabilities_rounded[i]+agespecific_probabilities_rounded[i-1])/3
}
for(i in 2:(ncol(afr_matrix))){
  agespecific_probabilities_rounded[i-1]<-(agespecific_probabilities_rounded[i-1]+agespecific_probabilities_rounded[i]+agespecific_probabilities_rounded[i-1])/3
}

#compare the one based on the literature versus the one from the data
plot(NA, xlim=c(1,ncol(afr_matrix)),ylim=c(0,0.5))
lines(agespecific_probabilities[1:ncol(afr_matrix)]~c(1:ncol(afr_matrix)))
lines(agespecific_probabilities_rounded~c(1:ncol(afr_matrix)))
lines(afr_age~c(11:61))

# Blank out this line to use the one based on the data
afr_age<-agespecific_probabilities_rounded

##Simulate first birth based on the wealth predictors ----

###Coefficients are constant across ages ----

# the way it is setup, the wealth variables affect the overall probability of giving birth at each age - that means, individuals reproduce earlier because they overall (at all ages) have higher probability of reproducing. The age biases arise because individuals who have already given birth drop out, leaving only those with a different wealth to give birth later. 

# To generate the actual ages at first birth, we take the age-specific probabilities plus the effect of the three wealth predictors.

set.seed(1574)

# Logistic function to keep probabilities in (0, 1)
logit <- function(x) { 1 / (1 + exp(-x)) }

# Simulate 1000 different scenarios (iterations)
num_simulations <- 1
num_ages <- ncol(afr_matrix)
#cumulative_probs_simulations <- matrix(NA, nrow = num_simulations, ncol = num_ages)

# Age-specific coefficients
alpha <- rnorm(1, 0, 1)
beta_z  <- rnorm(1, 0, 1)
gamma_z <- rnorm(1, 0, 1)
delta_z <- rnorm(1, 0, 1)

# Simulated birth data for each simulation - simulation from 10 years old onwards
for (sim in 1:num_simulations) {
  
  # Simulated births: 495 individuals x 74 ages
  simbirth <- as.data.frame(matrix(NA, ncol = num_ages, nrow = nrow(afr_matrix)))
  
  for(individual in 1:nrow(simbirth)) {
    
    # --- Age 10 (column 11): everyone is eligible ---
    linpred <- afr_age[11] +
      beta_z  * simwealth[individual, 11] +
      gamma_z * simshorttermwealth[individual, 11] +
      delta_z * simlongtermwealth[individual, 11]
    
    # Apply logit transformation to ensure probability is in (0, 1)
    ageprob <- logit(linpred)
    simbirth[individual, 11] <- rbinom(1, 1, ageprob)
    
    # --- Ages 11 to 50 (columns 12 to 51): only if not already given birth ---
    for(ages in 12:num_ages) {
      
      linpred <- afr_age[ages] +
        beta_z  * simwealth[individual, ages] +
        gamma_z * simshorttermwealth[individual, ages] +
        delta_z * simlongtermwealth[individual, ages]
      
      # Apply logit transformation to ensure probability is in (0, 1)
      ageprob <- logit(linpred)
      
      # If birth already occurred, leave age value as NA
      if (simbirth[individual, ages - 1] == 1 | is.na(simbirth[individual, ages - 1]) == T) {
        simbirth[individual, ages] <- NA  # birth already occurred
      } else {
        simbirth[individual, ages] <- rbinom(1, 1, ageprob)
      }
    }
  }
  
  #fix so the first 10 years are zeros instead of NAs
  simbirth[1:10] <- 0
} 
#check data
simbirth
#counts per column
apply(simbirth,2,sum,na.rm=T)

###Coefficients are age-specific ----

# Link wealth variables to afr with independent effects at each age - exactly what we are doing in the STAN model

set.seed(1578)

# Logistic function to keep probabilities in (0, 1)
logit <- function(x) { 1 / (1 + exp(-x)) }

# Simulate 1000 different scenarios (iterations)
num_simulations <- 1
num_ages <- 74
cumulative_probs_simulations <- matrix(NA, nrow = num_simulations, ncol = num_ages)

# Age-specific coefficients
beta_z  <- rnorm(74, 0, 1)
delta_z <- rnorm(74, 0, 1)
gamma_z <- rnorm(74, 0, 1)

# Simulated birth data for each simulation
for (sim in 1:num_simulations) {
  
  # Simulated births: 495 individuals x 74 ages
  simbirth <- as.data.frame(matrix(NA, ncol = num_ages, nrow = nrow(afr_matrix)))
  
  for(individual in 1:nrow(simbirth)) {
    
    # --- Age 10 (column 11): everyone is eligible ---
    linpred <- afr_age[11] +
      beta_z[11]  * simwealth[individual, 11] +
      gamma_z[11] * simshorttermwealth[individual, 11] +
      delta_z[11] * simlongtermwealth[individual, 11]
    
    # Apply logit transformation to ensure probability is in (0, 1)
    ageprob <- logit(linpred)
    simbirth[individual, 11] <- rbinom(1, 1, ageprob)
    
    # --- Ages 11 to 50 (columns 12 to 51): only if not already given birth ---
    for(ages in 12:num_ages) {
      
      linpred <- afr_age[ages] +
        beta_z[ages]  * simwealth[individual, ages] +
        gamma_z[ages] * simshorttermwealth[individual, ages] +
        delta_z[ages] * simlongtermwealth[individual, ages]
      
      # Apply logit transformation to ensure probability is in (0, 1)
      ageprob <- logit(linpred)
      
      # If birth already occurred, leave age value as NA
      if (simbirth[individual, ages - 1] == 1 | is.na(simbirth[individual, ages - 1]) == T) {
        simbirth[individual, ages] <- NA  # birth already occurred
      } else {
        simbirth[individual, ages] <- rbinom(1, 1, ageprob)
      }
    }
  }
  
  #fix so the first 10 years are zeros instead of NAs
  simbirth[1:10] <- 0
}
#check data
simbirth
#counts per column
apply(simbirth,2,sum,na.rm=T)

### #Centered age approach ----
# 
# # Alternative approach to link wealth variables to afr
# # It would mean introducing another predictor, mean-centered age: the median age is set to 0, ages younger than the median get negative values, ages older than the median positive values (calculate simply as age - median(age)). We would then use this together with the betawealth. In the stan model, we have a beta for each age - not sure whether that means we are loosing information because we are not pooling across the ages. 
# # Here we make the beta_wealth parameters age dependent - that is, they would be negative at younger ages and positive at older ages. I think it would mean we have a betawealth in the model that itself is not just coming from a prior, but based on another model
# # betamuwealth <- centeredage * betamuwealth
# 
# # Let's make it linear: intercept would be whether overall wealth has a positive or negative effect; slope is whether effect depends on age
# 
# centeredage<-seq(from=-1,to=2,length.out=51)
# 
# # Centered age is median age of first birth
# 
# # a positive effect (the slope) means that individuals are less likely to reproduce when they are young (because the centeredage is negative, leading to a reduction in the probability) but a higher probability to reproduce when they are old (because the centeredage is positive)
# # We could also change the intercept, the overall probability to have a first baby, according to wealth. For this example we keep the intercepts at zero though, to only look at shifts in reproduction in relation to wealth
# intercept_beta <- 0 # negative values would mean that poor are more likely to have babies
# slope_beta <- 0.1 # positive slope means wealthy have afr later
# 
# intercept_gamma <- 0
# slope_gamma <- 0.01 # positive slope means individuals with higher short-term wealth variability have afr later
# 
# intercept_delta <- 0
# slope_delta <- 0.01 # positive slope means individuals with higher long-term wealth variability have afr later
# 
# simbirth<-as.data.frame(matrix(NA,ncol=51,nrow=495))
# for(individual in 1:nrow(simbirth)){
#   beta_z<-intercept_beta+slope_beta*centeredage[1]
#   delta_z<-intercept_delta+slope_delta*centeredage[1]
#   gamma_z<-intercept_gamma+slope_gamma*centeredage[1]
#   ageprob<-afr_age[1]+simwealth[individual,1]*beta_z+simshorttermwealth[individual,1]*gamma_z+simlongtermwealth[individual,1]*delta_z
#   if(ageprob<0){ageprob<-0}
#   simbirth[individual,1]<-rbinom(1,1,ageprob)
#   for(ages in 2:ncol(simbirth)){
#     beta_z<-intercept_beta+slope_beta*centeredage[ages]
#     delta_z<-intercept_delta+slope_delta*centeredage[ages]
#     gamma_z<-intercept_gamma+slope_gamma*centeredage[ages]
#     ageprob<-afr_age[ages]+simwealth[individual,ages]*beta_z+simshorttermwealth[individual,ages]*gamma_z+simlongtermwealth[individual,ages]*delta_z
#     if(ageprob<0){ageprob<-0}
#     ifelse(simbirth[individual,(ages-1)]==1,simbirth[individual,ages]<-NA,simbirth[individual,ages]<-rbinom(1,1,ageprob))
#   }
# }
# 
# # We now have all the data in the same format as in the original data. That means we can perform the same data checks, plus run the inference model:
# # end data simulations--------------------------------------------------------
# 
# 
# 
# 
# #------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------
# # We can perform visualisations to compare the simulated data to the real data  ----
# #------------------------------------------------------------------------------------------------
# 
# # get the age at first reproduction for each individual
# simafr<-matrix(nrow=nrow(simbirth),ncol=2)
# simafr[,1]<-c(1:nrow(simafr))
# for(i in 1:nrow(simbirth)){
#   ifelse(sum(simbirth[i,],na.rm=T)==0,simafr[i,2]<-NA,simafr[i,2]<-which(simbirth[i,]==1))
# }
# 
# # get the average wealth for each individual
# avgwealth<-matrix(nrow=nrow(simwealth),ncol=2)
# avgwealth[,1]<-c(1:nrow(avgwealth))
# for(i in 1:nrow(simwealth)){
#   avgwealth[i,2]<-mean(as.numeric(simwealth[i,]))
# }
# 
# plot(simafr[,2]~avgwealth[,2])
# 
# afr_s<-matrix(NA,ncol=4,nrow=nrow(simafr))
# afr_s[,1]<-simafr[,1]
# afr_s[,2]<-simafr[,2]
# # Change the plot to show the distribution of afr according to whether individuals have low, medium, high wealth
# afr_s[,3]<-avgwealth[,2]
# afr_s<-as.data.frame(afr_s)
# 
# for(i in 1:nrow(afr_s)){
#   ifelse(afr_s[i,3]< -0.5,afr_s[i,4]<-1,ifelse(afr_s[i,3]>0.5,afr_s[i,4]<-3,afr_s[i,4]<-2))
# }
# 
# ggplot(afr_s,aes(x=as.factor(V4),y=V2))+geom_dotplot(binaxis='y',stackdir='center')
# 
# # compare the estimated average ages at first birth for the three groups
# # highest wealth
# mean(afr_s[afr_s$V4==3,]$V2,na.rm=T)
# # medium wealth
# mean(afr_s[afr_s$V4==2,]$V2,na.rm=T)
# # lowest wealth
# mean(afr_s[afr_s$V4==1,]$V2,na.rm=T)
# 
# 
# 
# 
# # Do this for the real data
# afr_matrix_na<-afr_matrix
# 
# for(j in 1:ncol(afr_matrix_na)){
#   for(i in 1:nrow(afr_matrix_na)){
#     if((afr_matrix_na[i,j]==-99)){
#       afr_matrix_na[i,j] <- NA
#     } else{
#       afr_matrix_na[i,j] <- afr_matrix_na[i,j]
#     }
#   }
# }
# 
# afr_r<-matrix(nrow=nrow(afr_matrix_na),ncol=5)
# afr_r[,1]<-c(1:nrow(afr_r))
# for(i in 1:nrow(afr_matrix_na)){
#   ifelse(sum(afr_matrix_na[i,],na.rm=T)==0,afr_r[i,2]<-NA,afr_r[i,2]<-which(afr_matrix_na[i,]==1))
# }
# 
# avgwealth_r<-matrix(nrow=nrow(absw_matrix),ncol=2)
# avgwealth_r[,1]<-c(1:nrow(avgwealth_r))
# for(i in 1:nrow(absw_matrix)){
#   avgwealth_r[i,2]<-mean(as.numeric(absw_matrix[i,]),na.rm=T)
# }
# 
# plot(afr_r[,2]~log(avgwealth_r[,2]))
# 
# # Change the plot to show the distribution of afr according to whether individuals have low, medium, high wealth
# afr_r[,3]<-avgwealth_r[,2]
# afr_r[,4]<-log(afr_r[,3])
# afr_r<-as.data.frame(afr_r)
# afr_r<-afr_r[is.na(afr_r[,2])==F,]
# afr_r<-afr_r[is.na(afr_r[,4])==F,]
# 
# afr_r[,5]<-ifelse(afr_r[,4]<median(afr_r[,4]),1,2)
# 
# for(i in 1:nrow(afr_r)){
#   ifelse(afr_r[i,4]<5.3,afr_r[i,5]<-1,ifelse(afr_r[i,4]>6.5,afr_r[i,5]<-3,afr_r[i,5]<-2))
# }
# 
# ggplot(afr_r,aes(x=as.factor(V5),y=V2))+geom_dotplot(binaxis='y',stackdir='center')
# 
# # compare the estimated average ages at first birth for the three groups
# # highest wealth
# mean(afr_r[afr_r$V5==3,]$V2)
# # medium wealth
# mean(afr_r[afr_r$V5==2,]$V2)
# # lowest wealth
# mean(afr_r[afr_r$V5==1,]$V2)
# 
# # end of visualisations -------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------
# 
# 
# 
# 
# 
# #------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------
# # For the analyses, we can use the full dataset of simulated wealth and age at first birth observations
# # We can also introduce missing observations in the wealth data  ----
# # The simulated wealth matrix has a complete history of wealth for each individual for each age - which is what must have happened
# # The real data only has observations of wealth of individuals at some of their ages, we do not have their full wealth history
# # We can reproduce this by varying how much of the simulated wealth data would have been observed
# 
# # The following block can be used to introduce missing observations in the wealth data
# # Create a coyp of the wealth dataset which we will use in the analyses
# sim_std_absw_restricted<-simwealth
# 
# # introduce missing data - assume that 50% of wealth data are missing
# for(j in 1:ncol(sim_std_absw_restricted)){
#   for(i in 1:nrow(sim_std_absw_restricted)){
#     if(rbinom(1,1,0.5)==1){
#       sim_std_absw_restricted[i,j] <- NA
#     } else{
#       sim_std_absw_restricted[i,j] <- sim_std_absw_restricted[i,j]
#     }
#   }
# }
# 
# #replace NAs with -99
# for(j in 1:ncol(sim_std_absw_restricted)){
#   for(i in 1:nrow(sim_std_absw_restricted)){
#     if(is.na(sim_std_absw_restricted[i,j])){
#       sim_std_absw_restricted[i,j] <- -99
#     } else{
#       sim_std_absw_restricted[i,j] <- sim_std_absw_restricted[i,j]
#     }
#   }
# }
# 
# for(j in 1:ncol(simbirth)){
#   for(i in 1:nrow(simbirth)){
#     if(is.na(simbirth[i,j])){
#       simbirth[i,j] <- -99
#     } else{
#       simbirth[i,j] <- simbirth[i,j]
#     }
#   }
# }
# 
# # end of missing data simulation -------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------
# 
# 
# 
# #------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------
# # We can now prepare all the data to be analysed in the STAN model  ----
# 
# 
# # We put all of this together in the list of data for the analyses
# simulated_list <- list(N = nrow(simbirth), #population size
#                        A = ncol(simbirth), #age
#                        wealth = as.matrix(sim_std_absw_restricted), #current absolute wealth
#                        baby = as.matrix(simbirth), #AFR
#                        mean_wealth = medianwealthperindividual # median wealth of each individual
# )
# #check data
# simulated_list
# 
# 
# ## Compile and fit model ----
# 
# # compile model
# model_simulated <- cmdstan_model("firstbaby_threewealth_unif.stan")
# 
# #fit model
# fit_simulated <- model_simulated$sample(data = simulated_list, 
#                                         chains = 4, 
#                                         parallel_chains = 15, 
#                                         adapt_delta = 0.95,
#                                         max_treedepth = 13,
#                                         init = 0)
# 
# # save fit 
# fit_simulated_csv <- rstan::read_stan_csv(fit_simulated$output_files())
# saveRDS(fit_simulated_csv, "fit_simulated_output.rds")
# #load RDS file
# rds_simulated <- readRDS("fit_simulated_output.rds")
# 
# #extract samples  ----
# post_simulated <- extract.samples(rds_simulated)
# 
# tab_simulated_alphagp <- precis(rds_simulated,depth=2,pars=c("alpha",
#                                                              "mu_raw",
#                                                              "mu_tau",
#                                                              "mu_delta"))
# #check table
# tab_sim_alphagp
# 
# #beta z
# #create summary table for beta_z
# tab_sim_beta_z <- precis(rds_simulated,depth=2,pars="beta_wealth_z")
# #check table
# tab_sim_beta_z
# #plot it!
# plot(tab_sim_beta_z)
# 
# # Compare with simulated values
# centeredage<-seq(from=-1,to=2,length.out=51)
# intercept_beta <- 0 # negative values would mean that poor are more likely to have babies
# slope_beta <- 0.1
# 
# # create a vector with the simulated betas
# simulated_beta_z<-intercept_beta+centeredage*slope_beta
# 
# plot(tab_sim_beta_z[,1]~inv_logit(simulated_beta_z))
# # They are not on the same scale because we weight the beta_z in the model by the beta_sigma, and the beta_z in the model modify the logit baseline probability rather than the actual probability - but the model finds that values are negative switching to positive.
# 
# 
# #beta sigma
# #create summary table for beta_sigma
# tab_sim_beta_sigma <- precis(rds_simulated,depth=2,pars="beta_wealth_sigma")
# #check table
# tab_sim_beta_sigma
# #plot it!
# plot(tab_sim_beta_sigma)
# 
# #gamma z
# #create summary table for gamma_z
# tab_sim_gamma_z <- precis(rds_simulated,depth=2,pars="gamma_wealth_z")
# #check table
# tab_sim_gamma_z
# #plot it!
# plot(tab_sim_gamma_z)
# 
# #gamma sigma
# #create summary table for gamma_sigma
# tab_sim_gamma_sigma <- precis(rds_simulated,depth=2,pars="gamma_wealth_sigma")
# #check table
# tab_sim_gamma_sigma
# 
# #delta z
# #create summary table for delta_z
# tab_sim_delta_z <- precis(rds_simulated,depth=2,pars="delta_wealth_z")
# #check table
# tab_sim_delta_z
# #plot it!
# plot(tab_sim_delta_z)
# 
# #delta sigma
# #create summary table for delta_sigma
# tab_sim_delta_sigma <- precis(rds_simulated,depth=2,pars="delta_wealth_sigma")
# #check table
# tab_sim_delta_sigma