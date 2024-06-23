#Wealth imputation

#Load packages
#install.packages("cmdstanr")
library(cmdstanr)
#install.packages("rethinking")
library(rethinking)
#install.packages("scales")
library(scales)

## Data wrangling of real data ----

#Load data
real_data_imp <- read.csv("dataf.csv")[,-1]
head(real_data_imp)

#Age-specific absolute wealth ----

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix_imp <- matrix(nrow = nrow(real_data_imp),ncol=max(real_data_imp$aoc)+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix_imp)){
  absw <- real_data_imp$absw95[i]
  age_absw <- real_data_imp$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data_imp$aoc[i]+1)){
    absw_matrix_imp[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data_imp$aoc[i]+1)){
      absw_matrix_imp[i,(real_data_imp$aoc[i]+1)] <- NA
    } else{
      absw_matrix_imp[i,age_absw] <- NA
    }
}
#check data
absw_matrix_imp
#98
for(i in 1:nrow(absw_matrix_imp)){
  absw <- real_data_imp$absw98[i]
  age_absw <- real_data_imp$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data_imp$aoc[i]){
    absw_matrix_imp[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data_imp$aoc[i]+1)){
      absw_matrix_imp[i,(real_data_imp$aoc[i]+1)] <- NA
    } else{
      absw_matrix_imp[i,age_absw] <- NA
    }
}
#check data
absw_matrix_imp
#00
for(i in 1:nrow(absw_matrix_imp)){
  absw <- real_data_imp$absw00[i]
  age_absw <- real_data_imp$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data_imp$aoc[i]){
    absw_matrix_imp[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data_imp$aoc[i]+1)){
      absw_matrix_imp[i,(real_data_imp$aoc[i]+1)] <- NA
    } else{
      absw_matrix_imp[i,age_absw] <- NA
    }
}
#check data
absw_matrix_imp
#02
for(i in 1:nrow(absw_matrix_imp)){
  absw <- real_data_imp$absw02[i]
  age_absw <- real_data_imp$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data_imp$aoc[i]){
    absw_matrix_imp[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data_imp$aoc[i]+1)){
      absw_matrix_imp[i,(real_data_imp$aoc[i]+1)] <- NA
    } else{
      absw_matrix_imp[i,age_absw] <- NA
    }
}
#check data
absw_matrix_imp
#04
for(i in 1:nrow(absw_matrix_imp)){
  absw <- real_data_imp$absw04[i]
  age_absw <- real_data_imp$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data_imp$aoc[i]){
    absw_matrix_imp[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data_imp$aoc[i]+1)){
      absw_matrix_imp[i,(real_data_imp$aoc[i]+1)] <- NA
    } else{
      absw_matrix_imp[i,age_absw] <- NA
    }
}
#check data
absw_matrix_imp
#06
for(i in 1:nrow(absw_matrix_imp)){
  absw <- real_data_imp$absw06[i]
  age_absw <- real_data_imp$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data_imp$aoc[i]){
    absw_matrix_imp[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data_imp$aoc[i]+1)){
      absw_matrix_imp[i,(real_data_imp$aoc[i]+1)] <- NA
    } else{
      absw_matrix_imp[i,age_absw] <- NA
    }
}
#check data
absw_matrix_imp
#10
for(i in 1:nrow(absw_matrix_imp)){
  absw <- real_data_imp$absw10[i]
  age_absw <- real_data_imp$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data_imp$aoc[i]+1)){
    absw_matrix_imp[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data_imp$aoc[i]+1)){
      absw_matrix_imp[i,(real_data_imp$aoc[i]+1)] <- NA
    } else{
      absw_matrix_imp[i,age_absw] <- NA
    }
}
#check data
absw_matrix_imp
#check the age-specific average of absolute wealth
apply(absw_matrix_imp,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix_imp,2,mean,na.rm=T)~c(1:ncol(absw_matrix_imp)),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix_imp <- matrix(standardize(log(as.vector(absw_matrix_imp))),ncol=ncol(absw_matrix_imp),nrow=nrow(absw_matrix_imp))
#check the data
std_absw_matrix_imp
#check the age-specific average of absolute wealth
apply(std_absw_matrix_imp,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix_imp,2,mean,na.rm=T)~c(1:(max(real_data_imp$aoc)+1)),xlab="Age",ylab="Average absolute wealth")

## Fit real data ----

#Wealth
#matrix identifying missing wealth data
wealth_miss_imp <- which(is.na(std_absw_matrix_imp),arr.ind = T)
#check data
wealth_miss_imp

#replace NAs with -99
for(j in 1:ncol(std_absw_matrix_imp)){
  for(i in 1:nrow(std_absw_matrix_imp)){
    if(is.na(std_absw_matrix_imp[i,j])){
      std_absw_matrix_imp[i,j] <- -99
    } else{
      std_absw_matrix_imp[i,j] <- std_absw_matrix_imp[i,j]
    }
  }
}
#check the data
std_absw_matrix_imp

#put all the data together
#create dataset
real_list_imp <- list(N = nrow(std_absw_matrix_imp), #population size
                   A = ncol(std_absw_matrix_imp), #age
                   wealth = std_absw_matrix_imp, #absolute current wealth change
                   N_miss = nrow(wealth_miss_imp), # number of missing values that need imputation
                   wealth_miss=wealth_miss_imp) # matrix indicating missing wealth data
#check data
real_list_imp

## Compile and fit model ----

# compile model

m_imp <- cmdstan_model("Model_code/wealth_imputation.stan")

#fit model
fit_imp_real <- m_imp$sample(data = real_list_imp, 
                               chains = 4, 
                               parallel_chains = 15, 
                               adapt_delta = 0.95,
                               max_treedepth = 13,
                               init = 0)


# save fit 
fit_imp_2_real <- fit_imp_real
saveRDS(fit_imp_2_real, "wealth_imp_real.rds")
#load RDS file
rds_imp_add_real <- readRDS("wealth_imp_real.rds")

# Extract samples if needed (not necessary if already done)
wealth_complete_draws <- rds_imp_add_real$draws(variables = "wealth_complete")
wealth_complete_array <- as_draws_array(wealth_complete_draws)
reshape_wealth_complete_array <- array(wealth_complete_array,dim=c(4000,nrow(std_absw_matrix_imp),ncol(std_absw_matrix_imp)))
wealth_complete_mean <- apply(reshape_wealth_complete_array, c(2, 3), mean)
