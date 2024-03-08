#Code to build the models for the Pimbwe project ----

## Model with Gaussian process of age ----

#Create synthetic data

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

#Age at first reproduction (AFR)
#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
afrs[,c(1:13,34:91)] <- 0
#randomly assign a positive output of AFR for individuals between ages 13 and 32
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afrs[i,j] <- rbinom(1,1,0.5)
    } else{
      afrs[i,j] <- 0
    }  
  }
}

library(dplyr)
afrs_distribution<-as.data.frame(summarise_each(as.data.frame(afrs), sum))
colnames(afrs_distribution)<-c(0:90)
plot(as.numeric(afrs_distribution[1,])~c(0:90))

#put all the data together
#create data
data <- list(N = N, #population size
A = A+1, #age
baby = afrs) #AFR

# compile model
library(cmdstanr)
m1 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/firstbaby.stan")

# fit model

fit1 <- m1$sample(data = data, 
                chains = 4, 
                parallel_chains = 4, 
                adapt_delta = 0.95,
                max_treedepth = 13,
                init = 0)

# save fit 

fit_1 <- rstan::read_stan_csv(fit1$output_files())
saveRDS(fit, "firstbaby.rds")

## Model with Gaussian process of age and absolute levels of material wealth ----

#Create synthetic data

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

#Age at first reproduction (AFR)
#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
afrs[,c(1:13,34:91)] <- 0
#randomly assign a positive output of AFR for individuals between ages 13 and 32
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afrs[i,j] <- rbinom(1,1,0.5)
    } else{
      afrs[i,j] <- 0
    }  
  }
}

#Absolute wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
wealth <- matrix(nrow=N,ncol=A+1)
#randomly assign an amount of wealth for each individual and age
for(j in 1:ncol(wealth)){
  for(i in 1:nrow(wealth)){
    wealth[i,j] <- rnorm(1,15,5)
  }
}

#put all the data together
#create data
data <- list(N = N, #population size
             A = A+1, #age
             wealth = wealth, #absolute wealth
             baby = afrs) #AFR
#check data
data

# compile model

m2 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/firstbaby_abswealth.stan")

# fit model

fit2 <- m2$sample(data = data, 
                chains = 4, 
                parallel_chains = 4, 
                adapt_delta = 0.95,
                max_treedepth = 13,
                init = 0)

# save fit 

fit_2 <- rstan::read_stan_csv(fit2$output_files())
saveRDS(fit, "firstbaby.rds")

## Model with Gaussian process of age, absolute and difference of material wealth ----

#Create synthetic data

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

#Age at first reproduction (AFR)
#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
afrs[,c(1:13,34:91)] <- 0
#randomly assign a positive output of AFR for individuals between ages 13 and 32
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afrs[i,j] <- rbinom(1,1,0.5)
    } else{
      afrs[i,j] <- 0
    }  
  }
}

#Absolute wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
wealth <- matrix(nrow=N,ncol=A+1)
#randomly assign an amount of wealth for each individual and age
for(j in 1:ncol(wealth)){
  for(i in 1:nrow(wealth)){
    wealth[i,j] <- rnorm(1,15,5)
  }
}

#Difference of wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
diffwealth <- matrix(nrow=N,ncol=A+1)
#assign zero change of wealth at birth
diffwealth[,1] <- 0
#randomly assign an amount of wealth for each individual and age
for(j in 2:ncol(diffwealth)){
  for(i in 1:nrow(diffwealth)){
    diffwealth[i,j] <- wealth[i,j] - wealth[i,j-1]
  }
}

#put all the data together
#create data
data <- list(N = N, #population size
             A = A+1, #age
             abswealth = wealth, #absolute wealth
             diffwealth = diffwealth, #difference in wealth
             baby = afrs) #AFR
#check data
data

# compile model

m3 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/firstbaby_diffwealth.stan")

# fit model

fit3 <- m$sample(data = data, 
                chains = 4, 
                parallel_chains = 4, 
                adapt_delta = 0.95,
                max_treedepth = 13,
                init = 0)

# save fit 

fit <- rstan::read_stan_csv(fit$output_files())
saveRDS(fit, "firstbaby.rds")




##### Dieter's approach


#Create synthetic data

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

# Wealth for each individual
# Start with wealth they have in their household at birth. Assume that changes between years are minor. That means differences among individuals are larger than differences within individuals

wealth <- matrix(nrow=N,ncol=A+1)

# Set the initial wealth of the household with an average of 15, maximum of 30
wealth[,1]<-rbinom(100,30,0.5)
hist(wealth[,1])
# Simulate changes in wealth over time. Small changes from one year to the next from the initial value
for(i in 1:nrow(wealth)){
  for(j in 2:ncol(wealth)){
    wealth[i,j] <- wealth[i,j-1] + round(rnorm(1,mean=0,sd=0.25),0)
    if(wealth[i,j]<0){wealth[i,j]<-0}
  }
}

# Need to consider how wealth affects age at first reproduction. We need age-specific offsets to get earlier ages or later ages at first birth. If there is only a single parameter, there can be no effect, because if wealthier individuals  have a higher probability to give birth at a younger age, they automatically will have a lower probability to give birth at a later age - so the overall effect of wealth on the probability to have a child is zero. There needs to be a vector that reflects how wealth impacts the probability to have a child at each age. So there need to be 91 beta_wealth estimates, one for each age.

# vector with assumed influence of wealth on probability to reproduce. This example assumes that individuals with more wealth are more likely to reproduce later.

# In this example there are 91 ages (0-90). We assume that individuals start reproducing at age 13, so the first 13 entries for ages 0-12 are zero, next we start with a negative influence that shifts at age 23 to positive values, and we assume that the maximum age of first reproduction is 33, after which the influence shifts to zero again
beta_wealth<-c(rep(0,13),seq(from=-0.01,to=0.01,length=20),rep(0,58))


#Age at first reproduction (AFR)
#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
# With this setup, we also need age-specific probabilities of having the first baby
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
# there are 91 values, the total sums to 1 (attributing the relative proportions across ages), probability peaks at ages 21-24
beta_age<-c(rep(0,13),0.01,0.01,0.02,0.03,0.04,0.05,0.06,0.08,0.10,0.10,0.10,0.10,0.08,0.06,0.05,0.04,0.03,0.02,0.01,0.01,rep(0,58) )

# assign a positive output of AFR for individuals between ages 13 and 32
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      currentprobability<-beta_age[j]+beta_wealth[j]*(wealth[i,j]/15)
      if(currentprobability<0){currentprobability<-0}
      afrs[i,j] <- rbinom(1,1,currentprobability)
    } else{
      afrs[i,j] <- 0
    }  
  }
}

library(dplyr)
afrs_distribution<-as.data.frame(summarise_each(as.data.frame(afrs), sum))
colnames(afrs_distribution)<-c(0:90)
plot(as.numeric(afrs_distribution[1,])~c(0:90))
plot(as.numeric(afrs_distribution[1,])~beta_wealth)
plot(as.numeric(afrs_distribution[1,])~beta_age)

afr_age<-as.data.frame(which(afrs==1,arr.ind=TRUE))
colnames(afr_age)<-c("id","afr")
afr_age<-afr_age[order(afr_age$id),]
ind_information<-as.data.frame(matrix(ncol=2,nrow=N))
colnames(ind_information)<-c("id","wealth")
ind_information$id<-c(1:N)
ind_information<-left_join(ind_information,afr_age,by="id")
ind_information$wealth<-rowMeans(wealth[,1:32])
plot(ind_information$afr~ind_information$wealth)



data <- list(N = N, #population size
             A = A+1, #age
             wealth = wealth, #absolute wealth
             baby = afrs) #AFR
#check data
data

# compile model

m2_v <- cmdstan_model("firstbaby_abswealth_vector.stan")

# fit model

fit2_v <- m2_v$sample(data = data, 
                  chains = 4, 
                  parallel_chains = 4, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)

# save fit 

fit_2_v <- rstan::read_stan_csv(fit2_v$output_files())


# Check whether the model recovers the simulated relationships

# First, the general profile of the likelihood of having the first baby by a given age
age_estimates<-precis(fit_2_v,depth=2,pars="age")

plot(inv_logit(age_estimates[,1])~c(1:91))

# Next, the relationship with wealth; this shows that wealth has a negative effect at younger ages (estimate smaller than zero) and a positive effect at older ages (estimate larger than zero). The estimates are noisy though, because with 100 indviduals only very few individuals will give birth at a given age, so the
beta_estimates<-precis(fit_2_v,depth=2,pars="wealth_beta")
plot(beta_estimates[14:33,1]~beta_wealth[14:33],ylim=c(0,0.2))

# With the actual data, we won't have the offsets, so we plot how the estimates relate to age

plot((beta_estimates[,1]~c(1:91)),ylim=c(-0.1,0.2))
abline(a=0,b=0)

# The issue here is, that the model will use wealth to predict the zeroes across all ages and when no individual reproduces at that age, the age-specific wealth-offset cannot be estimated and defaults to the prior; the estimates are clearly different from those ages where wealth can have an effect because some individuals start reproducing; and the estimates show the reverse relationship of the Gaussian process.
plot((beta_estimates[,1]~c(1:91)))

# It might be worth to restrict the estimation to only include the age rank where at least one individual in the dataset had their first child. That might still leave some ages in the rank where no-one had their first child (e.g. oldest age to have a first child is 30, but no woman had her first child at age 29). 
