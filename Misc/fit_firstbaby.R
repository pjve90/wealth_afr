#Code to build the models for the Pimbwe project ----

## Model with Gaussian process of exposure time ----

#Create synthetic data

#Exposure time
#simulate ages for each census
#create a vector with the ages for each census
#95
age_95 <- rbinom(100,19,0.5)
#98
age_98 <- age_95+3
#00
age_00 <- age_98+2
#02
age_02 <- age_00+2
#04
age_04 <- age_02+2
#06
age_06 <- age_04+2
#10
age_10 <- age_06+4
#create one matrix with it
ages <- as.matrix(cbind(age_95,age_98,age_00,age_02,age_04,age_06,age_10))

#Age at first reproduction (AFR)
#simulate binary ouput of AFR for each census
#create a vector with the binary output for each census
#95
afr_95<- rbinom(100,1,0.5)
#98
#create empty vector
afr_98 <- rep(0,100)
#fusion with previous afr
afrs <- cbind(afr_95,afr_98)
#loop
  for(i in 1:nrow(afrs)){
    if(rowSums(afrs)[i] == 1){
      afrs[i,2] <- 0
    } else{
      afrs[i,2] <- rbinom(1,1,0.5)
    }
  }
#00
#create empty vector
afr_00 <- rep(0,100)
#fusion with previous afr
afrs <- cbind(afrs,afr_00)
#loop
for(i in 1:nrow(afrs)){
  if(rowSums(afrs)[i] == 1){
    afrs[i,3] <- 0
  } else{
    afrs[i,3] <- rbinom(1,1,0.5)
  }
}
#02
#create empty vector
afr_02 <- rep(0,100)
#fusion with previous afr
afrs <- cbind(afrs,afr_02)
#loop
for(i in 1:nrow(afrs)){
  if(rowSums(afrs)[i] == 1){
    afrs[i,4] <- 0
  } else{
    afrs[i,4] <- rbinom(1,1,0.5)
  }
}
#04
#create empty vector
afr_04 <- rep(0,100)
#fusion with previous afr
afrs <- cbind(afrs,afr_04)
#loop
for(i in 1:nrow(afrs)){
  if(rowSums(afrs)[i] == 1){
    afrs[i,5] <- 0
  } else{
    afrs[i,5] <- rbinom(1,1,0.5)
  }
}
#06
#create empty vector
afr_06 <- rep(0,100)
#fusion with previous afr
afrs <- cbind(afrs,afr_06)
#loop
for(i in 1:nrow(afrs)){
  if(rowSums(afrs)[i] == 1){
    afrs[i,6] <- 0
  } else{
    afrs[i,6] <- rbinom(1,1,0.5)
  }
}
#10
#create empty vector
afr_10 <- rep(0,100)
#fusion with previous afr
afrs <- cbind(afrs,afr_10)
#loop
for(i in 1:nrow(afrs)){
  if(rowSums(afrs)[i] == 1){
    afrs[i,7] <- 0
  } else{
    afrs[i,7] <- rbinom(1,1,0.5)
  }
}
#create one matrix with it
afrs <- as.matrix(afrs)

#put all the data together
#create data
data <- list(N = 100, #population size
E = 7, #exposure time (i.e. number of censuses)
cens_age = ages, #ages at each census
baby = afrs) #AFR at each census

# compile model

m <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/firstbaby.stan")

# fit model

fit <- m$sample(data = data, 
                chains = 4, 
                parallel_chains = 4, 
                adapt_delta = 0.95,
                max_treedepth = 13,
                init = 0)

# save fit 

fit <- rstan::read_stan_csv(fit$output_files())
saveRDS(fit, "firstbaby.rds")


