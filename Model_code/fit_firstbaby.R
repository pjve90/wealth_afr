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

#put all the data together
#create data
data <- list(N = N, #population size
A = A+1, #age
baby = afrs) #AFR

# compile model

m1 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/firstbaby.stan")

# fit model

fit1 <- m$sample(data = data, 
                chains = 4, 
                parallel_chains = 4, 
                adapt_delta = 0.95,
                max_treedepth = 13,
                init = 0)

# save fit 

fit <- rstan::read_stan_csv(fit$output_files())
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

fit2 <- m$sample(data = data, 
                chains = 4, 
                parallel_chains = 4, 
                adapt_delta = 0.95,
                max_treedepth = 13,
                init = 0)

# save fit 

fit <- rstan::read_stan_csv(fit$output_files())
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
