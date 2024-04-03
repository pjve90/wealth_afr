# Simulated data with missing values that are being imputed in the model


# Model with absolute wealth ----

#The code in this script is meant to fit a Bayesian model with absolute wealth and a Gaussian process of age, in order to see the relationship between absolute wealth and the probability of first reproduction of women.
#First, there is data simulation that follows the expectations from the literature.
#Second, the simulated data is fitted to the Bayesian model.
#Third, we use the real data to see how the model fits. 
#Finally, there is plotting code

#Load packages
#install.packages("cmdstanr")
library(cmdstanr)
#install.packages("rethinking")
library(rethinking)

## Data simulation ----

#The script in this section is to create synthetic data that follows the causal relationship between absolute wealth and the probability of first reproduction.

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

#Absolute wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
wealth <- matrix(nrow=N,ncol=A)
#randomly assign an amount of wealth for each individual at age 0
wealth[,1] <- rnorm(100,15,5)
#simulate wealth of individuals through time, based on previous absolute wealth
for(j in 2:ncol(wealth)){
  for(i in 1:nrow(wealth)){
    wealth[i,j] <- wealth[i,j-1]+rnorm(1,0,0.25)
  }
}
#check the data
#see the data
head(wealth)
#check the age-specific absolute wealth
apply(wealth,2,mean)
#plot it
plot(apply(wealth,2,mean),xlab="Age",ylab="Average absolute wealth")

#standardise wealth data
#create a matrix to store the standardised data
std_wealth <- matrix(nrow=nrow(wealth),ncol=ncol(wealth))
#standardize wealth data per column
for(j in 1:ncol(std_wealth)){
  std_wealth[,j] <- standardize(wealth[,j])
}
#check the data
std_wealth

#simulate an age-specific parameter for wealth (beta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
beta_wealth<-c(rep(0,13),seq(from=-0.1,to=0.1,length=16),rep(0,61))
std_beta_wealth<-beta_wealth/5.5

#Age at first reproduction (AFR)

#simulate an age-specific parameter for AFR (mu)
mu_age<-c(rep(0,13),seq(from=0.001,to=0.2,length=6),seq(from=0.14,to=0.01,length=5),seq(from=0.01, to=0.001,length=15),rep(0,52))
mu_age
#check that they sum to 1
sum(mu_age)
#plot it!
plot(mu_age~c(1:length(mu_age)))

#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A)
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
afrs[,1:10] <- 0
#randomly assign a positive output of AFR for individuals
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afr_prob <- mu_age[j]+ #age
        std_beta_wealth[j]*std_wealth[i,j] #wealth
      ifelse(afr_prob<0,afr_prob<-0,afr_prob<-afr_prob)
      afrs[i,j] <- rbinom(1,1,afr_prob)
    } else{
      afrs[i,j] <- 0
    }  
  }
}

#check the data
#see the data
head(afrs)
#check the age-specific probability of FR
apply(afrs,2,sum)/N
#plot it
plot(apply(afrs,2,sum)/N,xlab="Age",ylab="Prob FR")
points(mu_age,col="grey",pch=4)

#plot age-specific relationship between wealth and AFR
#prepare data
plot_ind2 <- as.data.frame(matrix(ncol=2,nrow=N))
colnames(plot_ind2)<-c("afr","wealth")
#calculate the AFR for each individual
for(i in 1:nrow(plot_ind2)){
  if(sum(afrs[i,])==0){
    plot_ind2[i,]$afr<-NA
  }else{
    plot_ind2[i,]$afr<-which(afrs[i,]==1)
  }
}
#add the age-specific absolute wealth
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(plot_ind2)){
    plot_ind2[i,]$wealth<-std_wealth[i,j]
  }
}
# Individuals with more wealth have their first child later
plot(plot_ind2$afr~plot_ind2$wealth)


# Introduce missing data in the wealth data frame
for (i in 1:ncol(std_wealth)){
  for (j in 1:nrow(std_wealth)){
    if(runif(1,min=0,max=2)<0.4){std_wealth[j,i]<- -99} # 40% missing data
  }
}

head(std_wealth)


## Fit simulated data ----

#put all the data together
#create data
data <- list(N = N, #population size
             A = A, #age
             wealth = as.vector(t(std_wealth)), #absolute wealth
             baby = afrs, #AFR
             miss = sum((std_wealth)== -99), # number of missing values that need imputation
             #check data 
             wealth_m=which(as.vector(t(std_wealth))== -99)) # provide the indexes for the missing data

data

# compile model

m2 <- cmdstan_model("firstbaby_abswealth.stan")

# fit model

fit2 <- m2$sample(data = data, 
                  chains = 4, 
                  parallel_chains = 10, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)

# save fit 
fit_2 <- rstan::read_stan_csv(fit2$output_files())
saveRDS(fit_2, "firstbaby2.rds")
#load RDS file
rds2 <- readRDS("firstbaby2.rds")
#extract samples
post2 <- extract.samples(rds2)
