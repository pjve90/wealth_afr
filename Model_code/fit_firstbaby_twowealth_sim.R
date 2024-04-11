# Model with both wealth predictors ----

#The code in this script is meant to fit a Bayesian model with welath variability, absolute wealth and a Gaussian process of age, in order to see the relationship between wealth variability and the probability of first reproduction of women.
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

#The script in this section is to create synthetic data that follows the causal relationship between wealth variability and the probability of first reproduction.

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

#Absolute wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
abswealth <- matrix(nrow=N,ncol=A+1)
#randomly assign an amount of wealth for each individual at age 0
abswealth[,1] <- rnorm(100,15,5)
#change wealth of individuals through time
for(j in 2:ncol(abswealth)){
  for(i in 1:nrow(abswealth)){
    abswealth[i,j] <- abswealth[i,j-1]+rnorm(1,0,0.25)
  }
}
#check the data
#see the data
head(abswealth)
#check the age-specific absolute wealth
apply(abswealth,2,mean)
#plot it
plot(apply(abswealth,2,mean),xlab="Age",ylab="Average absolute wealth")

#standardise wealth data
std_abswealth <- matrix(standardize(as.vector(abswealth)),ncol=ncol(abswealth),nrow=nrow(abswealth))
#check the data
std_abswealth

#simulate an age-specific parameter for wealth (beta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
beta_wealth<-c(rep(0,13),seq(from=-0.1,to=0.1,length=16),rep(0,62))
beta_wealth
#check that they sum to 1
sum(beta_wealth)
#plot it!
plot(beta_wealth~c(1:length(beta_wealth)))

#Difference of wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rows and ages as columns (A+1) so the first column is birth)
diffwealth <- matrix(nrow=N,ncol=A+1)
#calculate the difference of wealth from one year to the next
for(j in 1:ncol(diffwealth)){
  for(i in 1:nrow(diffwealth)){
    if(j ==1){
      diffwealth[i,j] <- std_abswealth[i,j] - std_abswealth[i,j]
    } else{
      diffwealth[i,j] <- std_abswealth[i,j] - std_abswealth[i,j-1]
    }
  }
}
#check the data
#see the data
head(diffwealth)
#check the age-specific wealth variability
apply(diffwealth,2,mean)
#plot it
plot(apply(diffwealth,2,mean),xlab="Age",ylab="Average wealth variability")

#simulate an age-specific parameter for wealth variability
#if seq starts from a positive value and goes to a negative value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
gamma_wealth<-c(rep(0,13),seq(from=0.1,to=-0.1,length=16),rep(0,62))
gamma_wealth
#check that they sum to 1
sum(gamma_wealth)
#plot it!
plot(gamma_wealth~c(1:length(gamma_wealth)))

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
afrs <- matrix(nrow=N,ncol=A+1)
#randomly assign a positive output of AFR for individuals
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afr_prob <- mu_age[j]+ #age
        beta_wealth[j]*std_abswealth[i,j]+ #absolute wealth
        gamma_wealth[j]*diffwealth[i,j] #wealth variability
      if(afr_prob<0){afr_prob<-0}
      afrs[i,j] <- rbinom(1,1,afr_prob)
    } else{
      afrs[i,j] <- 0
    }  
  }
}

#check the data
#see the data
head(afrs)
#check the age-specific probability of AFR
apply(afrs,2,sum)/N
#plot it
plot(apply(afrs,2,sum)/N,xlab="Age",ylab="Prob FR")
points(mu_age,col="grey",pch=4)

# #plot age-specific relationship between absolute wealth and AFR
# #prepare data
# plot_ind3 <- as.data.frame(matrix(ncol=3,nrow=N))
# colnames(plot_ind3)<-c("afr","abs_wealth","wealth_diff")
# #calculate the AFR for each individual
# for(i in 1:nrow(plot_ind3)){
#   if(sum(afrs[i,])==0){
#     plot_ind3[i,]$afr<-NA
#   }else{
#     plot_ind3[i,]$afr<-which(afrs[i,]==1)
#   }
# }
# #add the age-specific absolute wealth
# for(i in 1:nrow(plot_ind3)){
#   plot_ind3[i,]$abs_wealth<-std_abswealth[i,which(afrs[i,]==1)]
# }
# #add the age-specific wealth variability
# for(i in 1:nrow(plot_ind3)){
#   plot_ind3[i,]$wealth_diff<-diffwealth[i,which(afrs[i,]==1)]
# }
# #plot the relationship between absolute wealth and AFR
# plot(plot_ind3$afr~plot_ind3$abs_wealth)
# #plot the relationship between wealth variability and AFR
# plot(plot_ind3$afr~plot_ind3$wealth_diff)

# Introduce missing data in the wealth data frame
for (j in 1:ncol(std_abswealth)){
  for (i in 1:nrow(std_abswealth)){
    if(runif(1,min=0,max=1)<0.4){std_abswealth[i,j]<- -99} # 40% missing data
  }
}
#check data
head(std_abswealth)

# Only take the years when individuals have a first baby
#check min and max ages at first reproduction
apply(afrs,2,sum)
#min
min(which(apply(afrs,2,sum)>0))
#14
#max
max(which(apply(afrs,2,sum)>0))
#39

std_abswealth_restricted<-std_abswealth[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]
afrs_restricted<-afrs[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]

## Fit simulated data, using the combined data imputation approach ----

#put all the data together
#create data
data4 <- list(N = nrow(afrs_restricted), #population size
              A = ncol(afrs_restricted), #age
              wealth = as.vector(t(std_abswealth_restricted)), #absolute wealth
              N_miss = sum((std_abswealth_restricted)== -99), # number of missing values that need imputation
              id_wealth_miss = which(as.vector(t(std_abswealth_restricted))== -99), # provide the indexes for the missing data
              baby = afrs_restricted #AFR
) 

#check data
data4

# compile model

m4_add <- cmdstan_model("Model_code/firstbaby_twowealth_additive.stan")

# fit model

fit4_add <- m4_add$sample(data = data4, 
                          chains = 4, 
                          parallel_chains = 10, 
                          adapt_delta = 0.95,
                          max_treedepth = 13,
                          init = 0)

# save fit 
fit_4_add <- rstan::read_stan_csv(fit4_add$output_files())
saveRDS(fit_4_add, "firstbaby4_add.rds")
#load RDS file
rds4_add <- readRDS("firstbaby4_add.rds")
#extract samples
post4_add <- extract.samples(rds4_add)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds4_add,pars="alpha")
#mu
#traceplot(rds4_add,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds4_add,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds4_add,pars="mu_tau")
#mu_kappa
traceplot(rds4_add,pars="mu_kappa")
#mu_delta
traceplot(rds4_add,pars="mu_delta")
#beta_wealth
#traceplot(rds4_add,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds4_add,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab4_add <- precis(rds4_add,depth=2,pars=c("alpha",
                                           "mu_raw",
                                           "mu_tau",
                                           "mu_delta"))
#check table
tab4_add
#create summary table for mu
tab4_add_mu <- precis(rds4_add,depth=2,pars="mu")
#check table
tab4_add_mu
plot(inv_logit(tab4_add_mu[,1]))
#create summary table for beta
tab4_add_beta <- precis(rds4_add,depth=2,pars="beta_wealth")
#check table
tab4_add_beta
plot((tab4_add_beta[,1]))
#create summary table for gamma
tab4_add_gamma <- precis(rds4_add,depth=2,pars="gamma_wealth")
#check table
tab4_add_gamma
plot((tab4_add_gamma[,1]))

## Plot the fit of the simulated data ----

### Absolute wealth ----

### Age ----

#simulate wealth values
simwealth_b <- seq(from=round(min(std_abswealth_restricted[which(std_abswealth_restricted > -99)]),1),to=round(max(std_abswealth_restricted[which(std_abswealth_restricted > -99)]),1),length.out=nrow(std_abswealth_restricted)) #specify according to range and length of wealth data
simwealth_b

#simulate wealth variability values
simwealth_g <- seq(from=round(min(diffwealth),1),to=round(max(diffwealth),1),length.out=nrow(diffwealth)) #specify according to range and length related to sample size
simwealth_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_add$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
plot(c(0,0.4)~c(round(min(std_abswealth_restricted[which(std_abswealth_restricted > -99)]),1),round(max(std_abswealth_restricted[which(std_abswealth_restricted > -99)]),1)),
     ylab="Prob. FR",
     xlab="Std. Absolute wealth",
     main="Model with both wealth predictors",
     type="n")

#add lines
for(k in 1:length(age_quantiles)){
  #create matrix to store the data
  p4_add_a <- matrix(nrow=nrow(post4_add$mu),ncol=length(simwealth_b))
  p4_add_a
  #fill it in with values for age 25
  for(j in 1:length(simwealth_b)){
    for(i in 1:nrow(post4_add$mu)){
      p4_add_a[i,j] <- inv_logit(post4_add$alpha[i] + #inv logit because originally is logit
                                  post4_add$mu[i,age_quantiles[k]] + #age
                                  post4_add$beta_wealth[i,age_quantiles[k]]*simwealth_b[j] + # absolute wealth 
                                  post4_add$gamma_wealth[i,age_quantiles[k]]*0) #wealth variability / zero because that is the average from the standardisation
    }
  }
  #check data
  p4_add_a
  #plot it!
  #prepare model prediction data
  plot_data4_add_a <- data.frame(wealth = simwealth_b,
                                 mean = apply(p4_add_a, 2, mean), 
                                 upp = apply(p4_add_a, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p4_add_a, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  lines(plot_data4_add_a$mean~plot_data4_add_a$wealth,col=palette[k])
}

### Wealth ----

#simulate wealth values
simwealth_add <- seq(from=round(min(std_abswealth_restricted[which(std_abswealth_restricted > -99)]),1),to=round(max(std_abswealth_restricted[which(std_abswealth_restricted > -99)]),1),length.out=nrow(std_abswealth_restricted)) #specify according to range and length of wealth data
simwealth_add
#get the deciles
deciles <- as.numeric(quantile(simwealth_add,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(0,ncol(post4_add$mu)),
     ylab="Prob. FR",
     xlab="Age",
#     xaxt="n",
     main="Model with absolute wealth",
     type="n")
#axis(1,at=seq(0,ncol(post4_add$mu),by=1),labels=min(which(apply(afrs,2,sum)>0)):(max(which(apply(afrs,2,sum)>0))+1))

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_add_b <- matrix(nrow=nrow(post4_add$mu),ncol=ncol(post4_add$mu))
  p4_add_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_add$mu)){
    for(i in 1:nrow(post4_add$mu)){
      p4_add_b[i,j] <- inv_logit(post4_add$alpha[i] + #inv logit because originally is logit
                                  post4_add$mu[i,j] + #age
                                  post4_add$beta_wealth[i,j]*deciles[k] + # absolute wealth 
                                  post4_add$gamma_wealth[i,j]*0) #wealth variability / zero because that is the average from the standardisation
    }
  }
  #check data
  p4_add_b
  #plot it!
  #prepare model prediction data
  plot_data4_add_b <- data.frame(age = 1:ncol(p4_add_b),
                                 mean = apply(p4_add_b, 2, mean), 
                                 upp = apply(p4_add_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p4_add_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  points(plot_data4_add_b$mean~plot_data4_add_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data4_add_b$mean~plot_data4_add_b$age,col=palette_b[k])
}

### Wealth variability ----

### Age ----

#simulate wealth values
simwealth_b <- seq(from=round(min(std_abswealth_restricted[which(std_abswealth_restricted > -99)]),1),to=round(max(std_abswealth_restricted[which(std_abswealth_restricted > -99)]),1),length.out=nrow(std_abswealth_restricted)) #specify according to range and length of wealth data
simwealth_b

#simulate wealth variability values
diffwealth_restricted<-diffwealth[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]
simwealth_g <- seq(from=round(min(diffwealth_restricted),1),to=round(max(diffwealth_restricted),1),length.out=nrow(diffwealth_restricted)) #specify according to range and length related to sample size
simwealth_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_add$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
plot(c(0,0.4)~c(min(simwealth_g),max(simwealth_g)),
     ylab="Prob. FR",
     xlab="Std. Absolute wealth",
     main="Model with both wealth predictors",
     type="n")

#add lines
for(k in 1:length(age_quantiles)){
  #create matrix to store the data
  p4_add_c <- matrix(nrow=nrow(post4_add$mu),ncol=length(simwealth_g))
  p4_add_c
  #fill it in with values for age 25
  for(j in 1:length(simwealth_g)){
    for(i in 1:nrow(post4_add$mu)){
      p4_add_c[i,j] <- inv_logit(post4_add$alpha[i] + #inv logit because originally is logit
                                  post4_add$mu[i,age_quantiles[k]] + #age
                                  post4_add$beta_wealth[i,age_quantiles[k]]*0 + # absolute wealth / zero because that is the average from the standardisation
                                  post4_add$gamma_wealth[i,age_quantiles[k]]*simwealth_g[j]) #wealth variability 
    }
  }
  #check data
  p4_add_c
  #plot it!
  #prepare model prediction data
  plot_data4_add_c <- data.frame(wealth = simwealth_g,
                                 mean = apply(p4_add_c, 2, mean), 
                                 upp = apply(p4_add_c, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p4_add_c, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  lines(plot_data4_add_c$mean~plot_data4_add_c$wealth,col=palette[k])
}

### Wealth ----

#simulate wealth values
simwealth_add <- seq(from=round(min(diffwealth_restricted),1),to=round(max(diffwealth_restricted),1),length.out=nrow(diffwealth_restricted)) #specify according to range and length related to sample size
simwealth_add
#get the deciles
deciles <- as.numeric(quantile(simwealth_add,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.12)~c(0,ncol(post4_add$mu)),
     ylab="Prob. FR",
     xlab="Age",
#     xaxt="n",
     main="Model with absolute wealth",
     type="n")
#axis(1,at=seq(0,ncol(post4_add$mu),by=1),labels=14:29)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_add_b <- matrix(nrow=nrow(post4_add$mu),ncol=ncol(post4_add$mu))
  p4_add_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_add$mu)){
    for(i in 1:nrow(post4_add$mu)){
       p4_add_b[i,j] <- inv_logit(post4_add$alpha[i] + #inv logit because originally is logit
                                   post4_add$mu[i,j] + #age
                                   post4_add$beta_wealth[i,j]*0 + # absolute wealth / zero because that is the average from the standardisation 
                                   post4_add$gamma_wealth[i,j]*deciles[k]) #wealth variability
    }
  }
  #check data
  p4_add_b
  #plot it!
  #prepare model prediction data
  plot_data4_add_b <- data.frame(age = 1:ncol(p4_add_b),
                                 mean = apply(p4_add_b, 2, mean), 
                                 upp = apply(p4_add_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p4_add_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  points(plot_data4_add_b$mean~plot_data4_add_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data4_add_b$mean~plot_data4_add_b$age,col=palette_b[k])
}


## Fit simulated data, using the combined data imputation approach and multiplicative ----

#put all the data together
#create data
data4 <- list(N = nrow(afrs_restricted), #population size
              A = ncol(afrs_restricted), #age
              wealth = as.vector(t(std_wealth_restricted)), #absolute wealth
              N_miss = sum((std_wealth_restricted)== -99), # number of missing values that need imputation
              id_wealth_miss = which(as.vector(t(std_wealth_restricted))== -99), # provide the indexes for the missing data
              baby = afrs_restricted #AFR
) 

#check data
data4

# compile model

m4_mult <- cmdstan_model("Model_code/firstbaby_twowealth_multiplicative.stan")

# fit model

fit4_mult<- m4_mult$sample(data = data4, 
                           chains = 4, 
                           parallel_chains = 10, 
                           adapt_delta = 0.95,
                           max_treedepth = 13,
                           init = 0)

# save fit 
fit_4_mult <- rstan::read_stan_csv(fit4_mult$output_files())
saveRDS(fit_4_mult, "firstbaby4_mult.rds")
#load RDS file
rds4_mult <- readRDS("firstbaby4.rds")
#extract samples
post4_mult <- extract.samples(rds4_mult)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds4_mult,pars="alpha")
#mu
#traceplot(rds4_mult,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds4_mult,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds4_mult,pars="mu_tau")
#mu_kappa
traceplot(rds4_mult,pars="mu_kappa")
#mu_delta
traceplot(rds4_mult,pars="mu_delta")
#beta_wealth
#traceplot(rds4_mult,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds4_mult,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab4 <- precis(rds4_mult,depth=2,pars=c("alpha",
                                        "mu_raw",
                                        "mu_tau",
                                        "mu_delta"))
#check table
tab4
#create summary table for mu
tab4_mu <- precis(rds4_mult,depth=2,pars="mu")
#check table
tab4_mu
plot(inv_logit(tab4_mu[,1]))
#create summary table for beta
tab4_beta <- precis(rds4_mult,depth=2,pars="beta_wealth")
#check table
tab4_beta
plot((tab4_beta[,1]))
#create summary table for gamma
tab4_gamma <- precis(rds4_mult,depth=2,pars="gamma_wealth")
#check table
tab4_gamma
plot((tab4_gamma[,1]))

## Plot the fit of the simulated data ----

### Absolute wealth ----

### Age ----

#simulate wealth values
simwealth_b <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_b

#simulate wealth variability values
simwealth_g <- seq(from=round(min(diffwealth),1),to=round(max(diffwealth),1),length.out=nrow(diffwealth)) #specify according to range and length related to sample size
simwealth_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_mult$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
plot(c(0,0.4)~c(min(diffwealth),max(diffwealth)),
     ylab="Prob. FR",
     xlab="Std. Absolute wealth",
     main="Model with both wealth predictors",
     type="n")

#add lines
for(k in seq(15,25,by=1)){
  #create matrix to store the data
  p4 <- matrix(nrow=nrow(post4_mult$mu),ncol=length(simwealth_b))
  p4
  #fill it in with values for age 25
  for(j in 1:length(simwealth_b)){
    for(i in 1:nrow(post4_mult$mu)){
      p4_real[i,j] <- inv_logit(post4_mult_real$alpha[i] + #inv logit because originally is logit
                                  post4_mult_real$mu[i,k] + #age
                                  post4_mult_real$beta_wealth[i,k]*simwealth_b[j] + # absolute wealth 
                                  post4_mult_real$gamma_wealth[i,k]*0) #wealth variability / zero because that is the average from the standardisation
    }
  }
  #check data
  p4
  #plot it!
  #prepare model prediction data
  plot_data4_mult_a <- data.frame(wealth = simwealth_b,
                                 mean = apply(p4, 2, mean), 
                                 upp = apply(p4, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p4, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  lines(plot_data4_mult_a$mean~plot_data4_mult_a$wealth,col=palette[k])
}

### Wealth ----

#simulate wealth values
simwealth_mult <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_mult
#get the deciles
deciles <- as.numeric(quantile(simwealth_mult,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(0,ncol(post4_mult$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth",
     type="n")
axis(1,at=seq(0,ncol(post4_mult$mu),by=1),labels=14:29)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_mult_b <- matrix(nrow=nrow(post4_mult$mu),ncol=ncol(post4_mult$mu))
  p4_mult_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_mult$mu)){
    for(i in 1:nrow(post4_mult$mu)){
      p4_real[i,j] <- inv_logit(post4_mult_real$alpha[i] + #inv logit because originally is logit
                                  post4_mult_real$mu[i,k] + #age
                                  post4_mult_real$beta_wealth[i,k]*simwealth_b[j] + # absolute wealth 
                                  post4_mult_real$gamma_wealth[i,k]*0) #wealth variability / zero because that is the average from the standardisation
    }
  }
  #check data
  p4_mult_b
  #plot it!
  #prepare model prediction data
  plot_data4_mult_b <- data.frame(age = 1:ncol(p4_mult_b),
                                 mean = apply(p4_mult_b, 2, mean), 
                                 upp = apply(p4_mult_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p4_mult_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  points(plot_data4_mult_b$mean~plot_data4_mult_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data4_mult_b$mean~plot_data4_mult_b$age,col=palette_b[k])
}

### Wealth variability ----

### Age ----

#simulate wealth values
simwealth_b <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_b

#simulate wealth variability values
simwealth_g <- seq(from=round(min(diffwealth),1),to=round(max(diffwealth),1),length.out=nrow(diffwealth)) #specify according to range and length related to sample size
simwealth_g

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post4_mult$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
plot(c(0,0.4)~c(min(diffwealth),max(diffwealth)),
     ylab="Prob. FR",
     xlab="Std. Absolute wealth",
     main="Model with both wealth predictors",
     type="n")

#add lines
for(k in seq(15,25,by=1)){
  #create matrix to store the data
  p4 <- matrix(nrow=nrow(post4_mult$mu),ncol=length(simwealth_b))
  p4
  #fill it in with values for age 25
  for(j in 1:length(simwealth_b)){
    for(i in 1:nrow(post4_mult$mu)){
      p4_real[i,j] <- inv_logit(post4_mult_real$alpha[i] + #inv logit because originally is logit
                                  post4_mult_real$mu[i,k] + #age
                                  post4_mult_real$beta_wealth[i,k]*0 + # absolute wealth / zero because that is the average from the standardisation
                                  post4_mult_real$gamma_wealth[i,k]*simwealth_g[j]) #wealth variability 
    }
  }
  #check data
  p4
  #plot it!
  #prepare model prediction data
  plot_data4_mult_a <- data.frame(wealth = simwealth_b,
                                 mean = apply(p4, 2, mean), 
                                 upp = apply(p4, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p4, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  lines(plot_data4_mult_a$mean~plot_data4_mult_a$wealth,col=palette[k])
}

### Wealth ----

#simulate wealth values
simwealth_mult <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_mult
#get the deciles
deciles <- as.numeric(quantile(simwealth_mult,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(0,ncol(post4_mult$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth",
     type="n")
axis(1,at=seq(0,ncol(post4_mult$mu),by=1),labels=14:29)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_mult_b <- matrix(nrow=nrow(post4_mult$mu),ncol=ncol(post4_mult$mu))
  p4_mult_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_mult$mu)){
    for(i in 1:nrow(post4_mult$mu)){
      p4_real[i,j] <- inv_logit(post4_mult_real$alpha[i] + #inv logit because originally is logit
                                  post4_mult_real$mu[i,k] + #age
                                  post4_mult_real$beta_wealth[i,k]*0 + # absolute wealth / zero because that is the average from the standardisation
                                  post4_mult_real$gamma_wealth[i,k]*simwealth_g[j]) #wealth variability 
    }
  }
  #check data
  p4_mult_b
  #plot it!
  #prepare model prediction data
  plot_data4_mult_b <- data.frame(age = 1:ncol(p4_mult_b),
                                 mean = apply(p4_mult_b, 2, mean), 
                                 upp = apply(p4_mult_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p4_mult_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afrs_restricted
  #change -99 to NAs
  for(j in 1:ncol(plot_afr4)){
    for(i in 1:nrow(plot_afr4)){
      if(plot_afr4[i,j]==-99){
        plot_afr4[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr4
  
  points(plot_data4_mult_b$mean~plot_data4_mult_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data4_mult_b$mean~plot_data4_mult_b$age,col=palette_b[k])
}

