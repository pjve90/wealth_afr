#Code to build the models for the Pimbwe project ----

#Load packages
#install.packages("cmdstanr")
library(cmdstanr)
#install.packages("rethinking")
library(rethinking)

## Model with Gaussian process of age ----

#Create synthetic data

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

#Age at first reproduction (AFR)

#simulate an age-specific parameter for AFR
mu_age<-c(rep(0,10),seq(from=0.01,to=0.1,length=11),rep(0.1,4),seq(from=0.1,to=0.01,length=17),rep(0,49))

#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
afrs[,1:10] <- 0
#randomly assign a positive output of AFR for individuals between ages 13 and 32
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afr_prob <- mu_age[j]
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
#check the age-specific frequency of FR
colSums(as.data.frame(afrs))/100
#plot it
plot(colSums(as.data.frame(afrs))/100,xlab="Age",ylab="Probability of first reproduction",type="l")

#put all the data together
#create dataset
data1 <- list(N = N, #population size
A = A+1, #age
baby = afrs) #AFR

# compile model
m1 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/Model_code/firstbaby.stan")

# fit model
fit1 <- m1$sample(data = data1, 
                chains = 4, 
                parallel_chains = 4, 
                adapt_delta = 0.95,
                max_treedepth = 13,
                init = 0)

# save fit 
fit_1 <- rstan::read_stan_csv(fit1$output_files())
saveRDS(fit_1, "firstbaby1.rds")
#load RDS file
rds1 <- readRDS("firstbaby1.rds")
#extract samples
post1 <- extract.samples(rds1)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds1,pars="alpha")
#mu
#traceplot(rds1,pars="mu") #only run if needed, because they are 91 plots
#mu_raw, mu_tau, mu_delta
#traceplot(rds1,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds1,pars="mu_tau")
#mu_kappa
traceplot(rds1,pars="mu_kappa")
#mu_delta
traceplot(rds1,pars="mu_delta")

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab1 <- precis(rds1,depth=3,pars=c("alpha",
                           "mu_raw",
                           "mu_tau",
                           "mu_delta"))
#check table
tab1
#create summary table for mu
tab1_mu <- precis(rds1,depth=3,pars="mu")
#check table
tab1_mu

#plot the predictions from the model with the data
#compute probability of FR at each age
p1 <- matrix(nrow=nrow(post1$mu),ncol=ncol(post1$mu))
p1
#fill it in
for(j in 1:ncol(post1$mu)){
  for(i in 1:nrow(post1$mu)){
    p1[i,j] <- inv_logit(post1$alpha[i] + post1$mu[i,j]) #inv logit because originally is logit
  }
}
#check data
p1
#plot it!
#prepare model prediction data
plot_data1 <- data.frame(age = 1:91, 
                        mu_mean = apply(p, 2, mean), 
                        mu_upp = apply(p, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                        mu_low = apply(p, 2, function(x) HPDI(x, prob = 0.9))[2, ]
                        ) 

#plot age random effect
plot(plot_data1$mu_mean~plot_data1$age,
     ylab="Probability of first reproduction",
     xlab="Age",
     main="Model with Gaussian process of age",
     type="l")
lines(plot_data1$mu_upp~plot_data1$age,col="red")
lines(plot_data1$mu_low~plot_data1$age,col="blue")
points(colSums(as.data.frame(afrs))/100~plot_data1$age,col="gold",pch=16)

## Model with Gaussian process of age and absolute levels of material wealth ----

#Create synthetic data

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 90 years old
A <- 90

#Absolute wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
wealth <- matrix(nrow=N,ncol=A+1)
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
colMeans(as.data.frame(wealth))
#plot it
plot(colMeans(as.data.frame(wealth)),xlab="Age",ylab="Average absolute wealth")

#standardise wealth data
#create a matrix to store the standardised data
std_wealth <- matrix(nrow=nrow(wealth),ncol=ncol(wealth))
#standardize wealth data per column
for(j in 1:ncol(std_wealth)){
  std_wealth[,j] <- standardize(wealth[,j])
}
#check the data
std_wealth

#simulate an age-specific parameter for wealth
beta_wealth<-c(rep(0,10),seq(from=-0.1,to=0.1,length=32),rep(0,49))

#Age at first reproduction (AFR)

#simulate an age-specific parameter for AFR
mu_age<-c(rep(0,10),seq(from=0.01,to=0.1,length=11),rep(0.1,4),seq(from=0.1,to=0.01,length=17),rep(0,49))

#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
afrs[,1:10] <- 0
#randomly assign a positive output of AFR for individuals
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afr_prob <- mu_age[j]+
        beta_wealth[j]*std_wealth[i,j]
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
#check the age-specific probability of FR
colSums(as.data.frame(afrs))/100
#plot it
plot(colSums(as.data.frame(afrs))/100,xlab="Age",ylab="Frequency")

#put all the data together
#create data
data <- list(N = N, #population size
             A = A+1, #age
             wealth = std_wealth, #absolute wealth
             baby = afrs) #AFR
#check data
data

# compile model

m2 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/Model_code/firstbaby_abswealth.stan")

# fit model

fit2 <- m2$sample(data = data, 
                chains = 4, 
                parallel_chains = 4, 
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

#check the model
#check trace of all main parameters
#alpha
traceplot(rds2,pars="alpha")
#mu
#traceplot(rds2,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds2,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds2,pars="mu_tau")
#mu_kappa
traceplot(rds2,pars="mu_kappa")
#mu_delta
traceplot(rds2,pars="mu_delta")
#beta_wealth
#traceplot(rds2,pars="beta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab2 <- precis(rds2,depth=3,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tab2
#create summary table for mu
tab2_mu <- precis(rds2,depth=3,pars="mu")
#check table
tab2_mu
#create summary table for beta
tab2_beta <- precis(rds2,depth=3,pars="beta_wealth")
#check table
tab2_beta

#compute probability of FR at each age
#simulate wealth values
range(post2$beta_wealth)
simwealth <- seq(from=round(min(range(post2$beta_wealth)),1),to=round(max(range(post2$beta_wealth)),1),length.out=nrow(std_wealth)) #specify according to range and length related to sample size
simwealth
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 25
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                          post2$mu[i,25] +
                          post2$beta_wealth[i,25]*simwealth[j]) 
  }
}
#check data
p2
#plot it!
#prepare model prediction data
plot_data2 <- data.frame(wealth = simwealth,
                        mean = apply(p2, 2, mean), 
                        upp = apply(p2, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                        low = apply(p2, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#plot wealth and probability of first reproduction
plot(plot_data2$mean~plot_data2$wealth,
     ylim=c(0,0.5),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth",
     type="l")
lines(plot_data2$upp~plot_data2$wealth,col="red")
lines(plot_data2$low~plot_data2$wealth,col="blue")
points(afrs[,25]/100~plot_data2$wealth,col="gold",pch=16)
#plot the simulated betas with the ones from the model
plot(apply(post2$wealth_beta,2,mean)~beta_wealth)

## Model with Gaussian process of age, absolute and difference of material wealth ----

#Create synthetic data

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
colMeans(as.data.frame(abswealth))
#plot it
plot(colMeans(as.data.frame(abswealth)),xlab="Age",ylab="Average absolute wealth")

#standardise wealth data
#create a matrix to store the standardised data
std_abswealth <- matrix(nrow=nrow(abswealth),ncol=ncol(abswealth))
#standardize wealth data per column
for(j in 1:ncol(std_abswealth)){
  std_abswealth[,j] <- standardize(abswealth[,j])
}
#check the data
std_wealth

#simulate an age-specific parameter for wealth
beta_abswealth<-c(rep(0,10),seq(from=-0.1,to=0.1,length=32),rep(0,49))

#Difference of wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
diffwealth <- matrix(nrow=N,ncol=A+1)
#assign zero change of wealth at birth
diffwealth[,1] <- 0
#randomly assign an amount of wealth for each individual and age
for(j in 2:ncol(diffwealth)){
  for(i in 1:nrow(diffwealth)){
    diffwealth[i,j] <- std_abswealth[i,j] - std_abswealth[i,j-1]
  }
}
#check the data
#see the data
head(diffwealth)
#check the age-specific absolute wealth
colMeans(as.data.frame(diffwealth))
#plot it
plot(colMeans(as.data.frame(diffwealth)),xlab="Age",ylab="Average wealth variability")

#simulate an age-specific parameter for wealth variability
gamma_diffwealth<-c(rep(0,10),seq(from=0.1,to=-0.1,length=32),rep(0,49))

#Age at first reproduction (AFR)

#simulate an age-specific parameter for AFR
mu_age<-c(rep(0,10),seq(from=0.01,to=0.1,length=11),rep(0.1,4),seq(from=0.1,to=0.01,length=17),rep(0,49))

#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#make that ages from birth until 12 and from 33 until 90 with AFR=0 (based on range of values in data)
afrs[,1:10] <- 0
#randomly assign a positive output of AFR for individuals
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(sum(afrs[i,1:j]))==TRUE & sum(afrs[i,1:j-1]) == 0){
      afr_prob <- mu_age[j]+
        beta_abswealth[j]*abswealth[i,j]+
        gamma_diffwealth[j]*diffwealth[i,j]
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
#check the age-specific frequency of FR
colSums(as.data.frame(afrs))/100
#plot it
plot(colSums(as.data.frame(afrs))/100,xlab="Age",ylab="Prob FR")

#put all the data together
#create data
data3 <- list(N = N, #population size
             A = A+1, #age
             abswealth = std_abswealth, #standardised absolute wealth
             diffwealth = diffwealth, #wealth variability
             baby = afrs) #AFR
#check data
data3

# compile model

m3 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/Model_code/firstbaby_diffwealth.stan")

# fit model

fit3 <- m3$sample(data = data3, 
                chains = 4, 
                parallel_chains = 4, 
                adapt_delta = 0.95,
                max_treedepth = 13,
                init = 0)

# save fit 
fit_3 <- rstan::read_stan_csv(fit3$output_files())
saveRDS(fit_3, "firstbaby3.rds")
#load RDS file
rds3 <- readRDS("firstbaby3.rds")
#extract samples
post3 <- extract.samples(rds3)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds3,pars="alpha")
#mu
#traceplot(rds3,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds3,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds3,pars="mu_tau")
#mu_kappa
traceplot(rds3,pars="mu_kappa")
#mu_delta
traceplot(rds3,pars="mu_delta")
#beta_wealth
#traceplot(rds3,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds3,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab3 <- precis(rds3,depth=3,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tab3
#create summary table for mu
tab3_mu <- precis(rds3,depth=3,pars="mu")
#check table
tab3_mu
#create summary table for beta
tab3_beta <- precis(rds3,depth=3,pars="beta_wealth")
#check table
tab3_beta
#create summary table for gamma
tab3_gamma <- precis(rds3,depth=3,pars="gamma_wealth")
#check table
tab3_gamma

#compute probability of FR at each age
#simulate wealth values
range(post3$gamma_wealth)
simwealth_g <- seq(from=-2.5,to=2.5,length.out=nrow(std_wealth)) #specify according to range and length according to sample size
simwealth_g
#create matrix to store the data
p3 <- matrix(nrow=nrow(post3$mu),ncol=length(simwealth_g))
p3
#fill it in with values for age 25
for(j in 1:length(simwealth_g)){
  for(i in 1:nrow(post3$mu)){
    p[i,j] <- inv_logit(post3$alpha[i] + #inv logit because originally is logit
                          post3$mu[i,25] +
                          post3$beta_wealth[i,25]*0 + #zero because that is the average from the standardization
                          post3$beta_wealth[i,25]*simwealth_g[j] 
                        ) 
  }
}
#check data
p3

#plot it!
#prepare model prediction data
plot_data3 <- data.frame(wealth = simwealth_g,
                         mean = apply(p3, 2, mean), 
                         upp = apply(p3, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                         low = apply(p3, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#plot wealth and probability of first reproduction
plot(plot_data3$mean~plot_data2$wealth,
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability",
     type="l")
lines(plot_data3$upp~plot_data2$wealth,col="red")
lines(plot_data3$low~plot_data2$wealth,col="blue")
points(colSums(as.data.frame(afrs))/100~plot_data3$wealth,col="gold",pch=16)
#plot the simulated gammas with the ones from the model
plot(apply(post3$gamma_wealth,2,mean)~gamma_wealth)

# Standardize data ----

#absolute wealth
#create a matrix
std_absw_matrix <- matrix(nrow=nrow(absw_matrix),ncol=ncol(absw_matrix))
#standardize wealth data per column
for(j in 1:ncol(std_absw_matrix)){
  std_absw_matrix[,j] <- standardize(absw_matrix[,j])
}
#check data
std_absw_matrix
#replace NaN with zero...not sure is right, though
for(j in 1:ncol(std_absw_matrix)){
  for(i in 1:nrow(std_absw_matrix)){
    if(is.na(std_absw_matrix[i,j])){
      std_absw_matrix[i,j] <- 0
    } else{
      std_absw_matrix[i,j] <- std_absw_matrix[i,j]
    }
  }
}
#check the data
std_absw_matrix
#check the age-specific frequency of absolute wealth
colMeans(as.data.frame(std_absw_matrix))
#plot it
plot(colMeans(as.data.frame(std_absw_matrix))~c(1:91),xlab="Age",ylab="Average std. absolute wealth")

#wealth variability
#create a matrix
std_diffw_matrix <- matrix(nrow=nrow(diffw_matrix),ncol=ncol(diffw_matrix))
#calculate the age-specific wealth variation with standardized values
for(j in 1:ncol(std_diffw_matrix)){
  for(i in 1:nrow(std_diffw_matrix)){
    if(j ==1){
      std_diffw_matrix[i,j] <- std_absw_matrix[i,j] - std_absw_matrix[i,j]
    } else{
      std_diffw_matrix[i,j] <- std_absw_matrix[i,j] - std_absw_matrix[i,j-1]
    }
  }
}
#check data
std_diffw_matrix
#check the age-specific frequency of absolute wealth
colMeans(as.data.frame(std_diffw_matrix))
#plot it
plot(colMeans(as.data.frame(std_diffw_matrix))~c(1:91),xlab="Age",ylab="Average std. absolute wealth")

#Fit model with real data ----

#put all the data together
#create data
data <- list(N = nrow(dataf), #population size
             A = ncol(afr_matrix), #age
             abswealth = std_absw_matrix, #standardised absolute wealth
             diffwealth = std_diffw_matrix, #standardised difference in wealth
             baby = afr_matrix) #AFR
#check data
data

# compile model

m3 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/Model_code/firstbaby_diffwealth.stan")

# fit model

fit3 <- m3$sample(data = data, 
                  chains = 4, 
                  parallel_chains = 4, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)

# save fit 

fit_3 <- rstan::read_stan_csv(fit3$output_files())
saveRDS(fit_3, "firstbaby3.rds")

#check the model
#summary of the model
fit3_estimates<-precis(fit3,depth=2)
fit3_estimates
#age
fit3_estimates_age <- precis(fit3,depth=2,pars="age")
fit3_estimates_age
plot(inv_logit(fit3_estimates_age[,1])~c(1:91),xlab="Age",ylab="Prob. of First Reproduction",ylim=c(0,1))
#absolute wealth
fit3_estimates_wealth <- precis(fit3,depth=2,pars="wealth_beta")
fit3_estimates_wealth
plot(inv_logit(fit3_estimates_wealth[,1])~c(1:91),xlab="Age",ylab="Coeff. of absolute wealth",ylim=c(0,1))
#Wealth variability
fit3_estimates_wealth <- precis(fit3,depth=2,pars="wealth_gamma")
fit3_estimates_wealth
plot(inv_logit(fit3_estimates_wealth[,1])~c(1:91),xlab="Age",ylab="Coeff. of wealth variability",ylim=c(0,1))

#load RDS file
rds3 <- readRDS("firstbaby3.rds")
#extract samples
post3 <- extract.samples(rds3)

#check trace of all main parameters
#alpha
traceplot(rds3,pars="alpha")
#age
traceplot(rds3,pars="age")
#age_raw
traceplot(rds3,pars="age_raw")
#age_kappa
traceplot(rds3,pars="age_kappa")
#age_tau
traceplot(rds3,pars="age_tau")
#age_delta
traceplot(rds3,pars="age_delta")
#wealth_beta
traceplot(rds3,pars="wealth_beta")
#wealth_gamma
traceplot(rds3,pars="wealth_gamma")

#summary of the model
#create summary table
tab3 <- precis(rds3,depth=3,pars=c("alpha",
                                   "age",
                                   "age_raw",
                                   "age_tau",
                                   "age_kappa",
                                   "age_delta",
                                   "wealth_beta",
                                   "wealth_gamma"))
#check table
tab3

#plot age random effect
plot(c(1:91)~inv_logit(tab3[2:92,1]),
     ylab="Age",
     xlab="Age coeff.",
     main="Model with Gaussian process of age, absolute and variability of wealth")
points(c(1:91)~beta_age,col="red")

#plot absolute wealth
plot(c(1:91)~tab3[187:277,1],
     ylab="Age",
     xlab="Absolute wealth coeff.",
     main="Model with Gaussian process of age, absolute and variability of wealth")
points(c(1:91)~beta_abswealth,col="red")

#plot wealth variability
plot(c(1:91)~tab3[278:nrow(tab3),1],
     ylab="Age",
     xlab="Wealth variability coeff.",
     main="Model with Gaussian process of age, absolute and variability of wealth")
points(c(1:91)~beta_diffwealth,col="red")








##### Dieter's approach -----


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
beta_age<-c(rep(0,10),seq(from=0.01,to=0.1,length=11),rep(0.1,4),seq(from=0.1,to=0.01,length=17),rep(0,49))

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
