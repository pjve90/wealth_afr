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
        beta_wealth[j]*std_wealth[i,j] #wealth
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
  plot_ind2[i,]$wealth<-std_wealth[i,afr]
  }
}
# Individuals with more wealth have their first child later
plot(plot_ind2$afr~plot_ind2$wealth)

## Fit simulated data ----

#put all the data together
#create data
data <- list(N = N, #population size
             A = A, #age
             wealth = std_wealth, #absolute wealth
             baby = afrs) #AFR
#check data
data

# compile model

m2 <- cmdstan_model("Model_code/firstbaby_abswealth.stan")

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
plot(inv_logit(tab2_mu[,1]))
#create summary table for beta
tab2_beta <- precis(rds2,depth=3,pars="beta_wealth")
#check table
tab2_beta
plot((tab2_beta[,1]))

## Plot the fit of the simulated data ----

#simulate wealth values
simwealth <- seq(from=round(min(std_wealth),1),to=round(max(std_wealth),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth

#colour palette
palette<-c(rep("NA",14),palette(gray(seq(0,.9,length.out = 11)))) #darker lines = younger ages, lighter lines = older ages

#plot empty plot
plot(c(0,0.4)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth",
     type="n")
#add lines
for(k in seq(15,25,by=1)){
  #create matrix to store the data
  p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
  p2
  #fill it in with values for age 25
  for(j in 1:length(simwealth)){
    for(i in 1:nrow(post2$mu)){
      p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                  post2$mu[i,k] + #age
                                  post2$beta_wealth[i,k]*simwealth[j]) #wealth
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
  #prepare afr probabilities from simulated data
  #create a matrix
  plot_afr2 <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2$mean~plot_data2$wealth,col=palette[k])
}

#plot one plot per age between 15 and 25

#define the layout
layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,0,10,10,11,11,0),nrow=4,ncol=6,byrow=T))

#plot wealth and probability of first reproduction at age 15
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 15",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 15
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,15] +
                                post2$beta_wealth[i,15]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[15])
lines(plot_data2$upp~plot_data2$wealth,col=palette,lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette,lty=2)
points(plot_afr2[,15]~plot_data2$wealth,col=alpha(palette[15],0.5),pch=16)

#plot wealth and probability of first reproduction at age 16 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 16",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 16
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,16] +
                                post2$beta_wealth[i,16]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[16])
lines(plot_data2$upp~plot_data2$wealth,col=palette[16],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[16],lty=2)
points(plot_afr2[,16]~plot_data2$wealth,col=alpha(palette[16],0.5),pch=16)

#plot wealth and probability of first reproduction at age 17 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 17",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 17
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,17] +
                                post2$beta_wealth[i,17]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[17])
lines(plot_data2$upp~plot_data2$wealth,col=palette[17],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[17],lty=2)
points(plot_afr2[,17]~plot_data2$wealth,col=alpha(palette[17],0.5),pch=16)

#plot wealth and probability of first reproduction at age 18 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 18",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 18
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,18] +
                                post2$beta_wealth[i,18]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[18])
lines(plot_data2$upp~plot_data2$wealth,col=palette[18],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[18],lty=2)
points(plot_afr2[,18]~plot_data2$wealth,col=alpha(palette[18],0.5),pch=16)

#plot wealth and probability of first reproduction at age 19 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 19",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 19
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,19] +
                                post2$beta_wealth[i,19]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[19])
lines(plot_data2$upp~plot_data2$wealth,col=palette[19],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[19],lty=2)
points(plot_afr2[,19]~plot_data2$wealth,col=alpha(palette[19],0.5),pch=16)

#plot wealth and probability of first reproduction at age 20 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 20",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 20
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,20] +
                                post2$beta_wealth[i,20]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[20])
lines(plot_data2$upp~plot_data2$wealth,col=palette[20],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[20],lty=2)
points(plot_afr2[,20]~plot_data2$wealth,col=alpha(palette[20],0.5),pch=16)

#plot wealth and probability of first reproduction at age 21 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 21",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 21
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,21] +
                                post2$beta_wealth[i,21]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[21])
lines(plot_data2$upp~plot_data2$wealth,col=palette[21],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[21],lty=2)
points(plot_afr2[,21]~plot_data2$wealth,col=alpha(palette[21],0.5),pch=16)

#plot wealth and probability of first reproduction at age 22 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 22",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 22
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,22] +
                                post2$beta_wealth[i,22]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[22])
lines(plot_data2$upp~plot_data2$wealth,col=palette[22],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[22],lty=2)
points(plot_afr2[,22]~plot_data2$wealth,col=alpha(palette[22],0.5),pch=16)

#plot wealth and probability of first reproduction at age 23 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 23",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 23
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,23] +
                                post2$beta_wealth[i,23]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[23])
lines(plot_data2$upp~plot_data2$wealth,col=palette[23],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[23],lty=2)
points(plot_afr2[,23]~plot_data2$wealth,col=alpha(palette[23],0.5),pch=16)

#plot wealth and probability of first reproduction at age 24 
plot(c(0,1)~c(min(simwealth),max(simwealth)),
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 24",
     type="n")
#create matrix to store the data
p2 <- matrix(nrow=nrow(post2$mu),ncol=length(simwealth))
p2
#fill it in with values for age 24
for(j in 1:length(simwealth)){
  for(i in 1:nrow(post2$mu)){
    p2[i,j] <- inv_logit(post2$alpha[i] + #inv logit because originally is logit
                                post2$mu[i,24] +
                                post2$beta_wealth[i,24]*simwealth[j]) 
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[24])
lines(plot_data2$upp~plot_data2$wealth,col=palette[24],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[24],lty=2)
points(plot_afr2[,24]~plot_data2$wealth,col=alpha(palette[24],0.5),pch=16)

#plot wealth and probability of first reproduction at age 25 
plot(c(0,0.2)~c(min(simwealth),max(simwealth)),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 25",
     type="n")
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
#prepare afr probabilities from simulated data
#create a matrix
plot_afr2 <- afr_matrix2
#change -99 to NAs
for(j in 1:ncol(plot_afr2)){
  for(i in 1:nrow(plot_afr2)){
    if(plot_afr2[i,j]==-99){
      plot_afr2[i,j] <- NA
    }
  }
}
#check the data
plot_afr2

lines(plot_data2$mean~plot_data2$wealth,col=palette[25])
lines(plot_data2$upp~plot_data2$wealth,col=palette[25],lty=2)
lines(plot_data2$low~plot_data2$wealth,col=palette[25],lty=2)
points(plot_afr2[,25]~plot_data2$wealth,col=alpha(palette[25],0.5),pch=16)

## Data wrangling of real data ----

#Load data
real_data2 <- read.csv("~/dataf.csv")[,-1]
head(real_data2)

# Age at first reproduction 

#create a matrix to store the age-specific age of censor
afr_matrix2 <- matrix(nrow=nrow(real_data2),ncol=A)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix2)){
  afr <- real_data2$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data2$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix2[i,] <- 0
    afr_matrix2[i,afr] <- 1
  } else{
    afr_matrix2[i,1:aoc] <- rep(0,length(afr_matrix2[i,1:aoc]))
  }
}
#check the data
afr_matrix2
#check the age-specific probability of FR
apply(afr_matrix2,2,sum,na.rm=T)/sum(apply(afr_matrix2,2,sum,na.rm=T))
#plot it
plot(apply(afr_matrix2,2,sum,na.rm=T)/sum(apply(afr_matrix2,2,sum,na.rm=T))~c(1:ncol(afr_matrix2)),xlab="Age",ylab="Probability of first reproduction",ylim=c(0,0.2))

#replace NAs with -99
for(j in 1:ncol(afr_matrix2)){
  for(i in 1:nrow(afr_matrix2)){
    if(is.na(afr_matrix2[i,j])){
      afr_matrix2[i,j] <- -99
    } else{
      afr_matrix2[i,j] <- afr_matrix2[i,j]
    }
  }
}
#check the data
afr_matrix2

#Age-specific absolute wealth

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix2 <- matrix(nrow = nrow(real_data2),ncol=A+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw95[i]
  age_absw <- real_data2$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#98
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw98[i]
  age_absw <- real_data2$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#00
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw00[i]
  age_absw <- real_data2$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#02
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw02[i]
  age_absw <- real_data2$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#04
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw04[i]
  age_absw <- real_data2$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#06
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw06[i]
  age_absw <- real_data2$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#10
for(i in 1:nrow(absw_matrix2)){
  absw <- real_data2$absw10[i]
  age_absw <- real_data2$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix2[i,age_absw] <- absw
  } else{
    absw_matrix2[i,age_absw] <- NA
  }
}
#check data
absw_matrix2
#check the age-specific average of absolute wealth
apply(absw_matrix2,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix2,2,mean,na.rm=T)~c(1:ncol(absw_matrix2)),xlab="Age",ylab="Average absolute wealth")

#NaN in columns where there are no values of wealth

# Simple data imputation 

#replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
for(i in 1:length(absw_matrix2[,1])){
  if(is.na(absw_matrix2[i,1]) & is.na(absw_matrix2[i,2])){
    absw_matrix2[i,1] <- mean(absw_matrix2[,1],na.rm = T)
  }else if(is.na(absw_matrix2[i,1]) & !is.na(absw_matrix2[i,2])){
    absw_matrix2[i,1] <- absw_matrix2[i,2]
  }
}
#check the data
absw_matrix2
sum(is.na(absw_matrix2[,1]))
#n=0
#replace the missing wealth data by putting the mean between the two ages it is
for(j in 2:(ncol(absw_matrix2)-1)){
  for(i in 1:nrow(absw_matrix2)){
    if(is.na(absw_matrix2[i,j])&!is.na(absw_matrix2[i,j-1])&!is.na(absw_matrix2[i,j+1])){
      absw_matrix2[i,j] <- mean(c(absw_matrix2[i,j-1],absw_matrix2[i,j+1]))
    } else if(is.na(absw_matrix2[i,j])&!is.na(absw_matrix2[i,j-1])&is.na(absw_matrix2[i,j+1])){
      absw_matrix2[i,j] <- mean(absw_matrix2[,j],na.rm=T)
    } 
  }
}
#check the data
absw_matrix2
sum(is.na(absw_matrix2))
#n=21824
#replace the missing wealth data by putting the mean between the two ages it is
for(j in 2:(ncol(absw_matrix2)-1)){
  for(i in 1:nrow(absw_matrix2)){
    if(is.na(absw_matrix2[i,j])&!is.na(absw_matrix2[i,j-1])&!is.na(absw_matrix2[i,j+1])){
      absw_matrix2[i,j] <- mean(c(absw_matrix2[i,j-1],absw_matrix2[i,j+1]))
    } else if(is.na(absw_matrix2[i,j])&!is.na(absw_matrix2[i,j-1])&is.na(absw_matrix2[i,j+1])){
      absw_matrix2[i,j] <- mean(absw_matrix2[,j],na.rm=T)
    } 
  }
}
#check the data
absw_matrix2
sum(is.na(absw_matrix2))
#n=9179
#replace the missing wealth data by putting either the mean between the two ages it is or by repeating the value from previous year
for(j in 2:(ncol(absw_matrix2)-1)){
  for(i in 1:nrow(absw_matrix2)){
    if(is.na(absw_matrix2[i,j])&length(is.na(absw_matrix2[i,j:91]))!=sum(is.na(absw_matrix2[i,j:91]))){
      absw_matrix2[i,j] <- mean(c(absw_matrix2[i,j-1],absw_matrix2[i,max(which(!is.na(absw_matrix2[i,])==T))]))
    } else if(is.na(absw_matrix2[i,j])&length(is.na(absw_matrix2[i,j:91]))==sum(is.na(absw_matrix2[i,j:91]))){
      absw_matrix2[i,j] <- absw_matrix2[i,j-1]
    }
  }
}
#check the data
absw_matrix2
sum(is.na(absw_matrix2))
#n=540
#replace last column with the values from last year
absw_matrix2[,max(ncol(absw_matrix2))] <- absw_matrix2[,max(ncol(absw_matrix2))-1]
#check the data
absw_matrix2
sum(is.na(absw_matrix2))
#n=0
#check the age-specific frequency of absolute wealth
apply(absw_matrix2,2,mean)
#plot it
plot(apply(absw_matrix2,2,mean)~c(1:91),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth per column
#create a matrix
std_absw_matrix2 <- matrix(nrow=nrow(absw_matrix2),ncol=ncol(absw_matrix2))
#standardize wealth data per column
for(j in 1:ncol(std_absw_matrix2)){
  std_absw_matrix2[,j] <- standardize(absw_matrix2[,j])
}
#check data
std_absw_matrix2
#replace NaN with zero...not sure is right, though...probably will change with bayesian imputation
for(j in 1:ncol(std_absw_matrix2)){
  for(i in 1:nrow(std_absw_matrix2)){
    if(is.na(std_absw_matrix2[i,j])){
      std_absw_matrix2[i,j] <- 0
    } else{
      std_absw_matrix2[i,j] <- std_absw_matrix2[i,j]
    }
  }
}
#check the data
std_absw_matrix2
#check the age-specific average of absolute wealth
apply(std_absw_matrix2,2,mean)
#plot it
plot(apply(std_absw_matrix2,2,mean)~c(1:(A+1)),xlab="Age",ylab="Average absolute wealth")

## Fit real data ----

#put all the data together
#create dataset
real_list2 <- list(N = nrow(real_data2), #population size
                   N_wm = 
                   A = ncol(afr_matrix2), #age
                   wealth = std_absw_matrix2, #absolute wealth
                   baby = afr_matrix2) #AFR

# fit model
fit2_real <- m2$sample(data = real_list2, 
                       chains = 4, 
                       parallel_chains = 10, 
                       adapt_delta = 0.95,
                       max_treedepth = 13,
                       init = 0)

# save fit 
fit_2_real <- rstan::read_stan_csv(fit2_real$output_files())
saveRDS(fit_2_real, "firstbaby2_real.rds")
#load RDS file
rds2_real <- readRDS("firstbaby2_real.rds")
#extract samples
post2_real <- extract.samples(rds2_real)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds2_real,pars="alpha")
#mu
#traceplot(rds2_real,pars="mu") #only run if needed, because they are 92 plots
#mu_raw
#traceplot(rds2_real,pars="mu_raw") #only run if needed, because they are 92 plots
#mu_tau
traceplot(rds2_real,pars="mu_tau")
#mu_kappa
traceplot(rds2_real,pars="mu_kappa")
#mu_delta
traceplot(rds2_real,pars="mu_delta")
#beta_wealth
#traceplot(rds2_real,pars="beta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab2_real <- precis(rds2_real,depth=3,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tab2_real
#create summary table for mu
tab2_mu_real <- precis(rds2_real,depth=3,pars="mu")
#check table
tab2_mu_real
#create summary table for beta
tab2_beta_real <- precis(rds2_real,depth=3,pars="beta_wealth")
#check table
tab2_beta_real

# To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
#mu
tab2_mu_real[,1]<-round(inv_logit(tab2_mu_real[,1]),3)
tab2_mu_real[,3]<-round(inv_logit(tab2_mu_real[,3]),3)
tab2_mu_real[,4]<-round(inv_logit(tab2_mu_real[,4]),3)
#beta_wealth
tab2_beta_real[,1]<-round(inv_logit(tab2_beta_real[,1]),3)
tab2_beta_real[,3]<-round(inv_logit(tab2_beta_real[,3]),3)
tab2_beta_real[,4]<-round(inv_logit(tab2_beta_real[,4]),3)

## Plot the fit of the real data ----

#simulate wealth values
range(real_list2$wealth) #use the range of values of real data as reference for simulation
simwealth_real <- seq(from=round(min(range(real_list2$wealth)),1),to=round(max(range(real_list2$wealth)),1),length.out=nrow(std_absw_matrix2)) #specify according to range and length related to sample size
simwealth_real

#colour palette
palette<-c(rep("NA",14),palette(gray(seq(0,.9,length.out = 11)))) #darker lines = younger ages, lighter lines = older ages

#plot empty plot
plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth",
     type="n")

#add lines
for(k in seq(15,25,by=1)){
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 25
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,k] + #age
                                  post2_real$beta_wealth[i,k]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[k])
}

#plot one plot per age between 15 and 25

#define the layout
layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,0,10,10,11,11,0),nrow=4,ncol=6,byrow=T))

#plot wealth and probability of first reproduction at age 15 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 15",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 15
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,15] + #age
                                  post2_real$beta_wealth[i,15]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[15])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette,lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette,lty=2)
  points(plot_afr2[,15]~plot_data2_real$wealth,col=alpha(palette[15],0.5),pch=16)

#plot wealth and probability of first reproduction at age 16 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 16",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 16
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,16] + #age
                                  post2_real$beta_wealth[i,16]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[16])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[16],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[16],lty=2)
  points(plot_afr2[,16]~plot_data2_real$wealth,col=alpha(palette[16],0.5),pch=16)

#plot wealth and probability of first reproduction at age 17 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 17",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 17
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,17] + #age
                                  post2_real$beta_wealth[i,17]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[17])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[17],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[17],lty=2)
  points(plot_afr2[,17]~plot_data2_real$wealth,col=alpha(palette[17],0.5),pch=16)

#plot wealth and probability of first reproduction at age 18 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 18",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 18
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,18] + #age
                                  post2_real$beta_wealth[i,18]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[18])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[18],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[18],lty=2)
  points(plot_afr2[,18]~plot_data2_real$wealth,col=alpha(palette[18],0.5),pch=16)

#plot wealth and probability of first reproduction at age 19 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 19",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 19
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,19] + #age
                                  post2_real$beta_wealth[i,19]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[19])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[19],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[19],lty=2)
  points(plot_afr2[,19]~plot_data2_real$wealth,col=alpha(palette[19],0.5),pch=16)

#plot wealth and probability of first reproduction at age 20 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 20",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 20
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,20] + #age
                                  post2_real$beta_wealth[i,20]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[20])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[20],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[20],lty=2)
  points(plot_afr2[,20]~plot_data2_real$wealth,col=alpha(palette[20],0.5),pch=16)

  #plot wealth and probability of first reproduction at age 21 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 21",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 21
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,21] + #age
                                  post2_real$beta_wealth[i,21]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[21])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[21],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[21],lty=2)
  points(plot_afr2[,21]~plot_data2_real$wealth,col=alpha(palette[21],0.5),pch=16)
  
#plot wealth and probability of first reproduction at age 22 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 22",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 22
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,22] + #age
                                  post2_real$beta_wealth[i,22]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[22])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[22],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[22],lty=2)
  points(plot_afr2[,22]~plot_data2_real$wealth,col=alpha(palette[22],0.5),pch=16)

  
#plot wealth and probability of first reproduction at age 23 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 23",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 23
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,23] + #age
                                  post2_real$beta_wealth[i,23]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[23])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[23],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[23],lty=2)
  points(plot_afr2[,23]~plot_data2_real$wealth,col=alpha(palette[23],0.5),pch=16)
  
#plot wealth and probability of first reproduction at age 24 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 24",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 24
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,24] + #age
                                  post2_real$beta_wealth[i,24]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[24])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[24],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[24],lty=2)
  points(plot_afr2[,24]~plot_data2_real$wealth,col=alpha(palette[24],0.5),pch=16)
  
#plot wealth and probability of first reproduction at age 25 
  plot(c(0,0.2)~c(min(range(real_list2$wealth)),max(range(real_list2$wealth))),
       ylim=c(0,1),
       ylab="Prob. FR",
       xlab="Wealth",
       main="Model with absolute wealth at age 25",
       type="n")
  #create matrix to store the data
  p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
  p2_real
  #fill it in with values for age 25
  for(j in 1:length(simwealth_real)){
    for(i in 1:nrow(post2_real$mu)){
      p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                                  post2_real$mu[i,25] + #age
                                  post2_real$beta_wealth[i,25]*simwealth_real[j]) #wealth
    }
  }
  #check data
  p2_real
  #plot it!
  #prepare model prediction data
  plot_data2_real <- data.frame(wealth = simwealth_real,
                                mean = apply(p2_real, 2, mean), 
                                upp = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afr_matrix2
  #change -99 to NAs
  for(j in 1:ncol(plot_afr2)){
    for(i in 1:nrow(plot_afr2)){
      if(plot_afr2[i,j]==-99){
        plot_afr2[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[25])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[25],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[25],lty=2)
  points(plot_afr2[,25]~plot_data2_real$wealth,col=alpha(palette[25],0.5),pch=16)