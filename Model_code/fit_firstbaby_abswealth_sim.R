# Model with absolute wealth ----

# Additive model ----

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
apply(wealth,2,mean)
#plot it
plot(apply(wealth,2,mean),xlab="Age",ylab="Average absolute wealth")

#standardise wealth data
std_wealth <- matrix(standardize(as.vector(wealth)),ncol=ncol(wealth),nrow=nrow(wealth))
#check the data
std_wealth

#simulate an age-specific parameter for wealth (beta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
beta_wealth<-c(rep(0,13),seq(from=-0.1,to=0.1,length=16),rep(0,62))
std_beta_wealth<-beta_wealth/sd(as.vector(wealth)) # adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale

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
apply(afrs,2,sum)
apply(afrs,2,sum)/N
#plot it
plot(apply(afrs,2,sum)/N,
     ylim=c(-0.01,0.2),
     xlab="Age",
     ylab="Probability of first reproduction",
     pch=16) #data
points(mu_age+std_beta_wealth,col=alpha(hcl.colors(10,"ag_Sunset")[5],0.7),pch=15) #mu+std_beta
lines(mu_age+std_beta_wealth,col=alpha(hcl.colors(10,"ag_Sunset")[5],0.7)) #mu+std_beta

#check age-specific relationship between wealth and prob. of FR
#plot empty plot
plot(c(-0.01,0.25)~c(min(std_wealth),max(std_wealth)),
     xlab="Age-specific absolute wealth",
     ylab="Age-specific prob. of FR",
     type="n"
     )
#add the curves
#more purple are younger and more yellow are older
curve(mu_age[15]+std_beta_wealth[15]*x,from=min(std_wealth),to=max(std_wealth),add=T,col=hcl.colors(6,"ag_Sunset")[1])
curve(mu_age[17]+std_beta_wealth[17]*x,from=min(std_wealth),to=max(std_wealth),add=T,col=hcl.colors(6,"ag_Sunset")[2])
curve(mu_age[19]+std_beta_wealth[19]*x,from=min(std_wealth),to=max(std_wealth),add=T,col=hcl.colors(6,"ag_Sunset")[3])
curve(mu_age[21]+std_beta_wealth[21]*x,from=min(std_wealth),to=max(std_wealth),add=T,col=hcl.colors(6,"ag_Sunset")[4])
curve(mu_age[23]+std_beta_wealth[23]*x,from=min(std_wealth),to=max(std_wealth),add=T,col=hcl.colors(6,"ag_Sunset")[5])
curve(mu_age[25]+std_beta_wealth[25]*x,from=min(std_wealth),to=max(std_wealth),add=T,col=hcl.colors(6,"ag_Sunset")[6])
#pattern is that at younger ages, poor individuals have a higher probability of FR whereas at older ages the rich ones have a higher probability of FR

# Introduce missing data in the wealth data frame
for (j in 1:ncol(std_wealth)){
  for (i in 1:nrow(std_wealth)){
    if(runif(1,min=0,max=1)<0.4){std_wealth[i,j]<- -99} # 40% missing data
  }
}
#check data
head(std_wealth)

# Only take the years when individuals have a first baby
#check min and max ages at first reproduction
apply(afrs,2,sum)
#min
min(which(apply(afrs,2,sum)>0))
#14
#max
max(which(apply(afrs,2,sum)>0))
#39

std_wealth_restricted<-std_wealth[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]
afrs_restricted<-afrs[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]

## Fit simulated data, using the combined data imputation approach ----

#put all the data together
#create data
data2 <- list(N = nrow(afrs_restricted), #population size
               A = ncol(afrs_restricted), #age
               wealth = as.vector(t(std_wealth_restricted)), #absolute wealth
               N_miss = sum((std_wealth_restricted)== -99), # number of missing values that need imputation
               id_wealth_miss = which(as.vector(t(std_wealth_restricted))== -99), # provide the indexes for the missing data
               baby = afrs_restricted #AFR
               ) 

#check data 
data2

# compile model

m2_add <- cmdstan_model("Model_code/firstbaby_abswealth_additive.stan")

# fit model

fit2_add <- m2_add$sample(data = data2, 
                            chains = 4, 
                            parallel_chains = 15, 
                            adapt_delta = 0.95,
                            max_treedepth = 13,
                            init = 0)

# save fit 
fit2_add <- rstan::read_stan_csv(fit2_add$output_files())
saveRDS(fit2_add, "firstbaby2_add.rds")
#load RDS file
rds2_add <- readRDS("firstbaby2_add.rds")
#extract samples
post2_add <- extract.samples(rds2_add)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds2_add,pars="alpha")
#mu
#traceplot(rds2_add,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds2_add,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds2_add,pars="mu_tau")
#mu_kappa
traceplot(rds2_add,pars="mu_kappa")
#mu_delta
traceplot(rds2_add,pars="mu_delta")
#beta_wealth
#traceplot(rds2_add,pars="beta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tabs2_add <- precis(rds2_add,depth=3,pars=c("alpha",
                                              "mu_raw",
                                              "mu_tau",
                                              "mu_delta"))
#check table
tabs2_add
#create summary table for mu
tabs2_add_mu <- precis(rds2_add,depth=3,pars="mu")
#check table
tabs2_add_mu
plot(inv_logit(tabs2_add_mu[,1]))
#create summary table for beta
tabs2_add_beta <- precis(rds2_add,depth=3,pars="beta_wealth")
#check table
tabs2_add_beta
plot((tabs2_add_beta[,1]))

## Plot the fit of the simulated data ----

### Age ----

#simulate wealth values
simwealth_add <- seq(from=round(min(std_wealth_restricted[which(std_wealth_restricted > -99)]),1),to=round(max(std_wealth_restricted[which(std_wealth_restricted > -99)]),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length of wealth data
simwealth_add

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post2_add$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(min(std_wealth_restricted[which(std_wealth_restricted > -99)]),max(std_wealth_restricted[which(std_wealth_restricted > -99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth",
     type="n")

#add lines
for(k in 1:length(age_quantiles)){
  #create matrix to store the data
  p2_add <- matrix(nrow=nrow(post2_add$mu),ncol=length(simwealth_add))
  p2_add
  #fill it in with values for age 25
  for(j in 1:length(simwealth_add)){
    for(i in 1:nrow(post2_add$mu)){
      p2_add[i,j] <- inv_logit(post2_add$alpha[i] + #inv logit because originally is logit
                                  post2_add$mu[i,age_quantiles[k]] + #age
                                  post2_add$beta_wealth[i,age_quantiles[k]]*simwealth_add[j]) #wealth
    }
  }
  #check data
  p2_add
  #plot it!
  #prepare model prediction data
  plot_data2_add <- data.frame(wealth = simwealth_add,
                                mean = apply(p2_add, 2, mean), 
                                upp = apply(p2_add, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_add, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from comb data
  #create a matrix
  plot_afr2 <- afrs_restricted
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
  
  lines(plot_data2_add$mean~plot_data2_add$wealth,col=palette[k])
}

### Wealth ----

#simulate wealth values
simwealth_add <- seq(from=round(min(std_wealth_restricted[which(std_wealth_restricted > -99)]),1),to=round(max(std_wealth_restricted[which(std_wealth_restricted > -99)]),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length of wealth data
simwealth_add
#get the deciles
deciles <- as.numeric(quantile(simwealth_add,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(0,ncol(post2_add$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth",
     type="n")
axis(1,at=seq(0,ncol(post2_add$mu),by=1),labels=min(which(apply(afrs,2,sum)>0)):(max(which(apply(afrs,2,sum)>0))+1))

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p2_add_b <- matrix(nrow=nrow(post2_add$mu),ncol=ncol(post2_add$mu))
  p2_add_b
  #fill it in with values for age 25
  for(j in 1:ncol(post2_add$mu)){
    for(i in 1:nrow(post2_add$mu)){
      p2_add_b[i,j] <- inv_logit(post2_add$alpha[i] + #inv logit because originally is logit
                                    post2_add$mu[i,j] + #age
                                    post2_add$beta_wealth[i,j]*deciles[k]) #wealth
    }
  }
  #check data
  p2_add_b
  #plot it!
  #prepare model prediction data
  plot_data2_add_b <- data.frame(age = 1:ncol(p2_add_b),
                                  mean = apply(p2_add_b, 2, mean), 
                                  upp = apply(p2_add_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p2_add_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afrs_restricted
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
  
  points(plot_data2_add_b$mean~plot_data2_add_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data2_add_b$mean~plot_data2_add_b$age,col=palette_b[k])
}

# Multiplicative model ----
# multiplicative interaction instead of additive

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
apply(wealth,2,mean)
#plot it
plot(apply(wealth,2,mean),xlab="Age",ylab="Average absolute wealth")

#standardise wealth data
std_wealth_restricted <- matrix(standardize(as.vector(wealth)),ncol=ncol(wealth),nrow=nrow(wealth))
#check the data
std_wealth_restricted

#simulate an age-specific parameter for wealth (beta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
beta_wealth<-c(rep(0,13),seq(from=-0.1,to=0.1,length=16),rep(0,62))
std_beta_wealth<-beta_wealth/sd(as.vector(wealth)) # adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale

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
      afr_prob <- mu_age[j]* #age
        exp(1)^(std_beta_wealth[j]*std_wealth_restricted[i,j]) #wealth
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
apply(afrs,2,sum)
apply(afrs,2,sum)/N
#plot it
plot(apply(afrs,2,sum)/N,
     ylim=c(-0.01,0.2),
     xlab="Age",
     ylab="Probability of first reproduction",
     pch=16) #data
lines(mu_age+std_beta_wealth,col=hcl.colors(10,"ag_Sunset")[5]) #mu+std_beta

#check age-specific relationship between wealth and prob. of FR
#plot empty plot
plot(c(-0.10,0.25)~c(min(std_wealth_restricted),max(std_wealth_restricted)),
     xlab="Age-specific absolute wealth",
     ylab="Age-specific prob. of FR",
     type="n"
)
#add the curves
#more purple are younger and more yellow are older
curve(mu_age[15]+std_beta_wealth[15]*x,from=min(std_wealth_restricted),to=max(std_wealth_restricted),add=T,col=hcl.colors(6,"ag_Sunset")[1])
curve(mu_age[17]+std_beta_wealth[17]*x,from=min(std_wealth_restricted),to=max(std_wealth_restricted),add=T,col=hcl.colors(6,"ag_Sunset")[2])
curve(mu_age[19]+std_beta_wealth[19]*x,from=min(std_wealth_restricted),to=max(std_wealth_restricted),add=T,col=hcl.colors(6,"ag_Sunset")[3])
curve(mu_age[21]+std_beta_wealth[21]*x,from=min(std_wealth_restricted),to=max(std_wealth_restricted),add=T,col=hcl.colors(6,"ag_Sunset")[4])
curve(mu_age[23]+std_beta_wealth[23]*x,from=min(std_wealth_restricted),to=max(std_wealth_restricted),add=T,col=hcl.colors(6,"ag_Sunset")[5])
curve(mu_age[25]+std_beta_wealth[25]*x,from=min(std_wealth_restricted),to=max(std_wealth_restricted),add=T,col=hcl.colors(6,"ag_Sunset")[6])
#pattern is that at younger ages, poor individuals have a higher probability of FR whereas at older ages the rich ones have a higher probability of FR

# Introduce missing data in the wealth data frame
for (j in 1:ncol(std_wealth_restricted)){
  for (i in 1:nrow(std_wealth_restricted)){
    if(runif(1,min=0,max=1)<0.4){std_wealth_restricted[i,j]<- -99} # 40% missing data
  }
}
#check data
head(std_wealth_restricted)

# Only take the years when individuals have a first baby
#check min and max ages at first reproduction
apply(afrs,2,sum)
#min
min(which(apply(afrs,2,sum)>0))
#14
#max
max(which(apply(afrs,2,sum)>0))
#39

std_wealth_restricted_restricted<-std_wealth_restricted[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]
afrs_restricted<-afrs[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]

## Fit simulated data, using the combined data imputation approach ----

#put all the data together
#create data
data2 <- list(N = nrow(afrs_restricted), #population size
              A = ncol(afrs_restricted), #age
              wealth = as.vector(t(std_wealth_restricted_restricted)), #absolute wealth
              N_miss = sum((std_wealth_restricted_restricted)== -99), # number of missing values that need imputation
              id_wealth_miss = which(as.vector(t(std_wealth_restricted_restricted))== -99), # provide the indexes for the missing data
              baby = afrs_restricted #AFR
) 

#check data 
data2

# compile model

m2_mult <- cmdstan_model("Model_code/firstbaby_abswealth_multiplicative.stan")

# fit model

fit2_mult <- m2_mult$sample(data = data2, 
                          chains = 4, 
                          parallel_chains = 15, 
                          adapt_delta = 0.95,
                          max_treedepth = 13,
                          init = 0)

# save fit 
fit2_mult <- rstan::read_stan_csv(fit2_mult$output_files())
saveRDS(fit2_mult, "firstbaby2_mult.rds")
#load RDS file
rds2_mult <- readRDS("firstbaby2_mult.rds")
#extract samples
post2_mult <- extract.samples(rds2_mult)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds2_mult,pars="alpha")
#mu
#traceplot(rds2_mult,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds2_mult,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds2_mult,pars="mu_tau")
#mu_kappa
traceplot(rds2_mult,pars="mu_kappa")
#mu_delta
traceplot(rds2_mult,pars="mu_delta")
#beta_wealth
#traceplot(rds2_mult,pars="beta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tabs2_mult <- precis(rds2_mult,depth=3,pars=c("alpha",
                                            "mu_raw",
                                            "mu_tau",
                                            "mu_delta"))
#check table
tabs2_mult
#create summary table for mu
tabs2_mult_mu <- precis(rds2_mult,depth=3,pars="mu")
#check table
tabs2_mult_mu
plot(inv_logit(tabs2_mult_mu[,1]))
#create summary table for beta
tabs2_mult_beta <- precis(rds2_mult,depth=3,pars="beta_wealth")
#check table
tabs2_mult_beta
plot((tabs2_mult_beta[,1]))

## Plot the fit of the simulated data ----

### Age ----

#simulate wealth values
simwealth_mult <- seq(from=round(min(std_wealth_restricted[which(std_wealth_restricted > -99)]),1),to=round(max(std_wealth_restricted[which(std_wealth_restricted > -99)]),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length of wealth data
simwealth_mult

#get age quantiles
age_quantiles <- as.numeric(round(quantile(1:ncol(post2_mult$mu),seq(0,1,0.25))))
age_quantiles

#colour palette
palette<-hcl.colors(length(age_quantiles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(min(std_wealth_restricted[which(std_wealth_restricted > -99)]),max(std_wealth_restricted[which(std_wealth_restricted > -99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth",
     type="n")

#add lines
for(k in 1:length(age_quantiles)){
  #create matrix to store the data
  p2_mult <- matrix(nrow=nrow(post2_mult$mu),ncol=length(simwealth_mult))
  p2_mult
  #fill it in with values for age 25
  for(j in 1:length(simwealth_mult)){
    for(i in 1:nrow(post2_mult$mu)){
      p2_mult[i,j] <- inv_logit(post2_mult$alpha[i] + #inv logit because originally is logit
                                 post2_mult$mu[i,k] + #age
                                 post2_mult$beta_wealth[i,k]*simwealth_mult[j]) #wealth
    }
  }
  #check data
  p2_mult
  #plot it!
  #prepare model prediction data
  plot_data2_mult <- data.frame(wealth = simwealth_mult,
                               mean = apply(p2_mult, 2, mean), 
                               upp = apply(p2_mult, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                               low = apply(p2_mult, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from comb data
  #create a matrix
  plot_afr2 <- afrs_restricted
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
  
  lines(plot_data2_mult$mean~plot_data2_mult$wealth,col=palette[k])
}

### Wealth ----

#simulate wealth values
simwealth_mult <- seq(from=round(min(std_wealth_restricted[which(std_wealth_restricted > -99)]),1),to=round(max(std_wealth_restricted[which(std_wealth_restricted > -99)]),1),length.out=nrow(std_wealth_restricted)) #specify according to range and length of wealth data
simwealth_mult
#get the deciles
deciles <- as.numeric(quantile(simwealth_mult,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(0,ncol(post2_mult$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth",
     type="n")
axis(1,at=seq(0,ncol(post2_mult$mu),by=1),labels=14:29)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p2_mult_b <- matrix(nrow=nrow(post2_mult$mu),ncol=ncol(post2_mult$mu))
  p2_mult_b
  #fill it in with values for age 25
  for(j in 1:ncol(post2_mult$mu)){
    for(i in 1:nrow(post2_mult$mu)){
      p2_mult_b[i,j] <- inv_logit(post2_mult$alpha[i] + #inv logit because originally is logit
                                   post2_mult$mu[i,j] + #age
                                   post2_mult$beta_wealth[i,j]*deciles[k]) #wealth
    }
  }
  #check data
  p2_mult_b
  #plot it!
  #prepare model prediction data
  plot_data2_mult_b <- data.frame(age = 1:ncol(p2_mult_b),
                                 mean = apply(p2_mult_b, 2, mean), 
                                 upp = apply(p2_mult_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p2_mult_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr2 <- afrs_restricted
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
  
  points(plot_data2_mult_b$mean~plot_data2_mult_b$age,col=alpha(palette_b[k],0.75),pch=15)
  lines(plot_data2_mult_b$mean~plot_data2_mult_b$age,col=palette_b[k])
}


