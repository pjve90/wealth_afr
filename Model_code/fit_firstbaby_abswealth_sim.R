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

id_wealth_miss <- matrix(nrow=nrow(std_wealth_restricted),ncol=ncol(std_wealth_restricted))

for (i in 1:nrow(std_wealth_restricted)){
  for(j in 1:ncol(std_wealth_restricted)){
    if(std_wealth_restricted[i,j] == -99){
      id_wealth_miss[i,j] <- 1
    }else{
      id_wealth_miss[i,j] <- 0
    }
  }
}

id_wealth_miss

## Fit simulated data, using the model in which wealth is transformed into a vector to use the merge function ----

#put all the data together
#create data
data2a <- list(N = nrow(afrs_restricted), #population size
             A = ncol(afrs_restricted), #age
             wealth = as.vector(t(std_wealth_restricted)), #absolute wealth
             baby = afrs_restricted, #AFR
             miss = sum((std_wealth_restricted)== -99), # number of missing values that need imputation
             wealth_m=which(as.vector(t(std_wealth_restricted))== -99)) # provide the indexes for the missing data

#check data 
data2a

# compile model

m2_simv <- cmdstan_model("Model_code/firstbaby_abswealth_vector.stan")

# fit model

fit2_simv <- m2_simv$sample(data = data2a, 
                  chains = 4, 
                  parallel_chains = 15, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)

# save fit 
fit2_simv <- rstan::read_stan_csv(fit2_simv$output_files())
saveRDS(fit2_simv, "firstbaby2_simv.rds")
#load RDS file
rds2_simv <- readRDS("firstbaby2_simv.rds")
#extract samples
post2_simv <- extract.samples(rds2_simv)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds2_simv,pars="alpha")
#mu
#traceplot(rds2_simv,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds2_simv,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds2_simv,pars="mu_tau")
#mu_kappa
traceplot(rds2_simv,pars="mu_kappa")
#mu_delta
traceplot(rds2_simv,pars="mu_delta")
#beta_wealth
#traceplot(rds2_simv,pars="beta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tabs2_simv <- precis(rds2_simv,depth=3,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tabs2_simv
#create summary table for mu
tabs2_simv_mu <- precis(rds2_simv,depth=3,pars="mu")
#check table
tabs2_simv_mu
plot(inv_logit(tabs2_simv_mu[,1]))
#create summary table for beta
tabs2_simv_beta <- precis(rds2_simv,depth=3,pars="beta_wealth")
#check table
tabs2_simv_beta
plot((tabs2_simv_beta[,1]))

## Plot the fit of the simulated data ----

#simulate wealth values
simwealth_simv <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_simv

#colour palette
palette<-hcl.colors(ncol(post2_simv$mu),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(min(std_wealth[which(std_wealth > -99)]),max(std_wealth[which(std_wealth > -99)])),
     ylab="Prob. FR",
     xlab="Absolute wealth",
     main="Model with absolute wealth",
     type="n")

#add lines
for(k in 1:ncol(post2_simv$mu)){
  #create matrix to store the data
  p2_simv <- matrix(nrow=nrow(post2_simv$mu),ncol=length(simwealth_simv))
  p2_simv
  #fill it in with values for age 25
  for(j in 1:length(simwealth_simv)){
    for(i in 1:nrow(post2_simv$mu)){
      p2_simv[i,j] <- inv_logit(post2_simv$alpha[i] + #inv logit because originally is logit
                                  post2_simv$mu[i,k] + #age
                                  post2_simv$beta_wealth[i,k]*simwealth_simv[j]) #wealth
    }
  }
  #check data
  p2_simv
  #plot it!
  #prepare model prediction data
  plot_data2_simv <- data.frame(wealth = simwealth_simv,
                                mean = apply(p2_simv, 2, mean), 
                                upp = apply(p2_simv, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p2_simv, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from simv data
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
  
  lines(plot_data2_simv$mean~plot_data2_simv$wealth,col=palette[k])
}

### Wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_simv <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_simv
#get the deciles
deciles <- as.numeric(quantile(simwealth_simv,seq(0,1,0.1)))
deciles

#colour palette
palette_b<-hcl.colors(length(deciles),"ag_sunset") #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,0.5)~c(0,ncol(post2_simv$mu)),
     ylab="Prob. FR",
     xlab="Age",
     xaxt="n",
     main="Model with absolute wealth",
     type="n")
axis(1,at=seq(0,ncol(post2_simv$mu),by=1),labels=14:40)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p2_simv_b <- matrix(nrow=nrow(post2_simv$mu),ncol=ncol(post2_simv$mu))
  p2_simv_b
  #fill it in with values for age 25
  for(j in 1:ncol(post2_simv$mu)){
    for(i in 1:nrow(post2_simv$mu)){
      p2_simv_b[i,j] <- inv_logit(post2_simv$alpha[i] + #inv logit because originally is logit
                                    post2_simv$mu[i,j] + #age
                                    post2_simv$beta_wealth[i,j]*deciles[k]) #wealth
    }
  }
  #check data
  p2_simv_b
  #plot it!
  #prepare model prediction data
  plot_data2_simv_b <- data.frame(age = 1:ncol(p2_simv_b),
                                  mean = apply(p2_simv_b, 2, mean), 
                                  upp = apply(p2_simv_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p2_simv_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
  
  points(plot_data2_simv_b$mean~plot_data2_simv_b$age,col=alpha(palette_b[k],0.75),pch=15)
}


Pabl## Fit simulated data, using the model in which missing values in wealth are imputed with an ifelse statement ----

#put all the data together
#create data
data2b <- list(N = nrow(afrs_restricted), #population size
             A = ncol(afrs_restricted), #age
             wealth = std_wealth_restricted, #absolute wealth
             baby = afrs_restricted) #AFR
#check data
data2b

# compile model

m2_simif <- cmdstan_model("Model_code/firstbaby_abswealth_ifelse.stan")

# fit model

fit2_simif <- m2_simif$sample(data = data2b, 
                  chains = 4, 
                  parallel_chains = 10, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)

# save fit 
fit2_simif <- rstan::read_stan_csv(fit2_simif$output_files())
saveRDS(fit2_simif, "firstbaby2_simif.rds")
#load RDS file
rds2_simif <- readRDS("firstbaby2_simif.rds")
#extract samples
post2_simif <- extract.samples(rds2_simif)


#check the model
#check trace of all main parameters
#alpha
traceplot(rds2_simif,pars="alpha")
#mu
#traceplot(rds2_simif,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds2_simif,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds2_simif,pars="mu_tau")
#mu_kappa
traceplot(rds2_simif,pars="mu_kappa")
#mu_delta
traceplot(rds2_simif,pars="mu_delta")
#beta_wealth
#traceplot(rds2_simif,pars="beta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tabs2_simif <- precis(rds2_simif,depth=3,pars=c("alpha",
                                              "mu_raw",
                                              "mu_tau",
                                              "mu_delta"))
#check table
tabs2_simif
#create summary table for mu
tabs2_simif_mu <- precis(rds2_simif,depth=3,pars="mu")
#check table
tabs2_simif_mu
plot(inv_logit(tabs2_simif_mu[,1]))
#create summary table for beta
tabs2_simif_beta <- precis(rds2_simif,depth=3,pars="beta_wealth")
#check table
tabs2_simif_beta
plot((tabs2_simif_beta[,1]))



########################################################################################
######## Alternative approach
# Wealth does not add/reduce probability, but modifies age-specific probabilities
# multiplicative interaction instead of additive


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
      afr_prob <- mu_age[j]* #age
        exp(1)^(std_beta_wealth[j]*std_wealth[i,j]) #wealth
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
plot(c(-0.10,0.25)~c(min(std_wealth),max(std_wealth)),
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


## Fit simulated data, using the model in which missing values in wealth are imputed with an ifelse statement ----

#put all the data together
#create data
data2b <- list(N = nrow(afrs_restricted), #population size
               A = ncol(afrs_restricted), #age
               wealth = as.matrix(std_wealth_restricted), #absolute wealth
               baby = afrs_restricted) #AFR
#check data
data2b

# compile model

m2_simmult <- cmdstan_model("Model_code/firstbaby_abswealth_multiplicative.stan")

# fit model

fit2_simmult <- m2_simmult$sample(data = data2b, 
                              chains = 4, 
                              parallel_chains = 10, 
                              adapt_delta = 0.95,
                              max_treedepth = 13,
                              init = 0)

# save fit 
fit2_simmult <- rstan::read_stan_csv(fit2_simmult$output_files())
saveRDS(fit2_simmult, "firstbaby2_simif.rds")
#load RDS file
rds2_simmult <- readRDS("firstbaby2_simmult.rds")
#extract samples
post2_simmult <- extract.samples(rds2_simmult)


#check the model
#check trace of all main parameters
#alpha
traceplot(post2_simmult,pars="alpha")
#mu
#traceplot(post2_simmult,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(post2_simmult,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(post2_simmult,pars="mu_tau")
#mu_kappa
traceplot(post2_simmult,pars="mu_kappa")
#mu_delta
traceplot(post2_simmult,pars="mu_delta")
#beta_wealth
#traceplot(post2_simmult,pars="beta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tabs2_simmult <- precis(post2_simmult,depth=3,pars=c("alpha",
                                              "mu_raw",
                                              "mu_tau",
                                              "mu_delta"))
#check table
tabs2_simmult
#create summary table for mu
tabs2_simmult_mu <- precis(rds2_simmult,depth=3,pars="mu")
#check table
tabs2_simif_mu
plot(inv_logit(tabs2_simmult_mu[,1]))
#create summary table for beta
tabs2_simmult_beta <- precis(rds2_simmult,depth=3,pars="beta_wealth")
#check table
tabs2_simmult_beta
plot((tabs2_simmult_beta[,1]))


