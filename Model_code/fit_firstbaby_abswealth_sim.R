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
#install.packages("scales")
library(scales)

## Data simulation ----

#The script in this section is to create synthetic data that follows the causal relationship between absolute wealth and the probability of first reproduction.

#Population size
#100 individuals
N <- 100

#Age
#maximum age of 73 years old (based on Pimbwe data)
A <- 73

#Absolute wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
wealth <- matrix(nrow=N,ncol=A+1)
#randomly assign an amount of wealth for each individual at age 0 - based on the distribution of household assets in the data
wealth[,1] <- exp(rnorm(100,5,1))
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

#log-transform and standardise wealth data
std_wealth <- matrix(standardize(log(as.vector(wealth))),ncol=ncol(wealth),nrow=nrow(wealth))
#check the data
std_wealth

#simulate an age-specific parameter for wealth (beta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
beta_wealth<-c(rep(0,13),seq(from=-0.1,to=0.1,length=20),rep(0,41))
beta_wealth
plot(beta_wealth~c(1:length(beta_wealth)))
# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_beta_wealth<-beta_wealth/sd(as.vector(wealth))
std_beta_wealth
plot(std_beta_wealth~c(1:length(std_beta_wealth)))

#Age at first reproduction (AFR)

#simulate an age-specific parameter for AFR (mu)
mu_age<-c(rep(0,12),seq(from=0.001,to=0.2,length=7),seq(from=0.14,to=0.01,length=5),seq(from=0.01, to=0.001,length=15),rep(0,35))
mu_age
#check that they sum to 1
sum(mu_age)
#plot it!
plot(cumprod(1-mu_age)~c(1:length(mu_age)),ylim=c(0,1))

#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#assign first column a zero, because they cannot have their first baby when born
afrs[,1] <- 0
afrs
#randomly assign a positive output of AFR for individuals
for(j in 2:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(!is.na(afrs[i,j-1])){
      if(afrs[i,j-1] == 0){
        afr_prob <- mu_age[j]+ #age
        std_beta_wealth[j]*std_wealth[i,j] #wealth
        if(afr_prob<0){afr_prob<-0}
      afrs[i,j] <- rbinom(1,1,afr_prob)
      }else{
        afrs[i,j] <- NA
        }
      } else{
        afrs[i,j] <- NA
    }
  }
}

#check the data
#see the data
head(afrs)
#check the age-specific probability of FR
apply(afrs,2,sum,na.rm=T)
apply(afrs,2,sum,na.rm=T)/N
#plot it
#color palette
palette <- c(1,12,7,2,11,6,3,10,8,4,9,5)
#plot
plot(cumprod(1-apply(afrs,2,sum,na.rm=T)/N),
     ylim=c(0,1),
     xlab="Age",
     ylab="Probability of first reproduction",
     col=hcl.colors(length(palette),"temps")[palette[2]],
     pch=16) #data
lines(cumprod(1-apply(afrs,2,sum,na.rm=T)/N),col=hcl.colors(length(palette),"temps")[palette[2]],lwd=2)
points(cumprod(1-(mu_age+std_beta_wealth)),col=hcl.colors(length(palette),"temps")[palette[1]],pch=15) #mu+std_beta
lines(cumprod(1-(mu_age+std_beta_wealth)),col=hcl.colors(length(palette),"temps")[palette[1]],lwd=2) #mu+std_beta

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
apply(afrs,2,sum,na.rm=T)
#min
min(which(apply(afrs,2,sum,na.rm=T)>0))
#14
#max
max(which(apply(afrs,2,sum,na.rm=T)>0))
#38

std_wealth_restricted<-std_wealth[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]
afrs_restricted<-afrs[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]

## Fit simulated data, using the combined data imputation approach ----

#replace NAs with -99
for(j in 1:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(is.na(afrs[i,j])){
      afrs[i,j] <- -99
    } else{
      afrs[i,j] <- afrs[i,j]
    }
  }
}
#check the data
afrs

#put all the data together
#create data
data2 <- list(N = nrow(afrs), #population size
               A = ncol(afrs), #age
               wealth = as.vector(t(std_wealth)), #absolute wealth
               N_miss = sum((std_wealth)== -99), # number of missing values that need imputation
               id_wealth_miss = which(as.vector(t(std_wealth))== -99), # provide the indexes for the missing data
               baby = afrs #AFR
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
rstan::traceplot(rds2_add,pars="alpha")
#mu
#traceplot(rds2_add,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds2_add,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
rstan::traceplot(rds2_add,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds2_add,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds2_add,pars="mu_delta")
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

#simulate wealth values
simwealth_add <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth_add
#get the deciles
deciles <- as.numeric(quantile(simwealth_add,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-palette[1:length(deciles)] #darker lines = younger ages, lighter lines = older ages

#plot empty plot
par(mfrow=c(1,1))
plot(c(0,1)~c(0,ncol(post2_add$mu)),
     ylab="Prob. FR",
     xlab="Age",
     main="Model with absolute wealth",
     type="n")

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
  
  points(cumprod(1-plot_data2_add_b$mean)~plot_data2_add_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette[k]],0.75),pch=15)
  lines(cumprod(1-plot_data2_add_b$mean)~plot_data2_add_b$age,col=hcl.colors(length(palette),"temps")[palette[k]])
  polygon(c(plot_data2_add_b$age,rev(plot_data2_add_b$age)),c(cumprod(1-plot_data2_add_b$low),rev(cumprod(1-plot_data2_add_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette[k]],0.5),border=NA)
}
