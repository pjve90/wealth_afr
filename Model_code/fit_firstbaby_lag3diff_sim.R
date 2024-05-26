# Model with lagged effect of absolute wealth change (3 year) ----

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
#500 individuals
N <- 500

#Age
#maximum age of 73 years old (based on Pimbwe data)
A <- 73

#Absolute wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
abswealth <- matrix(nrow=N,ncol=A+1)
#randomly assign an amount of wealth for each individual at age 0 - based on the distribution of household assets in the data
abswealth[,1] <- exp(rnorm(100,5,1))
#simulate wealth of individuals through time, based on previous absolute wealth
for(j in 2:ncol(abswealth)){
  for(i in 1:nrow(abswealth)){
    abswealth[i,j] <- abswealth[i,j-1]+rnorm(1,0,0.25)
  }
}
#check the data
#see the data
head(wealth)
#check the age-specific absolute wealth
apply(abswealth,2,mean)
#plot it
plot(apply(abswealth,2,mean),xlab="Age",ylab="Average absolute wealth")

#log-transform and standardise wealth data
std_abswealth <- matrix(standardize(log(as.vector(abswealth))),ncol=ncol(abswealth),nrow=nrow(abswealth))
#check the data
std_abswealth

#simulate an age-specific parameter for wealth (beta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
beta_wealth<-c(rep(0,12),seq(from=-0.1,to=0.09,length=19),seq(from=0.09,to=0.1,length=11),rep(0,32))
beta_wealth
plot(beta_wealth~c(1:length(beta_wealth)))
# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_beta_wealth<-beta_wealth/sd(as.vector(abswealth))
std_beta_wealth
plot(std_beta_wealth~c(1:length(std_beta_wealth)))

#Lagged difference of wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rows and ages as columns (A+1) so the first column is birth)
diffwealth <- matrix(nrow=N,ncol=A+1)
#calculate the difference of wealth from one year to the next
for(j in 1:ncol(diffwealth)){
  for(i in 1:nrow(diffwealth)){
    if(j ==1 | j ==2  | j ==3  | j ==4){
      diffwealth[i,j] <- abswealth[i,j] - abswealth[i,j]
    } else{
      diffwealth[i,j] <- abswealth[i,j] - abswealth[i,j-4]
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
#check the age-specific absolute wealth variability
apply(abs(diffwealth),2,mean)
#plot it
plot(apply(abs(diffwealth),2,mean),xlab="Age",ylab="Average wealth variability")

#standardise wealth variability
std_diffwealth <- matrix(standardize(log(abs(as.vector(diffwealth))+1)),ncol=ncol(diffwealth),nrow=nrow(diffwealth))
#check the data
std_diffwealth

#simulate an age-specific parameter for wealth variability
#if seq starts from a positive value and goes to a negative value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
iota_wealth<-c(rep(0,12),seq(from=0.1,to=-0.09,length=19),seq(from=-0.09,to=-0.1,length=11),rep(0,32))
iota_wealth
plot(iota_wealth~c(1:length(iota_wealth)))
# adjust for the fact that eta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_iota_wealth<-iota_wealth/sd(as.vector(abswealth))
std_iota_wealth
plot(std_iota_wealth~c(1:length(std_iota_wealth)))

#Age at first reproduction (AFR)

#simulate an age-specific parameter for AFR (mu)
mu_age<-c(rep(0,12),seq(from=0.001,to=0.3,length=9),seq(from=0.14,to=0.01,length=11),seq(from=0.01, to=0.001,length=10),rep(0,32))
length(mu_age)
mu_age
#plot it!
plot(mu_age~c(1:length(mu_age)),ylim=c(0,1))
plot(cumprod(1-mu_age)~c(1:length(mu_age)),ylim=c(0,1))

#simulate binary ouput of AFR for each age
#0=no first birth
#1=yes first birth
#create a matrix with individuals as rows and ages as columns (A+1 so the first column is birth)
afrs <- matrix(nrow=N,ncol=A+1)
#assign first column a zero, because they cannot have their first baby when born
afrs[,1:4] <- 0
afrs
#randomly assign a positive output of AFR for individuals
for(j in 5:ncol(afrs)){
  for(i in 1:nrow(afrs)){
    if(!is.na(afrs[i,j-1])){
      if(afrs[i,j-1] == 0){
        afr_prob <- mu_age[j]+ #age
          std_iota_wealth[j]*std_diffwealth[i,j-4] #lagged absolute wealth change by 2 years
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
apply(afrs,2,sum,na.rm = T)/apply(afrs,2,function(x)sum(!is.na(x)))
#plot it
#color palette
palette <- c(1,6,3,2,4,5)
#plot
plot(cumprod(1-apply(afrs,2,sum,na.rm = T)/apply(afrs,2,function(x)sum(!is.na(x)))),
     ylim=c(0,1),
     xlab="Age",
     ylab="Cumulative probability of first birth",
     col=hcl.colors(length(palette),"temps")[palette[2]],
     pch=16) #data
lines(cumprod(1-apply(afrs,2,sum,na.rm = T)/apply(afrs,2,function(x)sum(!is.na(x)))),col=hcl.colors(length(palette),"temps")[palette[2]],lwd=2)
points(cumprod(1-(mu_age+std_iota_wealth)),col=hcl.colors(length(palette),"temps")[palette[1]],pch=15) #mu+std_beta
lines(cumprod(1-(mu_age+std_iota_wealth)),col=hcl.colors(length(palette),"temps")[palette[1]],lwd=2) #mu+std_beta

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
apply(afrs,2,sum,na.rm=T)
#min
min(which(apply(afrs,2,sum,na.rm=T)>0))
#14
#max
max(which(apply(afrs,2,sum,na.rm=T)>0))
#38

std_abswealth_restricted<-std_abswealth[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]
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
data10 <- list(N = nrow(afrs), #population size
              A = ncol(afrs), #age
              wealth = as.vector(t(std_abswealth)), #absolute wealth
              N_miss = sum((std_abswealth)== -99), # number of missing values that need imputation
              id_wealth_miss = which(as.vector(t(std_abswealth))== -99), # provide the indexes for the missing data
              baby = afrs #AFR
) 

#check data 
data10

# compile model

m10_add <- cmdstan_model("Model_code/firstbaby_lag3diff.stan")

# fit model

fit10_add <- m10_add$sample(data = data10, 
                          chains = 4, 
                          parallel_chains = 15, 
                          adapt_delta = 0.95,
                          max_treedepth = 13,
                          init = 0)

# save fit 
fit10_add <- rstan::read_stan_csv(fit10_add$output_files())
saveRDS(fit10_add, "firstbaby10_add.rds")
#load RDS file
rds10_add <- readRDS("firstbaby10_add.rds")
#extract samples
post10_add <- extract.samples(rds10_add)

#check the model
#check trace of all main parameters
#alpha
rstan::traceplot(rds10_add,pars="alpha")
#mu
#traceplot(rds10_add,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds10_add,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
rstan::traceplot(rds10_add,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds10_add,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds10_add,pars="mu_delta")
#biota_wealth
#traceplot(rds10_add,pars="biota_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tabs10_add <- precis(rds10_add,depth=3,pars=c("alpha",
                                            "mu_raw",
                                            "mu_tau",
                                            "mu_delta"))
#check table
tabs10_add
#create summary table for mu
tabs10_add_mu <- precis(rds10_add,depth=3,pars="mu")
#check table
tabs10_add_mu
plot(tabs10_add_mu)
plot(cumprod(1-inv_logit(tabs10_add_mu[,1])),ylim=c(0,1))
#create summary table for beta
tabs10_add_iota <- precis(rds10_add,depth=3,pars="iota_wealth")
#check table
tabs10_add_iota
plot(tabs10_add_iota)
plot(cumprod(1-inv_logit(tabs10_add_iota[,1])),ylim=c(0,1))

## Plot the fit of the simulated data ----

#### All wealth classes ----

#simulate wealth values
simwealth_add <- seq(from=round(min(std_abswealth[which(std_abswealth > -99)]),1),to=round(max(std_abswealth[which(std_abswealth > -99)]),1),length.out=nrow(std_abswealth)) #specify according to range and length of wealth data
simwealth_add
#get the deciles
deciles <- as.numeric(quantile(simwealth_add,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-palette[1:length(deciles)] #darker lines = younger ages, lighter lines = older ages

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,8))

#plot empty plot
plot(c(0,1)~c(0,ncol(post10_add$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Model with absolute wealth",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(77.5,1,c("Poor","Middle","Rich"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p10_add_b <- matrix(nrow=nrow(post10_add$mu),ncol=ncol(post10_add$mu))
  p10_add_b
  #fill it in with values for age 25
  for(j in 1:ncol(post10_add$mu)){
    for(i in 1:nrow(post10_add$mu)){
      p10_add_b[i,j] <- inv_logit(post10_add$alpha[i] + #inv logit because originally is logit
                                   post10_add$mu[i,j] + #age
                                   post10_add$iota_wealth[i,j]*deciles[k]) #wealth
    }
  }
  #check data
  p10_add_b
  #plot it!
  #prepare model prediction data
  plot_data10_add_b <- data.frame(age = 1:ncol(p10_add_b),
                                 mean = apply(p10_add_b, 2, mean), 
                                 upp = apply(p10_add_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p10_add_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr10 <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr10)){
    for(i in 1:nrow(plot_afr10)){
      if(plot_afr10[i,j]==-99){
        plot_afr10[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr10
  
  points(cumprod(1-plot_data10_add_b$mean)~plot_data10_add_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]],pch=15)
  lines(cumprod(1-plot_data10_add_b$mean)~plot_data10_add_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]])
  polygon(c(plot_data10_add_b$age,rev(plot_data10_add_b$age)),c(cumprod(1-plot_data10_add_b$low),rev(cumprod(1-plot_data10_add_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[k]],0.5),border=NA)
}

## De-couple plot by min, mean, max ----

#simulate wealth values
simwealth_add <- seq(from=round(min(post10_add$wealth_full),1),to=round(max(post10_add$wealth_full),1),length.out=nrow(std_abswealth)) #specify according to range and length related to sample size
simwealth_add

#get the deciles
deciles <- as.numeric(quantile(simwealth_add,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-palette[1:length(deciles)] #darker lines = younger ages, lighter lines = older ages

#define layout of plots
par(mfrow=c(1,3),xpd=T,mar=c(5,5,4,8))

#### Minimum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post10_add$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Poor",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p10_add_0_b <- matrix(nrow=nrow(post10_add$mu),ncol=ncol(post10_add$mu))
p10_add_0_b
#fill it in with values for age 25
for(j in 1:ncol(post10_add$mu)){
  for(i in 1:nrow(post10_add$mu)){
    p10_add_0_b[i,j] <- inv_logit(post10_add$alpha[i] + #inv logit because originally is logit
                                   post10_add$mu[i,j] + #age
                                   post10_add$iota_wealth[i,j]*deciles[1]) #wealth
  }
}
#check data
p10_add_0_b
#plot it!
#prepare model prediction data
plot_data10_add_0_b <- data.frame(age = 1:ncol(p10_add_0_b),
                                 mean = apply(p10_add_0_b, 2, mean), 
                                 upp = apply(p10_add_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p10_add_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr10 <- afrs
#change -99 to NAs
for(j in 1:ncol(plot_afr10)){
  for(i in 1:nrow(plot_afr10)){
    if(plot_afr10[i,j]==-99){
      plot_afr10[i,j] <- NA
    }
  }
}
#check the data
plot_afr10

points(cumprod(1-plot_data10_add_0_b$mean)~plot_data10_add_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),pch=15)
lines(cumprod(1-plot_data10_add_0_b$mean)~plot_data10_add_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),lwd=2)
polygon(c(plot_data10_add_0_b$age,rev(plot_data10_add_0_b$age)),c(cumprod(1-plot_data10_add_0_b$low),rev(cumprod(1-plot_data10_add_0_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.5),border=NA)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post10_add$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Medium",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p10_add_50_b <- matrix(nrow=nrow(post10_add$mu),ncol=ncol(post10_add$mu))
p10_add_50_b
#fill it in with values for age 25
for(j in 1:ncol(post10_add$mu)){
  for(i in 1:nrow(post10_add$mu)){
    p10_add_50_b[i,j] <- inv_logit(post10_add$alpha[i] + #inv logit because originally is logit
                                    post10_add$mu[i,j] + #age
                                    post10_add$iota_wealth[i,j]*deciles[2]) #wealth
  }
}
#check data
p10_add_50_b
#plot it!
#prepare model prediction data
plot_data10_add_50_b <- data.frame(age = 1:ncol(p10_add_50_b),
                                  mean = apply(p10_add_50_b, 2, mean), 
                                  upp = apply(p10_add_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p10_add_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr10 <- afrs
#change -99 to NAs
for(j in 1:ncol(plot_afr10)){
  for(i in 1:nrow(plot_afr10)){
    if(plot_afr10[i,j]==-99){
      plot_afr10[i,j] <- NA
    }
  }
}
#check the data
plot_afr10

points(cumprod(1-plot_data10_add_50_b$mean)~plot_data10_add_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),pch=15)
lines(cumprod(1-plot_data10_add_50_b$mean)~plot_data10_add_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),lwd=2)
polygon(c(plot_data10_add_50_b$age,rev(plot_data10_add_50_b$age)),c(cumprod(1-plot_data10_add_50_b$low),rev(cumprod(1-plot_data10_add_50_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.5),border=NA)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post10_add$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Rich",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p10_add_100_b <- matrix(nrow=nrow(post10_add$mu),ncol=ncol(post10_add$mu))
p10_add_100_b
#fill it in with values for age 25
for(j in 1:ncol(post10_add$mu)){
  for(i in 1:nrow(post10_add$mu)){
    p10_add_100_b[i,j] <- inv_logit(post10_add$alpha[i] + #inv logit because originally is logit
                                     post10_add$mu[i,j] + #age
                                     post10_add$iota_wealth[i,j]*deciles[3]) #wealth
  }
}
#check data
p10_add_100_b
#plot it!
#prepare model prediction data
plot_data10_add_100_b <- data.frame(age = 1:ncol(p10_add_100_b),
                                   mean = apply(p10_add_100_b, 2, mean), 
                                   upp = apply(p10_add_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                   low = apply(p10_add_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr10 <- afrs
#change -99 to NAs
for(j in 1:ncol(plot_afr10)){
  for(i in 1:nrow(plot_afr10)){
    if(plot_afr10[i,j]==-99){
      plot_afr10[i,j] <- NA
    }
  }
}
#check the data
plot_afr10

points(cumprod(1-plot_data10_add_100_b$mean)~plot_data10_add_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),pch=15)
lines(cumprod(1-plot_data10_add_100_b$mean)~plot_data10_add_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),lwd=2)
polygon(c(plot_data10_add_100_b$age,rev(plot_data10_add_100_b$age)),c(cumprod(1-plot_data10_add_100_b$low),rev(cumprod(1-plot_data10_add_100_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.5),border=NA)

legend(77.5,1,c("Poor","Middle","Rich"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)
