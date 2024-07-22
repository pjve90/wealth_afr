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

## Data wrangling of real data ----

#Load data
real_data8 <- read.csv("dataf.csv")[,-1]
head(real_data8)

# Age at first reproduction 

#create a matrix to store the age-specific age of censor
afr_matrix8 <- matrix(nrow=nrow(real_data8),ncol=max(real_data8$aoc)+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix8)){
  afr <- real_data8$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data8$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix8[i,1:(afr-1)] <- 0
    afr_matrix8[i,afr] <- 1
  } else{
    afr_matrix8[i,1:aoc] <- rep(0,length(afr_matrix8[i,1:aoc]))
  }
}
#check the data
afr_matrix8
#check the age-specific probability of FR
apply(afr_matrix8,2,sum,na.rm=T)/apply(afr_matrix8,2,function(x)sum(!is.na(x)))
#plot it
plot(cumprod(1-apply(afr_matrix8,2,sum,na.rm=T)/apply(afr_matrix8,2,function(x)sum(!is.na(x))))~c(1:(max(real_data8$aoc)+1)),xlab="Age",ylab="Cumulative probability of first birth",ylim=c(0,1))

#replace NAs with -99
for(j in 1:ncol(afr_matrix8)){
  for(i in 1:nrow(afr_matrix8)){
    if(is.na(afr_matrix8[i,j])){
      afr_matrix8[i,j] <- -99
    } else{
      afr_matrix8[i,j] <- afr_matrix8[i,j]
    }
  }
}
#check the data
afr_matrix8

#Age-specific absolute wealth

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix8 <- matrix(nrow = nrow(real_data8),ncol=max(real_data8$aoc)+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix8)){
  absw <- real_data8$absw95[i]
  age_absw <- real_data8$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data8$aoc[i]+1)){
    absw_matrix8[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data8$aoc[i]+1)){
      absw_matrix8[i,(real_data8$aoc[i]+1)] <- NA
    } else{
      absw_matrix8[i,age_absw] <- NA
    }
}
#check data
absw_matrix8
#98
for(i in 1:nrow(absw_matrix8)){
  absw <- real_data8$absw98[i]
  age_absw <- real_data8$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data8$aoc[i]){
    absw_matrix8[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data8$aoc[i]+1)){
      absw_matrix8[i,(real_data8$aoc[i]+1)] <- NA
    } else{
      absw_matrix8[i,age_absw] <- NA
    }
}
#check data
absw_matrix8
#00
for(i in 1:nrow(absw_matrix8)){
  absw <- real_data8$absw00[i]
  age_absw <- real_data8$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data8$aoc[i]){
    absw_matrix8[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data8$aoc[i]+1)){
      absw_matrix8[i,(real_data8$aoc[i]+1)] <- NA
    } else{
      absw_matrix8[i,age_absw] <- NA
    }
}
#check data
absw_matrix8
#02
for(i in 1:nrow(absw_matrix8)){
  absw <- real_data8$absw02[i]
  age_absw <- real_data8$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data8$aoc[i]){
    absw_matrix8[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data8$aoc[i]+1)){
      absw_matrix8[i,(real_data8$aoc[i]+1)] <- NA
    } else{
      absw_matrix8[i,age_absw] <- NA
    }
}
#check data
absw_matrix8
#04
for(i in 1:nrow(absw_matrix8)){
  absw <- real_data8$absw04[i]
  age_absw <- real_data8$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data8$aoc[i]){
    absw_matrix8[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data8$aoc[i]+1)){
      absw_matrix8[i,(real_data8$aoc[i]+1)] <- NA
    } else{
      absw_matrix8[i,age_absw] <- NA
    }
}
#check data
absw_matrix8
#06
for(i in 1:nrow(absw_matrix8)){
  absw <- real_data8$absw06[i]
  age_absw <- real_data8$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data8$aoc[i]){
    absw_matrix8[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data8$aoc[i]+1)){
      absw_matrix8[i,(real_data8$aoc[i]+1)] <- NA
    } else{
      absw_matrix8[i,age_absw] <- NA
    }
}
#check data
absw_matrix8
#10
for(i in 1:nrow(absw_matrix8)){
  absw <- real_data8$absw10[i]
  age_absw <- real_data8$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data8$aoc[i]+1)){
    absw_matrix8[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data8$aoc[i]+1)){
      absw_matrix8[i,(real_data8$aoc[i]+1)] <- NA
    } else{
      absw_matrix8[i,age_absw] <- NA
    }
}
#check data
absw_matrix8
#check the age-specific average of absolute wealth
apply(absw_matrix8,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix8,2,mean,na.rm=T)~c(1:ncol(absw_matrix8)),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix8 <- matrix(standardize(log(as.vector(absw_matrix8))),ncol=ncol(absw_matrix8),nrow=nrow(absw_matrix8))
#check the data
std_absw_matrix8
#check the age-specific average of absolute wealth
apply(std_absw_matrix8,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix8,2,mean,na.rm=T)~c(1:(max(real_data8$aoc)+1)),xlab="Age",ylab="Average absolute wealth")

#change NAs for -99
#replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
for(j in 1:ncol(std_absw_matrix8)){
  for(i in 1:nrow(std_absw_matrix8)){
    if(is.na(std_absw_matrix8[i,j])){
      std_absw_matrix8[i,j] <- -99
    }
  }
}
#check the data
std_absw_matrix8

## Fit real data ----

# Only take the years when individuals have a first baby
#check min and max ages at first reproduction
#min
min(real_data8$afr,na.rm=T)
#13
#max
max(real_data8$afr,na.rm=T)
#32

std_wealth_restricted <- std_absw_matrix8[,round(min(real_data8$afr,na.rm=T)):round(max(real_data8$afr,na.rm=T))+1]
afrs_restricted <- afr_matrix8[,round(min(real_data8$afr,na.rm=T)):round(max(real_data8$afr,na.rm=T))+1]

#put all the data together
#create dataset
real_list8 <- list(N = nrow(real_data8), #population size
                   A = ncol(afr_matrix8), #age
                   wealth = as.vector(t(std_absw_matrix8)), #absolute wealth
                   baby = afr_matrix8, #AFR
                   N_miss = sum((std_absw_matrix8)== -99), # number of missing values that need imputation
                   id_wealth_miss =which(as.vector(t(std_absw_matrix8))== -99)) # provide the indexes for the missing data
#check data
real_list8

# compile model

m8_add <- cmdstan_model("Model_code/firstbaby_diffwealth_additive.stan")

#fit model
fit8_add_real <- m8_add$sample(data = real_list8, 
                               chains = 4, 
                               parallel_chains = 15, 
                               adapt_delta = 0.95,
                               max_treedepth = 13,
                               init = 0)


# save fit 
fit_8_add_real <- rstan::read_stan_csv(fit8_add_real$output_files())
saveRDS(fit_8_add_real, "firstbaby3_add_real.rds")
#load RDS file
rds8_add_real <- readRDS("firstbaby3_add_real.rds")
#extract samples
post8_add_real <- extract.samples(rds8_add_real)

#check the model
#check trace of all main parameters
#alpha
rstan::traceplot(rds8_add_real,pars="alpha")
#mu
#traceplot(rds8_add_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds8_add_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
rstan::traceplot(rds8_add_real,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds8_add_real,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds8_add_real,pars="mu_delta")
#gamma_wealth
#traceplot(rds8_add_real,pars="gamma_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds8_add_real,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab8_add_real <- precis(rds8_add_real,depth=2,pars=c("alpha",
                                                     "mu_raw",
                                                     "mu_tau",
                                                     "mu_delta"))
#check table
tab8_add_real
#create summary table for mu
tab8_add_real_mu <- precis(rds8_add_real,depth=2,pars="mu")
#check table
tab8_add_real_mu
plot(tab8_mu_add_real)
plot(cumprod(1-tab8_mu_add_real[,1]),ylim=c(0,1))
#create summary table for gamma
tab8_add_real_gamma <- precis(rds8_add_real,depth=2,pars="gamma_wealth")
#check table
tab8_add_real_gamma
plot(tab8_gamma_add_real)
plot(cumprod(1-tab8_gamma_add_real[,1]),ylim=c(0,1))

# # To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
# #mu
# tab8_add_real_mu[,1]<-round(inv_logit(tab8_add_real_mu[,1]),3)
# tab8_add_real_mu[,3]<-round(inv_logit(tab8_add_real_mu[,3]),3)
# tab8_add_real_mu[,4]<-round(inv_logit(tab8_add_real_mu[,4]),3)
# #gamma_wealth
# tab8_add_real_gamma[,1]<-round(inv_logit(tab8_add_real_gamma[,1]),3)
# tab8_add_real_gamma[,3]<-round(inv_logit(tab8_add_real_gamma[,3]),3)
# tab8_add_real_gamma[,4]<-round(inv_logit(tab8_add_real_gamma[,4]),3)

## Plot the fit of the real data ----

### Wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post8_add_real$wealth_full),1),to=round(max(post8_add_real$wealth_full),1),length.out=nrow(std_absw_matrix8)) #specify according to range and length related to sample size
simwealth_add_real
#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real,seq(0,1,0.5)))
deciles

#colour palette
palette <- c(1,3,6,2,4,5)
#select the numbers for color palette
palette_b<-palette[(length(deciles)+1):(length(deciles)+3)]

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,8))

#plot empty plot
plot(c(0,1)~c(0,ncol(post8_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Model with absolute change (1 year)",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(77.5,1,c("No change","Mid. change", "Max. change"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p8_add_real_b <- matrix(nrow=nrow(post8_add_real$mu),ncol=ncol(post8_add_real$mu))
  p8_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post8_add_real$mu)){
    for(i in 1:nrow(post8_add_real$mu)){
      p8_add_real_b[i,j] <- inv_logit(post8_add_real$alpha[i] + #inv logit because originally is logit
                                        post8_add_real$mu[i,j] + #age
                                        post8_add_real$delta_wealth[i,j]*deciles[k]) #wealth
    }
  }
  #check data
  p8_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data8_add_real_b <- data.frame(age = 1:ncol(p8_add_real_b),
                                      mean = apply(p8_add_real_b, 2, mean), 
                                      upp = apply(p8_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p8_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr8 <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr8)){
    for(i in 1:nrow(plot_afr8)){
      if(plot_afr8[i,j]==-99){
        plot_afr8[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr8
  
  points(cumprod(1-plot_data8_add_real_b$mean)~plot_data8_add_real_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]],pch=15)
  lines(cumprod(1-plot_data8_add_real_b$mean)~plot_data8_add_real_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]])
  polygon(c(plot_data8_add_real_b$age,rev(plot_data8_add_real_b$age)),c(cumprod(1-plot_data8_add_real_b$low),rev(cumprod(1-plot_data8_add_real_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[k]],0.5),border=NA)
}

## De-couple plot by min, mean, max ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post8_add_real$wealth_full),1),to=round(max(post8_add_real$wealth_full),1),length.out=nrow(std_absw_matrix8)) #specify according to range and length related to sample size
simwealth_add_real

#get the deciles
deciles <- as.numeric(quantile(simwealth_add_real,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-palette[1:length(deciles)] #darker lines = younger ages, lighter lines = older ages

#define layout of plots
par(mfrow=c(1,3),xpd=T,mar=c(5,5,4,8))

#### Minimum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post8_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="No change",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p8_add_real_0_b <- matrix(nrow=nrow(post8_add_real$mu),ncol=ncol(post8_add_real$mu))
p8_add_real_0_b
#fill it in with values for age 25
for(j in 1:ncol(post8_add_real$mu)){
  for(i in 1:nrow(post8_add_real$mu)){
    p8_add_real_0_b[i,j] <- inv_logit(post8_add_real$alpha[i] + #inv logit because originally is logit
                                        post8_add_real$mu[i,j] + #age
                                        post8_add_real$delta_wealth[i,j]*deciles[1]) #wealth
  }
}
#check data
p8_add_real_0_b
#plot it!
#prepare model prediction data
plot_data8_add_real_0_b <- data.frame(age = 1:ncol(p8_add_real_0_b),
                                      mean = apply(p8_add_real_0_b, 2, mean), 
                                      upp = apply(p8_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p8_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr8 <- afr_matrix8
#change -99 to NAs
for(j in 1:ncol(plot_afr8)){
  for(i in 1:nrow(plot_afr8)){
    if(plot_afr8[i,j]==-99){
      plot_afr8[i,j] <- NA
    }
  }
}
#check the data
plot_afr8

points(cumprod(1-plot_data8_add_real_0_b$mean)~plot_data8_add_real_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),pch=15)
lines(cumprod(1-plot_data8_add_real_0_b$mean)~plot_data8_add_real_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),lwd=2)
polygon(c(plot_data8_add_real_0_b$age,rev(plot_data8_add_real_0_b$age)),c(cumprod(1-plot_data8_add_real_0_b$low),rev(cumprod(1-plot_data8_add_real_0_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.5),border=NA)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post8_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Mid. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p8_add_real_50_b <- matrix(nrow=nrow(post8_add_real$mu),ncol=ncol(post8_add_real$mu))
p8_add_real_50_b
#fill it in with values for age 25
for(j in 1:ncol(post8_add_real$mu)){
  for(i in 1:nrow(post8_add_real$mu)){
    p8_add_real_50_b[i,j] <- inv_logit(post8_add_real$alpha[i] + #inv logit because originally is logit
                                         post8_add_real$mu[i,j] + #age
                                         post8_add_real$delta_wealth[i,j]*deciles[2]) #wealth
  }
}
#check data
p8_add_real_50_b
#plot it!
#prepare model prediction data
plot_data8_add_real_50_b <- data.frame(age = 1:ncol(p8_add_real_50_b),
                                       mean = apply(p8_add_real_50_b, 2, mean), 
                                       upp = apply(p8_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p8_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr8 <- afr_matrix8
#change -99 to NAs
for(j in 1:ncol(plot_afr8)){
  for(i in 1:nrow(plot_afr8)){
    if(plot_afr8[i,j]==-99){
      plot_afr8[i,j] <- NA
    }
  }
}
#check the data
plot_afr8

points(cumprod(1-plot_data8_add_real_50_b$mean)~plot_data8_add_real_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),pch=15)
lines(cumprod(1-plot_data8_add_real_50_b$mean)~plot_data8_add_real_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),lwd=2)
polygon(c(plot_data8_add_real_50_b$age,rev(plot_data8_add_real_50_b$age)),c(cumprod(1-plot_data8_add_real_50_b$low),rev(cumprod(1-plot_data8_add_real_50_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.5),border=NA)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post8_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Max. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p8_add_real_100_b <- matrix(nrow=nrow(post8_add_real$mu),ncol=ncol(post8_add_real$mu))
p8_add_real_100_b
#fill it in with values for age 25
for(j in 1:ncol(post8_add_real$mu)){
  for(i in 1:nrow(post8_add_real$mu)){
    p8_add_real_100_b[i,j] <- inv_logit(post8_add_real$alpha[i] + #inv logit because originally is logit
                                          post8_add_real$mu[i,j] + #age
                                          post8_add_real$delta_wealth[i,j]*deciles[3]) #wealth
  }
}
#check data
p8_add_real_100_b
#plot it!
#prepare model prediction data
plot_data8_add_real_100_b <- data.frame(age = 1:ncol(p8_add_real_100_b),
                                        mean = apply(p8_add_real_100_b, 2, mean), 
                                        upp = apply(p8_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                        low = apply(p8_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr8 <- afr_matrix8
#change -99 to NAs
for(j in 1:ncol(plot_afr8)){
  for(i in 1:nrow(plot_afr8)){
    if(plot_afr8[i,j]==-99){
      plot_afr8[i,j] <- NA
    }
  }
}
#check the data
plot_afr8

points(cumprod(1-plot_data8_add_real_100_b$mean)~plot_data8_add_real_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),pch=15)
lines(cumprod(1-plot_data8_add_real_100_b$mean)~plot_data8_add_real_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),lwd=2)
polygon(c(plot_data8_add_real_100_b$age,rev(plot_data8_add_real_100_b$age)),c(cumprod(1-plot_data8_add_real_100_b$low),rev(cumprod(1-plot_data8_add_real_100_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.5),border=NA)

legend(77.5,1,c("No change","Mid. change", "Max. change"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)

