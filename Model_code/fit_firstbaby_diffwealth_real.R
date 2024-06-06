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
real_data3 <- read.csv("dataf.csv")[,-1]
head(real_data3)

# Age at first reproduction ----

#create a matrix to store the age-specific age of censor
afr_matrix3 <- matrix(nrow=nrow(real_data3),ncol=max(real_data3$aoc)+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix3)){
  afr <- real_data3$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data3$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix3[i,1:(afr-1)] <- 0
    afr_matrix3[i,afr] <- 1
  } else{
    afr_matrix3[i,1:aoc] <- rep(0,length(afr_matrix3[i,1:aoc]))
  }
}
#check the data
afr_matrix3
#check the age-specific probability of FR
apply(afr_matrix3,2,sum,na.rm=T)/apply(afr_matrix3,2,function(x)sum(!is.na(x)))
#plot it
plot(cumprod(1-apply(afr_matrix3,2,sum,na.rm=T)/apply(afr_matrix3,2,function(x)sum(!is.na(x))))~c(1:(max(real_data3$aoc)+1)),xlab="Age",ylab="Cumulative probability of first birth",ylim=c(0,1))

#Age-specific absolute wealth ----

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix3 <- matrix(nrow = nrow(real_data3),ncol=max(real_data3$aoc)+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw95[i]
  age_absw <- real_data3$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data3$aoc[i]+1)){
    absw_matrix3[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data3$aoc[i]+1)){
      absw_matrix3[i,(real_data3$aoc[i]+1)] <- NA
    } else{
      absw_matrix3[i,age_absw] <- NA
    }
}
#check data
absw_matrix3
#98
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw98[i]
  age_absw <- real_data3$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data3$aoc[i]){
    absw_matrix3[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data3$aoc[i]+1)){
      absw_matrix3[i,(real_data3$aoc[i]+1)] <- NA
    } else{
      absw_matrix3[i,age_absw] <- NA
    }
}
#check data
absw_matrix3
#00
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw00[i]
  age_absw <- real_data3$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data3$aoc[i]){
    absw_matrix3[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data3$aoc[i]+1)){
      absw_matrix3[i,(real_data3$aoc[i]+1)] <- NA
    } else{
      absw_matrix3[i,age_absw] <- NA
    }
}
#check data
absw_matrix3
#02
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw02[i]
  age_absw <- real_data3$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data3$aoc[i]){
    absw_matrix3[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data3$aoc[i]+1)){
      absw_matrix3[i,(real_data3$aoc[i]+1)] <- NA
    } else{
      absw_matrix3[i,age_absw] <- NA
    }
}
#check data
absw_matrix3
#04
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw04[i]
  age_absw <- real_data3$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data3$aoc[i]){
    absw_matrix3[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data3$aoc[i]+1)){
      absw_matrix3[i,(real_data3$aoc[i]+1)] <- NA
    } else{
      absw_matrix3[i,age_absw] <- NA
    }
}
#check data
absw_matrix3
#06
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw06[i]
  age_absw <- real_data3$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data3$aoc[i]){
    absw_matrix3[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data3$aoc[i]+1)){
      absw_matrix3[i,(real_data3$aoc[i]+1)] <- NA
    } else{
      absw_matrix3[i,age_absw] <- NA
    }
}
#check data
absw_matrix3
#10
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw10[i]
  age_absw <- real_data3$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data3$aoc[i]+1)){
    absw_matrix3[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data3$aoc[i]+1)){
      absw_matrix3[i,(real_data3$aoc[i]+1)] <- NA
    } else{
      absw_matrix3[i,age_absw] <- NA
    }
}
#check data
absw_matrix3
#check the age-specific average of absolute wealth
apply(absw_matrix3,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix3,2,mean,na.rm=T)~c(1:ncol(absw_matrix3)),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix3 <- matrix(standardize(log(as.vector(absw_matrix3))),ncol=ncol(absw_matrix3),nrow=nrow(absw_matrix3))
#check the data
std_absw_matrix3
#check the age-specific average of absolute wealth
apply(std_absw_matrix3,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix3,2,mean,na.rm=T)~c(1:(max(real_data3$aoc)+1)),xlab="Age",ylab="Average absolute wealth")

# Age-specific current wealth change ----

#create a matrix to store the age-specific current wealth change
diffwealth_matrix3 <- matrix(nrow=nrow(real_data3),ncol=max(real_data3$aoc)+1)
#set first and column at zero since there is no wealth change at birth, and there is a gap of two years in the data collection
diffwealth_matrix3[,1:2] <- 0
diffwealth_matrix3
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(diffwealth_matrix3)){
  for(j in 3:ncol(diffwealth_matrix3)){
    diffwealth_matrix3[i,j] <- absw_matrix3[i,j] - absw_matrix3[i,j-2] #-2 because there is a gap of two years between data collection
 }
}
#check the data
diffwealth_matrix3

#change to absolute values
abs_diffwealth_matrix3 <- abs(diffwealth_matrix3)
#check the age-specific average of wealth change
apply(abs_diffwealth_matrix3,2,mean,na.rm=T)
#plot it
plot(apply(abs_diffwealth_matrix3,2,mean,na.rm=T)~c(1:ncol(abs_diffwealth_matrix3)),xlab="Age",ylab="Average absolute wealth change")
hist(abs_diffwealth_matrix3)

#standardise absolute wealth change
std_diffwealth_matrix3 <- matrix(standardize(as.vector(diffwealth_matrix3)),ncol=ncol(diffwealth_matrix3),nrow=nrow(diffwealth_matrix3))
#check the data
std_diffwealth_matrix3
#check the age-specific average of absolute wealth
apply(std_diffwealth_matrix3,2,mean,na.rm=T)
#plot it
plot(apply(std_diffwealth_matrix3,2,mean,na.rm=T)~c(1:(max(real_data2$aoc)+1)),xlab="Age",ylab="Average absolute wealth")
hist(std_diffwealth_matrix3)

## Fit real data ----

#Age at first birth
#replace NAs with -99
for(j in 1:ncol(afr_matrix3)){
  for(i in 1:nrow(afr_matrix3)){
    if(is.na(afr_matrix3[i,j])){
      afr_matrix3[i,j] <- -99
    } else{
      afr_matrix3[i,j] <- afr_matrix3[i,j]
    }
  }
}
#check the data
afr_matrix3

#matrix identifying missing wealth data
#create matrix
miss_diffw_matrix3 <- std_diffwealth_matrix3
#define missing the first two columns (1)
miss_diffw_matrix3[,1:2] <- 1
#identify if the individual i at age j has missing data (1) or not (0)
for (i in 1:nrow(std_diffwealth_matrix3)){
  for(j in 3:ncol(std_diffwealth_matrix3)){
    if(is.na(miss_diffw_matrix3[i,j])){
      miss_diffw_matrix3[i,j] <- 1 #missing data
    } else{
      miss_diffw_matrix3[i,j] <- 0 #not missing data
    }
  }
}
#check data
miss_diffw_matrix3

#replace NAs with -99
for(j in 1:ncol(std_diffwealth_matrix3)){
  for(i in 1:nrow(std_diffwealth_matrix3)){
    if(is.na(std_diffwealth_matrix3[i,j])){
      std_diffwealth_matrix3[i,j] <- -99
    } else{
      std_diffwealth_matrix3[i,j] <- std_diffwealth_matrix3[i,j]
    }
  }
}
#check the data
std_diffwealth_matrix3

#Subset the data for realistic ages
#Subset wealth and AFB for those between 10 years old and 50 years old.
#wealth
std_diffwealth_restricted <- std_diffwealth_matrix3[,11:51] #Adding 1, since first column in the matrix is year 0
#AFB
afrs_restricted <- afr_matrix3[,11:51] #Adding 1, since first column in the matrix is year 0
#missing wealth data
miss_diffwealth_restricted <- miss_diffw_matrix3[,11:51] #Adding 1, since first column in the matrix is year 0


#put all the data together
#create dataset
real_list3 <- list(N = nrow(afrs_restricted), #population size
                   A = ncol(afrs_restricted), #age
                   wealth = std_diffwealth_restricted, #absolute current wealth change
                   baby = afrs_restricted, #AFR
                   N_miss = sum(miss_diffwealth_restricted), # number of missing values that need imputation
                   wealth_miss=miss_diffwealth_restricted) # matrix indicating missing wealth data
#check data
real_list3

# compile model

m3_add <- cmdstan_model("Model_code/firstbaby_diffwealth_additive.stan")

#fit model
fit3_add_real <- m3_add$sample(data = real_list3, 
                       chains = 4, 
                       parallel_chains = 15, 
                       adapt_delta = 0.95,
                       max_treedepth = 13,
                       init = 0)


# save fit 
fit_3_add_real <- rstan::read_stan_csv(fit3_add_real$output_files())
saveRDS(fit_3_add_real, "firstbaby3_add_real.rds")
#load RDS file
rds3_add_real <- readRDS("firstbaby3_add_real.rds")
#extract samples
post3_add_real <- extract.samples(rds3_add_real)

#check the model
#check trace of all main parameters
#alpha
rstan::traceplot(rds3_add_real,pars="alpha")
#mu
#traceplot(rds3_add_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds3_add_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
rstan::traceplot(rds3_add_real,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds3_add_real,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds3_add_real,pars="mu_delta")
#gamma_wealth
#traceplot(rds3_add_real,pars="gamma_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds3_add_real,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab3_add_real <- precis(rds3_add_real,depth=2,pars=c("alpha",
                                             "mu_raw",
                                             "mu_tau",
                                             "mu_delta"))
#check table
tab3_add_real
#create summary table for mu
tab3_add_real_mu <- precis(rds3_add_real,depth=2,pars="mu")
#check table
tab3_add_real_mu
plot(tab3_add_real_mu)
#create summary table for gamma
tab3_add_real_gamma <- precis(rds3_add_real,depth=2,pars="gamma_wealth")
#check table
tab3_add_real_gamma
plot(tab3_add_real_gamma)

# # To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
# #mu
# tab3_add_real_mu[,1]<-round(inv_logit(tab3_add_real_mu[,1]),3)
# tab3_add_real_mu[,3]<-round(inv_logit(tab3_add_real_mu[,3]),3)
# tab3_add_real_mu[,4]<-round(inv_logit(tab3_add_real_mu[,4]),3)
# #gamma_wealth
# tab3_add_real_gamma[,1]<-round(inv_logit(tab3_add_real_gamma[,1]),3)
# tab3_add_real_gamma[,3]<-round(inv_logit(tab3_add_real_gamma[,3]),3)
# tab3_add_real_gamma[,4]<-round(inv_logit(tab3_add_real_gamma[,4]),3)

## Plot the fit of the real data ----

### Wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post3_add_real$wealth_full),1),to=round(max(post3_add_real$wealth_full),1),length.out=nrow(std_absw_matrix3)) #specify according to range and length related to sample size
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
plot(c(0,1)~c(0,ncol(post3_add_real$mu)),
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
  p3_add_real_b <- matrix(nrow=nrow(post3_add_real$mu),ncol=ncol(post3_add_real$mu))
  p3_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post3_add_real$mu)){
    for(i in 1:nrow(post3_add_real$mu)){
      p3_add_real_b[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                   post3_add_real$mu[i,j] + #age
                                   post3_add_real$gamma_wealth[i,j]*deciles[k]) #wealth change
    }
  }
  #check data
  p3_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data3_add_real_b <- data.frame(age = 1:ncol(p3_add_real_b),
                                 mean = apply(p3_add_real_b, 2, mean), 
                                 upp = apply(p3_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p3_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr3 <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr3)){
    for(i in 1:nrow(plot_afr3)){
      if(plot_afr3[i,j]==-99){
        plot_afr3[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr3
  
  points(cumprod(1-plot_data3_add_real_b$mean)~plot_data3_add_real_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]],pch=15)
  lines(cumprod(1-plot_data3_add_real_b$mean)~plot_data3_add_real_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]])
  polygon(c(plot_data3_add_real_b$age,rev(plot_data3_add_real_b$age)),c(cumprod(1-plot_data3_add_real_b$low),rev(cumprod(1-plot_data3_add_real_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[k]],0.5),border=NA)
}

## De-couple plot by min, mean, max ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post3_add_real$wealth_full),1),to=round(max(post3_add_real$wealth_full),1),length.out=nrow(std_absw_matrix3)) #specify according to range and length related to sample size
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
plot(c(0,1)~c(0,ncol(post3_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="No change",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p3_add_real_0_b <- matrix(nrow=nrow(post3_add_real$mu),ncol=ncol(post3_add_real$mu))
p3_add_real_0_b
#fill it in with values for age 25
for(j in 1:ncol(post3_add_real$mu)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real_0_b[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                   post3_add_real$mu[i,j] + #age
                                   post3_add_real$gamma_wealth[i,j]*deciles[1]) #wealth change
  }
}
#check data
p3_add_real_0_b
#plot it!
#prepare model prediction data
plot_data3_add_real_0_b <- data.frame(age = 1:ncol(p3_add_real_0_b),
                                 mean = apply(p3_add_real_0_b, 2, mean), 
                                 upp = apply(p3_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p3_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix3
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

points(cumprod(1-plot_data3_add_real_0_b$mean)~plot_data3_add_real_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),pch=15)
lines(cumprod(1-plot_data3_add_real_0_b$mean)~plot_data3_add_real_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),lwd=2)
polygon(c(plot_data3_add_real_0_b$age,rev(plot_data3_add_real_0_b$age)),c(cumprod(1-plot_data3_add_real_0_b$low),rev(cumprod(1-plot_data3_add_real_0_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.5),border=NA)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post3_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Mid. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p3_add_real_50_b <- matrix(nrow=nrow(post3_add_real$mu),ncol=ncol(post3_add_real$mu))
p3_add_real_50_b
#fill it in with values for age 25
for(j in 1:ncol(post3_add_real$mu)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real_50_b[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                    post3_add_real$mu[i,j] + #age
                                    post3_add_real$gamma_wealth[i,j]*deciles[2]) #wealth
  }
}
#check data
p3_add_real_50_b
#plot it!
#prepare model prediction data
plot_data3_add_real_50_b <- data.frame(age = 1:ncol(p3_add_real_50_b),
                                  mean = apply(p3_add_real_50_b, 2, mean), 
                                  upp = apply(p3_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p3_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix3
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

points(cumprod(1-plot_data3_add_real_50_b$mean)~plot_data3_add_real_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),pch=15)
lines(cumprod(1-plot_data3_add_real_50_b$mean)~plot_data3_add_real_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),lwd=2)
polygon(c(plot_data3_add_real_50_b$age,rev(plot_data3_add_real_50_b$age)),c(cumprod(1-plot_data3_add_real_50_b$low),rev(cumprod(1-plot_data3_add_real_50_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.5),border=NA)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post3_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Max. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p3_add_real_100_b <- matrix(nrow=nrow(post3_add_real$mu),ncol=ncol(post3_add_real$mu))
p3_add_real_100_b
#fill it in with values for age 25
for(j in 1:ncol(post3_add_real$mu)){
  for(i in 1:nrow(post3_add_real$mu)){
    p3_add_real_100_b[i,j] <- inv_logit(post3_add_real$alpha[i] + #inv logit because originally is logit
                                     post3_add_real$mu[i,j] + #age
                                     post3_add_real$gamma_wealth[i,j]*deciles[3]) #wealth
  }
}
#check data
p3_add_real_100_b
#plot it!
#prepare model prediction data
plot_data3_add_real_100_b <- data.frame(age = 1:ncol(p3_add_real_100_b),
                                   mean = apply(p3_add_real_100_b, 2, mean), 
                                   upp = apply(p3_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                   low = apply(p3_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr3 <- afr_matrix3
#change -99 to NAs
for(j in 1:ncol(plot_afr3)){
  for(i in 1:nrow(plot_afr3)){
    if(plot_afr3[i,j]==-99){
      plot_afr3[i,j] <- NA
    }
  }
}
#check the data
plot_afr3

points(cumprod(1-plot_data3_add_real_100_b$mean)~plot_data3_add_real_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),pch=15)
lines(cumprod(1-plot_data3_add_real_100_b$mean)~plot_data3_add_real_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),lwd=2)
polygon(c(plot_data3_add_real_100_b$age,rev(plot_data3_add_real_100_b$age)),c(cumprod(1-plot_data3_add_real_100_b$low),rev(cumprod(1-plot_data3_add_real_100_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.5),border=NA)

legend(77.5,1,c("No change","Mid. change", "Max. change"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)

