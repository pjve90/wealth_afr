# Model with absolute levels, wealth change, and moving variance ----

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
real_data5 <- read.csv("dataf.csv")[,-1]
head(real_data5)

# Age at first reproduction ----

#create a matrix to store the age-specific age of censor
afr_matrix5 <- matrix(nrow=nrow(real_data5),ncol=max(real_data5$aoc)+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix5)){
  afr <- real_data5$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data5$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix5[i,1:(afr-1)] <- 0
    afr_matrix5[i,afr] <- 1
  } else{
    afr_matrix5[i,1:aoc] <- rep(0,length(afr_matrix5[i,1:aoc]))
  }
}
#check the data
afr_matrix5
#check the age-specific probability of FR
apply(afr_matrix5,2,sum,na.rm=T)/apply(afr_matrix5,2,function(x)sum(!is.na(x)))
#plot it
plot(cumprod(1-apply(afr_matrix5,2,sum,na.rm=T)/apply(afr_matrix5,2,function(x)sum(!is.na(x))))~c(1:(max(real_data5$aoc)+1)),xlab="Age",ylab="Cumulative probability of first birth",ylim=c(0,1))

#Age-specific absolute wealth ----

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix5 <- matrix(nrow = nrow(real_data5),ncol=max(real_data5$aoc)+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix5)){
  absw <- real_data5$absw95[i]
  age_absw <- real_data5$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data5$aoc[i]+1)){
    absw_matrix5[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data5$aoc[i]+1)){
      absw_matrix5[i,(real_data5$aoc[i]+1)] <- NA
    } else{
      absw_matrix5[i,age_absw] <- NA
    }
}
#check data
absw_matrix5
#98
for(i in 1:nrow(absw_matrix5)){
  absw <- real_data5$absw98[i]
  age_absw <- real_data5$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data5$aoc[i]){
    absw_matrix5[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data5$aoc[i]+1)){
      absw_matrix5[i,(real_data5$aoc[i]+1)] <- NA
    } else{
      absw_matrix5[i,age_absw] <- NA
    }
}
#check data
absw_matrix5
#00
for(i in 1:nrow(absw_matrix5)){
  absw <- real_data5$absw00[i]
  age_absw <- real_data5$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data5$aoc[i]){
    absw_matrix5[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data5$aoc[i]+1)){
      absw_matrix5[i,(real_data5$aoc[i]+1)] <- NA
    } else{
      absw_matrix5[i,age_absw] <- NA
    }
}
#check data
absw_matrix5
#02
for(i in 1:nrow(absw_matrix5)){
  absw <- real_data5$absw02[i]
  age_absw <- real_data5$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data5$aoc[i]){
    absw_matrix5[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data5$aoc[i]+1)){
      absw_matrix5[i,(real_data5$aoc[i]+1)] <- NA
    } else{
      absw_matrix5[i,age_absw] <- NA
    }
}
#check data
absw_matrix5
#04
for(i in 1:nrow(absw_matrix5)){
  absw <- real_data5$absw04[i]
  age_absw <- real_data5$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data5$aoc[i]){
    absw_matrix5[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data5$aoc[i]+1)){
      absw_matrix5[i,(real_data5$aoc[i]+1)] <- NA
    } else{
      absw_matrix5[i,age_absw] <- NA
    }
}
#check data
absw_matrix5
#06
for(i in 1:nrow(absw_matrix5)){
  absw <- real_data5$absw06[i]
  age_absw <- real_data5$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data5$aoc[i]){
    absw_matrix5[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data5$aoc[i]+1)){
      absw_matrix5[i,(real_data5$aoc[i]+1)] <- NA
    } else{
      absw_matrix5[i,age_absw] <- NA
    }
}
#check data
absw_matrix5
#10
for(i in 1:nrow(absw_matrix5)){
  absw <- real_data5$absw10[i]
  age_absw <- real_data5$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data5$aoc[i]+1)){
    absw_matrix5[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data5$aoc[i]+1)){
      absw_matrix5[i,(real_data5$aoc[i]+1)] <- NA
    } else{
      absw_matrix5[i,age_absw] <- NA
    }
}
#check data
absw_matrix5
#check the age-specific average of absolute wealth
apply(absw_matrix5,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix5,2,mean,na.rm=T)~c(1:ncol(absw_matrix5)),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix5 <- matrix(standardize(log(as.vector(absw_matrix5))),ncol=ncol(absw_matrix5),nrow=nrow(absw_matrix5))
#check the data
std_absw_matrix5
#check the age-specific average of absolute wealth
apply(std_absw_matrix5,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix5,2,mean,na.rm=T)~c(1:(max(real_data5$aoc)+1)),xlab="Age",ylab="Average absolute wealth")

## Fit real data ----

###Prepare data ----

#Age at first birth
#replace NAs with -99
for(j in 1:ncol(afr_matrix5)){
  for(i in 1:nrow(afr_matrix5)){
    if(is.na(afr_matrix5[i,j])){
      afr_matrix5[i,j] <- -99
    } else{
      afr_matrix5[i,j] <- afr_matrix5[i,j]
    }
  }
}
#check the data
afr_matrix5

#Wealth
#matrix identifying missing wealth data
wealth_miss5 <- which(is.na(std_absw_matrix5),arr.ind = T)
#check data
wealth_miss5

#replace NAs with -99
for(j in 1:ncol(std_absw_matrix5)){
  for(i in 1:nrow(std_absw_matrix5)){
    if(is.na(std_absw_matrix5[i,j])){
      std_absw_matrix5[i,j] <- -99
    } else{
      std_absw_matrix5[i,j] <- std_absw_matrix5[i,j]
    }
  }
}
#check the data
std_absw_matrix5

#Subset the data for realistic ages
#Subset wealth and AFB for those between zero years old and 50 years old.
#wealth
std_absw_restricted <- std_absw_matrix5[,1:51] #Adding 1, since first column in the matrix is year 0
#AFB
afrs_restricted <- afr_matrix5[,1:51] #Adding 1, since first column in the matrix is year 0
#missing wealth data
wealth_miss_restricted <- wealth_miss5[wealth_miss5[,2] <= 51,] #Adding 1, since first column in the matrix is year 0


#put all the data together
#create dataset
real_list5 <- list(N = nrow(afrs_restricted), #population size
                   A = ncol(afrs_restricted), #age
                   wealth = std_absw_restricted, #absolute current wealth change
                   baby = afrs_restricted, #AFR
                   N_miss = nrow(wealth_miss_restricted), # number of missing values that need imputation
                   wealth_miss=wealth_miss_restricted) # matrix indicating missing wealth data
#check data
real_list5

### Compile and fit model ----

# compile model

m5_add <- cmdstan_model("Model_code/firstbaby_threewealth.stan")

#fit model
fit5_add_real <- m5_add$sample(data = real_list5, 
                               chains = 4, 
                               parallel_chains = 15, 
                               adapt_delta = 0.95,
                               max_treedepth = 13,
                               init = 0)


# save fit 
fit_5_add_real <- rstan::read_stan_csv(fit5_add_real$output_files())
saveRDS(fit_5_add_real, "firstbaby5_add_real.rds")
#load RDS file
rds5_add_real <- readRDS("firstbaby5_add_real.rds")
#extract samples
post5_add_real <- extract.samples(rds5_add_real)

#check the model
#check trace of all main parameters
#alpha
rstan::traceplot(rds5_add_real,pars="alpha")
#mu
#traceplot(rds5_add_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds5_add_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
rstan::traceplot(rds5_add_real,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds5_add_real,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds5_add_real,pars="mu_delta")
#delta_wealth
#traceplot(rds5_add_real,pars="delta_wealth") #only run if needed, because they are 91 plots
#delta_wealth
#traceplot(rds5_add_real,pars="delta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab5_add_real <- precis(rds5_add_real,depth=2,pars=c("alpha",
                                                     "mu_raw",
                                                     "mu_tau",
                                                     "mu_delta"))
#check table
tab5_add_real
#create summary table for mu
tab5_add_real_mu <- precis(rds5_add_real,depth=2,pars="mu")
#check table
tab5_add_real_mu
plot(tab5_add_real_mu)
#create summary table for delta
tab5_add_real_delta <- precis(rds5_add_real,depth=2,pars="delta_wealth")
#check table
tab5_add_real_delta
plot(tab5_add_real_delta)

# # To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
# #mu
# tab5_add_real_mu[,1]<-round(inv_logit(tab5_add_real_mu[,1]),3)
# tab5_add_real_mu[,3]<-round(inv_logit(tab5_add_real_mu[,3]),3)
# tab5_add_real_mu[,4]<-round(inv_logit(tab5_add_real_mu[,4]),3)
# #delta_wealth
# tab5_add_real_delta[,1]<-round(inv_logit(tab5_add_real_delta[,1]),3)
# tab5_add_real_delta[,3]<-round(inv_logit(tab5_add_real_delta[,3]),3)
# tab5_add_real_delta[,4]<-round(inv_logit(tab5_add_real_delta[,4]),3)

## Plot the fit of the real data ----

### Absolute Wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post5_add_real$wealth_full),1),to=round(max(post5_add_real$wealth_full),1),length.out=nrow(std_absw_matrix5)) #specify according to range and length related to sample size
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
plot(c(0,1)~c(0,ncol(post5_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Model with absolute change (1 year)",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(77.5,1,c("Poor","Middle", "Rich"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p5_add_real_b <- matrix(nrow=nrow(post5_add_real$mu),ncol=ncol(post5_add_real$mu))
  p5_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post5_add_real$mu)){
    for(i in 1:nrow(post5_add_real$mu)){
      p5_add_real_b[i,j] <- inv_logit(post5_add_real$alpha[i] + #inv logit because originally is logit
                                        post5_add_real$mu[i,j] + #age
                                        post5_add_real$beta_wealth[i,j]*deciles[k] + #absolute wealth
                                        post5_add_real$gamma_wealth[i,j]*0 + #wealth change
                                        post5_add_real$delta_wealth[i,j]*0) #moving variance
    }
  }
  #check data
  p5_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data5_add_real_b <- data.frame(age = 1:ncol(p5_add_real_b),
                                      mean = apply(p5_add_real_b, 2, mean), 
                                      upp = apply(p5_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p5_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr5 <- afr_matrix5
  #change -99 to NAs
  for(j in 1:ncol(plot_afr5)){
    for(i in 1:nrow(plot_afr5)){
      if(plot_afr5[i,j]==-99){
        plot_afr5[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr5
  
  points(cumprod(1-plot_data5_add_real_b$mean)~plot_data5_add_real_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]],pch=15)
  lines(cumprod(1-plot_data5_add_real_b$mean)~plot_data5_add_real_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]])
  polygon(c(plot_data5_add_real_b$age,rev(plot_data5_add_real_b$age)),c(cumprod(1-plot_data5_add_real_b$low),rev(cumprod(1-plot_data5_add_real_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[k]],0.5),border=NA)
}

### De-couple plot by min, mean, max ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post5_add_real$wealth_full),1),to=round(max(post5_add_real$wealth_full),1),length.out=nrow(std_absw_matrix5)) #specify according to range and length related to sample size
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
plot(c(0,1)~c(0,ncol(post5_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="No change",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p5_add_real_0_b <- matrix(nrow=nrow(post5_add_real$mu),ncol=ncol(post5_add_real$mu))
p5_add_real_0_b
#fill it in with values for age 25
for(j in 1:ncol(post5_add_real$mu)){
  for(i in 1:nrow(post5_add_real$mu)){
    p5_add_real_b[i,j] <- inv_logit(post5_add_real$alpha[i] + #inv logit because originally is logit
                                      post5_add_real$mu[i,j] + #age
                                      post5_add_real$beta_wealth[i,j]*deciles[1] + #absolute wealth
                                      post5_add_real$gamma_wealth[i,j]*0 + #wealth change
                                      post5_add_real$delta_wealth[i,j]*0) #moving variance
  }
}
#check data
p5_add_real_0_b
#plot it!
#prepare model prediction data
plot_data5_add_real_0_b <- data.frame(age = 1:ncol(p5_add_real_0_b),
                                      mean = apply(p5_add_real_0_b, 2, mean), 
                                      upp = apply(p5_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p5_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr5 <- afr_matrix5
#change -99 to NAs
for(j in 1:ncol(plot_afr5)){
  for(i in 1:nrow(plot_afr5)){
    if(plot_afr5[i,j]==-99){
      plot_afr5[i,j] <- NA
    }
  }
}
#check the data
plot_afr5

points(cumprod(1-plot_data5_add_real_0_b$mean)~plot_data5_add_real_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),pch=15)
lines(cumprod(1-plot_data5_add_real_0_b$mean)~plot_data5_add_real_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),lwd=2)
polygon(c(plot_data5_add_real_0_b$age,rev(plot_data5_add_real_0_b$age)),c(cumprod(1-plot_data5_add_real_0_b$low),rev(cumprod(1-plot_data5_add_real_0_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.5),border=NA)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post5_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Mid. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p5_add_real_50_b <- matrix(nrow=nrow(post5_add_real$mu),ncol=ncol(post5_add_real$mu))
p5_add_real_50_b
#fill it in with values for age 25
for(j in 1:ncol(post5_add_real$mu)){
  for(i in 1:nrow(post5_add_real$mu)){
    p5_add_real_b[i,j] <- inv_logit(post5_add_real$alpha[i] + #inv logit because originally is logit
                                      post5_add_real$mu[i,j] + #age
                                      post5_add_real$beta_wealth[i,j]*deciles[2] + #absolute wealth
                                      post5_add_real$gamma_wealth[i,j]*0 + #wealth change
                                      post5_add_real$delta_wealth[i,j]*0) #moving variance
  }
}
#check data
p5_add_real_50_b
#plot it!
#prepare model prediction data
plot_data5_add_real_50_b <- data.frame(age = 1:ncol(p5_add_real_50_b),
                                       mean = apply(p5_add_real_50_b, 2, mean), 
                                       upp = apply(p5_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p5_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr5 <- afr_matrix5
#change -99 to NAs
for(j in 1:ncol(plot_afr5)){
  for(i in 1:nrow(plot_afr5)){
    if(plot_afr5[i,j]==-99){
      plot_afr5[i,j] <- NA
    }
  }
}
#check the data
plot_afr5

points(cumprod(1-plot_data5_add_real_50_b$mean)~plot_data5_add_real_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),pch=15)
lines(cumprod(1-plot_data5_add_real_50_b$mean)~plot_data5_add_real_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),lwd=2)
polygon(c(plot_data5_add_real_50_b$age,rev(plot_data5_add_real_50_b$age)),c(cumprod(1-plot_data5_add_real_50_b$low),rev(cumprod(1-plot_data5_add_real_50_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.5),border=NA)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post5_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Max. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p5_add_real_100_b <- matrix(nrow=nrow(post5_add_real$mu),ncol=ncol(post5_add_real$mu))
p5_add_real_100_b
#fill it in with values for age 25
for(j in 1:ncol(post5_add_real$mu)){
  for(i in 1:nrow(post5_add_real$mu)){
    p5_add_real_b[i,j] <- inv_logit(post5_add_real$alpha[i] + #inv logit because originally is logit
                                      post5_add_real$mu[i,j] + #age
                                      post5_add_real$beta_wealth[i,j]*deciles[3] + #absolute wealth
                                      post5_add_real$gamma_wealth[i,j]*0 + #wealth change
                                      post5_add_real$delta_wealth[i,j]*0) #moving variance
  }
}
#check data
p5_add_real_100_b
#plot it!
#prepare model prediction data
plot_data5_add_real_100_b <- data.frame(age = 1:ncol(p5_add_real_100_b),
                                        mean = apply(p5_add_real_100_b, 2, mean), 
                                        upp = apply(p5_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                        low = apply(p5_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr5 <- afr_matrix5
#change -99 to NAs
for(j in 1:ncol(plot_afr5)){
  for(i in 1:nrow(plot_afr5)){
    if(plot_afr5[i,j]==-99){
      plot_afr5[i,j] <- NA
    }
  }
}
#check the data
plot_afr5

points(cumprod(1-plot_data5_add_real_100_b$mean)~plot_data5_add_real_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),pch=15)
lines(cumprod(1-plot_data5_add_real_100_b$mean)~plot_data5_add_real_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),lwd=2)
polygon(c(plot_data5_add_real_100_b$age,rev(plot_data5_add_real_100_b$age)),c(cumprod(1-plot_data5_add_real_100_b$low),rev(cumprod(1-plot_data5_add_real_100_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.5),border=NA)

legend(77.5,1,c("Poor","Middle", "Rich"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)

### Absolute Wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post5_add_real$wealth_full),1),to=round(max(post5_add_real$wealth_full),1),length.out=nrow(std_absw_matrix5)) #specify according to range and length related to sample size
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
plot(c(0,1)~c(0,ncol(post5_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Model with absolute change (1 year)",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(77.5,1,c("Poor","Middle", "Rich"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p5_add_real_b <- matrix(nrow=nrow(post5_add_real$mu),ncol=ncol(post5_add_real$mu))
  p5_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post5_add_real$mu)){
    for(i in 1:nrow(post5_add_real$mu)){
      p5_add_real_b[i,j] <- inv_logit(post5_add_real$alpha[i] + #inv logit because originally is logit
                                        post5_add_real$mu[i,j] + #age
                                        post5_add_real$beta_wealth[i,j]*deciles[k] + #absolute wealth
                                        post5_add_real$gamma_wealth[i,j]*0 + #wealth change
                                        post5_add_real$delta_wealth[i,j]*0) #moving variance
    }
  }
  #check data
  p5_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data5_add_real_b <- data.frame(age = 1:ncol(p5_add_real_b),
                                      mean = apply(p5_add_real_b, 2, mean), 
                                      upp = apply(p5_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p5_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr5 <- afr_matrix5
  #change -99 to NAs
  for(j in 1:ncol(plot_afr5)){
    for(i in 1:nrow(plot_afr5)){
      if(plot_afr5[i,j]==-99){
        plot_afr5[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr5
  
  points(cumprod(1-plot_data5_add_real_b$mean)~plot_data5_add_real_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]],pch=15)
  lines(cumprod(1-plot_data5_add_real_b$mean)~plot_data5_add_real_b$age,col=hcl.colors(length(palette),"temps")[palette_b[k]])
  polygon(c(plot_data5_add_real_b$age,rev(plot_data5_add_real_b$age)),c(cumprod(1-plot_data5_add_real_b$low),rev(cumprod(1-plot_data5_add_real_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[k]],0.5),border=NA)
}

### De-couple plot by min, mean, max ----

#simulate wealth values
simwealth_add_real <- seq(from=round(min(post5_add_real$wealth_full),1),to=round(max(post5_add_real$wealth_full),1),length.out=nrow(std_absw_matrix5)) #specify according to range and length related to sample size
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
plot(c(0,1)~c(0,ncol(post5_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="No change",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p5_add_real_0_b <- matrix(nrow=nrow(post5_add_real$mu),ncol=ncol(post5_add_real$mu))
p5_add_real_0_b
#fill it in with values for age 25
for(j in 1:ncol(post5_add_real$mu)){
  for(i in 1:nrow(post5_add_real$mu)){
    p5_add_real_b[i,j] <- inv_logit(post5_add_real$alpha[i] + #inv logit because originally is logit
                                      post5_add_real$mu[i,j] + #age
                                      post5_add_real$beta_wealth[i,j]*deciles[1] + #absolute wealth
                                      post5_add_real$gamma_wealth[i,j]*0 + #wealth change
                                      post5_add_real$delta_wealth[i,j]*0) #moving variance
  }
}
#check data
p5_add_real_0_b
#plot it!
#prepare model prediction data
plot_data5_add_real_0_b <- data.frame(age = 1:ncol(p5_add_real_0_b),
                                      mean = apply(p5_add_real_0_b, 2, mean), 
                                      upp = apply(p5_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p5_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr5 <- afr_matrix5
#change -99 to NAs
for(j in 1:ncol(plot_afr5)){
  for(i in 1:nrow(plot_afr5)){
    if(plot_afr5[i,j]==-99){
      plot_afr5[i,j] <- NA
    }
  }
}
#check the data
plot_afr5

points(cumprod(1-plot_data5_add_real_0_b$mean)~plot_data5_add_real_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),pch=15)
lines(cumprod(1-plot_data5_add_real_0_b$mean)~plot_data5_add_real_0_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),lwd=2)
polygon(c(plot_data5_add_real_0_b$age,rev(plot_data5_add_real_0_b$age)),c(cumprod(1-plot_data5_add_real_0_b$low),rev(cumprod(1-plot_data5_add_real_0_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.5),border=NA)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post5_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Mid. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p5_add_real_50_b <- matrix(nrow=nrow(post5_add_real$mu),ncol=ncol(post5_add_real$mu))
p5_add_real_50_b
#fill it in with values for age 25
for(j in 1:ncol(post5_add_real$mu)){
  for(i in 1:nrow(post5_add_real$mu)){
    p5_add_real_b[i,j] <- inv_logit(post5_add_real$alpha[i] + #inv logit because originally is logit
                                      post5_add_real$mu[i,j] + #age
                                      post5_add_real$beta_wealth[i,j]*deciles[2] + #absolute wealth
                                      post5_add_real$gamma_wealth[i,j]*0 + #wealth change
                                      post5_add_real$delta_wealth[i,j]*0) #moving variance
  }
}
#check data
p5_add_real_50_b
#plot it!
#prepare model prediction data
plot_data5_add_real_50_b <- data.frame(age = 1:ncol(p5_add_real_50_b),
                                       mean = apply(p5_add_real_50_b, 2, mean), 
                                       upp = apply(p5_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p5_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr5 <- afr_matrix5
#change -99 to NAs
for(j in 1:ncol(plot_afr5)){
  for(i in 1:nrow(plot_afr5)){
    if(plot_afr5[i,j]==-99){
      plot_afr5[i,j] <- NA
    }
  }
}
#check the data
plot_afr5

points(cumprod(1-plot_data5_add_real_50_b$mean)~plot_data5_add_real_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),pch=15)
lines(cumprod(1-plot_data5_add_real_50_b$mean)~plot_data5_add_real_50_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),lwd=2)
polygon(c(plot_data5_add_real_50_b$age,rev(plot_data5_add_real_50_b$age)),c(cumprod(1-plot_data5_add_real_50_b$low),rev(cumprod(1-plot_data5_add_real_50_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.5),border=NA)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post5_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Max. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p5_add_real_100_b <- matrix(nrow=nrow(post5_add_real$mu),ncol=ncol(post5_add_real$mu))
p5_add_real_100_b
#fill it in with values for age 25
for(j in 1:ncol(post5_add_real$mu)){
  for(i in 1:nrow(post5_add_real$mu)){
    p5_add_real_b[i,j] <- inv_logit(post5_add_real$alpha[i] + #inv logit because originally is logit
                                      post5_add_real$mu[i,j] + #age
                                      post5_add_real$beta_wealth[i,j]*deciles[3] + #absolute wealth
                                      post5_add_real$gamma_wealth[i,j]*0 + #wealth change
                                      post5_add_real$delta_wealth[i,j]*0) #moving variance
  }
}
#check data
p5_add_real_100_b
#plot it!
#prepare model prediction data
plot_data5_add_real_100_b <- data.frame(age = 1:ncol(p5_add_real_100_b),
                                        mean = apply(p5_add_real_100_b, 2, mean), 
                                        upp = apply(p5_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                        low = apply(p5_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr5 <- afr_matrix5
#change -99 to NAs
for(j in 1:ncol(plot_afr5)){
  for(i in 1:nrow(plot_afr5)){
    if(plot_afr5[i,j]==-99){
      plot_afr5[i,j] <- NA
    }
  }
}
#check the data
plot_afr5

points(cumprod(1-plot_data5_add_real_100_b$mean)~plot_data5_add_real_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),pch=15)
lines(cumprod(1-plot_data5_add_real_100_b$mean)~plot_data5_add_real_100_b$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),lwd=2)
polygon(c(plot_data5_add_real_100_b$age,rev(plot_data5_add_real_100_b$age)),c(cumprod(1-plot_data5_add_real_100_b$low),rev(cumprod(1-plot_data5_add_real_100_b$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.5),border=NA)

legend(77.5,1,c("Poor","Middle", "Rich"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)
