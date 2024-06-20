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
real_data4 <- read.csv("dataf.csv")[,-1]
head(real_data4)

# Age at first reproduction ----

#create a matrix to store the age-specific age of censor
afr_matrix4 <- matrix(nrow=nrow(real_data4),ncol=max(real_data4$aoc)+1)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix4)){
  afr <- real_data4$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data4$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix4[i,1:(afr-1)] <- 0
    afr_matrix4[i,afr] <- 1
  } else{
    afr_matrix4[i,1:aoc] <- rep(0,length(afr_matrix4[i,1:aoc]))
  }
}
#check the data
afr_matrix4
#check the age-specific probability of FR
apply(afr_matrix4,2,sum,na.rm=T)/apply(afr_matrix4,2,function(x)sum(!is.na(x)))
#plot it
plot(cumprod(1-apply(afr_matrix4,2,sum,na.rm=T)/apply(afr_matrix4,2,function(x)sum(!is.na(x))))~c(1:(max(real_data4$aoc)+1)),xlab="Age",ylab="Cumulative probability of first birth",ylim=c(0,1))

#Age-specific absolute wealth ----

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix4 <- matrix(nrow = nrow(real_data4),ncol=max(real_data4$aoc)+1)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw95[i]
  age_absw <- real_data4$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data4$aoc[i]+1)){
    absw_matrix4[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data4$aoc[i]+1)){
      absw_matrix4[i,(real_data4$aoc[i]+1)] <- NA
    } else{
      absw_matrix4[i,age_absw] <- NA
    }
}
#check data
absw_matrix4
#98
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw98[i]
  age_absw <- real_data4$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data4$aoc[i]){
    absw_matrix4[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data4$aoc[i]+1)){
      absw_matrix4[i,(real_data4$aoc[i]+1)] <- NA
    } else{
      absw_matrix4[i,age_absw] <- NA
    }
}
#check data
absw_matrix4
#00
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw00[i]
  age_absw <- real_data4$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data4$aoc[i]){
    absw_matrix4[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data4$aoc[i]+1)){
      absw_matrix4[i,(real_data4$aoc[i]+1)] <- NA
    } else{
      absw_matrix4[i,age_absw] <- NA
    }
}
#check data
absw_matrix4
#02
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw02[i]
  age_absw <- real_data4$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data4$aoc[i]){
    absw_matrix4[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data4$aoc[i]+1)){
      absw_matrix4[i,(real_data4$aoc[i]+1)] <- NA
    } else{
      absw_matrix4[i,age_absw] <- NA
    }
}
#check data
absw_matrix4
#04
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw04[i]
  age_absw <- real_data4$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data4$aoc[i]){
    absw_matrix4[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data4$aoc[i]+1)){
      absw_matrix4[i,(real_data4$aoc[i]+1)] <- NA
    } else{
      absw_matrix4[i,age_absw] <- NA
    }
}
#check data
absw_matrix4
#06
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw06[i]
  age_absw <- real_data4$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data4$aoc[i]){
    absw_matrix4[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data4$aoc[i]+1)){
      absw_matrix4[i,(real_data4$aoc[i]+1)] <- NA
    } else{
      absw_matrix4[i,age_absw] <- NA
    }
}
#check data
absw_matrix4
#10
for(i in 1:nrow(absw_matrix4)){
  absw <- real_data4$absw10[i]
  age_absw <- real_data4$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data4$aoc[i]+1)){
    absw_matrix4[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data4$aoc[i]+1)){
      absw_matrix4[i,(real_data4$aoc[i]+1)] <- NA
    } else{
      absw_matrix4[i,age_absw] <- NA
    }
}
#check data
absw_matrix4
#check the age-specific average of absolute wealth
apply(absw_matrix4,2,mean,na.rm=T)
#plot it
plot(apply(absw_matrix4,2,mean,na.rm=T)~c(1:ncol(absw_matrix4)),xlab="Age",ylab="Average absolute wealth")

#standardise absolute wealth
std_absw_matrix4 <- matrix(standardize(log(as.vector(absw_matrix4))),ncol=ncol(absw_matrix4),nrow=nrow(absw_matrix4))
#check the data
std_absw_matrix4
#check the age-specific average of absolute wealth
apply(std_absw_matrix4,2,mean,na.rm=T)
#plot it
plot(apply(std_absw_matrix4,2,mean,na.rm=T)~c(1:(max(real_data4$aoc)+1)),xlab="Age",ylab="Average absolute wealth")
hist(std_absw_matrix4)

## Fit real data ----

###Prepare data ----

#Age at first birth
#replace NAs with -99
for(j in 1:ncol(afr_matrix4)){
  for(i in 1:nrow(afr_matrix4)){
    if(is.na(afr_matrix4[i,j])){
      afr_matrix4[i,j] <- -99
    } else{
      afr_matrix4[i,j] <- afr_matrix4[i,j]
    }
  }
}
#check the data
afr_matrix4

#Wealth
#matrix identifying missing wealth data
wealth_miss4 <- which(is.na(std_absw_matrix4),arr.ind = T)
#check data
wealth_miss4

#replace NAs with -99
for(j in 1:ncol(std_absw_matrix4)){
  for(i in 1:nrow(std_absw_matrix4)){
    if(is.na(std_absw_matrix4[i,j])){
      std_absw_matrix4[i,j] <- -99
    } else{
      std_absw_matrix4[i,j] <- std_absw_matrix4[i,j]
    }
  }
}
#check the data
std_absw_matrix4

#Subset the data for realistic ages
#Subset wealth and AFB for those between zero years old and 50 years old.
#wealth
std_absw_restricted <- std_absw_matrix4[,1:51] #Adding 1, since first column in the matrix is year 0
#AFB
afrs_restricted <- afr_matrix4[,1:51] #Adding 1, since first column in the matrix is year 0
#missing wealth data
wealth_miss_restricted <- wealth_miss4[wealth_miss4[,2] <= 51,] #Adding 1, since first column in the matrix is year 0

#put all the data together
#create dataset
real_list4 <- list(N = nrow(afrs_restricted), #population size
                   A = ncol(afrs_restricted), #age
                   wealth = std_absw_restricted, #absolute wealth
                   baby = afrs_restricted, #AFR
                   N_miss = nrow(wealth_miss_restricted), # number of missing values that need imputation
                   wealth_miss=wealth_miss_restricted) # matrix indicating missing wealth data
#check data
real_list4

# compile model

m4_add <- cmdstan_model("Model_code/firstbaby_cmv.stan")

#fit model
fit4_add_real <- m4_add$sample(data = real_list4, 
                               chains = 4, 
                               parallel_chains = 15, 
                               adapt_delta = 0.95,
                               max_treedepth = 13,
                               init = 0)


# save fit 
fit_4_add_real <- rstan::read_stan_csv(fit4_add_real$output_files())
saveRDS(fit_4_add_real, "firstbaby4_add_real.rds")
#load RDS file
rds4_add_real <- readRDS("firstbaby4_add_real.rds")
#extract samples
post4_add_real <- extract.samples(rds4_add_real)

#check the model
#check trace of all main parameters
#alpha
rstan::traceplot(rds4_add_real,pars="alpha")
#mu
#traceplot(rds4_add_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds4_add_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
rstan::traceplot(rds4_add_real,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds4_add_real,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds4_add_real,pars="mu_delta")
#delta_wealth
#traceplot(rds4_add_real,pars="delta_wealth") #only run if needed, because they are 91 plots
#delta_wealth
#traceplot(rds4_add_real,pars="delta_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab4_add_real <- precis(rds4_add_real,depth=2,pars=c("alpha",
                                                     "mu_raw",
                                                     "mu_tau",
                                                     "mu_delta"))
#check table
tab4_add_real
#create summary table for mu
tab4_add_real_mu <- precis(rds4_add_real,depth=2,pars="mu")
#check table
tab4_add_real_mu
plot(tab4_add_real_mu)
#create summary table for delta_z
tab4_add_real_delta_z <- precis(rds4_add_real,depth=2,pars="delta_wealth_z")
#check table
tab4_add_real_delta_z
plot(tab4_add_real_delta_z)
#create summary table for delta_sigma
tab4_add_real_delta_sigma <- precis(rds4_add_real,depth=2,pars="delta_wealth_sigma")
#check table
tab4_add_real_delta_sigma
#plot(tab4_add_real_delta_sigma)

# # To present the results, it will help to convert them to the actual probability scale (estimated mu values are on logit scale)
# #mu
# tab4_add_real_mu[,1]<-round(inv_logit(tab4_add_real_mu[,1]),3)
# tab4_add_real_mu[,3]<-round(inv_logit(tab4_add_real_mu[,3]),3)
# tab4_add_real_mu[,4]<-round(inv_logit(tab4_add_real_mu[,4]),3)
# #delta_wealth
# tab4_add_real_delta[,1]<-round(inv_logit(tab4_add_real_delta[,1]),3)
# tab4_add_real_delta[,3]<-round(inv_logit(tab4_add_real_delta[,3]),3)
# tab4_add_real_delta[,4]<-round(inv_logit(tab4_add_real_delta[,4]),3)

## Plot the fit of the real data ----

### Long-term variability of wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth_msd_real <- seq(from=round(min(post4_add_real$wealth_msd),1),to=round(max(post4_add_real$wealth_msd),1),length.out=nrow(std_absw_matrix4)) #specify according to range and length related to sample size
simwealth_msd_real
#get the deciles
deciles <- as.numeric(quantile(simwealth_msd_real,seq(0,1,0.5)))
deciles

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_b<-palette[7:(length(deciles)+6)]
palette_b

#set parameters for a legend outside of the plot
par(mfrow=c(1,1),xpd=T,mar=c(5,5,4,8))

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Model with long-term variability",
     cex.axis=1.2,
     cex.lab=1.5,
     cex.main=1.5,
     type="n")
legend(55,1,c("No var.","Mid. var.", "Max. var."),lty=1,col=palette_b,lwd=2,pch=16)

#add lines
for(k in 1:(length(deciles))){
  #create matrix to store the data
  p4_add_real_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
  p4_add_real_b
  #fill it in with values for age 25
  for(j in 1:ncol(post4_add_real$mu)){
    for(i in 1:nrow(post4_add_real$mu)){
      p4_add_real_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                        post4_add_real$mu[i,j] + #age
                                        (post4_add_real$delta_wealth_z[i,j]*post4_add_real$delta_wealth_sigma[i])*deciles[k]) #moving variance
    }
  }
  #check data
  p4_add_real_b
  #plot it!
  #prepare model prediction data
  plot_data4_add_real_b <- data.frame(age = 1:ncol(p4_add_real_b),
                                      mean = apply(p4_add_real_b, 2, mean), 
                                      upp = apply(p4_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p4_add_real_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr4 <- afr_matrix4
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
  
  points(cumprod(1-plot_data4_add_real_b$mean)~plot_data4_add_real_b$age,col=palette_b[k],pch=15)
  lines(cumprod(1-plot_data4_add_real_b$mean)~plot_data4_add_real_b$age,col=palette_b[k],lwd=2)
  polygon(c(plot_data4_add_real_b$age,rev(plot_data4_add_real_b$age)),c(cumprod(1-plot_data4_add_real_b$low),rev(cumprod(1-plot_data4_add_real_b$upp))),col=alpha(palette_b[k],0.25),border=NA)
}

### De-couple plot by min, mean, max ----

#simulate wealth values
simwealth_msd_real <- seq(from=round(min(post4_add_real$wealth_msd),1),to=round(max(post4_add_real$wealth_msd),1),length.out=nrow(std_absw_matrix4)) #specify according to range and length related to sample size
simwealth_msd_real

#get the deciles
deciles <- as.numeric(quantile(simwealth_msd_real,seq(0,1,0.5)))
deciles

#colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_b<-palette[7:(length(deciles)+6)]
palette_b

#define layout of plots
par(mfrow=c(1,3),xpd=T,mar=c(5,5,4,8))

#### No long-term variability of wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="No long-term variability of wealth",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p4_add_real_0_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_0_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                      post4_add_real$mu[i,j] + #age
                                      (post4_add_real$delta_wealth_z[i,j]*post4_add_real$delta_wealth_sigma[i,j])*deciles[1]) #moving variance
  }
}
#check data
p4_add_real_0_b
#plot it!
#prepare model prediction data
plot_data4_add_real_0_b <- data.frame(age = 1:ncol(p4_add_real_0_b),
                                      mean = apply(p4_add_real_0_b, 2, mean), 
                                      upp = apply(p4_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                      low = apply(p4_add_real_0_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix4
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

points(cumprod(1-plot_data4_add_real_0_b$mean)~plot_data4_add_real_0_b$age,col=palette_b[1],pch=15)
lines(cumprod(1-plot_data4_add_real_0_b$mean)~plot_data4_add_real_0_b$age,col=palette_b[1],lwd=2)
polygon(c(plot_data4_add_real_0_b$age,rev(plot_data4_add_real_0_b$age)),c(cumprod(1-plot_data4_add_real_0_b$low),rev(cumprod(1-plot_data4_add_real_0_b$upp))),col=alpha(palette_b[1],0.25),border=NA)

#### Middle long-term variability of wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Middle long-term variability of wealth",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p4_add_real_50_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_50_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                      post4_add_real$mu[i,j] + #age
                                      (post4_add_real$delta_wealth_z[i,j]*post4_add_real$delta_wealth_sigma[i])*deciles[2]) #moving variance
  }
}
#check data
p4_add_real_50_b
#plot it!
#prepare model prediction data
plot_data4_add_real_50_b <- data.frame(age = 1:ncol(p4_add_real_50_b),
                                       mean = apply(p4_add_real_50_b, 2, mean), 
                                       upp = apply(p4_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                       low = apply(p4_add_real_50_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix4
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

points(cumprod(1-plot_data4_add_real_50_b$mean)~plot_data4_add_real_50_b$age,col=palette_b[2],pch=15)
lines(cumprod(1-plot_data4_add_real_50_b$mean)~plot_data4_add_real_50_b$age,col=palette_b[2],lwd=2)
polygon(c(plot_data4_add_real_50_b$age,rev(plot_data4_add_real_50_b$age)),c(cumprod(1-plot_data4_add_real_50_b$low),rev(cumprod(1-plot_data4_add_real_50_b$upp))),col=alpha(palette_b[2],0.25),border=NA)

#### Maximum long-term variability of wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post4_add_real$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Maximum long-term variability of wealth",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p4_add_real_100_b <- matrix(nrow=nrow(post4_add_real$mu),ncol=ncol(post4_add_real$mu))
p4_add_real_100_b
#fill it in with values for age 25
for(j in 1:ncol(post4_add_real$mu)){
  for(i in 1:nrow(post4_add_real$mu)){
    p4_add_real_b[i,j] <- inv_logit(post4_add_real$alpha[i] + #inv logit because originally is logit
                                      post4_add_real$mu[i,j] + #age
                                      (post4_add_real$delta_wealth_z[i,j]*post4_add_real$delta_wealth_sigma[i])*deciles[3]) #moving variance
  }
}
#check data
p4_add_real_100_b
#plot it!
#prepare model prediction data
plot_data4_add_real_100_b <- data.frame(age = 1:ncol(p4_add_real_100_b),
                                        mean = apply(p4_add_real_100_b, 2, mean), 
                                        upp = apply(p4_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                        low = apply(p4_add_real_100_b, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr4 <- afr_matrix4
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

points(cumprod(1-plot_data4_add_real_100_b$mean)~plot_data4_add_real_100_b$age,col=palette_b[3],pch=15)
lines(cumprod(1-plot_data4_add_real_100_b$mean)~plot_data4_add_real_100_b$age,col=palette_b[3],lwd=2)
polygon(c(plot_data4_add_real_100_b$age,rev(plot_data4_add_real_100_b$age)),c(cumprod(1-plot_data4_add_real_100_b$low),rev(cumprod(1-plot_data4_add_real_100_b$upp))),col=alpha(palette_b[3],0.25),border=NA)

legend(55,1,c("No var.","Mid. var.", "Max. var."),lty=1,col=palette_b,lwd=2,pch=16)
