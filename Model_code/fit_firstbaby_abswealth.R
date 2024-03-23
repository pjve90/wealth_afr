
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

### With real data ----

#Load data

real_data2 <- read.csv("~/wealth_afr/dataf.csv")[,-1]
head(real_data2)

#### Age at first reproduction ----

#create a matrix to store the age-specific age of censor
afr_matrix2 <- matrix(nrow=nrow(real_data2),ncol=91)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix2)){
  afr <- real_data2$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data2$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix2[i,1:(afr-1)] <- rep(0,length(afr_matrix2[i,1:(afr-1)]))
    afr_matrix2[i,afr] <- 1
  } else{
    afr_matrix2[i,1:aoc] <- rep(0,length(afr_matrix2[i,1:aoc]))
  }
}
#check the data
afr_matrix2
#check the age-specific probability of FR
colSums(as.data.frame(afr_matrix2),na.rm=T)/100
#plot it
plot(colSums(as.data.frame(afr_matrix2),na.rm = T)/100~c(1:91),xlab="Age",ylab="Probability of first reproduction",ylim=c(0,1))

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

#### Age-specific absolute wealth ----

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix2 <- matrix(nrow = nrow(real_data2),ncol=91)
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
#check the age-specific frequency of absolute wealth
colMeans(as.data.frame(absw_matrix2),na.rm=T)
#plot it
plot(colMeans(as.data.frame(absw_matrix2),na.rm = T)~c(1:91),xlab="Age",ylab="Average absolute wealth")

#NaN in columns where there are no values of wealth

##### Simple data imputation ----

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
#check the age-specific frequency of absolute wealth
apply(std_absw_matrix2,2,mean)
#plot it
plot(apply(std_absw_matrix2,2,mean)~c(1:91),xlab="Age",ylab="Average absolute wealth")

#prepare data for the model

#put all the data together
#create dataset
real_list2 <- list(N = nrow(real_data2), #population size
                   A = ncol(afr_matrix2), #age
                   wealth = std_absw_matrix2, #absolute wealth
                   baby = afr_matrix2) #AFR

# fit model
fit2_real <- m2$sample(data = real_list2, 
                       chains = 4, 
                       parallel_chains = 4, 
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

#compute probability of FR at each age
#simulate wealth values
range(post2_real$beta_wealth)
simwealth_real <- seq(from=round(min(range(post2_real$beta_wealth)),1),to=round(max(range(post2_real$beta_wealth)),1),length.out=nrow(std_absw_matrix2)) #specify according to range and length related to sample size
simwealth_real
#create matrix to store the data
p2_real <- matrix(nrow=nrow(post2_real$mu),ncol=length(simwealth_real))
p2_real
#fill it in with values for age 25
for(j in 1:length(simwealth_real)){
  for(i in 1:nrow(post2_real$mu)){
    p2_real[i,j] <- inv_logit(post2_real$alpha[i] + #inv logit because originally is logit
                           post2_real$mu[i,25] +
                           post2_real$beta_wealth[i,25]*simwealth_real[j]) 
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
#plot wealth and probability of first reproduction ----
#colour palette
palette <- c(rep(NA,14),hcl.colors(11,"Spectral"))
palette[20] <- "black"
#plot empty plot
plot(c(0,1)~c(-5,5),
     ylim=c(0,1),
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
                                  post2_real$mu[i,k] +
                                  post2_real$beta_wealth[i,k]*simwealth_real[j]) 
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

layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,0,10,10,11,11,0),nrow=4,ncol=6,byrow=T))

#plot wealth and probability of first reproduction at age 15 ----
  plot(c(0,1)~c(-5,5),
       ylim=c(0,1),
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
                                  post2_real$mu[i,15] +
                                  post2_real$beta_wealth[i,15]*simwealth_real[j]) 
    }
  }
  #chec15 data
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
  #chec15 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[15])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette,lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette,lty=2)
  points(plot_afr2[,15]~plot_data2_real$wealth,col=alpha(palette[15],0.5),pch=16)

#plot wealth and probability of first reproduction at age 16 ----
  plot(c(0,1)~c(-5,5),
       ylim=c(0,1),
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
                                  post2_real$mu[i,16] +
                                  post2_real$beta_wealth[i,16]*simwealth_real[j]) 
    }
  }
  #chec16 data
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
  #chec16 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[16])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[16],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[16],lty=2)
  points(plot_afr2[,16]~plot_data2_real$wealth,col=alpha(palette[16],0.5),pch=16)

#plot wealth and probability of first reproduction at age 17 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,17] +
                                  post2_real$beta_wealth[i,17]*simwealth_real[j]) 
    }
  }
  #chec17 data
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
  #chec17 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[17])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[17],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[17],lty=2)
  points(plot_afr2[,17]~plot_data2_real$wealth,col=alpha(palette[17],0.5),pch=16)

#plot wealth and probability of first reproduction at age 18 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,18] +
                                  post2_real$beta_wealth[i,18]*simwealth_real[j]) 
    }
  }
  #chec18 data
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
  #chec18 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[18])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[18],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[18],lty=2)
  points(plot_afr2[,18]~plot_data2_real$wealth,col=alpha(palette[18],0.5),pch=16)

#plot wealth and probability of first reproduction at age 19 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,19] +
                                  post2_real$beta_wealth[i,19]*simwealth_real[j]) 
    }
  }
  #chec19 data
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
  #chec19 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[19])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[19],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[19],lty=2)
  points(plot_afr2[,19]~plot_data2_real$wealth,col=alpha(palette[19],0.5),pch=16)

#plot wealth and probability of first reproduction at age 20 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,20] +
                                  post2_real$beta_wealth[i,20]*simwealth_real[j]) 
    }
  }
  #chec20 data
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
  #chec20 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[20])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[20],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[20],lty=2)
  points(plot_afr2[,20]~plot_data2_real$wealth,col=alpha(palette[20],0.5),pch=16)

  #plot wealth and probability of first reproduction at age 21 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,21] +
                                  post2_real$beta_wealth[i,21]*simwealth_real[j]) 
    }
  }
  #chec21 data
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
  #chec21 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[21])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[21],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[21],lty=2)
  points(plot_afr2[,21]~plot_data2_real$wealth,col=alpha(palette[21],0.5),pch=16)
  
#plot wealth and probability of first reproduction at age 22 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,22] +
                                  post2_real$beta_wealth[i,22]*simwealth_real[j]) 
    }
  }
  #chec22 data
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
  #chec22 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[22])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[22],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[22],lty=2)
  points(plot_afr2[,22]~plot_data2_real$wealth,col=alpha(palette[22],0.5),pch=16)

  
  #plot wealth and probability of first reproduction at age 23 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,23] +
                                  post2_real$beta_wealth[i,23]*simwealth_real[j]) 
    }
  }
  #chec23 data
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
  #chec23 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[23])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[23],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[23],lty=2)
  points(plot_afr2[,23]~plot_data2_real$wealth,col=alpha(palette[23],0.5),pch=16)
  
#plot wealth and probability of first reproduction at age 24 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,24] +
                                  post2_real$beta_wealth[i,24]*simwealth_real[j]) 
    }
  }
  #chec24 data
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
  #chec24 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[24])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[24],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[24],lty=2)
  points(plot_afr2[,24]~plot_data2_real$wealth,col=alpha(palette[24],0.5),pch=16)
  
  #plot wealth and probability of first reproduction at age 25 ----
  plot(c(0,1)~c(-5,5),
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
                                  post2_real$mu[i,25] +
                                  post2_real$beta_wealth[i,25]*simwealth_real[j]) 
    }
  }
  #chec25 data
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
  #chec25 the data
  plot_afr2
  
  lines(plot_data2_real$mean~plot_data2_real$wealth,col=palette[25])
  lines(plot_data2_real$upp~plot_data2_real$wealth,col=palette[25],lty=2)
  lines(plot_data2_real$low~plot_data2_real$wealth,col=palette[25],lty=2)
  points(plot_afr2[,25]~plot_data2_real$wealth,col=alpha(palette[25],0.5),pch=16)
  
  