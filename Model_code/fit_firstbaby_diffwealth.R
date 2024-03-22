## Model with Gaussian process of age, absolute and difference of material wealth ----

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
abswealth <- matrix(nrow=N,ncol=A+1)
#randomly assign an amount of wealth for each individual at age 0
abswealth[,1] <- rnorm(100,15,5)
#change wealth of individuals through time
for(j in 2:ncol(abswealth)){
  for(i in 1:nrow(abswealth)){
    abswealth[i,j] <- abswealth[i,j-1]+rnorm(1,0,0.25)
  }
}
#check the data
#see the data
head(abswealth)
#check the age-specific absolute wealth
colMeans(as.data.frame(abswealth))
#plot it
plot(colMeans(as.data.frame(abswealth)),xlab="Age",ylab="Average absolute wealth")

#standardise wealth data
#create a matrix to store the standardised data
std_abswealth <- matrix(nrow=nrow(abswealth),ncol=ncol(abswealth))
#standardize wealth data per column
for(j in 1:ncol(std_abswealth)){
  std_abswealth[,j] <- standardize(abswealth[,j])
}
#check the data
std_wealth

#simulate an age-specific parameter for wealth
beta_abswealth<-c(rep(0,10),seq(from=-0.1,to=0.1,length=22),rep(0,49))

#Difference of wealth
#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
diffwealth <- matrix(nrow=N,ncol=A+1)
#assign zero change of wealth at birth
diffwealth[,1] <- 0
#randomly assign an amount of wealth for each individual and age
for(j in 2:ncol(diffwealth)){
  for(i in 1:nrow(diffwealth)){
    diffwealth[i,j] <- std_abswealth[i,j] - std_abswealth[i,j-1]
  }
}
#check the data
#see the data
head(diffwealth)
#check the age-specific absolute wealth
colMeans(as.data.frame(diffwealth))
#plot it
plot(colMeans(as.data.frame(diffwealth)),xlab="Age",ylab="Average wealth variability")

#simulate an age-specific parameter for wealth variability
gamma_diffwealth<-c(rep(0,10),seq(from=0.1,to=-0.1,length=22),rep(0,49))

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
        beta_abswealth[j]*abswealth[i,j]+
        gamma_diffwealth[j]*diffwealth[i,j]
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
#check the age-specific frequency of FR
colSums(as.data.frame(afrs))/100
#plot it
plot(colSums(as.data.frame(afrs))/100,xlab="Age",ylab="Prob FR")

#put all the data together
#create data
data3 <- list(N = N, #population size
              A = A+1, #age
              abswealth = std_abswealth, #standardised absolute wealth
              diffwealth = diffwealth, #wealth variability
              baby = afrs) #AFR
#check data
data3

# compile model

m3 <- cmdstan_model("c:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/Model_code/firstbaby_diffwealth.stan")

# fit model

fit3 <- m3$sample(data = data3, 
                  chains = 4, 
                  parallel_chains = 4, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)

# save fit 
fit_3 <- rstan::read_stan_csv(fit3$output_files())
saveRDS(fit_3, "firstbaby3.rds")
#load RDS file
rds3 <- readRDS("firstbaby3.rds")
#extract samples
post3 <- extract.samples(rds3)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds3,pars="alpha")
#mu
#traceplot(rds3,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds3,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds3,pars="mu_tau")
#mu_kappa
traceplot(rds3,pars="mu_kappa")
#mu_delta
traceplot(rds3,pars="mu_delta")
#beta_wealth
#traceplot(rds3,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds3,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab3 <- precis(rds3,depth=2,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tab3
#create summary table for mu
tab3_mu <- precis(rds3,depth=2,pars="mu")
#check table
tab3_mu
#create summary table for beta
tab3_beta <- precis(rds3,depth=2,pars="beta_wealth")
#check table
tab3_beta
#create summary table for gamma
tab3_gamma <- precis(rds3,depth=2,pars="gamma_wealth")
#check table
tab3_gamma

#compute probability of FR at each age
#simulate wealth values
range(post3$gamma_wealth)
simwealth_g <- seq(from=-2.5,to=2.5,length.out=nrow(std_wealth)) #specify according to range and length according to sample size
simwealth_g
#create matrix to store the data
p3 <- matrix(nrow=nrow(post3$mu),ncol=length(simwealth_g))
p3
#fill it in with values for age 25
for(j in 1:length(simwealth_g)){
  for(i in 1:nrow(post3$mu)){
    p[i,j] <- inv_logit(post3$alpha[i] + #inv logit because originally is logit
                          post3$mu[i,25] +
                          post3$beta_wealth[i,25]*0 + #zero because that is the average from the standardization
                          post3$beta_wealth[i,25]*simwealth_g[j] 
    ) 
  }
}
#check data
p3

#plot it!
#prepare model prediction data
plot_data3 <- data.frame(wealth = simwealth_g,
                         mean = apply(p3, 2, mean), 
                         upp = apply(p3, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                         low = apply(p3, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#plot wealth and probability of first reproduction
plot(plot_data3$mean~plot_data2$wealth,
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability",
     type="l")
lines(plot_data3$upp~plot_data2$wealth,col="red")
lines(plot_data3$low~plot_data2$wealth,col="blue")
points(colSums(as.data.frame(afrs))/100~plot_data3$wealth,col="gold",pch=16)
#plot the simulated gammas with the ones from the model
plot(apply(post3$gamma_wealth,2,mean)~gamma_wealth)

#With real data ----

#Load data

real_data3 <- read.csv("~/wealth_afr/dataf.csv")[,-1]
head(real_data3)

### Calculate age-specific age at first reproduction ----

#create a matrix to store the age-specific age of censor
afr_matrix3 <- matrix(nrow=nrow(real_data3),ncol=91)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix3)){
  afr <- real_data3$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data3$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix3[i,1:(afr-1)] <- rep(0,length(afr_matrix3[i,1:(afr-1)]))
    afr_matrix3[i,afr] <- 1
  } else{
    afr_matrix3[i,1:aoc] <- rep(0,length(afr_matrix3[i,1:aoc]))
  }
}
#check the data
afr_matrix3
#check the age-specific probability of FR
colSums(as.data.frame(afr_matrix3),na.rm=T)/100
#plot it
plot(colSums(as.data.frame(afr_matrix3),na.rm = T)/100~c(1:91),xlab="Age",ylab="Probability of first reproduction")

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

#### Age-specific absolute wealth ----

#age-specific absolute wealth
#create matrix to store the age-specific amount of wealth
absw_matrix3 <- matrix(nrow = nrow(real_data3),ncol=91)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix3)){
  absw <- real_data3$absw95[i]
  age_absw <- real_data3$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
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
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
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
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
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
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
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
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
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
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
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
  if(!is.na(age_absw)){
    absw_matrix3[i,age_absw] <- absw
  } else{
    absw_matrix3[i,age_absw] <- NA
  }
}
#check data
absw_matrix3
#check the age-specific frequency of absolute wealth
colMeans(as.data.frame(absw_matrix3),na.rm=T)
#plot it
plot(colMeans(as.data.frame(absw_matrix3),na.rm = T)~c(1:91),xlab="Age",ylab="Average absolute wealth")

#NaN in columns where there are no values of wealth

#####Simple data imputation ----

#replace the wealth of a woman at birth (column 1) by the average of that age, if they do not have wealth at age 1 (column 2)
for(i in 1:length(absw_matrix3[,1])){
  if(is.na(absw_matrix3[i,1]) & is.na(absw_matrix3[i,2])){
    absw_matrix3[i,1] <- mean(absw_matrix3[,1],na.rm = T)
  }else if(is.na(absw_matrix3[i,1]) & !is.na(absw_matrix3[i,2])){
    absw_matrix3[i,1] <- absw_matrix3[i,2]
  }
}
#check the data
absw_matrix3
sum(is.na(absw_matrix3[,1]))
#n=0
#replace the missing wealth data by putting the mean between the two ages it is
for(j in 2:(ncol(absw_matrix3)-1)){
  for(i in 1:nrow(absw_matrix3)){
    if(is.na(absw_matrix3[i,j])&!is.na(absw_matrix3[i,j-1])&!is.na(absw_matrix3[i,j+1])){
      absw_matrix3[i,j] <- mean(c(absw_matrix3[i,j-1],absw_matrix3[i,j+1]))
    } else if(is.na(absw_matrix3[i,j])&!is.na(absw_matrix3[i,j-1])&is.na(absw_matrix3[i,j+1])){
      absw_matrix3[i,j] <- mean(absw_matrix3[,j],na.rm=T)
    } 
  }
}
#check the data
absw_matrix3
sum(is.na(absw_matrix3))
#n=21824
#replace the missing wealth data by putting the mean between the two ages it is
for(j in 2:(ncol(absw_matrix3)-1)){
  for(i in 1:nrow(absw_matrix3)){
    if(is.na(absw_matrix3[i,j])&!is.na(absw_matrix3[i,j-1])&!is.na(absw_matrix3[i,j+1])){
      absw_matrix3[i,j] <- mean(c(absw_matrix3[i,j-1],absw_matrix3[i,j+1]))
    } else if(is.na(absw_matrix3[i,j])&!is.na(absw_matrix3[i,j-1])&is.na(absw_matrix3[i,j+1])){
      absw_matrix3[i,j] <- mean(absw_matrix3[,j],na.rm=T)
    } 
  }
}
#check the data
absw_matrix3
sum(is.na(absw_matrix3))
#n=9179
#replace the missing wealth data by putting either the mean between the two ages it is or by repeating the value from previous year
for(j in 2:(ncol(absw_matrix3)-1)){
  for(i in 1:nrow(absw_matrix3)){
    if(is.na(absw_matrix3[i,j])&length(is.na(absw_matrix3[i,j:91]))!=sum(is.na(absw_matrix3[i,j:91]))){
      absw_matrix3[i,j] <- mean(c(absw_matrix3[i,j-1],absw_matrix3[i,max(which(!is.na(absw_matrix3[i,])==T))]))
    } else if(is.na(absw_matrix3[i,j])&length(is.na(absw_matrix3[i,j:91]))==sum(is.na(absw_matrix3[i,j:91]))){
      absw_matrix3[i,j] <- absw_matrix3[i,j-1]
    }
  }
}
#check the data
absw_matrix3
sum(is.na(absw_matrix3))
#n=540
#replace last column with the values from last year
absw_matrix3[,max(ncol(absw_matrix3))] <- absw_matrix3[,max(ncol(absw_matrix3))-1]
#check the data
absw_matrix3
sum(is.na(absw_matrix3))
#n=0
#check the age-specific frequency of absolute wealth
apply(absw_matrix3,2,mean)
#plot it
plot(apply(absw_matrix3,2,mean)~c(1:91),xlab="Age",ylab="Average absolute wealth")


#standardise absolute wealth per column
#create a matrix
std_absw_matrix3 <- matrix(nrow=nrow(absw_matrix3),ncol=ncol(absw_matrix3))
#standardize wealth data per column
for(j in 1:ncol(std_absw_matrix3)){
  std_absw_matrix3[,j] <- standardize(absw_matrix3[,j])
}
#check data
std_absw_matrix3
#replace NaN with zero...not sure is right, though...probably will change with bayesian imputation
for(j in 1:ncol(std_absw_matrix3)){
  for(i in 1:nrow(std_absw_matrix3)){
    if(is.na(std_absw_matrix3[i,j])){
      std_absw_matrix3[i,j] <- 0
    } else{
      std_absw_matrix3[i,j] <- std_absw_matrix3[i,j]
    }
  }
}
#check the data
std_absw_matrix3
#check the age-specific frequency of absolute wealth
apply(std_absw_matrix3,2,mean)
#plot it
plot(apply(std_absw_matrix3,2,mean)~c(1:91),xlab="Age",ylab="Average absolute wealth")

#### Age-specific change in wealth ----

#age-specific change in wealth
#create matrix to store the age-specific wealth variation
diffw_matrix3 <- matrix(nrow = nrow(real_data3),ncol=91)
#calculate the age-specific wealth variation
for(j in 1:ncol(diffw_matrix3)){
  for(i in 1:nrow(diffw_matrix3)){
    if(j ==1){
      diffw_matrix3[i,j] <- std_absw_matrix3[i,j] - std_absw_matrix3[i,j]
    } else{
      diffw_matrix3[i,j] <- std_absw_matrix3[i,j] - std_absw_matrix3[i,j-1]
    }
  }
}
#check data
diffw_matrix3
sum(is.na(diffw_matrix3))
#n=0
#check the age-specific frequency of wealth variability
apply(diffw_matrix3,2,mean)
#plot it
plot(apply(diffw_matrix3,2,mean)~c(1:91),xlab="Age",ylab="Wealth variability")


#### fit model ----

#put all the data together
#create data
real_list3 <- list(N = nrow(real_data3), #population size
              A = ncol(afr_matrix3), #age
              abswealth = std_absw_matrix3, #standardised absolute wealth
              diffwealth = diffw_matrix3, #wealth variability
              baby = afr_matrix3) #AFR
#check data
real_list3

fit3_real <- m3$sample(data = real_list3, 
                  chains = 4, 
                  parallel_chains = 4, 
                  adapt_delta = 0.95,
                  max_treedepth = 13,
                  init = 0)


# save fit 
fit_3_real <- rstan::read_stan_csv(fit3_real$output_files())
saveRDS(fit_3_real, "firstbaby3_real.rds")
#load RDS file
rds3_real <- readRDS("firstbaby3_real.rds")
#extract samples
post3_real <- extract.samples(rds3_real)

#check the model
#check trace of all main parameters
#alpha
traceplot(rds3_real,pars="alpha")
#mu
#traceplot(rds3_real,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds3_real,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
traceplot(rds3_real,pars="mu_tau")
#mu_kappa
traceplot(rds3_real,pars="mu_kappa")
#mu_delta
traceplot(rds3_real,pars="mu_delta")
#beta_wealth
#traceplot(rds3_real,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds3_real,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tab3_real <- precis(rds3_real,depth=2,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tab3_real
#create summary table for mu
tab3_real_mu <- precis(rds3_real,depth=2,pars="mu")
#check table
tab3_real_mu
#create summary table for beta
tab3_real_beta <- precis(rds3_real,depth=2,pars="beta_wealth")
#check table
tab3_real_beta
#create summary table for gamma
tab3_real_gamma <- precis(rds3_real,depth=2,pars="gamma_wealth")
#check table
tab3_real_gamma

#compute probability of FR at each age
#simulate wealth values
range(post3_real$gamma_wealth)
simwealth_g_real <- seq(from=round(min(range(post3_real$gamma_wealth)),1),to=round(max(range(post3_real$gamma_wealth)),1),length.out=nrow(std_absw_matrix3)) #specify according to range and length according to sample size
simwealth_g_real
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g))
p3_real
#fill it in with values for age 25
for(j in 1:length(simwealth_g)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                          post3_real$mu[i,25] +
                          post3_real$beta_wealth[i,25]*0 + #zero because that is the average from the standardization
                          post3_real$gamma_wealth[i,25]*simwealth_g[j] 
    ) 
  }
}
#check data
p3_real

#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g,
                         mean = apply(p3_real, 2, mean), 
                         upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                         low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#plot wealth and probability of first reproduction
plot(plot_data3_real$mean~plot_data3_real$wealth,
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Wealth variability",
     main="Model with wealth variability",
     type="l")
lines(plot_data3_real$upp~plot_data3_real$wealth,col="red")
lines(plot_data3_real$low~plot_data3_real$wealth,col="blue")
#points(afr_matrix3[,25]/100~plot_data3_real$wealth,col="gold",pch=16)
#plot the simulated gammas with the ones from the model
plot(apply(post3_real$gamma_wealth,2,mean)~gamma_wealth)

#plot wealth and probability of first reproduction ----
#colour palette
palette <- c(rep(NA,14),hcl.colors(11,"Spectral"))
#plot empty plot
plot(c(0,1)~c(-5,5),
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Standardised wealth variability",
     main="Model with wealth variability",
     type="n")
#add lines
for(k in seq(15,25,by=1)){
  #create matrix to store the data
  p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
  p3_real
  #fill it in with values for age 35
  for(j in 1:length(simwealth_g_real)){
    for(i in 1:nrow(post3_real$mu)){
      p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                  post3_real$mu[i,k] +
                                  post3_real$beta_wealth[i,k]*0 + #zero because that is the average from the standardization
                                  post3_real$gamma_wealth[i,k]*simwealth_g[j])
    }
  }
  #check data
  p3_real
  #plot it!
  #prepare model prediction data
  plot_data3_real <- data.frame(wealth = simwealth_g_real,
                                mean = apply(p3_real, 2, mean), 
                                upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
  
  lines(plot_data3_real$mean~plot_data3_real$wealth,col=palette[k])
}

#plot wealth and probability of first reproduction at age 10 ----
plot(c(0,1)~c(-5,5),
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 10",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 10
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,10] +
                                post3_real$beta_wealth[i,10]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,10]*simwealth_g[j])
  }
}
#chec10 data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
#chec10 the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=hcl.colors(35,"Zissou 1")[10])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=rainbow(35)[10],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=rainbow(35)[10],lty=2)
points(plot_afr3[,10]~plot_data3_real$wealth,col=alpha(hcl.colors(35,"Zissou 1")[10],0.3),pch=16)

#plot wealth and probability of first reproduction at age 15 ----
plot(c(0,1)~c(-5,5),
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 15",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 15
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,15] +
                                post3_real$beta_wealth[i,15]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,15]*simwealth_g[j])
  }
}
#chec15 data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
#chec15 the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=hcl.colors(35,"Zissou 1")[15])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=rainbow(35)[15],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=rainbow(35)[15],lty=2)
points(plot_afr3[,15]~plot_data3_real$wealth,col=alpha(hcl.colors(35,"Zissou 1")[15],0.3),pch=16)

#plot wealth and probability of first reproduction at age 20 ----
plot(c(0,1)~c(-5,5),
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 20",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 20
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,20] +
                                post3_real$beta_wealth[i,20]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,20]*simwealth_g[j])
  }
}
#chec30 data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
#chec30 the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=hcl.colors(35,"Zissou 1")[20])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=rainbow(35)[20],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=rainbow(35)[20],lty=2)
points(plot_afr3[,30]~plot_data3_real$wealth,col=alpha(hcl.colors(35,"Zissou 1")[20],0.3),pch=16)

#plot wealth and probability of first reproduction at age 25 ----
plot(c(0,1)~c(-5,5),
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 25",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 35
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,25] +
                                post3_real$beta_wealth[i,25]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,25]*simwealth_g[j])
  }
}
#chec35 data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
#chec35 the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=hcl.colors(35,"Zissou 1")[25])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=rainbow(35)[25],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=rainbow(35)[25],lty=2)
points(plot_afr3[,35]~plot_data3_real$wealth,col=alpha(hcl.colors(25,"Zissou 1")[25],0.3),pch=16)

#plot wealth and probability of first reproduction at age 30 ----
plot(c(0,1)~c(-5,5),
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 30",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 30
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,30] +
                                post3_real$beta_wealth[i,30]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,30]*simwealth_g[j])
  }
}
#chec30 data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
#chec30 the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=hcl.colors(35,"Zissou 1")[30])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=rainbow(35)[30],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=rainbow(35)[30],lty=2)
points(plot_afr3[,30]~plot_data3_real$wealth,col=alpha(hcl.colors(35,"Zissou 1")[30],0.3),pch=16)

#plot wealth and probability of first reproduction at age 35 ----
plot(c(0,1)~c(-5,5),
     ylim=c(0,1),
     ylab="Prob. FR",
     xlab="Wealth",
     main="Model with absolute wealth at age 35",
     type="n")
#create matrix to store the data
p3_real <- matrix(nrow=nrow(post3_real$mu),ncol=length(simwealth_g_real))
p3_real
#fill it in with values for age 35
for(j in 1:length(simwealth_g_real)){
  for(i in 1:nrow(post3_real$mu)){
    p3_real[i,j] <- inv_logit(post3_real$alpha[i] + #inv logit because originally is logit
                                post3_real$mu[i,35] +
                                post3_real$beta_wealth[i,35]*0 + #zero because that is the average from the standardization
                                post3_real$gamma_wealth[i,35]*simwealth_g[j])
  }
}
#check data
p3_real
#plot it!
#prepare model prediction data
plot_data3_real <- data.frame(wealth = simwealth_g_real,
                              mean = apply(p3_real, 2, mean), 
                              upp = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                              low = apply(p3_real, 2, function(x) HPDI(x, prob = 0.9))[2, ]
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
#chec35 the data
plot_afr3

lines(plot_data3_real$mean~plot_data3_real$wealth,col=hcl.colors(35,"Zissou 1")[35])
lines(plot_data3_real$upp~plot_data3_real$wealth,col=rainbow(35)[35],lty=2)
lines(plot_data3_real$low~plot_data3_real$wealth,col=rainbow(35)[35],lty=2)
points(plot_afr3[,35]~plot_data3_real$wealth,col=alpha(hcl.colors(35,"Zissou 1")[35],0.3),pch=16)
