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
beta_abswealth<-c(rep(0,10),seq(from=-0.1,to=0.1,length=32),rep(0,49))

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
gamma_diffwealth<-c(rep(0,10),seq(from=0.1,to=-0.1,length=32),rep(0,49))

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
tab3 <- precis(rds3,depth=3,pars=c("alpha",
                                   "mu_raw",
                                   "mu_tau",
                                   "mu_delta"))
#check table
tab3
#create summary table for mu
tab3_mu <- precis(rds3,depth=3,pars="mu")
#check table
tab3_mu
#create summary table for beta
tab3_beta <- precis(rds3,depth=3,pars="beta_wealth")
#check table
tab3_beta
#create summary table for gamma
tab3_gamma <- precis(rds3,depth=3,pars="gamma_wealth")
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

real_data3 <- read.csv("real_data3.csv")[,-1]
head(real_data3)

#Calculate age-specific age at first reproduction ----

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
