# Model with all wealth predictors ----

#The code in this script is meant to fit a Bayesian model with welath variability, absolute wealth and a Gaussian process of age, in order to see the relationship between wealth variability and the probability of first reproduction of women.
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

#The script in this section is to create synthetic data that follows the causal relationship between wealth variability and the probability of first reproduction.

#set seed
set.seed(1987)

#Population size
#500 individuals
N <- 500

#Age
#maximum age of 73 years old
A <- 73

### Absolute wealth ----

#### Simulate absolute wealth ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
abswealth <- matrix(nrow=N,ncol=A+1)
#randomly assign an amount of wealth for each individual at age 0
abswealth[,1] <- exp(rnorm(N,4.5,1))
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
apply(abswealth,2,mean)
#plot it
plot(apply(abswealth,2,mean),xlab="Age",ylab="Average absolute wealth",type="h",lwd=2)
points(apply(abswealth,2,mean),pch=16)
hist(abswealth)

#log-transform wealth data
log_abswealth <- matrix(log(as.vector(abswealth)),ncol=ncol(abswealth),nrow=nrow(abswealth))
#check the data
head(log_abswealth)
#standardise wealth data
std_abswealth <- matrix(standardize(as.vector(log_abswealth)),ncol=ncol(abswealth),nrow=nrow(abswealth))
#check the data
head(std_abswealth)
#check the age-specific absolute wealth
apply(std_abswealth,2,mean)
#plot it
plot(apply(std_abswealth,2,mean),xlab="Age",ylab="Average std. absolute wealth",type="h",lwd=2)
points(apply(std_abswealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(std_abswealth)

#### Simulate parameter for absolute wealth ----

#simulate an age-specific parameter for wealth (beta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
beta_wealth<-c(rep(0,13),seq(from=-0.01,to=0.01,length=20),rep(0,41))
beta_wealth
#plot it!
plot(beta_wealth~c(1:length(beta_wealth)),pch=16,xlab="Age",ylab="Beta parameter")
abline(h=0,lty=2)

# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_beta_wealth<-beta_wealth/sd(log_abswealth) 
std_beta_wealth
plot(std_beta_wealth~c(1:length(std_beta_wealth)),pch=16,xlab="Age",ylab="Std. beta parameter")
abline(h=0,lty=2)

### Current absolute wealth change ----

#### Simulate absolute wealth change for each age---- 

#create a matrix with individuals as rows and ages as columns (A+1) so the first column is birth)
diffwealth <- matrix(nrow=N,ncol=A+1)
#calculate the difference of wealth from one year to the next
for(j in 1:ncol(diffwealth)){
  for(i in 1:nrow(diffwealth)){
    if(j ==1){
      diffwealth[i,j] <- abswealth[i,j] - abswealth[i,j]
    } else{
      diffwealth[i,j] <- abswealth[i,j] - abswealth[i,j-1]
    }
  }
}
#check the data
#see the data
head(diffwealth)
#check the age-specific current absolute wealth change
apply(diffwealth,2,mean)
#plot it
plot(apply(diffwealth,2,mean),xlab="Age",ylab="Average current wealth change",type="h",lwd=2)
points(apply(diffwealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(diffwealth)

#get the absolute value of wealth change
abs_diffwealth <- matrix(abs(as.vector(diffwealth)),ncol=ncol(diffwealth),nrow=nrow(diffwealth))
#check data
abs_diffwealth
hist(abs_diffwealth)
#standardise the absolute wealth change
std_diffwealth <- matrix(standardize(as.vector(abs_diffwealth)),ncol=ncol(diffwealth),nrow=nrow(diffwealth))
#check the data
std_diffwealth
#plot it
plot(apply(std_diffwealth,2,mean),xlab="Age",ylab="Average std. current wealth change",type="h",lwd=2)
points(apply(std_diffwealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(std_diffwealth)

#### Simulate parameter for current absolute wealth change ----

#simulate an age-specific parameter for wealth variability
#if seq starts from a positive value and goes to a negative value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
gamma_wealth<-c(rep(0,13),seq(from=0.01,to=-0.01,length=20),rep(0,41))
gamma_wealth
#plot it
plot(gamma_wealth~c(1:length(gamma_wealth)),pch=16,xlab="Age",ylab="Gamma parameter")
abline(h=0,lty=2)

# adjust for the fact that gamma links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_gamma_wealth<-gamma_wealth/sd(abs_diffwealth)
std_gamma_wealth
plot(std_gamma_wealth~c(1:length(std_gamma_wealth)),pch=16,xlab="Age",ylab="Std. gamma parameter")
abline(h=0,lty=2)

### 1-year lagged absolute wealth ----

#### Simulate 1-year lagged absolute wealth ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
lagg1wealth <- matrix(nrow=N,ncol=A+1)
#calculate the 1-year lagged absolute wealth for each age
for(j in 2:ncol(lagg1wealth)){
  for(i in 1:nrow(lagg1wealth)){
    lagg1wealth[i,j] <- std_abswealth[i,j-1] #1-year lagged absolute wealth
  }
}
#check the data
#see the data
head(lagg1wealth)
#check the age-specific absolute wealth
apply(lagg1wealth,2,mean)
#plot it
plot(apply(lagg1wealth,2,mean),xlab="Age",ylab="Average 1-year lagged absolute wealth",type="h",lwd=2)
points(apply(lagg1wealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(lagg1wealth)

#### Simulate parameter for 1-year lagged absolute wealth ----

#simulate an age-specific parameter for 1-year lagged wealth (delta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
delta_wealth<-c(rep(0,13),seq(from=-0.01,to=0.01,length=20),rep(0,41))
delta_wealth
#plot it!
plot(delta_wealth~c(1:length(delta_wealth)),pch=16,xlab="Age",ylab="Delta parameter")
abline(h=0,lty=2)

# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_delta_wealth<-delta_wealth/sd(log_abswealth)
std_delta_wealth
#plot it
plot(std_delta_wealth~c(1:length(std_delta_wealth)),pch=16,xlab="Age",ylab="Std. delta parameter")
abline(h=0,lty=2)

### 1-year lagged absolute wealth change ----

#### Simulate absolute wealth change ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
lagg1diff <- matrix(nrow=N,ncol=A+1)
#calculate the 1-year lagged absolute wealth for each age
for(j in 3:ncol(lagg1diff)){
  for(i in 1:nrow(lagg1diff)){
    lagg1diff[i,j] <- abswealth[i,j] - abswealth[i,j-2] #1-year lagged absolute wealth
  }
}
#check the data
#see the data
head(lagg1diff)
#check the age-specific absolute wealth
apply(lagg1diff,2,mean)
#plot it
plot(apply(lagg1diff,2,mean),xlab="Age",ylab="Average 1-year lagged wealth change",type="h",lwd=2)
points(apply(lagg1diff,2,mean),pch=16)
abline(h=0,lty=2)
hist(lagg1diff)

#get the absolute value of wealth change
abs_lagg1diff <- matrix(abs(as.vector(lagg1diff)),ncol=ncol(lagg1diff),nrow=nrow(lagg1diff))
#check data
abs_lagg1diff
hist(abs_lagg1diff)
#standardise the absolute wealth change
std_lagg1diff <- matrix(standardize(as.vector(abs_lagg1diff)),ncol=ncol(lagg1diff),nrow=nrow(lagg1diff))
#check the data
std_lagg1diff
#plot it
plot(apply(std_lagg1diff,2,mean),xlab="Age",ylab="Average std. current wealth change",type="h",lwd=2)
points(apply(std_lagg1diff,2,mean),pch=16)
abline(h=0,lty=2)
hist(std_lagg1diff)

#### Simulate parameter for 1-year lagged absolute wealth change ----

#simulate an age-specific parameter for 1-year lagged wealth (epsilon)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
epsilon_wealth<-c(rep(0,13),seq(from=0.01,to=-0.01,length=20),rep(0,41))
epsilon_wealth
#plot it!
plot(epsilon_wealth~c(1:length(epsilon_wealth)),pch=16,xlab="Age",ylab="Epsilon parameter")
abline(h=0,lty=2)

# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_epsilon_wealth<-epsilon_wealth/sd(abs_lagg1diff,na.rm = T)
std_epsilon_wealth
#plot it!
plot(std_epsilon_wealth~c(1:length(std_epsilon_wealth)),pch=16,xlab="Age",ylab="Std. epsilon parameter")
abline(h=0,lty=2)

### 2-year lagged absolute wealth ----

#### Simulate absolute wealth ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
lagg2wealth <- matrix(nrow=N,ncol=A+1)
#calculate the 2-year lagged absolute wealth for each age
for(j in 3:ncol(lagg2wealth)){
  for(i in 1:nrow(lagg2wealth)){
    lagg2wealth[i,j] <- std_abswealth[i,j-2] #2-year lagged absolute wealth
  }
}
#check the data
#see the data
head(lagg2wealth)
#check the age-specific absolute wealth
apply(lagg2wealth,2,mean)
#plot it
plot(apply(lagg2wealth,2,mean),xlab="Age",ylab="Average 2-year lagged absolute wealth",type="h",lwd=2)
points(apply(lagg2wealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(lagg2wealth)

#### Simulate parameter for 2-year lagged absolute wealth ----

#simulate an age-specific parameter for 2-year lagged wealth (zeta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
zeta_wealth<-c(rep(0,13),seq(from=-0.01,to=0.01,length=20),rep(0,41))
zeta_wealth
#plot it!
plot(zeta_wealth~c(1:length(zeta_wealth)),pch=16,xlab="Age",ylab="Zeta parameter")
abline(h=0,lty=2)

# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_zeta_wealth<-zeta_wealth/sd(log_abswealth)
std_zeta_wealth
#plot it
plot(std_zeta_wealth~c(1:length(std_zeta_wealth)),pch=16,xlab="Age",ylab="Std. zeta parameter")
abline(h=0,lty=2)

### 2-year lagged absolute wealth change ----

#### Simulate absolute wealth change ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
lagg2diff <- matrix(nrow=N,ncol=A+1)
#calculate the 2-year lagged absolute wealth for each age
for(j in 4:ncol(lagg2diff)){
  for(i in 1:nrow(lagg2diff)){
    lagg2diff[i,j] <- abswealth[i,j] - abswealth[i,j-3] #2-year lagged absolute wealth
  }
}
#check the data
#see the data
head(lagg2diff)
#check the age-specific absolute wealth
apply(lagg2diff,2,mean)
#plot it
plot(apply(lagg2diff,2,mean),xlab="Age",ylab="Average 2-year lagged wealth change",type="h",lty=1.5)
points(apply(lagg2diff,2,mean),pch=16)
abline(h=0,lty=2)
hist(lagg2diff)

#get the absolute value of wealth change
abs_lagg2diff <- matrix(abs(as.vector(lagg2diff)),ncol=ncol(lagg2diff),nrow=nrow(lagg2diff))
#check data
abs_lagg2diff
hist(abs_lagg2diff)
#standardise the absolute wealth change
std_lagg2diff <- matrix(standardize(as.vector(abs_lagg2diff)),ncol=ncol(lagg2diff),nrow=nrow(lagg2diff))
#check the data
std_lagg2diff
#plot it
plot(apply(std_lagg2diff,2,mean),xlab="Age",ylab="Average std. current wealth change",type="h",lwd=2)
points(apply(std_lagg2diff,2,mean),pch=16)
abline(h=0,lty=2)
hist(std_lagg2diff)

#### Simulate parameter for 2-year lagged absolute wealth change ----

#simulate an age-specific parameter for 2-year lagged wealth change (eta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
eta_wealth<-c(rep(0,13),seq(from=0.01,to=-0.01,length=20),rep(0,41))
eta_wealth
#plot it!
plot(eta_wealth~c(1:length(eta_wealth)),pch=16,xlab="Age",ylab="Eta parameter")
abline(h=0,lty=2)

# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_eta_wealth<-eta_wealth/sd(abs_lagg2diff,na.rm = T)
std_eta_wealth
#plot it
plot(std_eta_wealth~c(1:length(std_eta_wealth)),pch=16,xlab="Age",ylab="Std. eta parameter")
abline(h=0,lty=2)

### 3-year lagged absolute wealth ----

#### Simulate absolute wealth ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
lagg3wealth <- matrix(nrow=N,ncol=A+1)
#calculate the 3-year lagged absolute wealth for each age
for(j in 4:ncol(lagg3wealth)){
  for(i in 1:nrow(lagg3wealth)){
    lagg3wealth[i,j] <- std_abswealth[i,j-3] #3-year lagged absolute wealth
  }
}
#check the data
#see the data
head(lagg3wealth)
#check the age-specific absolute wealth
apply(lagg3wealth,2,mean)
#plot it
plot(apply(lagg3wealth,2,mean),xlab="Age",ylab="Average 3-year lagged absolute wealth",type="h",lwd=2)
points(apply(lagg3wealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(lagg3wealth)

#### Simulate parameter for 3-year lagged absolute wealth ----

#simulate an age-specific parameter for 3-year lagged wealth (theta)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
theta_wealth<-c(rep(0,13),seq(from=-0.01,to=0.01,length=20),rep(0,41))
theta_wealth
#plot it!
plot(theta_wealth~c(1:length(theta_wealth)),pch=16,xlab="Age",ylab="Theta parameter")
abline(h=0,lty=2)

# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_theta_wealth<-theta_wealth/sd(log_abswealth)
std_theta_wealth
plot(std_theta_wealth~c(1:length(std_theta_wealth)),pch=16,xlab="Age",ylab="Std. theta parameter")
abline(h=0,lty=2)

### 3-year lagged absolute wealth change ----

#### Simulate absolute wealth change ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
lagg3diff <- matrix(nrow=N,ncol=A+1)
#calculate the 3-year lagged absolute wealth for each age
for(j in 5:ncol(lagg3diff)){
  for(i in 1:nrow(lagg3diff)){
    lagg3diff[i,j] <- abswealth[i,j] - abswealth[i,j-4] #3-year lagged absolute wealth
  }
}
#check the data
#see the data
head(lagg3diff)
#check the age-specific absolute wealth
apply(lagg3diff,2,mean)
#plot it
plot(apply(lagg3diff,2,mean),xlab="Age",ylab="Average 3-year lagged wealth change",type="h",lwd=2)
points(apply(lagg3diff,2,mean),pch=16)
abline(h=0,lty=2)
hist(lagg3diff)

#get the absolute value of wealth change
abs_lagg3diff <- matrix(abs(as.vector(lagg3diff)),ncol=ncol(lagg3diff),nrow=nrow(lagg3diff))
#check data
abs_lagg3diff
hist(abs_lagg3diff)
#standardise the absolute wealth change
std_lagg3diff <- matrix(standardize(as.vector(abs_lagg3diff)),ncol=ncol(lagg3diff),nrow=nrow(lagg3diff))
#check the data
std_lagg3diff
#plot it
plot(apply(std_lagg3diff,2,mean),xlab="Age",ylab="Average std. current wealth change",type="h",lwd=2)
points(apply(std_lagg3diff,2,mean),pch=16)
abline(h=0,lty=2)
hist(std_lagg3diff)

#### Simulate parameter for 3-year lagged absolute wealth change ----

#simulate an age-specific parameter for 3-year lagged wealth change (iota)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
iota_wealth<-c(rep(0,13),seq(from=0.01,to=-0.01,length=20),rep(0,41))
iota_wealth
#plot it!
plot(iota_wealth~c(1:length(iota_wealth)),pch=16,xlab="Age",ylab="Iota parameter")
abline(h=0,lty=2)

# adjust for the fact that biota links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_iota_wealth<-iota_wealth/sd(abs_lagg3diff,na.rm = T)
std_iota_wealth
plot(std_iota_wealth~c(1:length(std_iota_wealth)),pch=16,xlab="Age",ylab="Std. iota parameter")
abline(h=0,lty=2)

### Cumulative moving average ----

#### Simulate cumulative moving average ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
cmawealth <- matrix(nrow=N,ncol=A+1)
#calculate the 3-year lagged absolute wealth for each age
for(j in 1:ncol(cmawealth)){
  for(i in 1:nrow(cmawealth)){
    cmawealth[i,j] <- mean(abswealth[i,1:j]) #cumulative moving average
  }
}
#check the data
#see the data
head(cmawealth)
#check the age-specific absolute wealth
apply(cmawealth,2,mean)
#plot it
plot(apply(cmawealth,2,mean),xlab="Age",ylab="Average cumulative moving average of wealth",type="h",lwd=2)
points(apply(cmawealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(cmawealth)

#log-transform wealth data
log_cmawealth <- matrix(log(as.vector(cmawealth)),ncol=ncol(cmawealth),nrow=nrow(cmawealth))
#check the data
head(log_cmawealth)
#standardise wealth data
std_cmawealth <- matrix(standardize(as.vector(log_cmawealth)),ncol=ncol(cmawealth),nrow=nrow(cmawealth))
#check the data
head(std_cmawealth)
#check the age-specific cmaolute wealth
apply(std_cmawealth,2,mean)
#plot it
plot(apply(std_cmawealth,2,mean),xlab="Age",ylab="Average std. cmaolute wealth",type="h",lwd=2)
points(apply(std_cmawealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(std_cmawealth)

#### Simulate parameter for cumulative moving average ----

#simulate an age-specific parameter for cumulative moving average (kappa)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
kappa_wealth<-c(rep(0,13),seq(from=-0.01,to=0.01,length=20),rep(0,41))
kappa_wealth
#plot it!
plot(kappa_wealth~c(1:length(kappa_wealth)),pch=16,xlab="Age",ylab="Kappa parameter")
abline(h=0,lty=2)

# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_kappa_wealth<-kappa_wealth/sd(log_cmawealth) 
std_kappa_wealth
#plot it
plot(std_kappa_wealth~c(1:length(std_kappa_wealth)),pch=16,xlab="Age",ylab="Std. kappa parameter")
abline(h=0,lty=2)

### Cumulative moving variance ----

#### Simulate cumulative moving variance ----

#simulate absolute wealth for each age
#create a matrix with individuals as rowas and ages as columns (A+1) so the first column is birth)
cmvwealth <- matrix(nrow=N,ncol=A+1)
#calculate the 3-year lagged absolute wealth for each age
for(j in 1:ncol(cmvwealth)){
  for(i in 1:nrow(cmvwealth)){
    cmvwealth[i,j] <- var(abswealth[i,1:j]) #cumulative moving variance
  }
}
#check the data
#see the data
head(cmvwealth)
#check the age-specific absolute wealth
apply(cmvwealth,2,mean)
#plot it
plot(apply(cmvwealth,2,mean),xlab="Age",ylab="Average cumulative moving variance of wealth",type="h",lwd=2)
points(apply(cmvwealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(cmvwealth)

#log-transform wealth data
log_cmvwealth <- matrix(log(as.vector(cmvwealth)),ncol=ncol(cmvwealth),nrow=nrow(cmvwealth))
#check the data
head(log_cmvwealth)
#standardise wealth data
std_cmvwealth <- matrix(standardize(as.vector(log_cmvwealth)),ncol=ncol(cmvwealth),nrow=nrow(cmvwealth))
#check the data
head(std_cmvwealth)
#check the age-specific cmvolute wealth
apply(std_cmvwealth,2,mean)
#plot it
plot(apply(std_cmvwealth,2,mean),xlab="Age",ylab="Average std. cmvolute wealth",type="h",lwd=2)
points(apply(std_cmvwealth,2,mean),pch=16)
abline(h=0,lty=2)
hist(std_cmvwealth)

#### Simulate parameter for cumulative moving variance ----

#simulate an age-specific parameter for cumulative moving variance (lambda)
#if seq starts from a negative value and goes to a positive value, this means that individuals who have more wealth are less likely to have their first child at younger ages and more likely to have their first child at older ages
lambda_wealth<-c(rep(0,13),seq(from=0.01,to=-0.01,length=20),rep(0,41))
lambda_wealth
#plot it!
plot(lambda_wealth~c(1:length(lambda_wealth)),pch=16,xlab="Age",ylab="Lambda parameter")
abline(h=0,lty=2)

# adjust for the fact that beta links to the standardised values of wealth, so the relative effect is smaller on the standardised scale
std_lambda_wealth<-lambda_wealth/sd(log_cmvwealth, na.rm=T)
std_lambda_wealth
#plot it!
plot(std_lambda_wealth~c(1:length(std_lambda_wealth)),pch=16,xlab="Age",ylab="Std. lambda parameter")
abline(h=0,lty=2)

#Age at first reproduction (AFR) ----

#simulate an age-specific parameter for AFR (mu)
mu_age<-c(rep(0,12),seq(from=0.001,to=0.3,length=9),seq(from=0.14,to=0.01,length=11),seq(from=0.01, to=0.001,length=10),rep(0,32))
length(mu_age)
mu_age
#plot it!
plot(mu_age~c(1:length(mu_age)),ylim=c(0,1),pch=16)
lines(mu_age,pch=16,lwd=2)
plot(cumprod(mu_age)~c(1:length(mu_age)),ylim=c(0,1),pch=16)
lines(cumprod(1-mu_age),lwd=2)

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
          std_beta_wealth[j]*std_abswealth[i,j]+ #current absolute wealth
          std_gamma_wealth[j]*std_diffwealth[i,j]+ #current wealth change
          std_delta_wealth[j]*lagg1wealth[i,j]+ #1-year lagged absolute wealth
          std_epsilon_wealth[j]*std_lagg1diff[i,j]+ #1-year lagged wealth change
          std_zeta_wealth[j]*lagg2wealth[i,j]+ #2-year lagged absolute wealth
          std_eta_wealth[j]*std_lagg2diff[i,j]+ #2-year lagged wealth change
          std_theta_wealth[j]*lagg3wealth[i,j]+ #3-year lagged absolute wealth
          std_iota_wealth[j]*std_lagg3diff[i,j]+ #3-year lagged wealth change
          std_kappa_wealth[j]*std_cmawealth[i,j]+ #cumulative moving average of wealth
          std_lambda_wealth[j]*std_cmvwealth[i,j] #cumulative moving vaiance of wealth
        if(afr_prob<=0){afr_prob<-0}
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
#check the age-specific probability of AFR
apply(afrs,2,sum,na.rm = T)/apply(afrs,2,function(x)sum(!is.na(x)))
#plot it
#color palette
palette <- c(1,3,6,2,4,5)
#plot
plot(cumprod(1-apply(afrs,2,sum,na.rm=T)/apply(afrs,2,function(x)sum(!is.na(x)))),
     ylim=c(0,1),
     xlab="Age",
     ylab="Cumulative probability of first birth",
     col=hcl.colors(length(palette),"temps")[palette[2]],
     pch=15) #data
lines(cumprod(1-apply(afrs,2,sum,na.rm=T)/apply(afrs,2,function(x)sum(!is.na(x)))),col=hcl.colors(length(palette),"temps")[palette[2]],lwd=2)
points(cumprod(1-(mu_age+std_beta_wealth+std_gamma_wealth+std_delta_wealth+std_epsilon_wealth+std_zeta_wealth+std_eta_wealth+std_theta_wealth+std_iota_wealth+std_kappa_wealth+std_lambda_wealth)),col=hcl.colors(length(palette),"temps")[palette[1]],pch=15) #mu+std_beta+std_gamma
lines(cumprod(1-(mu_age+std_beta_wealth+std_gamma_wealth+std_delta_wealth+std_epsilon_wealth+std_zeta_wealth+std_eta_wealth+std_theta_wealth+std_iota_wealth+std_kappa_wealth+std_lambda_wealth)),col=hcl.colors(length(palette),"temps")[palette[1]],lwd=2) #mu+std_beta+std_gamma

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
min(which(apply(afrs,2,sum)>0))
#14
#max
max(which(apply(afrs,2,sum)>0))
#39

#std_abswealth_restricted<-std_abswealth[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]
#afrs_restricted<-afrs[,min(which(apply(afrs,2,sum)>0)):max(which(apply(afrs,2,sum)>0))]

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
data_all <- list(N = nrow(afrs), #population size
              A = ncol(afrs), #age
              wealth = as.vector(t(std_abswealth)), #absolute wealth
              N_miss = sum((std_abswealth)== -99), # number of missing values that need imputation
              id_wealth_miss = which(as.vector(t(std_abswealth))== -99), # provide the indexes for the missing data
              baby = afrs #AFR
) 

#check data
data_all

# compile model

m_all <- cmdstan_model("Model_code/firstbaby_allwealth.stan")

# fit model

fit_all <- m_all$sample(data = data_all, 
                          chains = 4, 
                          parallel_chains = 10, 
                          adapt_delta = 0.95,
                          max_treedepth = 13,
                          init = 0)

# save fit 
fit_all <- rstan::read_stan_csv(fit_all$output_files())
saveRDS(fit_all, "firstbabyall.rds")
#load RDS file
rds_all <- readRDS("firstbabyall.rds")
#extract samples
post_all <- extract.samples(rds_all)

#check the model
#check trace of all main parameters
#alpha
rstan::traceplot(rds_all,pars="alpha")
#mu
#traceplot(rds_all,pars="mu") #only run if needed, because they are 91 plots
#mu_raw
#traceplot(rds_all,pars="mu_raw") #only run if needed, because they are 91 plots
#mu_tau
rstan::traceplot(rds_all,pars="mu_tau")
#mu_kappa
rstan::traceplot(rds_all,pars="mu_kappa")
#mu_delta
rstan::traceplot(rds_all,pars="mu_delta")
#beta_wealth
#traceplot(rds_all,pars="beta_wealth") #only run if needed, because they are 91 plots
#gamma_wealth
#traceplot(rds_all,pars="gamma_wealth") #only run if needed, because they are 91 plots

#summary of the model
#create summary table
#create summary table for alpha and hiper priors of Gaussian process
tabs_all <- precis(rds_all,depth=2,pars=c("alpha",
                                           "mu_raw",
                                           "mu_tau",
                                           "mu_delta"))
#check table
tabs_all
#create summary table for mu
tabs_all_mu <- precis(rds_all,depth=2,pars="mu")
#check table
tabs_all_mu
plot(tabs_all_mu)
plot(cumprod(1-inv_logit(tabs_all_mu[,1])),ylim=c(0,1))
#create summary table for beta
tabs_all_beta <- precis(rds_all,depth=2,pars="beta_wealth")
#check table
tabs_all_beta
plot(tabs_all_beta)
plot(cumprod(1-inv_logit(tabs_all_beta[,1])),ylim=c(0,1))
#create summary table for gamma
tabs_all_gamma <- precis(rds_all,depth=2,pars="gamma_wealth")
#check table
tabs_all_gamma
plot(tabs_all_gamma)
plot(cumprod(1-inv_logit(tabs_all_gamma[,1])),ylim=c(0,1))

## Plot the fit of the simulated data ----

### Absolute wealth ----

#### All wealth classes ----

#simulate wealth values
simwealth <- seq(from=round(min(std_wealth[which(std_wealth > -99)]),1),to=round(max(std_wealth[which(std_wealth > -99)]),1),length.out=nrow(std_wealth)) #specify according to range and length of wealth data
simwealth
#get the deciles
deciles <- as.numeric(quantile(simwealth,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-palette[1:length(deciles)] #darker lines = younger ages, lighter lines = older ages

#set parameters for a legend outside of the plot
par(m_allrow=c(1,1),xpd=T,mar=c(5,5,4,8))

#plot empty plot
plot(c(0,1)~c(0,ncol(post_all$mu)),
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
  p_all <- matrix(nrow=nrow(post_all$mu),ncol=ncol(post_all$mu))
  p_all
  #fill it in with values for age 25
  for(j in 1:ncol(post_all$mu)){
    for(i in 1:nrow(post_all$mu)){
      p_all[i,j] <- inv_logit(post_all$alpha[i] + #inv logit because originally is logit
                                   post_all$mu[i,j] + #age
                                   post_all$beta_wealth[i,j]*deciles[k] + # absolute wealth 
                                   post_all$gamma_wealth[i,j]*0) #wealth variability / zero because that is the average from the standardisation
    }
  }
  #check data
  p_all
  #plot it!
  #prepare model prediction data
  plot_data_all <- data.frame(age = 1:ncol(p_all),
                                 mean = apply(p_all, 2, mean), 
                                 upp = apply(p_all, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p_all, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr_all <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr_all)){
    for(i in 1:nrow(plot_afr_all)){
      if(plot_afr_all[i,j]==-99){
        plot_afr_all[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr_all
  
  points(cumprod(1-plot_data_all$mean)~plot_data_all$age,col=hcl.colors(length(palette),"temps")[palette_b[k]],pch=15)
  lines(cumprod(1-plot_data_all$mean)~plot_data_all$age,col=hcl.colors(length(palette),"temps")[palette_b[k]])
  polygon(c(plot_data_all$age,rev(plot_data_all$age)),c(cumprod(1-plot_data_all$low),rev(cumprod(1-plot_data_all$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[k]],0.5),border=NA)
}

## De-couple plot by min, mean, max ----

#simulate wealth values
simwealth <- seq(from=round(min(post_all$wealth_full),1),to=round(max(post_all$wealth_full),1),length.out=nrow(std_absw_matrix4)) #specify according to range and length related to sample size
simwealth

#get the deciles
deciles <- as.numeric(quantile(simwealth,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-palette[1:length(deciles)] #darker lines = younger ages, lighter lines = older ages

#define layout of plots
par(m_allrow=c(1,3),xpd=T,mar=c(5,5,4,8))

#### Minimum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post_all$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Poor",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p_all_0 <- matrix(nrow=nrow(post_all$mu),ncol=ncol(post_all$mu))
p_all_0
#fill it in with values for age 25
for(j in 1:ncol(post_all$mu)){
  for(i in 1:nrow(post_all$mu)){
    p_all_0[i,j] <- inv_logit(post_all$alpha[i] + #inv logit because originally is logit
                                   post_all$mu[i,j] + #age
                                   post_all$beta_wealth[i,j]*deciles[1] + #absolute wealth
                                   post_all$gamma_wealth[i,j]*0) #wealth variability / zero because that is the average from the standardisation
  }
}
#check data
p_all_0
#plot it!
#prepare model prediction data
plot_data_all_0 <- data.frame(age = 1:ncol(p_all_0),
                                 mean = apply(p_all_0, 2, mean), 
                                 upp = apply(p_all_0, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p_all_0, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr_all <- afr_matrix4
#change -99 to NAs
for(j in 1:ncol(plot_afr_all)){
  for(i in 1:nrow(plot_afr_all)){
    if(plot_afr_all[i,j]==-99){
      plot_afr_all[i,j] <- NA
    }
  }
}
#check the data
plot_afr_all

points(cumprod(1-plot_data_all_0$mean)~plot_data_all_0$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),pch=15)
lines(cumprod(1-plot_data_all_0$mean)~plot_data_all_0$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),lwd=2)
polygon(c(plot_data_all_0$age,rev(plot_data_all_0$age)),c(cumprod(1-plot_data_all_0$low),rev(cumprod(1-plot_data_all_0$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.5),border=NA)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post_all$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Medium",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p_all_50 <- matrix(nrow=nrow(post_all$mu),ncol=ncol(post_all$mu))
p_all_50
#fill it in with values for age 25
for(j in 1:ncol(post_all$mu)){
  for(i in 1:nrow(post_all$mu)){
    p_all_50[i,j] <- inv_logit(post_all$alpha[i] + #inv logit because originally is logit
                                    post_all$mu[i,j] + #age
                                    post_all$beta_wealth[i,j]*deciles[2] + #absolute wealth
                                    post_all$gamma_wealth[i,j]*0) #wealth variability / zero because that is the average from the standardisation
  }
}
#check data
p_all_50
#plot it!
#prepare model prediction data
plot_data_all_50 <- data.frame(age = 1:ncol(p_all_50),
                                  mean = apply(p_all_50, 2, mean), 
                                  upp = apply(p_all_50, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p_all_50, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr_all <- afr_matrix4
#change -99 to NAs
for(j in 1:ncol(plot_afr_all)){
  for(i in 1:nrow(plot_afr_all)){
    if(plot_afr_all[i,j]==-99){
      plot_afr_all[i,j] <- NA
    }
  }
}
#check the data
plot_afr_all

points(cumprod(1-plot_data_all_50$mean)~plot_data_all_50$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),pch=15)
lines(cumprod(1-plot_data_all_50$mean)~plot_data_all_50$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),lwd=2)
polygon(c(plot_data_all_50$age,rev(plot_data_all_50$age)),c(cumprod(1-plot_data_all_50$low),rev(cumprod(1-plot_data_all_50$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.5),border=NA)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post_all$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Rich",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p_all_100 <- matrix(nrow=nrow(post_all$mu),ncol=ncol(post_all$mu))
p_all_100
#fill it in with values for age 25
for(j in 1:ncol(post_all$mu)){
  for(i in 1:nrow(post_all$mu)){
    p_all_100[i,j] <- inv_logit(post_all$alpha[i] + #inv logit because originally is logit
                                     post_all$mu[i,j] + #age
                                     post_all$beta_wealth[i,j]*deciles[3] + #absolute wealth
                                     post_all$gamma_wealth[i,j]*0) #wealth variability / zero because that is the average from the standardisation
  }
}
#check data
p_all_100
#plot it!
#prepare model prediction data
plot_data_all_100 <- data.frame(age = 1:ncol(p_all_100),
                                   mean = apply(p_all_100, 2, mean), 
                                   upp = apply(p_all_100, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                   low = apply(p_all_100, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr_all <- afr_matrix4
#change -99 to NAs
for(j in 1:ncol(plot_afr_all)){
  for(i in 1:nrow(plot_afr_all)){
    if(plot_afr_all[i,j]==-99){
      plot_afr_all[i,j] <- NA
    }
  }
}
#check the data
plot_afr_all

points(cumprod(1-plot_data_all_100$mean)~plot_data_all_100$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),pch=15)
lines(cumprod(1-plot_data_all_100$mean)~plot_data_all_100$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),lwd=2)
polygon(c(plot_data_all_100$age,rev(plot_data_all_100$age)),c(cumprod(1-plot_data_all_100$low),rev(cumprod(1-plot_data_all_100$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.5),border=NA)

legend(77.5,1,c("Poor","Middle","Rich"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)

## Wealth variability ----

#### All wealth classes ----

#simulate wealth values
simwealth <- seq(from=round(min(std_diffwealth[which(std_diffwealth > -99)]),1),to=round(max(std_diffwealth[which(std_diffwealth > -99)]),1),length.out=nrow(std_diffwealth)) #specify according to range and length of wealth data
simwealth
#get the deciles
deciles <- as.numeric(quantile(simwealth,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-palette[1:length(deciles)]

#set parameters for a legend outside of the plot
par(m_allrow=c(1,1),xpd=T,mar=c(5,5,4,8))

#plot empty plot
plot(c(0,1)~c(0,ncol(post_all$mu)),
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
  p_all <- matrix(nrow=nrow(post_all$mu),ncol=ncol(post_all$mu))
  p_all
  #fill it in with values for age 25
  for(j in 1:ncol(post_all$mu)){
    for(i in 1:nrow(post_all$mu)){
      p_all[i,j] <- inv_logit(post_all$alpha[i] + #inv logit because originally is logit
                                   post_all$mu[i,j] + #age
                                   post_all$beta_wealth[i,j]*0 + #absolute wealth / zero because that is the average from the standardisation
                                   post_all$gamma_wealth[i,j]*deciles[k]) #wealth variability
    }
  }
  #check data
  p_all
  #plot it!
  #prepare model prediction data
  plot_data_all <- data.frame(age = 1:ncol(p_all),
                                 mean = apply(p_all, 2, mean), 
                                 upp = apply(p_all, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p_all, 2, function(x) HPDI(x, prob = 0.9))[2, ]
  ) 
  #prepare afr probabilities from real data
  #create a matrix
  plot_afr_all <- afrs
  #change -99 to NAs
  for(j in 1:ncol(plot_afr_all)){
    for(i in 1:nrow(plot_afr_all)){
      if(plot_afr_all[i,j]==-99){
        plot_afr_all[i,j] <- NA
      }
    }
  }
  #check the data
  plot_afr_all
  
  points(cumprod(1-plot_data_all$mean)~plot_data_all$age,col=hcl.colors(length(palette),"temps")[palette_b[k]],pch=15)
  lines(cumprod(1-plot_data_all$mean)~plot_data_all$age,col=hcl.colors(length(palette),"temps")[palette_b[k]])
  polygon(c(plot_data_all$age,rev(plot_data_all$age)),c(cumprod(1-plot_data_all$low),rev(cumprod(1-plot_data_all$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[k]],0.5),border=NA)
}

## De-couple plot by min, mean, max ----

#simulate wealth values
simwealth <- seq(from=round(min(post_all$wealth_full),1),to=round(max(post_all$wealth_full),1),length.out=nrow(std_absw_matrix4)) #specify according to range and length related to sample size
simwealth

#get the deciles
deciles <- as.numeric(quantile(simwealth,seq(0,1,0.5)))
deciles

#colour palette
palette_b<-palette[1:length(deciles)] #darker lines = younger ages, lighter lines = older ages

#define layout of plots
par(m_allrow=c(1,3),xpd=T,mar=c(5,5,4,8))

#### Minimum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post_all$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="No change",
     type="n",
     cex.main=1.5,
     cex.lab=1.5,
     cex.axis=1.2
)

#create matrix to store the data
p_all_0 <- matrix(nrow=nrow(post_all$mu),ncol=ncol(post_all$mu))
p_all_0
#fill it in with values for age 25
for(j in 1:ncol(post_all$mu)){
  for(i in 1:nrow(post_all$mu)){
    p_all_0[i,j] <- inv_logit(post_all$alpha[i] + #inv logit because originally is logit
                                   post_all$mu[i,j] + #age
                                   post_all$beta_wealth[i,j]*0 + #absolute wealth / zero because that is the average from the standardisation
                                   post_all$gamma_wealth[i,j]*deciles[1]) #wealth variability
  }
}
#check data
p_all_0
#plot it!
#prepare model prediction data
plot_data_all_0 <- data.frame(age = 1:ncol(p_all_0),
                                 mean = apply(p_all_0, 2, mean), 
                                 upp = apply(p_all_0, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                 low = apply(p_all_0, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr_all <- afr_matrix4
#change -99 to NAs
for(j in 1:ncol(plot_afr_all)){
  for(i in 1:nrow(plot_afr_all)){
    if(plot_afr_all[i,j]==-99){
      plot_afr_all[i,j] <- NA
    }
  }
}
#check the data
plot_afr_all

points(cumprod(1-plot_data_all_0$mean)~plot_data_all_0$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),pch=15)
lines(cumprod(1-plot_data_all_0$mean)~plot_data_all_0$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.75),lwd=2)
polygon(c(plot_data_all_0$age,rev(plot_data_all_0$age)),c(cumprod(1-plot_data_all_0$low),rev(cumprod(1-plot_data_all_0$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[1]],0.5),border=NA)

#### Median wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post_all$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Mid. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p_all_50 <- matrix(nrow=nrow(post_all$mu),ncol=ncol(post_all$mu))
p_all_50
#fill it in with values for age 25
for(j in 1:ncol(post_all$mu)){
  for(i in 1:nrow(post_all$mu)){
    p_all_50[i,j] <- inv_logit(post_all$alpha[i] + #inv logit because originally is logit
                                    post_all$mu[i,j] + #age
                                    post_all$beta_wealth[i,j]*0 + #absolute wealth / zero because that is the average from the standardisation
                                    post_all$gamma_wealth[i,j]*deciles[2]) #wealth variability
  }
}
#check data
p_all_50
#plot it!
#prepare model prediction data
plot_data_all_50 <- data.frame(age = 1:ncol(p_all_50),
                                  mean = apply(p_all_50, 2, mean), 
                                  upp = apply(p_all_50, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                  low = apply(p_all_50, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr_all <- afr_matrix4
#change -99 to NAs
for(j in 1:ncol(plot_afr_all)){
  for(i in 1:nrow(plot_afr_all)){
    if(plot_afr_all[i,j]==-99){
      plot_afr_all[i,j] <- NA
    }
  }
}
#check the data
plot_afr_all

points(cumprod(1-plot_data_all_50$mean)~plot_data_all_50$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),pch=15)
lines(cumprod(1-plot_data_all_50$mean)~plot_data_all_50$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.75),lwd=2)
polygon(c(plot_data_all_50$age,rev(plot_data_all_50$age)),c(cumprod(1-plot_data_all_50$low),rev(cumprod(1-plot_data_all_50$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[2]],0.5),border=NA)

#### Maximum wealth ----

#plot empty plot
plot(c(0,1)~c(0,ncol(post_all$mu)),
     ylab="Cumulative probability of first birth",
     xlab="Age",
     main="Max. change",
     yaxt="n",
     type="n",
     cex.main=1.5,
     cex.lab=1.5)
axis(2,cex.axis=1.2)

#create matrix to store the data
p_all_100 <- matrix(nrow=nrow(post_all$mu),ncol=ncol(post_all$mu))
p_all_100
#fill it in with values for age 25
for(j in 1:ncol(post_all$mu)){
  for(i in 1:nrow(post_all$mu)){
    p_all_100[i,j] <- inv_logit(post_all$alpha[i] + #inv logit because originally is logit
                                     post_all$mu[i,j] + #age
                                     post_all$beta_wealth[i,j]*0 + #absolute wealth / zero because that is the average from the standardisation
                                     post_all$gamma_wealth[i,j]*deciles[3]) #wealth variability
  }
}
#check data
p_all_100
#plot it!
#prepare model prediction data
plot_data_all_100 <- data.frame(age = 1:ncol(p_all_100),
                                   mean = apply(p_all_100, 2, mean), 
                                   upp = apply(p_all_100, 2, function(x) HPDI(x, prob = 0.9))[1, ], 
                                   low = apply(p_all_100, 2, function(x) HPDI(x, prob = 0.9))[2, ]
) 
#prepare afr probabilities from real data
#create a matrix
plot_afr_all <- afr_matrix4
#change -99 to NAs
for(j in 1:ncol(plot_afr_all)){
  for(i in 1:nrow(plot_afr_all)){
    if(plot_afr_all[i,j]==-99){
      plot_afr_all[i,j] <- NA
    }
  }
}
#check the data
plot_afr_all

points(cumprod(1-plot_data_all_100$mean)~plot_data_all_100$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),pch=15)
lines(cumprod(1-plot_data_all_100$mean)~plot_data_all_100$age,col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.75),lwd=2)
polygon(c(plot_data_all_100$age,rev(plot_data_all_100$age)),c(cumprod(1-plot_data_all_100$low),rev(cumprod(1-plot_data_all_100$upp))),col=alpha(hcl.colors(length(palette),"temps")[palette_b[3]],0.5),border=NA)

legend(77.5,1,c("No change","Mid. change", "Max. change"),lty=1,col=hcl.colors(length(palette),"temps")[palette_b],lwd=2,pch=16)
