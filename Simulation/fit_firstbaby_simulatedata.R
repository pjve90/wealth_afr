#Code to validate the model using synthetic data ----

# There are two purposes for the simulations
# 1) determine whether the stan model can reliably recover the age-specific effects of wealth on the probability to have a first birth
# For this, we generate simulated wealth data, set the age-specific effects for each wealth variable on the probability to have a first birth, generate the reproductive history of the simulated women from these probabilities and their simulated wealth, and determine whether the probabilities inferred by the stan model from these reproductive histories reflect the simulated ones. We perform three simulations here, varying which of the three wealth variables (current amount, short term change, long term variability) has the most influence on the probability to have a first birth

# 2) check whether the imputation of age-specific wealth data from the more limited census data changes the power to recover the age-specific effects of wealth on the probability to have a first birth 
# Using the same data as above, we assume that a percentage of the simulated wealth data would not have been observed. We use the same three simulations as above, blinding 92% of the wealth data, and check if this affects the inferences.


# Running the simulations will generate the following outputs:
# Six csv files with the posterior estimates of the six different models; a text file with the results of the correlation between the simulated and inferred age-specific effects for each of the six simulations (three with the full wealth data, three with the imputed wealth data); plus six pdf files with the plots showing the similarity between the simulated and inferred age-specific effects. High correlations indicate that our STAN model accurately infers the relationship between the wealth predictors and age at first birth.


# Load the necessary libraries  ----

library(cmdstanr)
library(rethinking)
library(scales)
library(corrplot)
library(ggplot2)
library(scales)
library(rstan)

#Import the data ----

# We base the simulations on the real data.  
# We therefore first load these data to calculate the distributions of the key variables in the original data to check whether the simulated data reflects these.
# We first load the data directly from github.
real_data <- read.csv(url("https://raw.githubusercontent.com/pjve90/wealth_afr/refs/heads/master/Data/dataf.csv"), header=T, sep=",", stringsAsFactors=F)[,-1] 

#Data transformation ----

# We then transform the data to calculate the three predictor variables. These transformations are the same that are used to prepare the data for the analyses.

## Age at first reproduction ----
#create a matrix to store the age-specific age of censor
afr_matrix <- matrix(nrow=nrow(real_data),ncol=max(real_data$aoc)+1) #add 1 because column 1 is age 0 (birth)
#calculate for each age when the woman is censored (1) or not (0)
for(i in 1:nrow(afr_matrix)){
  afr <- real_data$afr[i] + 1 #adding 1 so if she reproduces in the same year as registered = 1
  aoc <- real_data$aoc[i] + 1 #adding 1 so if she is censored in the same year as registered = 1
  if(!is.na(afr)){
    afr_matrix[i,1:(afr-1)] <- 0
    afr_matrix[i,afr] <- 1
  } else{
    afr_matrix[i,1:aoc] <- rep(0,length(afr_matrix[i,1:aoc]))
  }
}


#Calculate the age-specific probabilities of first birth
agespecific_probabilities<-apply(afr_matrix,2,sum,na.rm=T)/apply(afr_matrix,2,function(x)sum(!is.na(x)))


##Material wealth ----

##Current absolute wealth ----

#create matrix to store the amount of wealth at each age
absw_matrix <- matrix(nrow = nrow(real_data),ncol=max(real_data$aoc)+1) #add 1 because column 1 is age 0 (birth)
#calculate for each age the amount of wealth the household of a woman has, based on each census
#95
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw95[i]
  age_absw <- real_data$age_absw95[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data$aoc[i]+1)){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}

#98
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw98[i]
  age_absw <- real_data$age_absw98[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}

#00
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw00[i]
  age_absw <- real_data$age_absw00[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}

#02
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw02[i]
  age_absw <- real_data$age_absw02[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}

#04
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw04[i]
  age_absw <- real_data$age_absw04[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}
#06
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw06[i]
  age_absw <- real_data$age_absw06[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= real_data$aoc[i]){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}

#10
for(i in 1:nrow(absw_matrix)){
  absw <- real_data$absw10[i]
  age_absw <- real_data$age_absw10[i] + 1 #adding 1 so if she reproduces/censors in the same is registered = 1
  if(!is.na(age_absw) & age_absw <= (real_data$aoc[i]+1)){
    absw_matrix[i,age_absw] <- absw
  } else
    if(!is.na(age_absw) & age_absw > (real_data$aoc[i]+1)){
      absw_matrix[i,(real_data$aoc[i]+1)] <- NA
    } else{
      absw_matrix[i,age_absw] <- NA
    }
}


#standardise the log-transformed current absolute wealth
std_absw_matrix <- matrix(standardize(log(as.vector(absw_matrix))),ncol=ncol(absw_matrix),nrow=nrow(absw_matrix))


##Short-term wealth variability ----

#create matrix
change_matrix <-  matrix(nrow = nrow(std_absw_matrix),ncol=ncol(std_absw_matrix))

#calculate the short-term wealth variability
for(i in 1:nrow(change_matrix)){
  for(j in 1:2){
    change_matrix[i,j] <- 0 #setting zero change at birth and first year, since wealth change is calculated with a 2-years lag
  }
  for(j in 3:ncol(change_matrix)){
    change_matrix[i,j] = abs(std_absw_matrix[i,j] - std_absw_matrix[i,j-2]) #calculating the 2-years lagged wealth change
  }
}


#Long-term wealth variability - 
#create matrix
msdw_matrix <-  matrix(nrow = nrow(std_absw_matrix),ncol=ncol(std_absw_matrix))

#calculate the long-term wealth variability
for(i in 1:nrow(msdw_matrix)){
  for(j in 1:10){
    msdw_matrix[i,j] <- 0 #setting zero standard deviation from birth until age 10 at birth and first year, since wealth change is calculated with a 10-years window
  }
  for(j in 11:ncol(msdw_matrix)){
    msdw_matrix[i,j] = sd(std_absw_matrix[i,(j-10):j],na.rm=T) #calculating the moving standard deviation with a 10-years window
  }
}


##Individual median ----

# We use the median wealth of each individual as the starting point for the simulations, to reflect the scale of the interindividual differences.
#Get the median of each individual
medianwealthperindividual<-NA
for(individual in 1:nrow(std_absw_matrix)){
  medianwealthperindividual[individual]<-median(std_absw_matrix[individual,],na.rm=T)
}
#check the data
medianwealthperindividual
#If there are individuals without wealth data, sample random values from a normal(0,1) distribution since the data is standardised
medianwealthperindividual[is.na(medianwealthperindividual)]<-rnorm(sum(is.na(medianwealthperindividual)),0,1)
#check the data
medianwealthperindividual

# Data  simulation ----

# For repeatability and to allow comparison between scenarios 1 and 2 (power with imputation of wealth data), we set a seed whenever there is a random processes of the simulation generate the same outcome each time the simulation is run. In case you want to check independent runs of the simulation, blank out the lines with the command set.seed

# First, we generate the three predictor variables: absolute amount of wealth, short-term variability in wealth, and long-term variability in wealth.
# We start by simulating the absolute wealth of each individual. The wealth distribution of the original dataset is slightly skewed (more individuals with low wealth, some very rich individuals). To reflect this, we set up a process that makes it more likely that an average individual will have slightly less than the average wealth, but some individuals might end up with more. We also include the trend in the original data that wealth increases as individuals get older. 
# We simulate 495 individuals (the same as in the original data), one in each row. For these individuals, we simulate 73 years, one in each column. We chose 73 years since that is the oldest age in the sample

## Simulate material wealth ----

### Current wealth ----

# We set up the data frame to store the data
simwealth<-matrix(NA,ncol=ncol(std_absw_matrix),nrow=nrow(std_absw_matrix))
#check the data
simwealth

# The wealth data we simulate reflect the standardised values. That means that the average will be close to 0 and the standard deviation close to 1.

# We start at the youngest age. To create variation among individuals, we assign them a random value sampled from a normal distribution with average = median individual wealth, and sigma = 1.
for(individual in 1:nrow(simwealth)){
  simwealth[individual,1] <- rnorm(1,medianwealthperindividual[individual],1)
}
#check data
simwealth[,1]

#We now simulate the change in wealth for each individual across each year of their age.
#The process we use to reflect the change in absolute wealth from one year to the next matches the process in the imputation of missing data in the analyses. We first determine whether individuals show a large change (which could for example reflect that they changed households) or whether they have a wealth that is similar to the year before. If the value stays similar, we take the value from the year before and add only a little bit of noise (sigma of 0.25). If the value changes, we take a new value from a normal distribution with a mean of their median wealth (to reflect that wealthy individuals tend to stay wealthy) and a larger amount of noise (sigma of 1) to create variation among individuals.
set.seed(1934)
for(individual in 1:nrow(simwealth)){
  for(ages in 2:ncol(simwealth)){
  if(rbinom(1,1,0.125)==1) #assume that the probability individuals change wealth by a lot is low, 12.5%
      {simwealth[individual,ages]<-rnorm(1,medianwealthperindividual[individual],1) #change to a new level of wealth
}else{
    simwealth[individual,ages]<-rnorm(1,simwealth[individual,ages-1],0.25) # (almost) no change from one year to the next
}
}
}
#check the data
simwealth

# We can then check the overall distribution of the three predictor values in the simulated data, and compare it to their distribution in the real data
plot(density(simwealth),ylim=c(0,0.45)) # simulated data is the black line
lines(density(std_absw_matrix,na.rm=T),col="red") # observed data is the red line

# We can also check whether wealth within individuals wealth is correlated from one year to the next
plot(simwealth[,14]~simwealth[,13])

###Short-term wealth variability ----

# Based on the amount of wealth individuals have, we calculate their absolute change in wealth from one year to the next to create the predictor variable of short-term variability in wealth. 
#create matrix
simshorttermwealth<-matrix(NA,ncol=ncol(std_absw_matrix),nrow=nrow(std_absw_matrix))
#calculate the short-term wealth variability
for(individual in 1:nrow(simshorttermwealth)){
  for(ages in 3:ncol(simwealth)){
    simshorttermwealth[individual,ages]<-abs(simwealth[individual,ages]-simwealth[individual,ages-2])
  }
}
#check data
simshorttermwealth

# We now simulate all ages, so we add values for the first two years. These will not affect the simulation because the age-specific probability to have a first child at these ages is zero
simshorttermwealth[,c(1,2)]<-rnorm(2*nrow(simshorttermwealth),mean=mean(simshorttermwealth,na.rm=T),sd=0.2)

### Long-term wealth variability ----

# Based on the amount of wealth individuals, we calculate the standard deviation over the past 10 years to create the predictor variable of long-term variability in wealth. 
#Create matrix
simlongtermwealth<-matrix(NA,ncol=ncol(std_absw_matrix),nrow=nrow(std_absw_matrix))
#Calculate the long-term wealth variability
for(individual in 1:nrow(simlongtermwealth)){
  for(ages in 11:ncol(simwealth)){
    simlongtermwealth[individual,ages]<-sd(as.numeric(simwealth[individual,(ages-10):(ages)]))
  }
}
#check data
simlongtermwealth

# We now simulate all ages, so we add values for the first ten years. These will not affect the simulation because the age-specific probability to have a first child at these ages is zero
simlongtermwealth[,c(1:10)]<-rnorm(10*nrow(simlongtermwealth),mean=mean(simlongtermwealth,na.rm=T),sd=0.2)


## Simulate age-specific probabilities of first birth ----

# First, need the age-specific probabilities of birth. We base these probabilities directly on the observed age-specific probabilities here. Given the small sample, the observed data is not a distribution but has some peaks. We smooth these to generate a distribution
agespecific_probabilities_rounded<-rep(0,74)
for(i in 2:(ncol(afr_matrix))){
  agespecific_probabilities_rounded[i-1]<-(agespecific_probabilities[i-1]+agespecific_probabilities[i]+agespecific_probabilities[i-1])/3
}
for(i in 2:(ncol(afr_matrix))){
  agespecific_probabilities_rounded[i-1]<-(agespecific_probabilities_rounded[i-1]+agespecific_probabilities_rounded[i]+agespecific_probabilities_rounded[i-1])/3
}
for(i in 2:(ncol(afr_matrix))){
  agespecific_probabilities_rounded[i-1]<-(agespecific_probabilities_rounded[i-1]+agespecific_probabilities_rounded[i]+agespecific_probabilities_rounded[i-1])/3
}
for(i in 2:(ncol(afr_matrix))){
  agespecific_probabilities_rounded[i-1]<-(agespecific_probabilities_rounded[i-1]+agespecific_probabilities_rounded[i]+agespecific_probabilities_rounded[i-1])/3
}

afr_age<-agespecific_probabilities_rounded
# plot the age-specific probabilities to give birth
plot(afr_age~c(1:74))

# These values are the complete probabilities that a woman will have her first child at any given age we observe in the actual data. We use these as the baseline age-specific probabilities in the simulations.
afr_age_baseline<-afr_age

##Simulate first birth based on the wealth predictors ----

###Coefficients are age-specific ----

# Link wealth variables to afr with independent effects at each age - exactly what we are doing in the STAN model

set.seed(1578) 

# We specifically want to assess whether our model can detect instances where one of the wealth variables shifts the age of first birth to be earlier or later. For the shift toward earlier or later, we introduce another predictor, mean-centered age: the median age at first birth is set to 0, ages younger than he median get negative values, ages older than the median positive values (calculated simply as age - median(age). Later births now means that probabilities to have the first birth would be higher at ages older than the median age, earlier birth means that probabilities to have the first birth would be higher at ages younter than the median age. We use this together with the wealth predictors.
 # Here we make the beta_wealth parameters age dependent - that is, they would be negative at younger ages and positive at older ages. I think it would mean we have a betawealth in the model that itself is not just coming rom a prior, but based on another model
 # betamuwealth <- centeredage * betamuwealth

# Centered age is median age of first birth
# Figure out median age at first birth
which(afr_age==max(afr_age)) # 19
 
 # We introduce a slope that changes the effecs relative to the age. We only model this for the relevant age range, and we want to make it symmetrical so that the overall probability is similar - so from ages 11 to 18 the centered age is negative, at age 19 it is zero, from ages 20 to 33 it is positive, and after that there is no longer an effect because no individuals reproduce. Because there are more values after the centered age (individuals can have their first child for more years after the age of 19 then before), we need to reduce each of the age-specific values such that the overall probability does not change. 
 
 centeredage<-c(rep(0,10),seq(from=-0.75,to=0,length.out=9),seq(from=0.05,to=0.75,length.out=14)^2,rep(0,41))
 
 
 # a positive effect (the slope) means that individuals are less likely to reproduce when they are young (because the centered age is negative for ages younger than the median, leading to a reduction in the probability) but a higher probability to reproduce when they are old (because the centered age is positive for ages larger than the median age). The effects are simulated on a logit scale, which are added to the logit scale baseline probability, before being transformed back into the probabilities that a woman of a given wealth will have her first child at the respective age. Given that effects are age-specific, on a logit scale, and linked to the centered age, they are best summarized through their total effect. 
 
 # We perform three simulations - 1) absolute wealth has a 4x larger effect, 2) short term wealth change has a 4x larger effect, 3) long term wealth variability has a 4x larger effect. We assume that even the strongest effect only leads to a relatively small shift in the age at first birth.

#### Current wealth ----
 
# 1) absolute wealth has the largest effect, this creates the age-specific effects for the three wealth predictors
 aw_beta <- 1*centeredage # positive slope means wealthy have afr later
 aw_gamma <- -0.25*centeredage # negative slope means individuals with higher short-term wealth variability have afr later, but influence is 4x lower than for absolute wealth
  aw_delta <- -0.25*centeredage # negative slope means individuals with higher short-term wealth variability have afr later, but influence is 4x lower than for absolute wealth
 
# To get a coefficient plot (similar to Figure 6 in the manuscript), we plot the effect sizes over age
#current wealth
  plot(c(51:1)~aw_beta[51:1],ylim=c(51,1),pch=16)
  abline(v=0,lty=2)
#short-term wealth variability
  plot(c(51:1)~aw_gamma[51:1],ylim=c(51,1),pch=16)
  abline(v=0,lty=2)
#long-term wealth variability
  plot(c(51:1)~aw_delta[51:1],ylim=c(51,1),pch=16)
  abline(v=0,lty=2)

#####Checking the effect sizes ----  

#define colour palette
#numbers for color palette
palette <- palette.colors(9,"Okabe-Ito")
#select the numbers for color palette
palette_a<-palette[1:3]
palette_a
#shape of points
shape <- c(15:17)
#line type
type <- c(1:3)

#Age-specific probabilities of first birth

# We simulate the effects on the logit scale, so we first need to transform the baseline age-specific probabilities, add the effects, and retransform this into the total age-specific probabilities - we can show the shift in probabilities for individuals who have 1 sd more wealth than the average (rich) and 1 sd less wealth (poor)
  #maximum wealth (1)
  ageprobs_rich<-logit(afr_age_baseline)+1*aw_beta
  ageprobs_rich<-inv_logit(ageprobs_rich)
  #check data
  ageprobs_rich
  #minimum wealth (-1)
  ageprobs_poor<-logit(afr_age_baseline)+(-1)*aw_beta
  ageprobs_poor<-inv_logit(ageprobs_poor)
  #check data
  ageprobs_poor

#the age-specific probabilities for medium wealth are the age-specific probabilities of first birth, since the average wealth is equal to zero as it is standardised

#Plot it!    
  plot(inv_logit(-aw_beta+logit(afr_age_baseline))~c(1:74),ylim=c(0,0.3),col=palette_a[1],pch=shape[1]) # poor individuals
  points(afr_age_baseline~c(1:74),col=palette_a[2],pch=shape[2]) # baseline probability for individuals with average wealth
  points(inv_logit(aw_beta+logit(afr_age_baseline))~c(1:74),col=palette_a[3],pch=shape[3]) #rich individuals
  legend(x="topright",pch=shape,col=palette_a,legend=c("Min.","Med.","Max."))
  
#Expected mean age at first birth
  
# We can calculate the expected mean age at first birth for individuals who have 1 sd more wealth than average
  #maximum wealth (1)
  std_ageprobs_rich<-0
  for(i in 2:74){
    std_ageprobs_rich[i]<-(1-sum(std_ageprobs_rich[c(1:(i-1))]))*ageprobs_rich[i]
  }
  #check data
  std_ageprobs_rich
  #expected mean age at first birth
  which(cumsum(std_ageprobs_rich)>0.5)[1]
  
  #minimum wealth (-1)
  std_ageprobs_poor<-0
  for(i in 2:74){
    std_ageprobs_poor[i]<-(1-sum(std_ageprobs_poor[c(1:(i-1))]))*ageprobs_poor[i]
  }
  #check data
  std_ageprobs_poor
  #expected mean age at first birth
  which(cumsum(std_ageprobs_poor)>0.5)[1]
  
  #average wealth (0)
  # compare it to the expected mean age at first birth for individuals who have average wealth
  std_afr_age_baseline<-0
  for(i in 2:74){
    std_afr_age_baseline[i]<-(1-sum(std_afr_age_baseline[c(1:(i-1))]))*afr_age_baseline[i]
  }
  #check data
  std_afr_age_baseline
  #expected mean age at first birth
  which(cumsum(std_afr_age_baseline)>0.5)[1]

#Cumulative probabilities of first birth
  
# plot the cumulative, similar to Figure 3 in the manuscript - remember, these are the expected values, they will differ later because there is stochasticity in when exactly individuals will have their first child.
  #minimum wealth
  plot(cumsum(std_ageprobs_poor)[1:41]~c(1:41),col=palette_a[1],pch=shape[1],xlab="age",ylab="cumulative probability first birth")
  lines(cumsum(std_ageprobs_poor)[1:41]~c(1:41),col=palette_a[1],lty=type[1])
  #medium wealth
  points(cumsum(std_afr_age_baseline)[1:41]~c(1:41),col=palette_a[2],pch=shape[2])
  lines(cumsum(std_afr_age_baseline)[1:41]~c(1:41),col=palette_a[2],lty=type[2])
  #maximum wealth
  points(cumsum(std_ageprobs_rich)[1:41]~c(1:41),col=palette_a[3],pch=shape[3])
  lines(cumsum(std_ageprobs_rich)[1:41]~c(1:41),col=palette_a[3],lty=type[3])
  legend(x="bottomright",pch=shape,lty=type,col=palette_a,legend=c("Min.","Med.","Max."))
  

# If we selected our effect sizes, we can then create the dataframe that records for each simulated women whether she had her first child at a given age or not depending on her wealth. We set it so that reproduction starts the earliest at age 11 
 aw_simbirth<-as.data.frame(matrix(NA,ncol=74,nrow=495))
 for(individual in 1:nrow(aw_simbirth)){
   for(ages in 1:10){
   aw_simbirth[individual,ages]<-0
   }
   for(ages in 11:ncol(aw_simbirth)){
     ageprob<-logit(afr_age_baseline[ages])+simwealth[individual,ages]*aw_beta[ages]+simshorttermwealth[individual,ages]*aw_gamma[ages]+simlongtermwealth[individual,ages]*aw_delta[ages]
     ageprob<-inv_logit(ageprob)
     ifelse(aw_simbirth[individual,(ages-1)]==1,aw_simbirth[individual,ages]<-NA,aw_simbirth[individual,ages]<-rbinom(1,1,ageprob))
   }
 }
 
 
 #check data
 aw_simbirth
 #counts per column
 apply(aw_simbirth,2,sum,na.rm=T)

# as a rough way to check whether our simulation works, we can plot for each individual whether their age at first birth is linked to their median wealth
# we create a matrix with a single row for each individual where we store their median wealth and age at first birth
wealthvsafr<-matrix(ncol=2,nrow=nrow(aw_simbirth))
wealthvsafr[,1]<-medianwealthperindividual
for(i in 1:nrow(wealthvsafr)){
  ifelse(sum(aw_simbirth[i,],na.rm=T)==0, wealthvsafr[i,2]<-NA,   wealthvsafr[i,2]<-which(aw_simbirth[i,]==1))
}

# The plot shows that when we set the effect to 1 x the age-specific (centered age) modulators, individuals who have 1 SD more or less wealth than the average reproduce ~0.5 years later/earlier. The exact values can differ a bit because the process above is stochastic.
plot(wealthvsafr[,2]~wealthvsafr[,1])
mean(wealthvsafr[wealthvsafr[,1]>1,2],na.rm=T) #rich individuals
mean(wealthvsafr[wealthvsafr[,1]< -1,2],na.rm=T) # poor individuals
# poor individuals in the simulation have their first child on average 1 year earlier than the richest individuals.

####Short-term wealth variabiliy ----

 # 2) short term wealth change has the largest effect
 sc_beta <- 0.25*centeredage # positive slope means wealthy have afr later, effect is 4x less than for short term wealth changes
 sc_gamma <- -1*centeredage # negative slope means individuals with higher short-term wealth changes have afr earlier
 sc_delta <- -0.25*centeredage # negative slope means individuals with higher long-term wealth variability have afr later, effect is 4x less than for short term wealth changes

 # To get a coefficient plot (similar to Figure 6 in the manuscript), we plot the effect sizes over age
 #current wealth
 plot(c(51:1)~sc_beta[51:1],ylim=c(51,1),pch=16)
 abline(v=0,lty=2)
 #short-term wealth variability
 plot(c(51:1)~sc_gamma[51:1],ylim=c(51,1),pch=16)
 abline(v=0,lty=2)
 #long-term wealth variability
 plot(c(51:1)~sc_delta[51:1],ylim=c(51,1),pch=16)
 abline(v=0,lty=2)

#####Checking the effect sizes ----  
 
 #define colour palette
 #numbers for color palette
 palette <- palette.colors(9,"Okabe-Ito")
 #select the numbers for color palette
 palette_a<-palette[1:3]
 palette_a
 #shape of points
 shape <- c(15:17)
 #line type
 type <- c(1:3)
 
 #Age-specific probabilities of first birth
 
 # We simulate the effects on the logit scale, so we first need to transform the baseline age-specific probabilities, add the effects, and retransform this into the total age-specific probabilities - we can show the shift in probabilities for individuals who have 1 sd more wealth than the average (rich) and 1 sd less wealth (poor)
 #maximum change (1)
 ageprobs_maxsc<-logit(afr_age_baseline)+1*sc_gamma
 ageprobs_maxsc<-inv_logit(ageprobs_maxsc)
 #check data
 ageprobs_maxsc
 #medium change (0.5)
 ageprobs_medsc<-logit(afr_age_baseline)+(0.5)*sc_gamma
 ageprobs_medsc<-inv_logit(ageprobs_medsc)
 #check data
 ageprobs_medsc
 #minimum change (0)
 ageprobs_minsc<-logit(afr_age_baseline)+(0)*sc_gamma
 ageprobs_minsc<-inv_logit(ageprobs_minsc)
 #check data
 ageprobs_minsc
 
#Plot it!    
 plot(afr_age_baseline~c(1:74),ylim=c(0,0.3),col=palette_a[1],pch=shape[1]) # minimum change
 points(inv_logit(0.5*sc_gamma+logit(afr_age_baseline))~c(1:74),col=palette_a[2],pch=shape[2]) # average change
 points(inv_logit(sc_gamma+logit(afr_age_baseline))~c(1:74),col=palette_a[3],pch=shape[3]) #maximum change
 legend(x="topright",pch=shape,col=palette_a,legend=c("Min.","Med.","Max."))
 
 #Expected mean age at first birth
 
 # We can calculate the expected mean age at first birth for individuals who have 1 sd more wealth than average
 #maximum wealth (1)
 std_ageprobs_rich<-0
 for(i in 2:74){
   std_ageprobs_rich[i]<-(1-sum(std_ageprobs_rich[c(1:(i-1))]))*ageprobs_rich[i]
 }
 #check data
 std_ageprobs_rich
 #expected mean age at first birth
 which(cumsum(std_ageprobs_rich)>0.5)[1]
 
 #minimum wealth (-1)
 std_ageprobs_poor<-0
 for(i in 2:74){
   std_ageprobs_poor[i]<-(1-sum(std_ageprobs_poor[c(1:(i-1))]))*ageprobs_poor[i]
 }
 #check data
 std_ageprobs_poor
 #expected mean age at first birth
 which(cumsum(std_ageprobs_poor)>0.5)[1]
 
 #average wealth (0)
 # compare it to the expected mean age at first birth for individuals who have average wealth
 std_afr_age_baseline<-0
 for(i in 2:74){
   std_afr_age_baseline[i]<-(1-sum(std_afr_age_baseline[c(1:(i-1))]))*afr_age_baseline[i]
 }
 #check data
 std_afr_age_baseline
 #expected mean age at first birth
 which(cumsum(std_afr_age_baseline)>0.5)[1]
 
 #Cumulative probabilities of first birth
 
 # plot the cumulative, similar to Figure 3 in the manuscript - remember, these are the expected values, they will differ later because there is stochasticity in when exactly individuals will have their first child.
 #minimum wealth
 plot(cumsum(std_ageprobs_poor)[1:41]~c(1:41),col=palette_a[1],pch=shape[1],xlab="age",ylab="cumulative probability first birth")
 lines(cumsum(std_ageprobs_poor)[1:41]~c(1:41),col=palette_a[1],lty=type[1])
 #medium wealth
 points(cumsum(std_afr_age_baseline)[1:41]~c(1:41),col=palette_a[2],pch=shape[2])
 lines(cumsum(std_afr_age_baseline)[1:41]~c(1:41),col=palette_a[2],lty=type[2])
 #maximum wealth
 points(cumsum(std_ageprobs_rich)[1:41]~c(1:41),col=palette_a[3],pch=shape[3])
 lines(cumsum(std_ageprobs_rich)[1:41]~c(1:41),col=palette_a[3],lty=type[3])
 legend(x="bottomright",pch=shape,lty=type,col=palette_a,legend=c("Min.","Med.","Max."))
 
 
 # We create the dataframe that records for each simulated women whether she had her first child at a given age or not. We set it so that reproduction starts the earliest at age 13 
 sc_simbirth<-as.data.frame(matrix(NA,ncol=74,nrow=495))
 for(individual in 1:nrow(sc_simbirth)){
   for(ages in 1:12){
     sc_simbirth[individual,ages]<-0
   }
   for(ages in 13:ncol(sc_simbirth)){
     ageprob<-logit(afr_age_baseline[ages])+simwealth[individual,ages]*sc_beta[ages]+simshorttermwealth[individual,ages]*sc_gamma[ages]+simlongtermwealth[individual,ages]*sc_delta[ages]
     ageprob<-inv_logit(ageprob)
     ifelse(sc_simbirth[individual,(ages-1)]==1,sc_simbirth[individual,ages]<-NA,sc_simbirth[individual,ages]<-rbinom(1,1,ageprob))
   }
 }
 
 
 # 3) long term wealth variability has the largest effect
 lv_beta <- 0.25*centeredage # positive slope means wealthy have afr later, effect is 4x less than for long term variability in wealth
  lv_gamma <- -0.25*centeredage # negative slope means individuals with higher short-term wealth changes have afr earlier, effect is 4x less than for the long term variability in wealth
  lv_delta <- 1*centeredage # negative slope means individuals with higher long-term wealth variability have afr earlier
 
 # We create the dataframe that records for each simulated women whether she had her first child at a given age or not. We set it so that reproduction starts the earliest at age 13 
 lv_simbirth<-as.data.frame(matrix(NA,ncol=74,nrow=495))
 for(individual in 1:nrow(lv_simbirth)){
   for(ages in 1:12){
     lv_simbirth[individual,ages]<-0
   }
   for(ages in 13:ncol(lv_simbirth)){
     ageprob<-logit(afr_age_baseline[ages])+simwealth[individual,ages]*lv_beta[ages]+simshorttermwealth[individual,ages]*lv_gamma[ages]+simlongtermwealth[individual,ages]*lv_delta[ages]
     ageprob<-inv_logit(ageprob)
     ifelse(lv_simbirth[individual,(ages-1)]==1,lv_simbirth[individual,ages]<-NA,lv_simbirth[individual,ages]<-rbinom(1,1,ageprob))
   }
 }
 
 
# # We now have all the data in the same format as in the original data. That means we can perform the same data checks, plus run the inference model, to assess our aim 1.

#Fit the data in the model ----

#Scenario 1: Current wealth --- 
  
# We can now prepare all the data to be analysed in the STAN model  ----
# We will run six analyses: three with the full wealth dataset, and three with the wealth dataset which has missing values

 # 1) full wealth data, absolute wealth strongest predictor
 # We put all of this together in the list of data for the analyses
 aw_full_simulated_list <- list(N = nrow(aw_simbirth_res), #population size
                                A = ncol(aw_simbirth_res), #age
                                wealth = as.matrix(simwealth_res), #current absolute wealth
                                baby = as.matrix(aw_simbirth_res), #AFR
                                median_wealth = medianwealthperindividual # median wealth of each individual
 )
 #check data
 aw_full_simulated_list
 
 ## Compile and fit model ----
 # compile model
 model_simulated <- cmdstan_model("~/wealth_afr/Simulation/firstbaby_threewealth_unif.stan")
 
 #fit model
 aw_full_fit_simulated <- model_simulated$sample(data = aw_full_simulated_list, 
                                                 chains = 4, 
                                                 parallel_chains = 15, 
                                                 adapt_delta = 0.99,
                                                 max_treedepth = 13,
                                                 iter_warmup = 2000,
                                                 iter_sampling = 2000,
                                                 init = 0)
 
 
 
 # save fit 
 aw_full_fit_simulated_csv <- rstan::read_stan_csv(aw_full_fit_simulated$output_files())
 saveRDS(aw_full_fit_simulated_csv, "aw_full_fit_simulated_output.rds")
 #load RDS file
 aw_full_rds_simulated <- readRDS("aw_full_fit_simulated_output.rds")
 
 ## Model diagnostics ----
 
 #check trace of all parameters
 #alpha
 rstan::traceplot(aw_full_rds_simulated,pars="alpha")
 #mu
 traceplot(aw_full_rds_simulated,pars="mu") 
 #mu_raw
 traceplot(aw_full_rds_simulated,pars="mu_raw")
 #mu_tau
 rstan::traceplot(aw_full_rds_simulated,pars="mu_tau")
 #mu_kappa
 rstan::traceplot(aw_full_rds_simulated,pars="mu_kappa")
 #mu_delta
 rstan::traceplot(aw_full_rds_simulated,pars="mu_delta")
 #beta_wealth_z
 traceplot(aw_full_rds_simulated,pars="beta_wealth_z") 
 #beta_wealth_sigma
 traceplot(aw_full_rds_simulated,pars="beta_wealth_sigma") 
 #gamma_wealth
 traceplot(aw_full_rds_simulated,pars="gamma_wealth_z") 
 #gamma_wealth
 traceplot(aw_full_rds_simulated,pars="gamma_wealth_sigma") 
 #delta_wealth
 traceplot(aw_full_rds_simulated,pars="delta_wealth_z") 
 #delta_wealth
 traceplot(aw_full_rds_simulated,pars="delta_wealth_sigma") 
 
 # generate output for simulation with full data where current absolute wealth has the strongest effect
 #beta z
 #create summary table for beta_z
 aw_full_tab_sim_beta_z <- precis(aw_full_rds_simulated,depth=2,pars="beta_wealth_z")
 #check table
 aw_full_tab_sim_beta_z
 
 #beta sigma
 #create summary table for beta_sigma
 aw_full_tab_sim_beta_sigma <- precis(aw_full_rds_simulated,depth=2,pars="beta_wealth_sigma")
 #check table
 aw_full_tab_sim_beta_sigma
 
 #gamma z
 #create summary table for gamma_z
 aw_full_tab_sim_gamma_z <- precis(aw_full_rds_simulated,depth=2,pars="gamma_wealth_z")
 #check table
 aw_full_tab_sim_gamma_z
 
 #gamma sigma
 #create summary table for gamma_sigma
 aw_full_tab_sim_gamma_sigma <- precis(aw_full_rds_simulated,depth=2,pars="gamma_wealth_sigma")
 #check table
 aw_full_tab_sim_gamma_sigma
 
 #delta z
 #create summary table for delta_z
 aw_full_tab_sim_delta_z <- precis(aw_full_rds_simulated,depth=2,pars="delta_wealth_z")
 #check table
 aw_full_tab_sim_delta_z
 
 #delta sigma
 #create summary table for delta_sigma
 aw_full_tab_sim_delta_sigma <- precis(aw_full_rds_simulated,depth=2,pars="delta_wealth_sigma")
 #check table
 aw_full_tab_sim_delta_sigma
 
#Plot it!

par(mfrow=c(1,3))
plot(aw_full_tab_sim_beta_z)
points(aw_beta[11:40]~c(40:11),col="blue")
plot(aw_full_tab_sim_gamma_z)
points(aw_gamma[11:40]~c(40:11),col="red")
plot(aw_full_tab_sim_delta_z)
points(aw_delta[11:40]~c(40:11),col="gold")

# plot(aw_full_tab_sim_beta_z[,1]*aw_full_tab_sim_beta_sigma[1,1]~aw_beta[11:40],xlab="simulated beta",ylab="estimated beta")
# title("effects of absolute wealth")
# plot(aw_full_tab_sim_gamma_z[,1]*aw_full_tab_sim_gamma_sigma[1,]~aw_gamma[11:40],xlab="simulated gamma",ylab="estimated gamma")
# title("effects of short-term wealth")
# plot(aw_full_tab_sim_delta_z[,1]*aw_full_tab_sim_delta_sigma[1,]~aw_delta[11:40],xlab="simulated delta",ylab="estimated delta")
# title("effects of long-term wealth")

aw_full_correlations<-rbind(summary(lm(aw_full_tab_sim_beta_z[,1]*tab_sim_beta_sigma[1,]~aw_beta[11:40])),summary(lm(aw_full_tab_sim_gamma_z[,1]*tab_sim_gamma_sigma[1,]~aw_gamma[11:40])),summary(lm(aw_full_tab_sim_delta_z[,1]*tab_sim_delta_sigma[1,]~aw_delta[11:40])))

 
 
 

 # For aim 2, we introduce missing observations in the wealth data  ----
# The simulated wealth matrix has a complete history of wealth for each individual for each age - which is what must have happened
# The real data only has observations of wealth of individuals at some of their ages, we do not have their full wealth history
# We can reproduce this by varying how much of the simulated wealth data would have been observed

# Create a copy of the wealth dataset with the same number of rows and columns as the original, which we will use in the analyses for the data imputation
sim_wealth_imputation<-matrix(NA,ncol=ncol(simwealth),nrow=nrow(simwealth))
 
# There were 7 censuses which happened 2 years apart. The age at which any given individual was first visited is random, but it was mostly at a young age before they had their first child - so for each individual, we pick a random age less than 30, and only take the wealth value from that age plus the values from when they were 2, 4, 6, 8, 10, and 12 years older. Individuals are in different columns, ages are in rows.
for(j in 1:nrow(sim_wealth_imputation)){
    age_first_census<-runif(1,min=1,max=30)
    sim_wealth_imputation[j,age_first_census]<-simwealth[j,age_first_census]
    sim_wealth_imputation[j,age_first_census+2]<-simwealth[j,age_first_census+2]
    sim_wealth_imputation[j,age_first_census+4]<-simwealth[j,age_first_census+4]
    sim_wealth_imputation[j,age_first_census+6]<-simwealth[j,age_first_census+6]
    sim_wealth_imputation[j,age_first_census+8]<-simwealth[j,age_first_census+8]
    sim_wealth_imputation[j,age_first_census+10]<-simwealth[j,age_first_census+10]
    sim_wealth_imputation[j,age_first_census+12]<-simwealth[j,age_first_census+12]
 }
 
sim_wealth_imputation 
# We can compare this with the observed wealth data
std_absw_matrix

 
# The matrices recording the simulated missing wealth (sim_wealth_imputation) and the simulated birth data (aw_simbirth, sc_simbirth, lv_simbirth) contain missing values. 
 # We need to replace NAs with -99 for this to be correctly recognized in the stan models
 for(j in 1:ncol(sim_wealth_imputation)){
   for(i in 1:nrow(sim_wealth_imputation)){
     if(is.na(sim_wealth_imputation[i,j])){
       sim_wealth_imputation[i,j] <- -99
     } else{
       sim_wealth_imputation[i,j] <- sim_wealth_imputation[i,j]
     }
   }
 }
 
 for(j in 1:ncol(aw_simbirth)){
   for(i in 1:nrow(aw_simbirth)){
     if(is.na(aw_simbirth[i,j])){
       aw_simbirth[i,j] <- -99
     } else{
       aw_simbirth[i,j] <- aw_simbirth[i,j]
     }
   }
 }
 
 for(j in 1:ncol(sc_simbirth)){
   for(i in 1:nrow(sc_simbirth)){
     if(is.na(sc_simbirth[i,j])){
       sc_simbirth[i,j] <- -99
     } else{
       sc_simbirth[i,j] <- sc_simbirth[i,j]
     }
   }
 }

 for(j in 1:ncol(lv_simbirth)){
   for(i in 1:nrow(lv_simbirth)){
     if(is.na(lv_simbirth[i,j])){
       lv_simbirth[i,j] <- -99
     } else{
       lv_simbirth[i,j] <- lv_simbirth[i,j]
     }
   }
 }
 

# To speed up the analyses, we restrict the matrices to only those columns with the ages where women could have had their first child, we do not need to estimate associations at other ages.  
 aw_simbirth_res<-aw_simbirth[,11:40] 
 sc_simbirth_res<-sc_simbirth[,11:40] 
 lv_simbirth_res<-lv_simbirth[,11:40] 
 simwealth_res<-simwealth[,11:40]
 sim_wealth_imputation_res<-sim_wealth_imputation[,11:40]
 
#------------------------------------------------------------------------------------------------

 
 # There sometimes seems to be an issue with extracting the posterior sample with the rstan command above
 # This is an alternative approach
#  drawsarray_firstsimulated<-aw_full_fit_simulated$draws()
# drawsdataframe_firstsimulated<-as_draws_df(drawsarray_firstsimulated)
# aw_full_rds_simulated<-data.frame(drawsdataframe_firstsimulated)
# When using this alternative approach, the "pars"ing of information below inside the precis commands does not work
# Add the following to obtain the correct rows with the relevant estimates
 # Example for beta_wealth_z
  # aw_full_tab_sim_beta_z <- precis(drawsdataframe_firstsimulated,depth=2)
  # aw_full_tab_sim_beta_z <- aw_full_tab_sim_beta_z[grep("beta_wealth_z",rownames(aw_full_tab_sim_beta_z)),]
 

 
 # 2) full wealth data, short term wealth strongest predictor
 # We put all of this together in the list of data for the analyses
 sc_full_simulated_list <- list(N = nrow(sc_simbirth_res), #population size
                                A = ncol(sc_simbirth_res), #age
                                wealth = as.matrix(simwealth_res), #current absolute wealth
                                baby = as.matrix(sc_simbirth_res), #AFR
                                median_wealth = medianwealthperindividual # median wealth of each individual
 )
 #check data
 sc_full_simulated_list
 
 ## Compile and fit model ----
 #fit model
 sc_full_fit_simulated <- model_simulated$sample(data = sc_full_simulated_list, 
                                                 chains = 4, 
                                                 parallel_chains = 15, 
                                                 adapt_delta = 0.99,
                                                 max_treedepth = 13,
                                                 iter_warmup = 2000,
                                                 iter_sampling = 2000,
                                                 init = 0)
 
 # save fit 
 sc_full_fit_simulated_csv <- rstan::read_stan_csv(sc_full_fit_simulated$output_files())
 saveRDS(sc_full_fit_simulated_csv, "sc_full_fit_simulated_output.rds")
 #load RDS file
 sc_full_rds_simulated <- readRDS("sc_full_fit_simulated_output.rds")
 
 ## Model diagnostics ----
 
 #check trace of all parameters
 #alpha
 rstan::traceplot(sc_full_rds_simulated,pars="alpha")
 #mu
 traceplot(sc_full_rds_simulated,pars="mu") 
 #mu_raw
 traceplot(sc_full_rds_simulated,pars="mu_raw")
 #mu_tau
 rstan::traceplot(sc_full_rds_simulated,pars="mu_tau")
 #mu_kappa
 rstan::traceplot(sc_full_rds_simulated,pars="mu_kappa")
 #mu_delta
 rstan::traceplot(sc_full_rds_simulated,pars="mu_delta")
 #beta_wealth_z
 traceplot(sc_full_rds_simulated,pars="beta_wealth_z") 
 #beta_wealth_sigma
 traceplot(sc_full_rds_simulated,pars="beta_wealth_sigma") 
 #gamma_wealth
 traceplot(sc_full_rds_simulated,pars="gamma_wealth_z") 
 #gamma_wealth
 traceplot(sc_full_rds_simulated,pars="gamma_wealth_sigma") 
 #delta_wealth
 traceplot(sc_full_rds_simulated,pars="delta_wealth_z") 
 #delta_wealth
 traceplot(sc_full_rds_simulated,pars="delta_wealth_sigma") 
 
 
 # 3) full wealth data, long term wealth strongest predictor
 # We put all of this together in the list of data for the analyses
 lv_full_simulated_list <- list(N = nrow(lv_simbirth_res), #population size
                                A = ncol(lv_simbirth_res), #age
                                wealth = as.matrix(simwealth_res), #current absolute wealth
                                baby = as.matrix(lv_simbirth_res), #AFR
                                median_wealth = medianwealthperindividual # median wealth of each individual
 )
 #check data
 lv_full_simulated_list
 
 ## Compile and fit model ----
 #fit model
 lv_full_fit_simulated <- model_simulated$sample(data = lv_full_simulated_list, 
                                                 chains = 4, 
                                                 parallel_chains = 15, 
                                                 adapt_delta = 0.99,
                                                 max_treedepth = 13,
                                                 iter_warmup = 2000,
                                                 iter_sampling = 2000,
                                                 init = 0)
 
 # save fit 
 lv_full_fit_simulated_csv <- rstan::read_stan_csv(lv_full_fit_simulated$output_files())
 saveRDS(lv_full_fit_simulated_csv, "lv_full_fit_simulated_output.rds")
 #load RDS file
 lv_full_rds_simulated <- readRDS("lv_full_fit_simulated_output.rds")
 
 ## Model diagnostics ----
 
 #check trace of all parameters
 #alpha
 rstan::traceplot(lv_full_rds_simulated,pars="alpha")
 #mu
 traceplot(lv_full_rds_simulated,pars="mu") 
 #mu_raw
 traceplot(lv_full_rds_simulated,pars="mu_raw")
 #mu_tau
 rstan::traceplot(lv_full_rds_simulated,pars="mu_tau")
 #mu_kappa
 rstan::traceplot(lv_full_rds_simulated,pars="mu_kappa")
 #mu_delta
 rstan::traceplot(lv_full_rds_simulated,pars="mu_delta")
 #beta_wealth_z
 traceplot(lv_full_rds_simulated,pars="beta_wealth_z") 
 #beta_wealth_sigma
 traceplot(lv_full_rds_simulated,pars="beta_wealth_sigma") 
 #gamma_wealth
 traceplot(lv_full_rds_simulated,pars="gamma_wealth_z") 
 #gamma_wealth
 traceplot(lv_full_rds_simulated,pars="gamma_wealth_sigma") 
 #delta_wealth
 traceplot(lv_full_rds_simulated,pars="delta_wealth_z") 
 #delta_wealth
 traceplot(lv_full_rds_simulated,pars="delta_wealth_sigma") 
 
 
 # 4) incomplete wealth data, absolute wealth strongest predictor
 # We put all of this together in the list of data for the analyses
 aw_imputed_simulated_list <- list(N = nrow(aw_simbirth_res), #population size
                                A = ncol(aw_simbirth_res), #age
                                wealth = as.matrix(sim_wealth_imputation_res), #current absolute wealth
                                baby = as.matrix(aw_simbirth_res), #AFR
                                median_wealth = medianwealthperindividual # median wealth of each individual
 )
 
 ## Compile and fit model ----
 #fit model
 aw_imputed_fit_simulated <- model_simulated$sample(data = aw_imputed_simulated_list, 
                                                    chains = 4, 
                                                    parallel_chains = 15, 
                                                    adapt_delta = 0.99,
                                                    max_treedepth = 13,
                                                    iter_warmup = 2000,
                                                    iter_sampling = 2000,
                                                    init = 0)
 
 # save fit 
 aw_imputed_fit_simulated_csv <- rstan::read_stan_csv(aw_imputed_fit_simulated$output_files())
 saveRDS(aw_imputed_fit_simulated_csv, "aw_imputed_fit_simulated_output.rds")
 #load RDS file
 aw_imputed_rds_simulated <- readRDS("aw_imputed_fit_simulated_output.rds")
 
 
 # 5) imputed wealth data, short term wealth strongest predictor
 # We put all of this together in the list of data for the analyses
 sc_imputed_simulated_list <- list(N = nrow(sc_simbirth_res), #population size
                                A = ncol(sc_simbirth_res), #age
                                wealth = as.matrix(sim_wealth_imputation_res), #current absolute wealth
                                baby = as.matrix(sc_simbirth_res), #AFR
                                median_wealth = medianwealthperindividual # median wealth of each individual
 )
 #check data
 simulated_list
 
 ## Compile and fit model ----
 #fit model
 sc_imputed_fit_simulated <- model_simulated$sample(data = sc_imputed_simulated_list, 
                                                    chains = 4, 
                                                    parallel_chains = 15, 
                                                    adapt_delta = 0.99,
                                                    max_treedepth = 13,
                                                    iter_warmup = 2000,
                                                    iter_sampling = 2000,
                                                    init = 0)
 
 # save fit 
 sc_imputed_fit_simulated_csv <- rstan::read_stan_csv(sc_imputed_fit_simulated$output_files())
 saveRDS(sc_imputed_fit_simulated_csv, "sc_imputed_fit_simulated_output.rds")
 #load RDS file
 sc_imputed_rds_simulated <- readRDS("sc_imputed_fit_simulated_output.rds")
 
 
 # 6) imputed wealth data, long term wealth strongest predictor
 # We put all of this together in the list of data for the analyses
 lv_imputed_simulated_list <- list(N = nrow(lv_simbirth_res), #population size
                                A = ncol(lv_simbirth_res), #age
                                wealth = as.matrix(sim_wealth_imputation_res), #current absolute wealth
                                baby = as.matrix(lv_simbirth_res), #AFR
                                median_wealth = medianwealthperindividual # median wealth of each individual
 )
 #check data
 simulated_list
 
 ## Compile and fit model ----
 #fit model
 lv_imputed_fit_simulated <- model_simulated$sample(data = lv_imputed_simulated_list, 
                                                    chains = 4, 
                                                    parallel_chains = 15, 
                                                    adapt_delta = 0.99,
                                                    max_treedepth = 13,
                                                    iter_warmup = 2000,
                                                    iter_sampling = 2000,
                                                    init = 0)
 
 # save fit 
 lv_imputed_fit_simulated_csv <- rstan::read_stan_csv(lv_imputed_fit_simulated$output_files())
 saveRDS(lv_imputed_fit_simulated_csv, "lv_imputed_fit_simulated_output.rds")
 #load RDS file
 lv_imputed_rds_simulated <- readRDS("lv_imputed_fit_simulated_output.rds")
 
 
 
 
 
 # generate output for simulation with full data where current absolute wealth has the strongest effect
 #beta z
 #create summary table for beta_z
 aw_full_tab_sim_beta_z <- precis(aw_full_rds_simulated,depth=2,pars="beta_wealth_z")
 #check table
 aw_full_tab_sim_beta_z
 
 #beta sigma
 #create summary table for beta_sigma
 aw_full_tab_sim_beta_sigma <- precis(aw_full_rds_simulated,depth=2,pars="beta_wealth_sigma")
 #check table
 aw_full_tab_sim_beta_sigma
 
 #gamma z
 #create summary table for gamma_z
 aw_full_tab_sim_gamma_z <- precis(aw_full_rds_simulated,depth=2,pars="gamma_wealth_z")
 #check table
 aw_full_tab_sim_gamma_z
 
 #gamma sigma
 #create summary table for gamma_sigma
 aw_full_tab_sim_gamma_sigma <- precis(aw_full_rds_simulated,depth=2,pars="gamma_wealth_sigma")
 #check table
 aw_full_tab_sim_gamma_sigma
 
 #delta z
 #create summary table for delta_z
 aw_full_tab_sim_delta_z <- precis(aw_full_rds_simulated,depth=2,pars="delta_wealth_z")
 #check table
 aw_full_tab_sim_delta_z
 
 #delta sigma
 #create summary table for delta_sigma
 aw_full_tab_sim_delta_sigma <- precis(aw_full_rds_simulated,depth=2,pars="delta_wealth_sigma")
 #check table
 aw_full_tab_sim_delta_sigma
 
 
 pdf("aw_full_plot.pdf")
 par(mfrow=c(1,3))
 plot(aw_full_tab_sim_beta_z[,1]*aw_full_tab_sim_beta_sigma[1,1]~aw_beta[11:40],xlab="simulated beta",ylab="estimated beta")
 title("effects of absolute wealth")
 plot(aw_full_tab_sim_gamma_z[,1]*aw_full_tab_sim_gamma_sigma[1,]~aw_gamma[11:40],xlab="simulated gamma",ylab="estimated gamma")
 title("effects of short-term wealth")
 plot(aw_full_tab_sim_delta_z[,1]*aw_full_tab_sim_delta_sigma[1,]~aw_delta[11:40],xlab="simulated delta",ylab="estimated delta")
 title("effects of long-term wealth")
 dev.off()
 
 aw_full_correlations<-rbind(summary(lm(aw_full_tab_sim_beta_z[,1]*tab_sim_beta_sigma[1,]~aw_beta[11:40])),summary(lm(aw_full_tab_sim_gamma_z[,1]*tab_sim_gamma_sigma[1,]~aw_gamma[11:40])),summary(lm(aw_full_tab_sim_delta_z[,1]*tab_sim_delta_sigma[1,]~aw_delta[11:40])))
 
 write.csv(aw_full_correlations,file="aw_full_correlations.csv")
 
 
 # generate output for simulation with full data where short-term wealth change has the strongest effect
 #beta z
 #create summary table for beta_z
 sc_full_tab_sim_beta_z <- precis(sc_full_rds_simulated,depth=2,pars="beta_wealth_z")
 #check table
 sc_full_tab_sim_beta_z
 
 #beta sigma
 #create summary table for beta_sigma
 sc_full_tab_sim_beta_sigma <- precis(sc_full_rds_simulated,depth=2,pars="beta_wealth_sigma")
 #check table
 sc_full_tab_sim_beta_sigma
 
 #gamma z
 #create summary table for gamma_z
 sc_full_tab_sim_gamma_z <- precis(sc_full_rds_simulated,depth=2,pars="gamma_wealth_z")
 #check table
 sc_full_tab_sim_gamma_z
 
 #gamma sigma
 #create summary table for gamma_sigma
 sc_full_tab_sim_gamma_sigma <- precis(sc_full_rds_simulated,depth=2,pars="gamma_wealth_sigma")
 #check table
 sc_full_tab_sim_gamma_sigma
 
 #delta z
 #create summary table for delta_z
 sc_full_tab_sim_delta_z <- precis(sc_full_rds_simulated,depth=2,pars="delta_wealth_z")
 #check table
 sc_full_tab_sim_delta_z
 
 #delta sigma
 #create summary table for delta_sigma
 sc_full_tab_sim_delta_sigma <- precis(sc_full_rds_simulated,depth=2,pars="delta_wealth_sigma")
 #check table
 sc_full_tab_sim_delta_sigma
 
 pdf("sc_full_plot.pdf")
 par(mfrow=c(1,3))
 plot(sc_full_tab_sim_beta_z[,1]*tab_sim_beta_sigma[1,]~sc_beta[11:40],xlab="simulated beta",ylab="estimated beta")
 title("effects of absolute wealth")
 plot(sc_full_tab_sim_gamma_z[,1]*tab_sim_gamma_sigma[1,]~sc_gamma[11:40],xlab="simulated gamma",ylab="estimated gamma")
 title("effects of short-term wealth")
 plot(sc_full_tab_sim_delta_z[,1]*tab_sim_delta_sigma[1,]~sc_delta[11:40],xlab="simulated delta",ylab="estimated delta")
 title("effects of long-term wealth")
 dev.off()
 
 sc_full_correlations<-rbind(summary(lm(sc_full_tab_sim_beta_z[,1]*tab_sim_beta_sigma[1,]~sc_beta[11:40])),summary(lm(sc_full_tab_sim_gamma_z[,1]*tab_sim_gamma_sigma[1,]~sc_gamma[11:40])),summary(lm(sc_full_tab_sim_delta_z[,1]*tab_sim_delta_sigma[1,]~sc_delta[11:40])))
 
 write.csv(sc_full_correlations,file="sc_full_correlations.csv")
 

 # generate output for simulation with full data where long-term wealth variability has the strongest effect
 #beta z
 #create summary table for beta_z
 lv_full_tab_sim_beta_z <- precis(lv_full_rds_simulated,depth=2,pars="beta_wealth_z")
 #check table
 lv_full_tab_sim_beta_z
 
 #beta sigma
 #create summary table for beta_sigma
 tab_sim_beta_sigma <- precis(lv_full_rds_simulated,depth=2,pars="beta_wealth_sigma")
 #check table
 tab_sim_beta_sigma
 
 #gamma z
 #create summary table for gamma_z
 lv_full_tab_sim_gamma_z <- precis(lv_full_rds_simulated,depth=2,pars="gamma_wealth_z")
 #check table
 lv_full_tab_sim_gamma_z
 
 #gamma sigma
 #create summary table for gamma_sigma
 lv_full_tab_sim_gamma_sigma <- precis(lv_full_rds_simulated,depth=2,pars="gamma_wealth_sigma")
 #check table
 lv_full_tab_sim_gamma_sigma
 
 #delta z
 #create summary table for delta_z
 lv_full_tab_sim_delta_z <- precis(lv_full_rds_simulated,depth=2,pars="delta_wealth_z")
 #check table
 lv_full_tab_sim_delta_z
 
 #delta sigma
 #create summary table for delta_sigma
 lv_full_tab_sim_delta_sigma <- precis(lv_full_rds_simulated,depth=2,pars="delta_wealth_sigma")
 #check table
 lv_full_tab_sim_delta_sigma
 
 pdf("lv_full_plot.pdf")
 par(mfrow=c(1,3))
 plot(lv_full_tab_sim_beta_z[,1]*tab_sim_beta_sigma[1,]~lv_beta[11:40],xlab="simulated beta",ylab="estimated beta")
 title("effects of absolute wealth")
 plot(lv_full_tab_sim_gamma_z[,1]*tab_sim_gamma_sigma[1,]~lv_gamma[11:40],xlab="simulated gamma",ylab="estimated gamma")
 title("effects of short-term wealth")
 plot(lv_full_tab_sim_delta_z[,1]*tab_sim_delta_sigma[1,]~lv_delta[11:40],xlab="simulated delta",ylab="estimated delta")
 title("effects of long-term wealth")
 dev.off()
 
 lv_full_correlations<-rbind(summary(lm(lv_full_tab_sim_beta_z[,1]*tab_sim_beta_sigma[1,]~lv_beta[11:40])),summary(lm(lv_full_tab_sim_gamma_z[,1]*tab_sim_gamma_sigma[1,]~lv_gamma[11:40])),summary(lm(lv_full_tab_sim_delta_z[,1]*tab_sim_delta_sigma[1,]~lv_delta[11:40])))
 
 write.csv(lv_full_correlations,file="lv_full_correlations.csv")
 
  
 
 
 # generate output for simulation with imputed data where current absolute wealth has the strongest effect
 #beta z
 #create summary table for beta_z
 aw_imputed_tab_sim_beta_z <- precis(aw_imputed_rds_simulated,depth=2,pars="beta_wealth_z")
 #check table
 aw_imputed_tab_sim_beta_z
 
 #beta sigma
 #create summary table for beta_sigma
 aw_imupted_tab_sim_beta_sigma <- precis(aw_imputed_rds_simulated,depth=2,pars="beta_wealth_sigma")
 #check table
 aw_imupted_tab_sim_beta_sigma
 
 #gamma z
 #create summary table for gamma_z
 aw_imputed_tab_sim_gamma_z <- precis(aw_imputed_rds_simulated,depth=2,pars="gamma_wealth_z")
 #check table
 aw_imputed_tab_sim_gamma_z
 
 #gamma sigma
 #create summary table for gamma_sigma
 aw_imputed_tab_sim_gamma_sigma <- precis(aw_imputed_rds_simulated,depth=2,pars="gamma_wealth_sigma")
 #check table
 aw_imputed_tab_sim_gamma_sigma
 
 #delta z
 #create summary table for delta_z
 aw_imputed_tab_sim_delta_z <- precis(aw_imputed_rds_simulated,depth=2,pars="delta_wealth_z")
 #check table
 aw_imputed_tab_sim_delta_z
 
 #delta sigma
 #create summary table for delta_sigma
 aw_imputed_tab_sim_delta_sigma <- precis(aw_imputed_rds_simulated,depth=2,pars="delta_wealth_sigma")
 #check table
 aw_imputed_tab_sim_delta_sigma
 
 pdf("aw_imputed_plot.pdf")
 par(mfrow=c(1,3))
 plot(aw_imputed_tab_sim_beta_z[,1]*tab_sim_beta_sigma[1,]~aw_beta[11:40],xlab="simulated beta",ylab="estimated beta")
 title("effects of absolute wealth")
 plot(aw_imputed_tab_sim_gamma_z[,1]*tab_sim_gamma_sigma[1,]~aw_gamma[11:40],xlab="simulated gamma",ylab="estimated gamma")
 title("effects of short-term wealth")
 plot(aw_imputed_tab_sim_delta_z[,1]*tab_sim_delta_sigma[1,]~aw_delta[11:40],xlab="simulated delta",ylab="estimated delta")
 title("effects of long-term wealth")
 dev.off()
 
 aw_imputed_correlations<-rbind(summary(lm(aw_imputed_tab_sim_beta_z[,1]*aw_imputed_tab_sim_beta_sigma[1,]~aw_beta[11:40])),summary(lm(aw_imputed_tab_sim_gamma_z[,1]*aw_imputed_tab_sim_gamma_sigma[1,]~aw_gamma[11:40])),summary(lm(aw_imputed_tab_sim_delta_z[,1]*tab_sim_delta_sigma[1,]~aw_delta[11:40])))
 
 write.csv(aw_imputed_correlations,file="aw_imputed_correlations.csv")
 
 
 
 
 # generate output for simulation with imputed data where short-term wealth change has the strongest effect
 #beta z
 #create summary table for beta_z
 sc_imputed_tab_sim_beta_z <- precis(sc_imputed_rds_simulated,depth=2,pars="beta_wealth_z")
 #check table
 sc_imputed_tab_sim_beta_z
 
 #beta sigma
 #create summary table for beta_sigma
 sc_imputed_tab_sim_beta_sigma <- precis(sc_imputed_rds_simulated,depth=2,pars="beta_wealth_sigma")
 #check table
 sc_imputed_tab_sim_beta_sigma
 
 #gamma z
 #create summary table for gamma_z
 sc_imputed_tab_sim_gamma_z <- precis(sc_imputed_rds_simulated,depth=2,pars="gamma_wealth_z")
 #check table
 sc_imputed_tab_sim_gamma_z
 
 #gamma sigma
 #create summary table for gamma_sigma
 sc_imputed_tab_sim_gamma_sigma <- precis(sc_imputed_rds_simulated,depth=2,pars="gamma_wealth_sigma")
 #check table
 sc_imputed_tab_sim_gamma_sigma
 
 #delta z
 #create summary table for delta_z
 sc_imputed_tab_sim_delta_z <- precis(sc_imputed_rds_simulated,depth=2,pars="delta_wealth_z")
 #check table
 sc_imputed_tab_sim_delta_z
 
 #delta sigma
 #create summary table for delta_sigma
 sc_imputed_tab_sim_delta_sigma <- precis(sc_imputed_rds_simulated,depth=2,pars="delta_wealth_sigma")
 #check table
 sc_imputed_tab_sim_delta_sigma
 
 
 pdf("sc_imputed_plot.pdf")
 par(mfrow=c(1,3))
 plot(sc_imputed_tab_sim_beta_z[,1]*sc_imputed_tab_sim_beta_sigma[1,]~sc_beta[11:40],xlab="simulated beta",ylab="estimated beta")
 title("effects of absolute wealth")
 plot(sc_imputed_tab_sim_gamma_z[,1]*sc_imputed_tab_sim_gamma_sigma[1,]~sc_gamma[11:40],xlab="simulated gamma",ylab="estimated gamma")
 title("effects of short-term wealth")
 plot(sc_imputed_tab_sim_delta_z[,1]*sc_imputed_tab_sim_delta_sigma[1,]~sc_delta[11:40],xlab="simulated delta",ylab="estimated delta")
 title("effects of long-term wealth")
 dev.off()
 
 sc_imputed_correlations<-rbind(summary(lm(sc_imputed_tab_sim_beta_z[,1]*sc_imputed_tab_sim_beta_sigma[1,]~sc_beta[11:40])),summary(lm(sc_imputed_tab_sim_gamma_z[,1]*sc_imputed_tab_sim_gamma_sigma[1,]~sc_gamma[11:40])),summary(lm(sc_imputed_tab_sim_delta_z[,1]*sc_imputed_tab_sim_delta_sigma[1,]~sc_delta[11:40])))
 
 write.csv(sc_imputed_correlations,file="sc_imputed_correlations.csv")
 
 
 
 
 # generate output for simulation with imputed data where long-term wealth variability has the strongest effect
 #beta z
 #create summary table for beta_z
 lv_imputed_tab_sim_beta_z <- precis(lv_imputed_rds_simulated,depth=2,pars="beta_wealth_z")
 #check table
 lv_imputed_tab_sim_beta_z
 
 #beta sigma
 #create summary table for beta_sigma
 lv_imputed_tab_sim_beta_sigma <- precis(lv_imputed_rds_simulated,depth=2,pars="beta_wealth_sigma")
 #check table
 lv_imputed_tab_sim_beta_sigma
 
 #gamma z
 #create summary table for gamma_z
 lv_imputed_tab_sim_gamma_z <- precis(lv_imputed_rds_simulated,depth=2,pars="gamma_wealth_z")
 #check table
 lv_imputed_tab_sim_gamma_z
 
 #gamma sigma
 #create summary table for gamma_sigma
 lv_imputed_tab_sim_gamma_sigma <- precis(lv_imputed_rds_simulated,depth=2,pars="gamma_wealth_sigma")
 #check table
 lv_imputed_tab_sim_gamma_sigma
 
 #delta z
 #create summary table for delta_z
 lv_imputed_tab_sim_delta_z <- precis(lv_imputed_rds_simulated,depth=2,pars="delta_wealth_z")
 #check table
 lv_imputed_tab_sim_delta_z
 
 #delta sigma
 #create summary table for delta_sigma
 lv_imputed_tab_sim_delta_sigma <- precis(lv_imputed_rds_simulated,depth=2,pars="delta_wealth_sigma")
 #check table
 lv_imputed_tab_sim_delta_sigma

 
 pdf("lv_imputed_plot.pdf")
 par(mfrow=c(1,3))
 plot(lv_imputed_tab_sim_beta_z[,1]*lv_imputed_tab_sim_beta_sigma[1,]~vl_beta[11:40],xlab="simulated beta",ylab="estimated beta")
 title("effects of absolute wealth")
 plot(lv_imputed_tab_sim_gamma_z[,1]*lv_imputed_tab_sim_gamma_sigma[1,]~lv_gamma[11:40],xlab="simulated gamma",ylab="estimated gamma")
 title("effects of short-term wealth")
 plot(lv_imputed_tab_sim_delta_z[,1]*lv_imputed_tab_sim_delta_sigma[1,]~lv_delta[11:40],xlab="simulated delta",ylab="estimated delta")
 title("effects of long-term wealth")
 dev.off()
 
 lv_imputed_correlations<-rbind(summary(lm(lv_imputed_tab_sim_beta_z[,1]*lv_imputed_tab_sim_beta_sigma[1,]~lv_beta[11:40])),summary(lm(lv_imputed_tab_sim_gamma_z[,1]*lv_imputed_tab_sim_gamma_sigma[1,]~lv_gamma[11:40])),summary(lm(lv_imputed_tab_sim_delta_z[,1]*lv_imputed_tab_sim_delta_sigma[1,]~lv_delta[11:40])))
 
 write.csv(lv_imputed_correlations,file="lv_imputed_correlations.csv")
 
