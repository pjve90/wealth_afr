
# Impute missing wealth data - wealth data are standardized, so we replace missing values with random draws from a normal distribution with mean 0 and standard deviation 1
baby_std_absw_restricted<-std_absw_restricted

for(k in 1:nrow(baby_std_absw_restricted)){
  for(l in 1:ncol(baby_std_absw_restricted)){
  if(baby_std_absw_restricted[k,l]== -99){baby_std_absw_restricted[k,l]<-rnorm(1,mean=0,sd=1)}
    }
}

# create vectors to store the wealth change when individuals had no baby and when individuals had a baby
nobabywealth<-NA
babywealth<-NA

# loop through the birth data. In years when an individual had a baby, store the wealth change (wealth year baby was born minus wealth year before) in the babywealth vector; when individual had no baby, store the wealth change in the nobabywealth vector.
for(i in 1:nrow(afrs_restricted)){
  for(j in 2:ncol(afrs_restricted)){
    
    if(afrs_restricted[i,j]==0){nobabywealth<-c(nobabywealth,(baby_std_absw_restricted[i,j]-baby_std_absw_restricted[i,j-1]) )}
    if(afrs_restricted[i,j]==1){
      babywealth<-c(babywealth,(baby_std_absw_restricted[i,j]-baby_std_absw_restricted[i,j-1]) )
      break
    }
  }
  
}

# Remove the original entry from the two vectors
babywealth<-babywealth[2:length(babywealth)]
nobabywealth<-nobabywealth[2:length(nobabywealth)]

# create a data frame for the model
babywealthdata<-list(baby=c(rep(2,length(babywealth)),rep(1,length(nobabywealth))),
                            wealth=c(babywealth,nobabywealth)
)

# Run the model, assuming that the change in wealth depends on whether individuals did or did not have a baby.
babywealthmodel<-ulam(
  alist(
    wealth ~ dnorm( mu , sigma ) ,
    mu <- a[baby] ,
    a[baby]~dnorm(0,1),
    sigma~dexp(1)
  ) , data=babywealthdata,cmdstan=T ,log_lik=T, chains=4,cores=4)

precis(babywealthmodel,depth=2)
# mean   sd  5.5% 94.5% rhat ess_bulk
# a[1]   0.04 0.02  0.00  0.08    1  1817.10 - in a year withouth a birth, women increase their wealth
# a[2]  -0.08 0.08 -0.21  0.06    1  1977.36 - in a year with a birth, women decrease their wealth
# sigma  1.37 0.02  1.34  1.39    1  1705.74

post<-extract.samples(babywealthmodel)
contrast<-post$a[,2]-post$a[,1]
precis(contrast)
# mean   sd  5.5% 94.5%   histogram
# contrast -0.12 0.09 -0.26  0.02  - giving birth is more likely to lead to a loss of wealth than not giving birth


# Plot the distributions
boxplot(babywealthdata$wealth~babywealthdata$baby)

