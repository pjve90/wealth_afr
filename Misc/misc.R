#import data
x <- read.csv("dataf.csv")[,-1]
#check data
x
#select wealth
x[,c("absw95","absw98","absw00","absw02","absw04","absw06","absw10")]
#check the length of wealth observation of one observation
length(which(!is.na(x[1,c("absw95","absw98","absw00","absw02","absw04","absw06","absw10")])))
#define the length 
for(i in 1:nrow(x)){
  x$length[i] <- length(which(!is.na(x[i,c("absw95","absw98","absw00","absw02","absw04","absw06","absw10")])))

}
#check length
x$length
#see sample with  minimum 3 observations of wealth
length(which(x$length>=3)) #n=338
x[which(x$length>=3),]
hist(x$afr[which(x$length>=3)])


logit(p(i,t)) = A[age[i,t]] + B*[coef_var[i,t]]

wealth[i,t] ~ normal(wealth_mu[i], wealth_sigma[i])
coef_var[i,t] = wealth_sigma[i]) / wealth_mu[i]

