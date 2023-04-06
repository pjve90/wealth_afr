# PhD project on wealth and age at first reproduction ----

# This script is meant to collect all the necessary code to build up the models and explore the data necessary to understand the relationship between wealth and age at first reproduction.

#install package to import excel file
#install.packages("readxl")
library(readxl)
#install package to manipulate transparency in plots
#install.packages("scales")
#library(scales)

## Data exploration ----

### Import data ----
#import data
#demographic data
data_demo <- read_excel("C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/data/pablo_demog_ui_dec 23 2022.xlsx",sheet = "cases with AFB & UI sent")
#wealth data
data_wealth <- read_excel("C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/data/hsh wealth for pablo_dec 23 2022.xls", sheet = "hshold data (n=1695)")

### Merge data ----

#subset wealth data by year
#1995
data_wealth_95 <- data_wealth[which(data_wealth$UIyearXhsh > 1995000 & data_wealth$UIyearXhsh < 1996000),]
#1998
data_wealth_98 <- data_wealth[which(data_wealth$UIyearXhsh > 1998000 & data_wealth$UIyearXhsh < 1999000),]
#2000
data_wealth_00 <- data_wealth[which(data_wealth$UIyearXhsh > 2000000 & data_wealth$UIyearXhsh < 2001000),]
#2002
data_wealth_02 <- data_wealth[which(data_wealth$UIyearXhsh > 2002000 & data_wealth$UIyearXhsh < 2003000),]
#2004
data_wealth_04 <- data_wealth[which(data_wealth$UIyearXhsh > 2004000 & data_wealth$UIyearXhsh < 2005000),]
#2006
data_wealth_06 <- data_wealth[which(data_wealth$UIyearXhsh > 2006000 & data_wealth$UIyearXhsh < 2007000),]
#2010
data_wealth_10 <- data_wealth[which(data_wealth$UIyearXhsh > 2010000 & data_wealth$UIyearXhsh < 2011000),]

#change UIyearXhsh name by year
#1995
names(data_wealth_95)[names(data_wealth_95) == "UIyearXhsh"] <- "UIyearXhsh95"
colnames(data_wealth_95) <- c("UIyearXhsh95","acused95","ttsacks95","jjunezero95","LstockValField_rdKts95","HsVal_rdKts95","HsTotAsset_rdKts95","SumValue_rdKts95","IWC95","Loiske_rev95")
#1998
names(data_wealth_98)[names(data_wealth_98) == "UIyearXhsh"] <- "UIyearXhsh98"
colnames(data_wealth_98) <- c("UIyearXhsh98","acused98","ttsacks98","jjunezero98","LstockValField_rdKts98","HsVal_rdKts98","HsTotAsset_rdKts98","SumValue_rdKts98","IWC98","Loiske_rev98")
#2000
names(data_wealth_00)[names(data_wealth_00) == "UIyearXhsh"] <- "UIyearXhsh00"
colnames(data_wealth_00) <- c("UIyearXhsh00","acused00","ttsacks00","jjunezero00","LstockValField_rdKts00","HsVal_rdKts00","HsTotAsset_rdKts00","SumValue_rdKts00","IWC00","Loiske_rev00")
#2002
names(data_wealth_02)[names(data_wealth_02) == "UIyearXhsh"] <- "UIyearXhsh02"
colnames(data_wealth_02) <- c("UIyearXhsh02","acused02","ttsacks02","jjunezero02","LstockValField_rdKts02","HsVal_rdKts02","HsTotAsset_rdKts02","SumValue_rdKts02","IWC02","Loiske_rev02")
#2004
names(data_wealth_04)[names(data_wealth_04) == "UIyearXhsh"] <- "UIyearXhsh04"
colnames(data_wealth_04) <- c("UIyearXhsh04","acused04","ttsacks04","jjunezero04","LstockValField_rdKts04","HsVal_rdKts04","HsTotAsset_rdKts04","SumValue_rdKts04","IWC04","Loiske_rev04")
#2006
names(data_wealth_06)[names(data_wealth_06) == "UIyearXhsh"] <- "UIyearXhsh06"
colnames(data_wealth_06) <- c("UIyearXhsh06","acused06","ttsacks06","jjunezero06","LstockValField_rdKts06","HsVal_rdKts06","HsTotAsset_rdKts06","SumValue_rdKts06","IWC06","Loiske_rev06")
#2010
names(data_wealth_10)[names(data_wealth_10) == "UIyearXhsh"] <- "UIyearXhsh10"
colnames(data_wealth_10) <- c("UIyearXhsh10","acused10","ttsacks10","jjunezero10","LstockValField_rdKts10","HsVal_rdKts10","HsTotAsset_rdKts10","SumValue_rdKts10","IWC10","Loiske_rev10")

#merge with demographic data
#1995
merge_1 <- merge(data_demo,data_wealth_95,by=c("UIyearXhsh95"),all.x=T)
#1998
merge_1 <- merge(merge_1,data_wealth_98,by=c("UIyearXhsh98"),all.x=T)
#2000
merge_1 <- merge(merge_1,data_wealth_00,by=c("UIyearXhsh00"),all.x=T)
#2002
merge_1 <- merge(merge_1,data_wealth_02,by=c("UIyearXhsh02"),all.x=T)
#2004
merge_1 <- merge(merge_1,data_wealth_04,by=c("UIyearXhsh04"),all.x=T)
#2006
merge_1 <- merge(merge_1,data_wealth_06,by=c("UIyearXhsh06"),all.x=T)
#2010
merge_1 <- merge(merge_1,data_wealth_10,by=c("UIyearXhsh10"),all.x=T)

#women who are in the sampled window

#sample size
#check the number of women who gave birth after the data collection started
table(merge_1$in_sampled_window)
#n=335
#subset sample
sample <- merge_1[merge_1$in_sampled_window==1,]

##Logistic regression ----

### Model with only alpha ----

#define some prior
set.seed(1990)
hist(rbinom(350,45,0.5))
prob <- 0.5
#create synthetic data
synth_data <- data.frame(AFR=rbinom(350,45,0.5))
hist(synth_data$AFR)
#prepare data for model
synth_data_list <- list(
  AFR=synth_data$AFR
)
#define model
mlog1 <- ulam(
  alist(
    AFR ~ dbinom(45,prob),
    logit(prob) <- a,
    a ~ dnorm(0,0.5)
  ),data=synth_data_list,chains = 4
)
mlog2 <- ulam(
  alist(
    AFR ~ dbinom(45,prob),
    logit(prob) <- a,
    a ~ dnorm(0,1)
  ),data=synth_data_list,chains = 4
)
mlog3 <- ulam(
  alist(
    AFR ~ dbinom(45,prob),
    logit(prob) <- a,
    a ~ dnorm(0,1.5)
  ),data=synth_data_list,chains = 4
)
mlog4 <- ulam(
  alist(
    AFR ~ dbinom(45,prob),
    logit(prob) <- a,
    a ~ dnorm(0,2)
  ),data=synth_data_list,chains = 4
)
#check priors
set.seed(1960)
prior1 <- extract.prior(mlog1,n=350)
p1 <- inv_logit(prior1$a)
dens(p1,adj=0.1)
prior2 <- extract.prior(mlog2,n=350)
p2 <- inv_logit(prior2$a)
dens(p2,adj=0.1)
prior3 <- extract.prior(mlog3,n=350)
p3 <- inv_logit(prior3$a)
dens(p3,adj=0.1)
prior4 <- extract.prior(mlog4,n=350)
p4 <- inv_logit(prior4$a)
dens(p4,adj=0.1)

#prior for alpha=dnorm(0,2)


#
# 
# - Define a generative model of the sample.
# - Define a specific estimand.
# - Design a statistical way to produce estimate.
# - Test the statistical model using the generative model.
# - Analyse the sample (summarise).
# 

