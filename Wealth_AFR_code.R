# PhD project on wealth and age at first reproduction ----

# This script is meant to collect all the necessary code to build up the models and explore the data necessary to understand the relationship between wealth and age at first reproduction.

## Data exploration ----

### Import data ----
#install package to import excel file
install.packages("readxl")
library(readxl)
#import data
#demographic data
data_demo <- read_excel("C:/Users/pjvar/Nextcloud/PhD/Chapter 3/data/pablo_demog_ui_dec 23 2022.xlsx",sheet = "cases with AFB & UI sent")
#wealth data
data_wealth <- read_excel("C:/Users/pjvar/Nextcloud/PhD/Chapter 3/data/hsh wealth for pablo_dec 23 2022.xls", sheet = "hshold data (n=1695)")

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

### Women who are in the sampled window

#sample size
#check the number of women who gave birth after the data collection started
table(merge_1$in_sampled_window)
#n=335
#subset sample
sample <- merge_1[merge_1$in_sampled_window==1,]


### Demographic data ----

#twin status
table(sample$twinYN)
#n=14 twin status

#### Age at first reproduction ----
#check their age at first reproduction (AFR)
summary(sample$AFB)
#min=12.58
#median=18.43
#mean=18.98
#max=32
#check for NAs
sum(is.na(sample$AFB))
#n=0
#plot it!
hist(sample$AFB,xlab="AFR",breaks=32)

#### Age ----
#check the year of birth
summary(sample$DOBYR)
#min=1966
#median=1984
#mean=1984
#max=1999
#check for NAs
sum(is.na(sample$DOBYR))
#n=0
#plot it!
hist(sample$DOBYR,xlab="Year of birth",breaks=33)

#### Relationship with household head ----

#Relationship with household head in 1995 (britstatus95)
#check britstatus95
table(sample$BritStatus95)
#check for NAs
sum(is.na(sample$BritStatus95))
#n=136
#plot it!
plot(table(sample$BritStatus95),xlab="Relationship with household")

#Relationship with household head in 1998 (britstatus98)
#check britstatus98
table(sample$britstatus98)
#check for NAs
sum(is.na(sample$britstatus98))
#n=106
#plot it!
plot(table(sample$britstatus98),xlab="Relationship with household")

#Relationship with household head in 2000 (britstatus00)
#check britstatus00
table(sample$britstatus00)
#check for NAs
sum(is.na(sample$britstatus00))
#n=86
#plot it!
plot(table(sample$britstatus00),xlab="Relationship with household")

#Relationship with household head in 2002 (britstatus02)
#check britstatus02
table(sample$britstatus02)
#check for NAs
sum(is.na(sample$britstatus02))
#n=74
#plot it!
plot(table(sample$britstatus02),xlab="Relationship with household")

#Relationship with household head in 2004 (britstatus04)
#check britstatus04
table(sample$britstatus04)
#check for NAs
sum(is.na(sample$britstatus04))
#n=67
#plot it!
plot(table(sample$britstatus04),xlab="Relationship with household")

#Relationship with household head in 2006 (britstatus06)
#check britstatus06
table(sample$britstatus06)
#check for NAs
sum(is.na(sample$britstatus06))
#n=63
#plot it!
plot(table(sample$britstatus06),xlab="Relationship with household")

#Relationship with household head in 2010 (britstatus10)
#check britstatus10
table(sample$britstatus10)
#check for NAs
sum(is.na(sample$britstatus10))
#n=52
#plot it!
plot(table(sample$britstatus10),xlab="Relationship with household")

#number of individuals with britstatus in all censuses
summary(complete.cases(sample[c("BritStatus95","britstatus98","britstatus00","britstatus02","britstatus04","britstatus06","britstatus10")]))
#n=179

#### Household/farm that focal is associated with ----

#Household/farm that focal is associated with in 1995 (h95n)
#check h95n
table(sample$h95n)
#check for NAs
sum(is.na(sample$h95n))
#n=136
#plot it!
plot(table(sample$h95n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 1998 (h98n)
#check h98n
table(sample$h98n)
#check for NAs
sum(is.na(sample$h98n))
#n=136
#plot it!
plot(table(sample$h98n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2000 (h00n)
#check h00n
table(sample$h00n)
#check for NAs
sum(is.na(sample$h00n))
#n=136
#plot it!
plot(table(sample$h00n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2002 (h02n)
#check h02n
table(sample$h02n)
#check for NAs
sum(is.na(sample$h02n))
#n=136
#plot it!
plot(table(sample$h02n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2004 (h04n)
#check h04n
table(sample$h04n)
#check for NAs
sum(is.na(sample$h04n))
#n=136
#plot it!
plot(table(sample$h04n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2006 (h06n)
#check h06n
table(sample$h06n)
#check for NAs
sum(is.na(sample$h06n))
#n=136
#plot it!
plot(table(sample$h06n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2010 (h10n)
#check h10n
table(sample$h10n)
#check for NAs
sum(is.na(sample$h10n))
#n=136
#plot it!
plot(table(sample$h10n),xlab="Household focal is associated with")

#number of individuals with hxxn in all censuses
summary(complete.cases(sample[c("h95n","h98n","h00n","h02n","h04n","h06n","h10n")]))
#n=186

#### Household/farm that focal is living in ----

#Household/farm that focal is associated with in 1995 (h95a)
#check h95a
table(sample$h95a)
#check for NAs
sum(is.na(sample$h95a))
#n=136
#plot it!
plot(table(sample$h95a),xlab="Household focal lives in")

#Household/farm that focal is associated with in 1998 (h98a)
#check h98a
table(sample$h98a)
#check for NAs
sum(is.na(sample$h98a))
#n=106
#plot it!
plot(table(sample$h98a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2000 (h00a)
#check h00a
table(sample$h00a)
#check for NAs
sum(is.na(sample$h00a))
#n=86
#plot it!
plot(table(sample$h00a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2002 (h02n)
#check h02a
table(sample$h02a)
#check for NAs
sum(is.na(sample$h02a))
#n=72
#plot it!
plot(table(sample$h02a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2004 (h04a)
#check h04a
table(sample$h04a)
#check for NAs
sum(is.na(sample$h04a))
#n=60
#plot it!
plot(table(sample$h04a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2006 (h06a)
#check h06a
table(sample$h06a)
#check for NAs
sum(is.na(sample$h06a))
#n=56
#plot it!
plot(table(sample$h06a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2010 (h10a)
#check h10a
table(sample$h10a)
#check for NAs
sum(is.na(sample$h10a))
#n=48
#plot it!
plot(table(sample$h10a),xlab="Household focal is associated with")

#number of individuals with hxxn in all censuses
summary(complete.cases(sample[c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")]))
#n=186

### Matching ID ----

#1995 
#summary
sample$UIyearXhsh95
table(sample$UIyearXhsh95)
#check NAs
sum(is.na(sample$UIyearXhsh95))
#n=145

#1998 
#summary
sample$UIyearXhsh98
table(sample$UIyearXhsh98)
#check NAs
sum(is.na(sample$UIyearXhsh98))
#n=137

#2000 
#summary
sample$UIyearXhsh00
table(sample$UIyearXhsh00)
#check NAs
sum(is.na(sample$UIyearXhsh00))
#n=135

#2002 
#summary
sample$UIyearXhsh02
table(sample$UIyearXhsh02)
#check NAs
sum(is.na(sample$UIyearXhsh02))
#n=130

#2004 
#summary
sample$UIyearXhsh04
table(sample$UIyearXhsh04)
#check NAs
sum(is.na(sample$UIyearXhsh04))
#n=141

#2006 
#summary
sample$UIyearXhsh06
table(sample$UIyearXhsh06)
#check NAs
sum(is.na(sample$UIyearXhsh06))
#n=147

#2010 
#summary
sample$UIyearXhsh10
table(sample$UIyearXhsh10)
#check NAs
sum(is.na(sample$UIyearXhsh10))
#n=132

#number of individuals with matching wealth data in all censuses
summary(complete.cases(sample[c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10")]))
#n=89

#number of individuals with wealth data and household information in all censuses
summary(complete.cases(sample[c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10","BritStatus95","britstatus98","britstatus00","britstatus02","britstatus04","britstatus06","britstatus10","h95n","h98n","h00n","h02n","h04n","h06n","h10n")]))
#n=89

## Material wealth data ----

### Cash crops ----

#### Summary statistics per year ----

#1995
summary(sample$ttsacks95)
sd(sample$ttsacks95,na.rm=T)
#min=0
#median=6.5
#mean=7.339
#max=25.5
#sd=5.339
#check NAs
sum(is.na(sample$ttsacks95))
#n=157
#plot it!
hist(sample$ttsacks95,breaks=75)
plot(density(sample$ttsacks95,na.rm=T))

#1998
summary(sample$ttsacks98)
sd(sample$ttsacks98,na.rm=T)
#min=0
#median=3
#mean=4.45
#max=24
#sd=4.343
#check NAs
sum(is.na(sample$ttsacks98))
#n=141
#plot it!
hist(sample$ttsacks98,breaks=75)
plot(density(sample$ttsacks98,na.rm=T))

#2000
summary(sample$ttsacks00)
sd(sample$ttsacks00,na.rm=T)
#min=0
#median=5
#mean=7.3
#max=48
#sd=7.968
#check NAs
sum(is.na(sample$ttsacks00))
#n=141
#plot it!
hist(sample$ttsacks00,breaks=75)
plot(density(sample$ttsacks00,na.rm=T))

#2002
summary(sample$ttsacks02)
sd(sample$ttsacks02,na.rm=T)
#min=0
#median=6.5
#mean=8.161
#max=49
#sd=7.859
#check NAs
sum(is.na(sample$ttsacks02))
#n=140
#plot it!
hist(sample$ttsacks02,breaks=75)
plot(density(sample$ttsacks02,na.rm=T))

#2004
summary(sample$ttsacks04)
sd(sample$ttsacks04,na.rm=T)
#min=0
#median=6.33
#mean=10.67
#max=58.5
#sd=11.424
#check NAs
sum(is.na(sample$ttsacks04))
#n=150
#plot it!
hist(sample$ttsacks04,breaks=75)
plot(density(sample$ttsacks04,na.rm=T))

#2006
summary(sample$ttsacks06)
sd(sample$ttsacks06,na.rm=T)
#min=0
#median=10.5
#mean=13.19
#max=66
#sd=12.671
#check NAs
sum(is.na(sample$ttsacks06))
#n=158
#plot it!
hist(sample$ttsacks06,breaks=75)
plot(density(sample$ttsacks06,na.rm=T))

#2010
summary(sample$ttsacks10)
sd(sample$ttsacks10,na.rm=T)
#min=0
#median=7.5
#mean=12.68
#max=74
#sd=14.5
#check NAs
sum(is.na(sample$ttsacks10))
#n=140
#plot it!
hist(sample$ttsacks10,breaks=75)
plot(density(sample$ttsacks10,na.rm=T))

#### Plot them together ----

#histograms
layout( matrix(c(1,1,2,2,3,3,4,4,0,5,5,6,6,7,7,0), nrow=2, byrow=TRUE) )
hist(sample$ttsacks95,breaks=75)
hist(sample$ttsacks98,breaks=75)
hist(sample$ttsacks00,breaks=75)
hist(sample$ttsacks02,breaks=75)
hist(sample$ttsacks04,breaks=75)
hist(sample$ttsacks06,breaks=75)
hist(sample$ttsacks10,breaks=75)

#density plots
layout( matrix(c(1,1,2,2,3,3,4,4,0,5,5,6,6,7,7,0), nrow=2, byrow=TRUE) )
plot(density(sample$ttsacks95,na.rm=T),xlim=c(0,80))
plot(density(sample$ttsacks98,na.rm=T),xlim=c(0,80))
plot(density(sample$ttsacks00,na.rm=T),xlim=c(0,80))
plot(density(sample$ttsacks02,na.rm=T),xlim=c(0,80))
plot(density(sample$ttsacks04,na.rm=T),xlim=c(0,80))
plot(density(sample$ttsacks06,na.rm=T),xlim=c(0,80))
plot(density(sample$ttsacks10,na.rm=T),xlim=c(0,80))

#Riana's plot
#sample from the database
#set seed
set.seed(1)
#sample
x <- sample[sample(nrow(sample),50),]
#check the variables
#check for NAs
sum(is.na(x$AFB)) #n=0
sum(is.na(x$ttsacks95)) #n=21
sum(is.na(x$ttsacks98)) #n=18
sum(is.na(x$ttsacks00)) #n=15
sum(is.na(x$ttsacks02)) #n=16
sum(is.na(x$ttsacks04)) #n=21
sum(is.na(x$ttsacks06)) #n=20
sum(is.na(x$ttsacks10)) #n=19
#look at the sample
x[,c("AFB","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]
#check how many many women don't have cash crops data
x[which(sum(is.na(x[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==7)),c("AFB","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]

### Farming land ----

### Months without grain ----

### Holdings, house value, and assets ----