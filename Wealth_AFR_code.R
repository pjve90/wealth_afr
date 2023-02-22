# PhD project on wealth and age at first reproduction ----

# This script is meant to collect all the necessary code to build up the models and explore the data necessary to understand the relationship between wealth and age at first reproduction.

## Data exploration ----

### Import data ----
#install package to import excel file
install.packages("readxl")
library(readxl)
#import data
#demographic data
data_demo <- read_excel("C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/data/pablo_demog_ui_dec 23 2022.xlsx",sheet = "cases with AFB & UI sent")
#wealth data
data_wealth <- read_excel("C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/data/hsh wealth for pablo_dec 23 2022.xls", sheet = "hshold data (n=1695)")

### Demographic data ----

#sample size
#check the number of women who gave birth after the data collection started
table(data_demo$in_sampled_window)
#n=335
#subset sample
sample_demo <- data_demo[data_demo$in_sampled_window==1,]

#twin status
table(sample_demo$twinYN)
#n=14 twin status

#### Age at first reproduction ----
#check their age at first reproduction (AFR)
summary(sample_demo$AFB)
#min=12.58
#median=18.43
#mean=18.98
#max=32
#check for NAs
sum(is.na(sample_demo$AFB))
#n=0
#plot it!
hist(sample_demo$AFB,xlab="AFR",breaks=32)

#### Age ----
#check the year of birth
summary(sample_demo$DOBYR)
#min=1966
#median=1984
#mean=1984
#max=1999
#check for NAs
sum(is.na(sample_demo$DOBYR))
#n=0
#plot it!
hist(sample_demo$DOBYR,xlab="Year of birth",breaks=33)

#### Relationship with household head ----

#Relationship with household head in 1995 (britstatus95)
#check britstatus05
table(sample_demo$BritStatus95)
#check for NAs
sum(is.na(sample_demo$BritStatus95))
#n=136
#plot it!
plot(table(sample_demo$BritStatus95),xlab="Relationship with household")

#Relationship with household head in 1998 (britstatus98)
#check britstatus05
table(sample_demo$britstatus98)
#check for NAs
sum(is.na(sample_demo$britstatus98))
#n=136
#plot it!
plot(table(sample_demo$britstatus98),xlab="Relationship with household")

#Relationship with household head in 2000 (britstatus00)
#check britstatus05
table(sample_demo$britstatus00)
#check for NAs
sum(is.na(sample_demo$britstatus00))
#n=86
#plot it!
plot(table(sample_demo$britstatus00),xlab="Relationship with household")

#Relationship with household head in 2002 (britstatus02)
#check britstatus05
table(sample_demo$britstatus02)
#check for NAs
sum(is.na(sample_demo$britstatus02))
#n=74
#plot it!
plot(table(sample_demo$britstatus02),xlab="Relationship with household")

#Relationship with household head in 2004 (britstatus04)
#check britstatus05
table(sample_demo$britstatus04)
#check for NAs
sum(is.na(sample_demo$britstatus04))
#n=67
#plot it!
plot(table(sample_demo$britstatus04),xlab="Relationship with household")

#Relationship with household head in 2006 (britstatus06)
#check britstatus05
table(sample_demo$britstatus06)
#check for NAs
sum(is.na(sample_demo$britstatus06))
#n=63
#plot it!
plot(table(sample_demo$britstatus06),xlab="Relationship with household")

#Relationship with household head in 2010 (britstatus10)
#check britstatus05
table(sample_demo$britstatus10)
#check for NAs
sum(is.na(sample_demo$britstatus10))
#n=52
#plot it!
plot(table(sample_demo$britstatus10),xlab="Relationship with household")

#number of individuals with britstatus in all censuses
summary(complete.cases(sample_demo[c("BritStatus95","britstatus98","britstatus00","britstatus02","britstatus04","britstatus06","britstatus10")]))
#n=179

#### Household/farm that focal is associated with ----

#Household/farm that focal is associated with in 1995 (h95n)
#check h95n
table(sample_demo$h95n)
#check for NAs
sum(is.na(sample_demo$h95n))
#n=136
#plot it!
plot(table(sample_demo$h95n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 1998 (h98n)
#check h98n
table(sample_demo$h98n)
#check for NAs
sum(is.na(sample_demo$h98n))
#n=136
#plot it!
plot(table(sample_demo$h98n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2000 (h00n)
#check h00n
table(sample_demo$h00n)
#check for NAs
sum(is.na(sample_demo$h00n))
#n=136
#plot it!
plot(table(sample_demo$h00n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2002 (h02n)
#check h02n
table(sample_demo$h02n)
#check for NAs
sum(is.na(sample_demo$h02n))
#n=136
#plot it!
plot(table(sample_demo$h02n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2004 (h04n)
#check h04n
table(sample_demo$h04n)
#check for NAs
sum(is.na(sample_demo$h04n))
#n=136
#plot it!
plot(table(sample_demo$h04n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2006 (h06n)
#check h06n
table(sample_demo$h06n)
#check for NAs
sum(is.na(sample_demo$h06n))
#n=136
#plot it!
plot(table(sample_demo$h06n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2010 (h10n)
#check h10n
table(sample_demo$h10n)
#check for NAs
sum(is.na(sample_demo$h10n))
#n=136
#plot it!
plot(table(sample_demo$h10n),xlab="Household focal is associated with")

#number of individuals with hxxn in all censuses
summary(complete.cases(sample_demo[c("h95n","h98n","h00n","h02n","h04n","h06n","h10n")]))
#n=186

#### Household/farm that focal is living in ----

#Household/farm that focal is associated with in 1995 (h95a)
#check h95a
table(sample_demo$h95a)
#check for NAs
sum(is.na(sample_demo$h95a))
#n=136
#plot it!
plot(table(sample_demo$h95a),xlab="Household focal lives in")

#Household/farm that focal is associated with in 1998 (h98a)
#check h98a
table(sample_demo$h98a)
#check for NAs
sum(is.na(sample_demo$h98a))
#n=106
#plot it!
plot(table(sample_demo$h98a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2000 (h00a)
#check h00a
table(sample_demo$h00a)
#check for NAs
sum(is.na(sample_demo$h00a))
#n=86
#plot it!
plot(table(sample_demo$h00a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2002 (h02n)
#check h02a
table(sample_demo$h02a)
#check for NAs
sum(is.na(sample_demo$h02a))
#n=72
#plot it!
plot(table(sample_demo$h02a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2004 (h04a)
#check h04a
table(sample_demo$h04a)
#check for NAs
sum(is.na(sample_demo$h04a))
#n=60
#plot it!
plot(table(sample_demo$h04a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2006 (h06a)
#check h06a
table(sample_demo$h06a)
#check for NAs
sum(is.na(sample_demo$h06a))
#n=56
#plot it!
plot(table(sample_demo$h06a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2010 (h10a)
#check h10a
table(sample_demo$h10a)
#check for NAs
sum(is.na(sample_demo$h10a))
#n=48
#plot it!
plot(table(sample_demo$h10a),xlab="Household focal is associated with")

#number of individuals with hxxn in all censuses
summary(complete.cases(sample_demo[c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")]))
#n=186

### Matching ID ----

#1995 
#summary
sample_demo$UIyearXhsh95
table(sample_demo$UIyearXhsh95)
#check NAs
sum(is.na(sample_demo$UIyearXhsh95))
#n=145

#1998 
#summary
sample_demo$UIyearXhsh98
table(sample_demo$UIyearXhsh98)
#check NAs
sum(is.na(sample_demo$UIyearXhsh98))
#n=137

#2000 
#summary
sample_demo$UIyearXhsh00
table(sample_demo$UIyearXhsh00)
#check NAs
sum(is.na(sample_demo$UIyearXhsh00))
#n=135

#2002 
#summary
sample_demo$UIyearXhsh02
table(sample_demo$UIyearXhsh02)
#check NAs
sum(is.na(sample_demo$UIyearXhsh02))
#n=130

#2004 
#summary
sample_demo$UIyearXhsh04
table(sample_demo$UIyearXhsh04)
#check NAs
sum(is.na(sample_demo$UIyearXhsh04))
#n=141

#2006 
#summary
sample_demo$UIyearXhsh06
table(sample_demo$UIyearXhsh06)
#check NAs
sum(is.na(sample_demo$UIyearXhsh06))
#n=147

#2010 
#summary
sample_demo$UIyearXhsh10
table(sample_demo$UIyearXhsh10)
#check NAs
sum(is.na(sample_demo$UIyearXhsh10))
#n=132

#number of individuals with matching wealth data in all censuses
summary(complete.cases(sample_demo[c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10")]))
#n=89

#number of individuals with wealth data and household information in all censuses
summary(complete.cases(sample_demo[c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10","BritStatus95","britstatus98","britstatus00","britstatus02","britstatus04","britstatus06","britstatus10","h95n","h98n","h00n","h02n","h04n","h06n","h10n")]))
#n=89

## Material wealth data ----

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
x <- merge(data_demo,data_wealth_95,by=c("UIyearXhsh95"),all.x=T)
#1998
x <- merge(x,data_wealth_98,by=c("UIyearXhsh98"),all.x=T)
#2000
x <- merge(x,data_wealth_00,by=c("UIyearXhsh00"),all.x=T)
#2002
x <- merge(x,data_wealth_02,by=c("UIyearXhsh02"),all.x=T)
#2004
x <- merge(x,data_wealth_04,by=c("UIyearXhsh04"),all.x=T)
#2006
x <- merge(x,data_wealth_06,by=c("UIyearXhsh06"),all.x=T)
#2010
x <- merge(x,data_wealth_10,by=c("UIyearXhsh10"),all.x=T)

### Farming land ----

### Cash crops ----

### Months without grain ----

### Holdings, house value, and assets ----