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

#sample from the database for Riana's plot
#set seed
set.seed(2690)
#sample
x <- sample[sample(nrow(sample),100),]

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

#check the variables
#check for NAs
#AFR
sum(is.na(x$AFB)) #n=0
#1995
#matching ID
sum(is.na(x$UIyearXhsh95)) #n=21
#cash crops
sum(is.na(x$ttsacks95)) #n=21
#1998
#matching ID
sum(is.na(x$UIyearXhsh98)) #n=17
#cash crops
sum(is.na(x$ttsacks98)) #n=18
#2000
#matching ID
sum(is.na(x$UIyearXhsh00)) #n=15
#cash crops
sum(is.na(x$ttsacks00)) #n=15
#2002
#matching ID
sum(is.na(x$UIyearXhsh02)) #n=15
#cash crops
sum(is.na(x$ttsacks02)) #n=16
#2004
#matching ID
sum(is.na(x$UIyearXhsh04)) #n=19
#cash crops
sum(is.na(x$ttsacks04)) #n=21
#2006
#matching ID
sum(is.na(x$UIyearXhsh06)) #n=20
#cash crops
sum(is.na(x$ttsacks06)) #n=20
#2010
#matching ID
sum(is.na(x$UIyearXhsh10)) #n=19
#cash crops
sum(is.na(x$ttsacks10)) #n=19

#look at the sample
x[,c("code","AFB","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]
#check how many many women don't have cash crops data
x[rowSums(is.na(x[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])) == 7,c("code","AFB","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]
nrow(x[rowSums(is.na(x[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])) == 7,c("code","AFB","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])
#n=9
#subset as TRUE/FALSE
y_ttsacks <- is.na(x[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])
#plot it!
colors <- c("white","black")
plot(NULL,xlim=c(0,ncol(y_ttsacks)),ylim=c(0,nrow(y_ttsacks)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_ttsacks)), c(col(y_ttsacks))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_ttsacks)]
     )

### Farming land ----

#### Summary statistics per year ----

#1995
summary(sample$acused95)
sd(sample$acused95,na.rm=T)
#min=0
#median=3
#mean=3.862
#max=15
#sd=2.73
#check NAs
sum(is.na(sample$acused95))
#n=157
#plot it!
hist(sample$acused95,breaks=15)
plot(density(sample$acused95,na.rm=T))

#1998
summary(sample$acused98)
sd(sample$acused98,na.rm=T)
#min=0
#median=4
#mean=4.527
#max=12.5
#sd=2.409
#check NAs
sum(is.na(sample$acused98))
#n=147
#plot it!
hist(sample$acused98,breaks=13)
plot(density(sample$acused98,na.rm=T))

#2000
summary(sample$acused00)
sd(sample$acused00,na.rm=T)
#min=0
#median=3
#mean=3.354
#max=12
#sd=2.169
#check NAs
sum(is.na(sample$acused00))
#n=142
#plot it!
hist(sample$acused00,breaks=12)
plot(density(sample$acused00,na.rm=T))

#2002
summary(sample$acused02)
sd(sample$acused02,na.rm=T)
#min=0
#median=2
#mean=2.544
#max=13
#sd=1.926
#check NAs
sum(is.na(sample$acused02))
#n=148
#plot it!
hist(sample$acused02,breaks=13)
plot(density(sample$acused02,na.rm=T))

#2004
summary(sample$acused04)
sd(sample$acused04,na.rm=T)
#min=0
#median=2
#mean=2.377
#max=8
#sd=1.697
#check NAs
sum(is.na(sample$acused04))
#n=150
#plot it!
hist(sample$acused04,breaks=8)
plot(density(sample$acused04,na.rm=T))

#2006
summary(sample$acused06)
sd(sample$acused06,na.rm=T)
#min=0
#median=2
#mean=2.612
#max=13
#sd=2.022
#check NAs
sum(is.na(sample$acused06))
#n=158
#plot it!
hist(sample$acused06,breaks=13)
plot(density(sample$acused06,na.rm=T))

#2010
summary(sample$acused10)
sd(sample$acused10,na.rm=T)
#min=0
#median=2
#mean=3.023
#max=9
#sd=2.037
#check NAs
sum(is.na(sample$acused10))
#n=140
#plot it!
hist(sample$acused10,breaks=9)
plot(density(sample$acused10,na.rm=T))

#### Plot them together ----

#histograms
layout( matrix(c(1,1,2,2,3,3,4,4,0,5,5,6,6,7,7,0), nrow=2, byrow=TRUE) )
hist(sample$acused95,breaks=15)
hist(sample$acused98,breaks=15)
hist(sample$acused00,breaks=15)
hist(sample$acused02,breaks=15)
hist(sample$acused04,breaks=15)
hist(sample$acused06,breaks=15)
hist(sample$acused10,breaks=15)

#density plots
layout( matrix(c(1,1,2,2,3,3,4,4,0,5,5,6,6,7,7,0), nrow=2, byrow=TRUE) )
plot(density(sample$acused95,na.rm=T),xlim=c(0,18))
plot(density(sample$acused98,na.rm=T),xlim=c(0,18))
plot(density(sample$acused00,na.rm=T),xlim=c(0,18))
plot(density(sample$acused02,na.rm=T),xlim=c(0,18))
plot(density(sample$acused04,na.rm=T),xlim=c(0,18))
plot(density(sample$acused06,na.rm=T),xlim=c(0,18))
plot(density(sample$acused10,na.rm=T),xlim=c(0,18))

#Riana's plot

#check the variables
#check for NAs
#AFR
sum(is.na(x$AFB)) #n=0
#1995
#matching ID
sum(is.na(x$UIyearXhsh95)) #n=46
#cash crops
sum(is.na(x$acused95)) #n=48
#1998
#matching ID
sum(is.na(x$UIyearXhsh98)) #n=39
#cash crops
sum(is.na(x$acused98)) #n=43
#2000
#matching ID
sum(is.na(x$UIyearXhsh00)) #n=38
#cash crops
sum(is.na(x$acused00)) #n=41
#2002
#matching ID
sum(is.na(x$UIyearXhsh02)) #n=34
#cash crops
sum(is.na(x$acused02)) #n=43
#2004
#matching ID
sum(is.na(x$UIyearXhsh04)) #n=42
#cash crops
sum(is.na(x$acused04)) #n=44
#2006
#matching ID
sum(is.na(x$UIyearXhsh06)) #n=40
#cash crops
sum(is.na(x$acused06)) #n=44
#2010
#matching ID
sum(is.na(x$UIyearXhsh10)) #n=37
#cash crops
sum(is.na(x$acused10)) #n=41

#look at the sample
x[,c("code","AFB","acused95","acused98","acused00","acused02","acused04","acused06","acused10")]
#check how many many women don't have cash crops data
x[rowSums(is.na(x[,c("acused95","acused98","acused00","acused02","acused04","acused06","acused10")])) == 7,c("code","AFB","acused95","acused98","acused00","acused02","acused04","acused06","acused10")]
nrow(x[rowSums(is.na(x[,c("acused95","acused98","acused00","acused02","acused04","acused06","acused10")])) == 7,c("code","AFB","acused95","acused98","acused00","acused02","acused04","acused06","acused10")])
#n=9
#subset as TRUE/FALSE
y_acused <- is.na(x[,c("acused95","acused98","acused00","acused02","acused04","acused06","acused10")])
#plot it!
colors <- c("white","black")
par(mfrow=c(1,1))
plot(NULL,xlim=c(0,ncol(y_acused)),ylim=c(0,nrow(y_acused)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_acused)), c(col(y_acused))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_acused)]
)

### Months without grain ----

#### Summary statistics per year ----

#1995
summary(sample$jjunezero95)
sd(sample$jjunezero95,na.rm=T)
#min=0
#median=5
#mean=4.581
#max=10
#sd=2.026
#check NAs
sum(is.na(sample$jjunezero95))
#n=175
#plot it!
hist(sample$jjunezero95,breaks=10)
plot(density(sample$jjunezero95,na.rm=T))

#1998 -- full of 99 and NAs
#summary(sample$jjunezero98)
#sd(sample$jjunezero98,na.rm=T)
#min=
#median=
#mean=
#max=
#sd=
#check NAs
#sum(is.na(sample$jjunezero98))
#n=
#plot it!
#hist(sample$jjunezero98,breaks=13)
#plot(density(sample$jjunezero98,na.rm=T))

#2000
summary(sample$jjunezero00)
sd(sample$jjunezero00,na.rm=T)
#min=0
#median=3
#mean=3.691
#max=10
#sd=2.685
#check NAs
sum(is.na(sample$jjunezero00))
#n=157
#plot it!
hist(sample$jjunezero00,breaks=10)
plot(density(sample$jjunezero00,na.rm=T))

#2002
summary(sample$jjunezero02)
sd(sample$jjunezero02,na.rm=T)
#min=0
#median=6
#mean=5.929
#max=12
#sd=2.719
#check NAs
sum(is.na(sample$jjunezero02))
#n=153
#plot it!
hist(sample$jjunezero02,breaks=12)
plot(density(sample$jjunezero02,na.rm=T))

#2004
summary(sample$jjunezero04)
sd(sample$jjunezero04,na.rm=T)
#min=0
#median=6
#mean=6.416
#max=12
#sd=2.961
#check NAs
sum(is.na(sample$jjunezero04))
#n=181
#plot it!
hist(sample$jjunezero04,breaks=12)
plot(density(sample$jjunezero04,na.rm=T))

#2006
summary(sample$jjunezero06)
sd(sample$jjunezero06,na.rm=T)
#min=0
#median=5
#mean=5.92
#max=12
#sd=2.913
#check NAs
sum(is.na(sample$jjunezero06))
#n=173
#plot it!
hist(sample$jjunezero06,breaks=12)
plot(density(sample$jjunezero06,na.rm=T))

#2010
summary(sample$jjunezero10)
sd(sample$jjunezero10,na.rm=T)
#min=0
#median=4
#mean=4.163
#max=12
#sd=3.182
#check NAs
sum(is.na(sample$jjunezero10))
#n=163
#plot it!
hist(sample$jjunezero10,breaks=12)
plot(density(sample$jjunezero10,na.rm=T))

#### Plot them together ----

#histograms
par(mfrow=c(2,3))
hist(sample$jjunezero95,breaks=12)
#hist(sample$jjunezero98,breaks=15)
hist(sample$jjunezero00,breaks=12)
hist(sample$jjunezero02,breaks=12)
hist(sample$jjunezero04,breaks=12)
hist(sample$jjunezero06,breaks=12)
hist(sample$jjunezero10,breaks=12)

#density plots
par(mfrow=c(2,3))
plot(density(sample$jjunezero95,na.rm=T),xlim=c(0,15))
#plot(density(sample$jjunezero98,na.rm=T),xlim=c(0,15))
plot(density(sample$jjunezero00,na.rm=T),xlim=c(0,15))
plot(density(sample$jjunezero02,na.rm=T),xlim=c(0,15))
plot(density(sample$jjunezero04,na.rm=T),xlim=c(0,15))
plot(density(sample$jjunezero06,na.rm=T),xlim=c(0,15))
plot(density(sample$jjunezero10,na.rm=T),xlim=c(0,15))

#Riana's plot

#check the variables
#check for NAs
#AFR
sum(is.na(x$AFB)) #n=0
#1995
#matching ID
sum(is.na(x$UIyearXhsh95)) #n=43
#cash crops
sum(is.na(x$jjunezero95)) #n=46
#1998
#matching ID
sum(is.na(x$UIyearXhsh98)) #n=37
#cash crops
sum(is.na(x$jjunezero98)) #n=39
#2000
#matching ID
sum(is.na(x$UIyearXhsh00)) #n=41
#cash crops
sum(is.na(x$jjunezero00)) #n=44
#2002
#matching ID
sum(is.na(x$UIyearXhsh02)) #n=39
#cash crops
sum(is.na(x$jjunezero02)) #n=43
#2004
#matching ID
sum(is.na(x$UIyearXhsh04)) #n=42
#cash crops
sum(is.na(x$jjunezero04)) #n=57
#2006
#matching ID
sum(is.na(x$UIyearXhsh06)) #n=44
#cash crops
sum(is.na(x$jjunezero06)) #n=51
#2010
#matching ID
sum(is.na(x$UIyearXhsh10)) #n=35
#cash crops
sum(is.na(x$jjunezero10)) #n=46

#look at the sample
x[,c("code","AFB","jjunezero95","jjunezero98","jjunezero00","jjunezero02","jjunezero04","jjunezero06","jjunezero10")]
#check how many many women don't have cash crops data
x[rowSums(is.na(x[,c("jjunezero95","jjunezero98","jjunezero00","jjunezero02","jjunezero04","jjunezero06","jjunezero10")])) == 7,c("code","AFB","jjunezero95","jjunezero98","jjunezero00","jjunezero02","jjunezero04","jjunezero06","jjunezero10")]
nrow(x[rowSums(is.na(x[,c("jjunezero95","jjunezero98","jjunezero00","jjunezero02","jjunezero04","jjunezero06","jjunezero10")])) == 7,c("code","AFB","jjunezero95","jjunezero98","jjunezero00","jjunezero02","jjunezero04","jjunezero06","jjunezero10")])
#n=13
#subset as TRUE/FALSE
y_jjunezero <- is.na(x[,c("jjunezero95","jjunezero00","jjunezero02","jjunezero04","jjunezero06","jjunezero10")])
#plot it!
colors <- c("white","black")
par(mfrow=c(1,1))
plot(NULL,xlim=c(0,ncol(y_jjunezero)),ylim=c(0,nrow(y_jjunezero)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5),labels=c(1995,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_jjunezero)), c(col(y_jjunezero))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_jjunezero)]
)

### Holdings, house value, and assets ----

#### Summary statistics per year ----

#1995
summary(sample$SumValue_rdKts95)
sd(sample$SumValue_rdKts95,na.rm=T)
#min=35.1
#median=58.83
#mean=130.73
#max=680.55
#sd=150.74
#check NAs
sum(is.na(sample$SumValue_rdKts95))
#n=175
#plot it!
hist(sample$SumValue_rdKts95,breaks=70)
plot(density(sample$SumValue_rdKts95,na.rm=T))

#1998
summary(sample$SumValue_rdKts98)
sd(sample$SumValue_rdKts98,na.rm=T)
#min=49.09
#median=128.46
#mean=246.33
#max=1291.12
#sd=290.213
#check NAs
sum(is.na(sample$SumValue_rdKts98))
#n=141
#plot it!
hist(sample$SumValue_rdKts98,breaks=100)
plot(density(sample$SumValue_rdKts98,na.rm=T))

#2000
summary(sample$SumValue_rdKts00)
sd(sample$SumValue_rdKts00,na.rm=T)
#min=60.44
#median=171.81
#mean=291.63
#max=1622.78
#sd=345.569
#check NAs
sum(is.na(sample$SumValue_rdKts00))
#n=141
#plot it!
hist(sample$SumValue_rdKts00,breaks=100)
plot(density(sample$SumValue_rdKts00,na.rm=T))

#2002
summary(sample$SumValue_rdKts02)
sd(sample$SumValue_rdKts02,na.rm=T)
#min=70.62
#median=231.12
#mean=396.36
#max=1954.89
#sd=465.305
#check NAs
sum(is.na(sample$SumValue_rdKts02))
#n=140
#plot it!
hist(sample$SumValue_rdKts02,breaks=100)
plot(density(sample$SumValue_rdKts02,na.rm=T))

#2004
summary(sample$SumValue_rdKts04)
sd(sample$SumValue_rdKts04,na.rm=T)
#min=84.23
#median=313.4
#mean=609.31
#max=3423.55
#sd=727.3033
#check NAs
sum(is.na(sample$SumValue_rdKts04))
#n=150
#plot it!
hist(sample$SumValue_rdKts04,breaks=100)
plot(density(sample$SumValue_rdKts04,na.rm=T))

#2006
summary(sample$SumValue_rdKts06)
sd(sample$SumValue_rdKts06,na.rm=T)
#min=99.87
#median=414.91
#mean=975.58
#max=4974.61
#sd=1229.256
#check NAs
sum(is.na(sample$SumValue_rdKts06))
#n=158
#plot it!
hist(sample$SumValue_rdKts06,breaks=100)
plot(density(sample$SumValue_rdKts06,na.rm=T))

#2010
summary(sample$SumValue_rdKts10)
sd(sample$SumValue_rdKts10,na.rm=T)
#min=144.5
#median=891.8
#mean=1962.9
#max=12527.7
#sd=2506.797
#check NAs
sum(is.na(sample$SumValue_rdKts10))
#n=140
#plot it!
hist(sample$SumValue_rdKts10,breaks=100)
plot(density(sample$SumValue_rdKts10,na.rm=T))

#### Plot them together ----

#histograms
layout( matrix(c(1,1,2,2,3,3,4,4,0,5,5,6,6,7,7,0), nrow=2, byrow=TRUE) )
hist(sample$SumValue_rdKts95,breaks=100)
hist(sample$SumValue_rdKts98,breaks=100)
hist(sample$SumValue_rdKts00,breaks=100)
hist(sample$SumValue_rdKts02,breaks=100)
hist(sample$SumValue_rdKts04,breaks=100)
hist(sample$SumValue_rdKts06,breaks=100)
hist(sample$SumValue_rdKts10,breaks=100)

#density plots
layout( matrix(c(1,1,2,2,3,3,4,4,0,5,5,6,6,7,7,0), nrow=2, byrow=TRUE) )
plot(density(sample$SumValue_rdKts95,na.rm=T),xlim=c(0,6000))
plot(density(sample$SumValue_rdKts98,na.rm=T),xlim=c(0,6000))
plot(density(sample$SumValue_rdKts00,na.rm=T),xlim=c(0,6000))
plot(density(sample$SumValue_rdKts02,na.rm=T),xlim=c(0,6000))
plot(density(sample$SumValue_rdKts04,na.rm=T),xlim=c(0,6000))
plot(density(sample$SumValue_rdKts06,na.rm=T),xlim=c(0,6000))
plot(density(sample$SumValue_rdKts10,na.rm=T),xlim=c(0,14000))

#Riana's plot

#check the variables
#check for NAs
#AFR
sum(is.na(x$AFB)) #n=0
#1995
#matching ID
sum(is.na(x$UIyearXhsh95)) #n=48
#cash crops
sum(is.na(x$SumValue_rdKts95)) #n=49
#1998
#matching ID
sum(is.na(x$UIyearXhsh98)) #n=44
#cash crops
sum(is.na(x$SumValue_rdKts98)) #n=45
#2000
#matching ID
sum(is.na(x$UIyearXhsh00)) #n=45
#cash crops
sum(is.na(x$SumValue_rdKts00)) #n=46
#2002
#matching ID
sum(is.na(x$UIyearXhsh02)) #n=51
#cash crops
sum(is.na(x$SumValue_rdKts02)) #n=52
#2004
#matching ID
sum(is.na(x$UIyearXhsh04)) #n=50
#cash crops
sum(is.na(x$SumValue_rdKts04)) #n=52
#2006
#matching ID
sum(is.na(x$UIyearXhsh06)) #n=48
#cash crops
sum(is.na(x$SumValue_rdKts06)) #n=50
#2010
#matching ID
sum(is.na(x$UIyearXhsh10)) #n=37
#cash crops
sum(is.na(x$SumValue_rdKts10)) #n=39

#look at the sample
x[,c("code","AFB","SumValue_rdKts95","SumValue_rdKts98","SumValue_rdKts00","SumValue_rdKts02","SumValue_rdKts04","SumValue_rdKts06","SumValue_rdKts10")]
#check how many many women don't have cash crops data
x[rowSums(is.na(x[,c("SumValue_rdKts95","SumValue_rdKts98","SumValue_rdKts00","SumValue_rdKts02","SumValue_rdKts04","SumValue_rdKts06","SumValue_rdKts10")])) == 7,c("code","AFB","SumValue_rdKts95","SumValue_rdKts98","SumValue_rdKts00","SumValue_rdKts02","SumValue_rdKts04","SumValue_rdKts06","SumValue_rdKts10")]
nrow(x[rowSums(is.na(x[,c("SumValue_rdKts95","SumValue_rdKts98","SumValue_rdKts00","SumValue_rdKts02","SumValue_rdKts04","SumValue_rdKts06","SumValue_rdKts10")])) == 7,c("code","AFB","SumValue_rdKts95","SumValue_rdKts98","SumValue_rdKts00","SumValue_rdKts02","SumValue_rdKts04","SumValue_rdKts06","SumValue_rdKts10")])
#n=13
#subset as TRUE/FALSE
y_sumvalue <- is.na(x[,c("SumValue_rdKts95","SumValue_rdKts98","SumValue_rdKts00","SumValue_rdKts02","SumValue_rdKts04","SumValue_rdKts06","SumValue_rdKts10")])
#plot it!
colors <- c("white","black")
par(mfrow=c(1,1))
plot(NULL,xlim=c(0,ncol(y_sumvalue)),ylim=c(0,nrow(y_sumvalue)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_sumvalue)), c(col(y_sumvalue))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_sumvalue)]
)

## All together ----

#plot it!
colors <- c("white","black")
par(mfrow=c(2,2))
plot(NULL,xlim=c(0,ncol(y_ttsacks)),ylim=c(0,nrow(y_ttsacks)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_ttsacks)), c(col(y_ttsacks))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_ttsacks)]
)
plot(NULL,xlim=c(0,ncol(y_acused)),ylim=c(0,nrow(y_acused)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_acused)), c(col(y_acused))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_acused)]
)
plot(NULL,xlim=c(0,ncol(y_jjunezero)),ylim=c(0,nrow(y_jjunezero)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5),labels=c(1995,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_jjunezero)), c(col(y_jjunezero))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_jjunezero)]
)
plot(NULL,xlim=c(0,ncol(y_sumvalue)),ylim=c(0,nrow(y_sumvalue)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_sumvalue)), c(col(y_sumvalue))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_sumvalue)]
)

