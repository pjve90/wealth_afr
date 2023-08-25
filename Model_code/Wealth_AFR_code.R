# PhD project on wealth and age at first reproduction ----

# This script is meant to collect all the necessary code to build up the models and explore the data necessary to understand the relationship between wealth and age at first reproduction.

#install package to import excel file
#install.packages("readxl")
library(readxl)

## Data exploration ----

### Import data ----
#import data
#demographic data
data_demo <- read_excel("C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/data/pablo_demog_ui_may 29 2023.xlsx",sheet = "all selected cases with UI sent")
#wealth data
data_wealth <- read_excel("C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/data/hsh wealth for pablo_May 29 2023.xls", sheet = "hshold data (n=1705)")

### Merge data ----

#subset wealth data by year
#1995
data_wealth_95 <- data_wealth[which(data_wealth$UIyearXhsh > 1995000 & data_wealth$UIyearXhsh < 1998000),]
#1998
data_wealth_98 <- data_wealth[which(data_wealth$UIyearXhsh > 1998000 & data_wealth$UIyearXhsh < 2000000),]
#2000
data_wealth_00 <- data_wealth[which(data_wealth$UIyearXhsh > 2000000 & data_wealth$UIyearXhsh < 2002000),]
#2002
data_wealth_02 <- data_wealth[which(data_wealth$UIyearXhsh > 2002000 & data_wealth$UIyearXhsh < 2004000),]
#2004
data_wealth_04 <- data_wealth[which(data_wealth$UIyearXhsh > 2004000 & data_wealth$UIyearXhsh < 2006000),]
#2006
data_wealth_06 <- data_wealth[which(data_wealth$UIyearXhsh > 2006000 & data_wealth$UIyearXhsh < 2010000),]
#2010
data_wealth_10 <- data_wealth[which(data_wealth$UIyearXhsh > 2010000),]

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
#check the number of individuals with known age at first reproduction
table(merge_1$`AFB for pablo`[merge_1$`AFB for pablo` < 999])
length(merge_1$`AFB for pablo`[merge_1$`AFB for pablo` < 999])
#n=1805
#check the number of individuals with known age at first reproduction, kids listed and wealth data associated
table(merge_1$`AFB for pablo`[merge_1$`AFB for pablo` < 999 & merge_1$C2.2 == 1.1 | merge_1$`AFB for pablo` < 999 & merge_1$C2.2 == 2.1])
length(merge_1$`AFB for pablo`[merge_1$`AFB for pablo` < 999 & merge_1$C2.2 == 1.1 | merge_1$`AFB for pablo` < 999 & merge_1$C2.2 == 2.1])
#n=1581
#check the number of women with known age at first reproduction, kids listed and wealth data associated
table(merge_1$`AFB for pablo`[merge_1$`AFB for pablo` < 999 & merge_1$SexN == 0 & merge_1$C2.2 == 1.1 | merge_1$`AFB for pablo` < 999 & merge_1$SexN == 0 & merge_1$C2.2 == 2.1])
length(merge_1$`AFB for pablo`[merge_1$`AFB for pablo` < 999 & merge_1$SexN == 0 & merge_1$C2.2 == 1.1 | merge_1$`AFB for pablo` < 999 & merge_1$SexN == 0 & merge_1$C2.2 == 2.1])
#n=837

#subset sample
sample <- merge_1[merge_1$`AFB for pablo` < 999 & merge_1$`AFB for pablo`+merge_1$DOBYR >= 1995 & merge_1$SexN == 0 & merge_1$C2.2 == 1.1 | merge_1$`AFB for pablo` < 999 & merge_1$`AFB for pablo`+merge_1$DOBYR >= 1995 & merge_1$SexN == 0 & merge_1$C2.2 == 2.1,]
nrow(sample)
#n=557

### Demographic data ----

#twin status
table(sample$`twinY/N`)
#n=23 twin status

#### Age at first reproduction ----
#check their age at first reproduction (AFR)
summary(sample$`AFB for pablo`[sample$`AFB for pablo` < 100])
sd(sample$`AFB for pablo`[sample$`AFB for pablo` < 100])
#min=13.48
#median=18.43
#mean=18.91
#max=32
#sd=2.924
#check for NAs
length(sample$`AFB for pablo`[sample$`AFB for pablo` == 999])
#n=0
#plot it!
#with women who hasn't reproduced yet
hist(sample$`AFB for pablo`, prob=T,breaks=20, main="Histogram of AFR, with all women",xlab="AFR")
lines(density(sample$`AFB for pablo`),lwd=2)
#only women that have had their first child
hist(sample$`AFB for pablo`[sample$`AFB for pablo` < 100], prob=T,breaks=20, main="Histogram of AFR, with women who have had a child",xlab="AFR")
lines(density(sample$`AFB for pablo`[sample$`AFB for pablo` < 100]),lwd=2)

#check how many had their first baby until each year of data collection
#1995
nrow(sample[which(sample$`AFB for pablo`+sample$DOBYR >= 1995 & sample$`AFB for pablo`+sample$DOBYR < 1996),])
#n=21
#1998
nrow(sample[which(sample$`AFB for pablo`+sample$DOBYR >= 1996 & sample$`AFB for pablo`+sample$DOBYR < 1999),])
#n=54
#2000
nrow(sample[which(sample$`AFB for pablo`+sample$DOBYR >= 1999 & sample$`AFB for pablo`+sample$DOBYR < 2001),])
#n=39
#2002
nrow(sample[which(sample$`AFB for pablo`+sample$DOBYR >= 2001 & sample$`AFB for pablo`+sample$DOBYR < 2003),])
#n=39
#2004
nrow(sample[which(sample$`AFB for pablo`+sample$DOBYR >= 2003 & sample$`AFB for pablo`+sample$DOBYR < 2005),])
#n=44
#2006
nrow(sample[which(sample$`AFB for pablo`+sample$DOBYR >= 2005 & sample$`AFB for pablo`+sample$DOBYR < 2007),])
#n=30
#2010
nrow(sample[which(sample$`AFB for pablo`+sample$DOBYR >= 2007 & sample$`AFB for pablo`+sample$DOBYR < 2011),])
#n=58
#after 2010
nrow(sample[which(sample$`AFB for pablo`+sample$DOBYR >= 2011),])
#n=272

#### Age ----
#check the year of birth
summary(sample$DOBYR)
sd(sample$DOBYR)
#min=1933
#median=1990
#mean=1989
#max=2002
#sd=9.06
#check for NAs
sum(is.na(sample$DOBYR))
#n=0
#plot it!
hist(sample$DOBYR, prob=T,breaks=20, main="Histogram of year of birth",xlab="Year of birth")
lines(density(sample$DOBYR),lwd=2)

#### Relationship with household head ----

#Relationship with household head in 1995 (britstatus95)
#check britstatus95
table(sample$`Brit Status95`)
#check for NAs
sum(is.na(sample$`Brit Status95`))
#n=296
#plot it!
plot(table(sample$`Brit Status95`),xlab="Relationship with household")

#Relationship with household head in 1998 (britstatus98)
#check britstatus98
table(sample$`brit status98`)
#check for NAs
sum(is.na(sample$`brit status98`))
#n=210
#plot it!
plot(table(sample$`brit status98`),xlab="Relationship with household")

#Relationship with household head in 2000 (britstatus00)
#check britstatus00
table(sample$`brit status00`)
#check for NAs
sum(is.na(sample$`brit status00`))
#n=154
#plot it!
plot(table(sample$`brit status00`),xlab="Relationship with household")

#Relationship with household head in 2002 (britstatus02)
#check britstatus02
table(sample$`brit status02`)
#check for NAs
sum(is.na(sample$`brit status02`))
#n=111
#plot it!
plot(table(sample$`brit status02`),xlab="Relationship with household")

#Relationship with household head in 2004 (britstatus04)
#check britstatus04
table(sample$`brit status04`)
#check for NAs
sum(is.na(sample$`brit status04`))
#n=101
#plot it!
plot(table(sample$`brit status04`),xlab="Relationship with household")

#Relationship with household head in 2006 (britstatus06)
#check britstatus06
table(sample$`brit status06`)
#check for NAs
sum(is.na(sample$`brit status06`))
#n=94
#plot it!
plot(table(sample$`brit status06`),xlab="Relationship with household")

#Relationship with household head in 2010 (britstatus10)
#check britstatus10
table(sample$`brit status10`)
#check for NAs
sum(is.na(sample$`brit status10`))
#n=84
#plot it!
plot(table(sample$`brit status10`),xlab="Relationship with household")

#number of individuals with britstatus in all censuses
summary(complete.cases(sample[,c("Brit Status95","brit status98","brit status00","brit status02","brit status04","brit status06","brit status10")]))
#n=217

#### Household/farm that focal is associated with ----

#Household/farm that focal is associated with in 1995 (h95n)
#check h95n
table(sample$h95n)
#check for NAs
sum(is.na(sample$h95n))
#n=296
#plot it!
plot(table(sample$h95n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 1998 (h98n)
#check h98n
table(sample$h98n)
#check for NAs
sum(is.na(sample$h98n))
#n=210
#plot it!
plot(table(sample$h98n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2000 (h00n)
#check h00n
table(sample$h00n)
#check for NAs
sum(is.na(sample$h00n))
#n=154
#plot it!
plot(table(sample$h00n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2002 (h02n)
#check h02n
table(sample$h02n)
#check for NAs
sum(is.na(sample$h02n))
#n=108
#plot it!
plot(table(sample$h02n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2004 (h04n)
#check h04n
table(sample$h04n)
#check for NAs
sum(is.na(sample$h04n))
#n=91
#plot it!
plot(table(sample$h04n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2006 (h06n)
#check h06n
table(sample$h06n)
#check for NAs
sum(is.na(sample$h06n))
#n=84
#plot it!
plot(table(sample$h06n),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2010 (h10n)
#check h10n
table(sample$h10n)
#check for NAs
sum(is.na(sample$h10n))
#n=78
#plot it!
plot(table(sample$h10n),xlab="Household focal is associated with")

#number of individuals with hxxn in all censuses
summary(complete.cases(sample[c("h95n","h98n","h00n","h02n","h04n","h06n","h10n")]))
#n=226

#### Household/farm that focal is living in ----

#Household/farm that focal is associated with in 1995 (h95a)
#check h95a
table(sample$h95a)
#a = 13
#m = 19
#p = 229
#check for NAs
sum(is.na(sample$h95a))
#n=296
#plot it!
plot(table(sample$h95a),xlab="Household focal lives in")

#Household/farm that focal is associated with in 1998 (h98a)
#check h98a
table(sample$h98a)
#a = 46
#m = 5
#p = 296
#check for NAs
sum(is.na(sample$h98a))
#n=210
#plot it!
plot(table(sample$h98a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2000 (h00a)
#check h00a
table(sample$h00a)
#a = 73
#m = 1
#p = 329
#check for NAs
sum(is.na(sample$h00a))
#n=154
#plot it!
plot(table(sample$h00a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2002 (h02n)
#check h02a
table(sample$h02a)
#a = 92
#m = 10
#p = 344
#tag = 3
#check for NAs
sum(is.na(sample$h02a))
#n=108
#plot it!
plot(table(sample$h02a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2004 (h04a)
#check h04a
table(sample$h04a)
#a = 113
#m = 13
#p = 330
#tag = 10
#check for NAs
sum(is.na(sample$h04a))
#n=91
#plot it!
plot(table(sample$h04a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2006 (h06a)
#check h06a
table(sample$h06a)
#a = 123
#m = 8
#p = 332
#rep = 1
#tag = 9
#check for NAs
sum(is.na(sample$h06a))
#n=84
#plot it!
plot(table(sample$h06a),xlab="Household focal is associated with")

#Household/farm that focal is associated with in 2010 (h10a)
#check h10a
table(sample$h10a)
#a = 124
#m = 3
#p = 346
#tag = 6
#check for NAs
sum(is.na(sample$h10a))
#n=78
#plot it!
plot(table(sample$h10a),xlab="Household focal is associated with")

#number of individuals with hxxa in all censuses
summary(complete.cases(sample[c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")]))
#n=226

#identify which individuals have NA in hxxa
for(i in 1:nrow(sample)){
  if(sum(is.na(sample[i,c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")])==T) == 7){
    sample$sumnahxxa[i] <- sum(is.na(sample[i,c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")])==T)
    sample$delete[i] <- "yes"
  } else{
    sample$sumnahxxa[i] <- sum(is.na(sample[i,c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")])==T)
    sample$delete[i] <- "no"
  }
}
table(sample$delete)
#no=544
#yes=13

### Matching ID ----

#1995 
#summary
sample$UIyearXhsh95
table(sample$UIyearXhsh95)
#check NAs
sum(is.na(sample$UIyearXhsh95))
#n=309

#1998 
#summary
sample$UIyearXhsh98
table(sample$UIyearXhsh98)
#check NAs
sum(is.na(sample$UIyearXhsh98))
#n=256

#2000 
#summary
sample$UIyearXhsh00
table(sample$UIyearXhsh00)
#check NAs
sum(is.na(sample$UIyearXhsh00))
#n=227

#2002 
#summary
sample$UIyearXhsh02
table(sample$UIyearXhsh02)
#check NAs
sum(is.na(sample$UIyearXhsh02))
#n=203

#2004 
#summary
sample$UIyearXhsh04
table(sample$UIyearXhsh04)
#check NAs
sum(is.na(sample$UIyearXhsh04))
#n=214

#2006 
#summary
sample$UIyearXhsh06
table(sample$UIyearXhsh06)
#check NAs
sum(is.na(sample$UIyearXhsh06))
#n=217

#2010 
#summary
sample$UIyearXhsh10
table(sample$UIyearXhsh10)
#check NAs
sum(is.na(sample$UIyearXhsh10))
#n=208

#number of individuals with matching wealth data in all censuses
summary(complete.cases(sample[c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10")]))
#n=107

#number of individuals with wealth data and household information in all censuses
summary(complete.cases(sample[c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10","h95n","h98n","h00n","h02n","h04n","h06n","h10n")]))
#n=107

#number of individuals with no matching wealth data
for(i in 1:nrow(sample)){
  if(sum(is.na(sample[i,c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10")])==T) == 7){
    sample$sumnaUI[i] <- sum(is.na(sample[i,c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10")])==T)
    sample$deleteUI[i] <- "yes"
  } else{
    sample$sumnaUIa[i] <- sum(is.na(sample[i,c("UIyearXhsh95","UIyearXhsh98","UIyearXhsh00","UIyearXhsh02","UIyearXhsh04","UIyearXhsh06","UIyearXhsh10")])==T)
    sample$deleteUI2[i] <- "no"
  }
}
table(sample$delete2)
#no=540
#yes=17


## Material wealth data ----

### Cash crops ----

#### Summary statistics per year ----

#1995
summary(sample$ttsacks95)
sd(sample$ttsacks95,na.rm=T)
#min=0
#median=6
#mean=7.051
#max=25.5
#sd=5.227
#check NAs
sum(is.na(sample$ttsacks95))
#n=329
#plot it!
hist(sample$ttsacks95, prob=T,breaks=20, main="Cash crops in 1995",xlab="Cash crops")
lines(density(sample$ttsacks95,na.rm = T),lwd=2)

#1998
summary(sample$ttsacks98)
sd(sample$ttsacks98,na.rm=T)
#min=0
#median=3.13
#mean=4.7456
#max=31.250
#sd=4.694
#check NAs
sum(is.na(sample$ttsacks98))
#n=262
#plot it!
hist(sample$ttsacks98, prob=T,breaks=20, main="Cash crops in 1998",xlab="Cash crops")
lines(density(sample$ttsacks98,na.rm = T),lwd=2)

#2000
summary(sample$ttsacks00)
sd(sample$ttsacks00,na.rm=T)
#min=0
#median=5
#mean=7.135
#max=48
#sd=8.006
#check NAs
sum(is.na(sample$ttsacks00))
#n=236
#plot it!
hist(sample$ttsacks00, prob=T,breaks=20, main="Cash crops in 2000",xlab="Cash crops")
lines(density(sample$ttsacks00,na.rm = T),lwd=2)

#2002
summary(sample$ttsacks02)
sd(sample$ttsacks02,na.rm=T)
#min=0
#median=7
#mean=8.821
#max=49
#sd=8.85
#check NAs
sum(is.na(sample$ttsacks02))
#n=220
#plot it!
hist(sample$ttsacks02, prob=T,breaks=20, main="Cash crops in 2002",xlab="Cash crops")
lines(density(sample$ttsacks02,na.rm = T),lwd=2)

#2004
summary(sample$ttsacks04)
sd(sample$ttsacks04,na.rm=T)
#min=0
#median=7
#mean=12.48
#max=95
#sd=14.016
#check NAs
sum(is.na(sample$ttsacks04))
#n=233
#plot it!
hist(sample$ttsacks04, prob=T,breaks=20, main="Cash crops in 2004",xlab="Cash crops")
lines(density(sample$ttsacks04,na.rm = T),lwd=2)

#2006
summary(sample$ttsacks06)
sd(sample$ttsacks06,na.rm=T)
#min=0
#median=11.2
#mean=14.72
#max=66
#sd=13.513
#check NAs
sum(is.na(sample$ttsacks06))
#n=234
#plot it!
hist(sample$ttsacks06, prob=T,breaks=20, main="Cash crops in 2006",xlab="Cash crops")
lines(density(sample$ttsacks06,na.rm = T),lwd=2)

#2010
summary(sample$ttsacks10)
sd(sample$ttsacks10,na.rm=T)
#min=0
#median=7.5
#mean=15.26
#max=132
#sd=21.778
#check NAs
sum(is.na(sample$ttsacks10))
#n=222
#plot it!
hist(sample$ttsacks10, prob=T,breaks=20, main="Cash crops in 2010",xlab="Cash crops")
lines(density(sample$ttsacks10,na.rm = T),lwd=2)

#### Checking NAs ----

#number of individuals with ttsacksxx in all censuses
summary(complete.cases(sample[c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]))
#n=89

#individuals without any cash crop data (7 NAs in total)
for(i in 1:nrow(sample)){
  if(sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==T) == 7){
    sample$sumnacrop[i] <- sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==T)
    sample$deletecrop[i] <- "yes"
  } else{
    sample$sumnacrop[i] <- sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==T)
    sample$deletecrop[i] <- "no"
  }
}
table(sample$deletecrop)
#no=529
#yes=28

#check who they are
#those without information in hxxa and cash crops
nahxxattsacks <- sample[which(sample$delete == "yes" & sample$deletecrop == "yes"),c("t15nnn","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","deleteUI")]
nrow(nahxxattsacks)
nahxxattsacks
#n=13
#those without information in hxxa but have information in cash crops
sample[which(sample$delete == "yes" & sample$deletecrop == "no"),c("t15nnn","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","deleteUI")]
#n=0
#those with information in hxxa but not in cash crops
nattsacks <- sample[which(sample$delete == "no" & sample$deletecrop == "yes"),c("t15nnn","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","deleteUI")]
nrow(nattsacks)
nattsacks
#n=15

#those where hhxa="p" and ttsacksxx=NA
#create variables first
sample$sumnapcrop95 <- rep(NA,nrow(sample))
sample$sumnapcrop98 <- rep(NA,nrow(sample))
sample$sumnapcrop00 <- rep(NA,nrow(sample))
sample$sumnapcrop02 <- rep(NA,nrow(sample))
sample$sumnapcrop04 <- rep(NA,nrow(sample))
sample$sumnapcrop06 <- rep(NA,nrow(sample))
sample$sumnapcrop10 <- rep(NA,nrow(sample))
#run loop
for(i in 1:nrow(sample)){
        if(sample$h95a[i]=="p" & is.na(sample$h95a[i]) == FALSE & is.na(sample$ttsacks95[i]) == TRUE){
                sample$sumnapcrop95[i] <-  1
        } else
                if(sample$h98a[i]=="p" & is.na(sample$h98a[i]) == FALSE & is.na(sample$ttsacks98[i]) == TRUE){
                        sample$sumnapcrop98[i] <-  1
        }else
                        if(sample$h00a[i]=="p" & is.na(sample$h00a[i]) == FALSE & is.na(sample$ttsacks00[i]) == TRUE){
                                sample$sumnapcrop00[i] <-  1
                        }else
                                if(sample$h02a[i]=="p" & is.na(sample$h02a[i]) == FALSE & is.na(sample$ttsacks02[i]) == TRUE){
                                        sample$sumnapcrop02[i] <-  1
                                }else
                                        if(sample$h04a[i]=="p" & is.na(sample$h04a[i]) == FALSE & is.na(sample$ttsacks04[i]) == TRUE){
                                                sample$sumnapcrop04[i] <-  1
                                        }else
                                                if(sample$h06a[i]=="p" & is.na(sample$h06a[i]) == FALSE & is.na(sample$ttsacks06[i]) == TRUE){
                                                        sample$sumnapcrop06[i] <-  1
                                                }else
                                                        if(sample$h10a[i]=="p" & is.na(sample$h10a[i]) == FALSE & is.na(sample$ttsacks10[i]) == TRUE){
                                                                sample$sumnapcrop10[i] <-  1
                                                        }else {
               sample$sumnapcrop95[i] <- 0
               sample$sumnapcrop98[i] <- 0
               sample$sumnapcrop00[i] <- 0
               sample$sumnapcrop02[i] <- 0
               sample$sumnapcrop04[i] <- 0
               sample$sumnapcrop06[i] <- 0
               sample$sumnapcrop10[i] <- 0
          }
}
#check who they are
napttsacks <- sample[which(sample$sumnapcrop95==1 | sample$sumnapcrop98==1 | sample$sumnapcrop00==1 | sample$sumnapcrop02==1 | sample$sumnapcrop04==1 | sample$sumnapcrop06==1 | sample$sumnapcrop10==1),c("t15nnn","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","deleteUI")]
nrow(napttsacks)
#n=23

#those where hhxa="m" and ttsacksxx!=NA
for(i in 1:nrow(sample)){
        if(sample$h95a[i]=="m" & is.na(sample$h95a[i]) == FALSE & is.na(sample$ttsacks95[i]) == TRUE){
                sample$sumnamcrop95[i] <-  1
        } else
                if(sample$h98a[i]=="m" & is.na(sample$h98a[i]) == FALSE & is.na(sample$ttsacks98[i]) == TRUE){
                        sample$sumnamcrop98[i] <-  1
                }else
                        if(sample$h00a[i]=="m" & is.na(sample$h00a[i]) == FALSE & is.na(sample$ttsacks00[i]) == TRUE){
                                sample$sumnamcrop00[i] <-  1
                        }else
                                if(sample$h02a[i]=="m" & is.na(sample$h02a[i]) == FALSE & is.na(sample$ttsacks02[i]) == TRUE){
                                        sample$sumnamcrop02[i] <-  1
                                }else
                                        if(sample$h04a[i]=="m" & is.na(sample$h04a[i]) == FALSE & is.na(sample$ttsacks04[i]) == TRUE){
                                                sample$sumnamcrop04[i] <-  1
                                        }else
                                                if(sample$h06a[i]=="m" & is.na(sample$h06a[i]) == FALSE & is.na(sample$ttsacks06[i]) == TRUE){
                                                        sample$sumnamcrop06[i] <-  1
                                                }else
                                                        if(sample$h10a[i]=="m" & is.na(sample$h10a[i]) == FALSE & is.na(sample$ttsacks10[i]) == TRUE){
                                                                sample$sumnamcrop10[i] <-  1
                                                        }else {
                                                                sample$sumnamcrop95[i] <- 0
                                                                sample$sumnamcrop98[i] <- 0
                                                                sample$sumnamcrop00[i] <- 0
                                                                sample$sumnamcrop02[i] <- 0
                                                                sample$sumnamcrop04[i] <- 0
                                                                sample$sumnamcrop06[i] <- 0
                                                                sample$sumnamcrop10[i] <- 0
                                                        }
}
#check who they are
namttsacks <- sample[which(sample$sumnamcrop95==1 | sample$sumnamcrop98==1 | sample$sumnamcrop00==1 | sample$sumnamcrop02==1 | sample$sumnamcrop04==1 | sample$sumnamcrop06==1 | sample$sumnamcrop10==1),c("t15nnn","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","deleteUI")]
nrow(namttsacks)
#n=24

#those where hhxa="a" and ttsacksxx!=NA
for(i in 1:nrow(sample)){
        if(sample$h95a[i]=="a" & is.na(sample$h95a[i]) == FALSE & is.na(sample$ttsacks95[i]) == FALSE){
                sample$sumnaacrop95[i] <-  1
        } else
                if(sample$h98a[i]=="a" & is.na(sample$h98a[i]) == FALSE & is.na(sample$ttsacks98[i]) == FALSE){
                        sample$sumnaacrop98[i] <-  1
                }else
                        if(sample$h00a[i]=="a" & is.na(sample$h00a[i]) == FALSE & is.na(sample$ttsacks00[i]) == FALSE){
                                sample$sumnaacrop00[i] <-  1
                        }else
                                if(sample$h02a[i]=="a" & is.na(sample$h02a[i]) == FALSE & is.na(sample$ttsacks02[i]) == FALSE){
                                        sample$sumnaacrop02[i] <-  1
                                }else
                                        if(sample$h04a[i]=="a" & is.na(sample$h04a[i]) == FALSE & is.na(sample$ttsacks04[i]) == FALSE){
                                                sample$sumnaacrop04[i] <-  1
                                        }else
                                                if(sample$h06a[i]=="a" & is.na(sample$h06a[i]) == FALSE & is.na(sample$ttsacks06[i]) == FALSE){
                                                        sample$sumnaacrop06[i] <-  1
                                                }else
                                                        if(sample$h10a[i]=="a" & is.na(sample$h10a[i]) == FALSE & is.na(sample$ttsacks10[i]) == FALSE){
                                                                sample$sumnaacrop10[i] <-  1
                                                        }else {
                                                                sample$sumnaacrop95[i] <- 0
                                                                sample$sumnaacrop98[i] <- 0
                                                                sample$sumnaacrop00[i] <- 0
                                                                sample$sumnaacrop02[i] <- 0
                                                                sample$sumnaacrop04[i] <- 0
                                                                sample$sumnaacrop06[i] <- 0
                                                                sample$sumnaacrop10[i] <- 0
                                                        }
}
#check who they are
sample[which(sample$sumnaacrop95==1 | sample$sumnaacrop98==1 | sample$sumnaacrop00==1 | sample$sumnaacrop02==1 | sample$sumnaacrop04==1 | sample$sumnaacrop06==1 | sample$sumnaacrop10==1),c("t15nnn","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]
nrow(sample[which(sample$sumnaacrop95==1 | sample$sumnaacrop98==1 | sample$sumnaacrop00==1 | sample$sumnaacrop02==1 | sample$sumnaacrop04==1 | sample$sumnaacrop06==1 | sample$sumnaacrop10==1),c("t15nnn","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])
#n=0

## Cleaning sample  ----

#make one sample
nasample <- Reduce(function(x,y)merge(x,y,all=TRUE),list(nahxxattsacks,nattsacks,napttsacks,namttsacks))
nasample
nrow(nasample)
#n=91

#get rid of NAs in UI
nasample2 <- nasample[which(nasample$deleteUI=="no"),]
nasample2
nrow(nasample2)
#n=74

#check those where woman is absent of household and wealth data is missing
nasample2[which(nasample2$h95a=="a" & is.na(nasample2$ttsacks95) == T | nasample2$h98a=="a" & is.na(nasample2$ttsacks98) == T | nasample2$h00a=="a" & is.na(nasample2$ttsacks00) == T | nasample2$h02a=="a" & is.na(nasample2$ttsacks02) == T | nasample2$h04a=="a" & is.na(nasample2$ttsacks04) == T | nasample2$h06a=="a" & is.na(nasample2$ttsacks06) == T | nasample2$h10a=="a" & is.na(nasample2$ttsacks10) == T ),]
nrow(nasample2[which(nasample2$h95a=="a" & is.na(nasample2$ttsacks95) == T | nasample2$h98a=="a" & is.na(nasample2$ttsacks98) == T | nasample2$h00a=="a" & is.na(nasample2$ttsacks00) == T | nasample2$h02a=="a" & is.na(nasample2$ttsacks02) == T | nasample2$h04a=="a" & is.na(nasample2$ttsacks04) == T | nasample2$h06a=="a" & is.na(nasample2$ttsacks06) == T | nasample2$h10a=="a" & is.na(nasample2$ttsacks10) == T ),])
#n=38

#check those where woman is present of household and wealth data was not collected
nasample2[which(nasample2$h95a=="m" & is.na(nasample2$ttsacks95) == T | nasample2$h98a=="m" & is.na(nasample2$ttsacks98) == T | nasample2$h00a=="m" & is.na(nasample2$ttsacks00) == T | nasample2$h02a=="m" & is.na(nasample2$ttsacks02) == T | nasample2$h04a=="m" & is.na(nasample2$ttsacks04) == T | nasample2$h06a=="m" & is.na(nasample2$ttsacks06) == T | nasample2$h10a=="m" & is.na(nasample2$ttsacks10) == T ),]
nrow(nasample2[which(nasample2$h95a=="m" & is.na(nasample2$ttsacks95) == T | nasample2$h98a=="m" & is.na(nasample2$ttsacks98) == T | nasample2$h00a=="m" & is.na(nasample2$ttsacks00) == T | nasample2$h02a=="m" & is.na(nasample2$ttsacks02) == T | nasample2$h04a=="m" & is.na(nasample2$ttsacks04) == T | nasample2$h06a=="m" & is.na(nasample2$ttsacks06) == T | nasample2$h10a=="m" & is.na(nasample2$ttsacks10) == T ),])
#n=40

#check those where woman is present of household and wealth data is missing
nasample2[which(nasample2$h95a=="p" & is.na(nasample2$ttsacks95) == T | nasample2$h98a=="p" & is.na(nasample2$ttsacks98) == T | nasample2$h00a=="p" & is.na(nasample2$ttsacks00) == T | nasample2$h02a=="p" & is.na(nasample2$ttsacks02) == T | nasample2$h04a=="p" & is.na(nasample2$ttsacks04) == T | nasample2$h06a=="p" & is.na(nasample2$ttsacks06) == T | nasample2$h10a=="p" & is.na(nasample2$ttsacks10) == T ),]
nrow(nasample2[which(nasample2$h95a=="p" & is.na(nasample2$ttsacks95) == T | nasample2$h98a=="p" & is.na(nasample2$ttsacks98) == T | nasample2$h00a=="p" & is.na(nasample2$ttsacks00) == T | nasample2$h02a=="p" & is.na(nasample2$ttsacks02) == T | nasample2$h04a=="p" & is.na(nasample2$ttsacks04) == T | nasample2$h06a=="p" & is.na(nasample2$ttsacks06) == T | nasample2$h10a=="p" & is.na(nasample2$ttsacks10) == T ),])
#n=43

#clean sample based on UI then
sample2 <- sample[which(sample$deleteUI=="no"),]
sample2[,c("t15nnn","AFB for pablo","DOBYR","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]
nrow(sample2)

#### Plot them together ----

#ridgeline plot
#prepare data
z <- data.frame(ttsacks=sample2$ttsacks95,year=rep(1995,nrow(sample2)))
a <- data.frame(ttsacks=sample2$ttsacks98,year=rep(1998,nrow(sample2)))
r <- rbind(z,a)
a <- data.frame(ttsacks=sample2$ttsacks00,year=rep(2000,nrow(sample2)))
r <- rbind(r,a)
a <- data.frame(ttsacks=sample2$ttsacks02,year=rep(2002,nrow(sample2)))
r <- rbind(r,a)
a <- data.frame(ttsacks=sample2$ttsacks04,year=rep(2004,nrow(sample2)))
r <- rbind(r,a)
a <- data.frame(ttsacks=sample2$ttsacks06,year=rep(2006,nrow(sample2)))
r <- rbind(r,a)
a <- data.frame(ttsacks=sample2$ttsacks10,year=rep(2010,nrow(sample2)))
r <- rbind(r,a)
r <- r[!is.na(r$ttsacks),]
#plot it!
ggplot(r,aes(x=ttsacks,y=as.factor(year),fill=as.factor(year)))+
  stat_density_ridges(quantiles=0.5
                      , quantile_lines = T
                      , geom = "density_ridges_gradient"
                      , position="raincloud"
                      , scale=0.95)+
  scale_fill_viridis(discrete=T
                     ,name="Quantile"
                     ,alpha=0.5
                     ,option="magma")+
  theme_classic()+
  xlab("Cash crop sacks")+
  ylab("Year")+
  theme(legend.position = "none")

#Riana's plot

#check the variables
#check for NAs
#AFR
sum(is.na(x$AFB)) #n=0
#1995
#matching ID
sum(is.na(x$UIyearXhsh95)) #n=42
#h95a
sum(is.na(x$h95a)) #n=39
#cash crops
sum(is.na(x$ttsacks95)) #n=50
#1998
#matching ID
sum(is.na(x$UIyearXhsh98)) #n=17
#h98a
sum(is.na(x$h98a)) #n=31
#cash crops
sum(is.na(x$ttsacks98)) #n=18
#2000
#matching ID
sum(is.na(x$UIyearXhsh00)) #n=15
#h00a
sum(is.na(x$h00a)) #n=24
#cash crops
sum(is.na(x$ttsacks00)) #n=15
#2002
#matching ID
sum(is.na(x$UIyearXhsh02)) #n=15
#h02a
sum(is.na(x$h02a)) #n=20
#cash crops
sum(is.na(x$ttsacks02)) #n=16
#2004
#matching ID
sum(is.na(x$UIyearXhsh04)) #n=19
#h04a
sum(is.na(x$h04a)) #n=15
#cash crops
sum(is.na(x$ttsacks04)) #n=21
#2006
#matching ID
sum(is.na(x$UIyearXhsh06)) #n=20
#h06a
sum(is.na(x$h06a)) #n=14
#cash crops
sum(is.na(x$ttsacks06)) #n=20
#2010
#matching ID
sum(is.na(x$UIyearXhsh10)) #n=19
#h10a
sum(is.na(x$h10a)) #n=12
#cash crops
sum(is.na(x$ttsacks10)) #n=19

#look at the sample
x[,c("code","AFB","h95a","ttsacks95","h98a","ttsacks98","h00a","ttsacks00","h02a","ttsacks02","h04a","ttsacks04","h06a","ttsacks06","h10a","ttsacks10")]
#check how many many women don't have cash crops data
x[rowSums(is.na(x[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])) == 7,c("code","AFB","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]
nrow(x[rowSums(is.na(x[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])) == 7,c("code","AFB","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])
#n=9
#check how many many women don't have household presence data
x[rowSums(is.na(x[,c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")])) == 7,c("code","AFB","h95a","h98a","h00a","h02a","h04a","h06a","h10a")]
nrow(x[rowSums(is.na(x[,c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")])) == 7,c("code","AFB","h95a","h98a","h00a","h02a","h04a","h06a","h10a")])
#n=3
#subset as TRUE/FALSE
y_ttsacks <- is.na(x[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])
#plot it!
colors <- palette(hcl.colors(4,"viridi"))
pdf(file="C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/ttsacks_chess.pdf",15,12)
plot(NULL,xlim=c(0,ncol(y_ttsacks)),ylim=c(0,nrow(y_ttsacks)),xlab=c("Census year"),ylab=c("ID"),xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_ttsacks)), c(col(x[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(x[,c("h95a","h98a","h00a","h02a","h04a","h06a","h10a")])]
     )
dev.off()

#### Absolute cash crops ----

##### Absolute ----

for(i in 1:nrow(sample)){
  sample$ttsacksabs[i] <- sum(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]),na.rm=TRUE)
}
#check it out
sample[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","ttsacksabs")]
sample[which(is.na(sample[,c("ttsacksabs")])==TRUE),c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","ttsacksabs")]

##### Cumulative abs ----

#calculate the cumulative abs for everybody from 1995 until 2010
for (i in 1:nrow(sample)) {
  sample$cumabs95[i] <- sum(as.numeric(sample[i,c("ttsacks95")]),na.rm = T)
  sample$cumabs98[i] <- sum(as.numeric(sample[i,c("ttsacks95","ttsacks98")]),na.rm = T)
  sample$cumabs00[i] <- sum(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00")]),na.rm = T)
  sample$cumabs02[i] <- sum(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02")]),na.rm = T)
  sample$cumabs04[i] <- sum(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04")]),na.rm = T)
  sample$cumabs06[i] <- sum(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06")]),na.rm = T)
  sample$cumabs10[i] <- sum(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]),na.rm = T)
}
#check it out
head(sample[,c("ttsacks95","cumabs95","ttsacks98","cumabs98","ttsacks00","cumabs00","ttsacks02","cumabs02","ttsacks04","cumabs04","ttsacks06","cumabs06","ttsacks10","cumabs10")])

##### Cumulative abs until AFB ----

#create variables first
sample$cumabsafb <- rep(NA,nrow(sample))
#calculate
for (i in 1:nrow(sample)) {
  if(sample$AFB[i] + sample$DOBYR[i] < 1998 & is.na(sample$ttsacks95[i])==FALSE){
    sample$cumabsafb[i] <- sample$cumabs95[i]
  }else
    if (sample$AFB[i] + sample$DOBYR[i] < 1998 & is.na(sample$ttsacks95[i])==TRUE){
      sample$cumabsafb[i] <- NA
    }else
      if(sample$AFB[i] + sample$DOBYR[i] < 2000 & sample$AFB[i] + sample$DOBYR[i] >= 1998 & sum(is.na(sample[i,c("ttsacks95","ttsacks98")])==T) < 2){
        sample$cumabsafb[i] <- sample$cumabs98[i]
      }else
        if (sample$AFB[i] + sample$DOBYR[i] < 2000 & sample$AFB[i] + sample$DOBYR[i] >= 1998 & sum(is.na(sample[i,c("ttsacks95","ttsacks98")])==T) == 2){
          sample$cumabsafb[i] <- NA
        }else
          if(sample$AFB[i] + sample$DOBYR[i] < 2002 & sample$AFB[i] + sample$DOBYR[i] >= 2000 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00")])==T) < 3){
            sample$cumabsafb[i] <- sample$cumabs00[i]
          }else
            if (sample$AFB[i] + sample$DOBYR[i] < 2002 & sample$AFB[i] + sample$DOBYR[i] >= 2000 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00")])==T) == 3){
              sample$cumabsafb[i] <- NA
            }else
              if(sample$AFB[i] + sample$DOBYR[i] < 2004 & sample$AFB[i] + sample$DOBYR[i] >= 2002 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02")])==T) < 4){
                sample$cumabsafb[i] <- sample$cumabs02[i]
              }else
                if (sample$AFB[i] + sample$DOBYR[i] < 2004 & sample$AFB[i] + sample$DOBYR[i] >= 2002 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02")])==T) == 4){
                  sample$cumabsafb[i] <- NA
                }else
                  if(sample$AFB[i] + sample$DOBYR[i] < 2006 & sample$AFB[i] + sample$DOBYR[i] >= 2004 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04")])==T) < 5){
                    sample$cumabsafb[i] <- sample$cumabs04[i]
                  }else
                    if (sample$AFB[i] + sample$DOBYR[i] < 2006 & sample$AFB[i] + sample$DOBYR[i] >= 2004 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04")])==T) == 5){
                      sample$cumabsafb[i] <- NA
                    }else
                      if(sample$AFB[i] + sample$DOBYR[i] < 2010 & sample$AFB[i] + sample$DOBYR[i] >= 2006 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06")])==T) < 6){
                        sample$cumabsafb[i] <- sample$cumabs06[i]
                      }else
                        if (sample$AFB[i] + sample$DOBYR[i] < 2010 & sample$AFB[i] + sample$DOBYR[i] >= 2006 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06")])==T) == 6){
                          sample$cumabsafb[i] <- NA
                        }else
                          if(sample$AFB[i] + sample$DOBYR[i] >= 2010 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==T) < 7){
                            sample$cumabsafb[i] <- sample$cumabs10[i]
                          }else
                            if (sample$AFB[i] + sample$DOBYR[i] >= 2010 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==T) == 7){
                              sample$cumabsafb[i] <- NA
      }
}
#check it out
sample[which(is.na(sample$cumabsafb)==TRUE),c("t15nnn","AFB","DOBYR","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cumabsafb")]
nrow(sample[which(is.na(sample$cumabsafb)==TRUE),c("t15nnn","AFB","DOBYR","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cumabsafb")])

#### Mean cash crops ----

##### Mean ----

for(i in 1:nrow(sample)){
sample$ttsacksmean[i] <- mean(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]),na.rm=TRUE)
}
#check it out
sample[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","ttsacksmean")]
sample[which(is.na(sample[,c("ttsacksmean")])==TRUE),c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","ttsacksmean")]

##### Cumulative mean ----

#calculate the cumulative mean for everybody from 1995 until 2010
for (i in 1:nrow(sample)) {
  sample$cummean95[i] <- mean(as.numeric(sample[i,c("ttsacks95")]),na.rm = T)
  sample$cummean98[i] <- mean(as.numeric(sample[i,c("ttsacks95","ttsacks98")]),na.rm = T)
  sample$cummean00[i] <- mean(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00")]),na.rm = T)
  sample$cummean02[i] <- mean(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02")]),na.rm = T)
  sample$cummean04[i] <- mean(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04")]),na.rm = T)
  sample$cummean06[i] <- mean(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06")]),na.rm = T)
  sample$cummean10[i] <- mean(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]),na.rm = T)
}
#check it out
head(sample[,c("ttsacks95","cummean95","ttsacks98","cummean98","ttsacks00","cummean00","ttsacks02","cummean02","ttsacks04","cummean04","ttsacks06","cummean06","ttsacks10","cummean10")])

##### Cumulative mean until AFB ----

#create variables first
sample$cummeanafb <- rep(NA,nrow(sample))
#calculate
for (i in 1:nrow(sample)) {
  if(sample$AFB[i] + sample$DOBYR[i] < 1998){
    sample$cummeanafb[i] <- sample$cummean95[i]
  }else
    if (sample$AFB[i] + sample$DOBYR[i] < 2000 & sample$AFB[i] + sample$DOBYR[i] >= 1998){
      sample$cummeanafb[i] <- sample$cummean98[i]
    }else
      if (sample$AFB[i] + sample$DOBYR[i] < 2002 & sample$AFB[i] + sample$DOBYR[i] >= 2000){
        sample$cummeanafb[i] <- sample$cummean00[i]
      }else
        if (sample$AFB[i] + sample$DOBYR[i] < 2004 & sample$AFB[i] + sample$DOBYR[i] >= 2002){
          sample$cummeanafb[i] <- sample$cummean02[i]
        }else
          if (sample$AFB[i] + sample$DOBYR[i] < 2006 & sample$AFB[i] + sample$DOBYR[i] >= 2004){
            sample$cummeanafb[i] <- sample$cummean04[i]
          }else
            if (sample$AFB[i] + sample$DOBYR[i] < 2010 & sample$AFB[i] + sample$DOBYR[i] >= 2006){
              sample$cummeanafb[i] <- sample$cummean06[i]
            }else{
              sample$cummeanafb[i] <- sample$cummean10[i]
            }
}
#check it out
sample[which(is.na(sample$cummeanafb)==TRUE),c("t15nnn","AFB","DOBYR","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cummeanafb")]
nrow(sample[which(is.na(sample$cummeanafb)==TRUE),c("t15nnn","AFB","DOBYR","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cummeanafb")])

#### Standard deviation cash crops ----

##### Standard deviation ----

for(i in 1:nrow(sample)){
  sample$ttsackssd[i] <- sd(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]),na.rm=TRUE)
}
#check it out
sample[,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","ttsackssd")]
sample[which(is.na(sample[,c("ttsackssd")])==TRUE),c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","ttsackssd")]

##### Cumulative sd ----

#calculate the cumulative sd for everybody from 1995 until 2010
for (i in 1:nrow(sample)) {
  sample$cumsd95[i] <- sd(as.numeric(sample[i,c("ttsacks95")]),na.rm = T)
  sample$cumsd98[i] <- sd(as.numeric(sample[i,c("ttsacks95","ttsacks98")]),na.rm = T)
  sample$cumsd00[i] <- sd(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00")]),na.rm = T)
  sample$cumsd02[i] <- sd(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02")]),na.rm = T)
  sample$cumsd04[i] <- sd(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04")]),na.rm = T)
  sample$cumsd06[i] <- sd(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06")]),na.rm = T)
  sample$cumsd10[i] <- sd(as.numeric(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")]),na.rm = T)
}
#check it out
head(sample[,c("ttsacks95","cumsd95","ttsacks98","cumsd98","ttsacks00","cumsd00","ttsacks02","cumsd02","ttsacks04","cumsd04","ttsacks06","cumsd06","ttsacks10","cumsd10")])

##### Cumulative sd until AFB ----

#create variables first
sample$cumsdafb <- rep(NA,nrow(sample))
#calculate
for (i in 1:nrow(sample)) {
  if(sample$AFB[i] + sample$DOBYR[i] < 1998 & is.na(sample$ttsacks95[i])==FALSE){
    sample$cumsdafb[i] <- 0
  }else
    if (sample$AFB[i] + sample$DOBYR[i] < 1998 & is.na(sample$ttsacks95[i])==TRUE){
      sample$cumsdafb[i] <- NA
    }else
      if(sample$AFB[i] + sample$DOBYR[i] < 2000 & sample$AFB[i] + sample$DOBYR[i] >= 1998 & sum(is.na(sample[i,c("ttsacks95","ttsacks98")])==F) > 1){
        sample$cumsdafb[i] <- sample$cumsd98[i]
      }else
        if(sample$AFB[i] + sample$DOBYR[i] < 2000 & sample$AFB[i] + sample$DOBYR[i] >= 1998 & sum(is.na(sample[i,c("ttsacks95","ttsacks98")])==F) == 1){
          sample$cumsdafb[i] <- 0
        }else
        if (sample$AFB[i] + sample$DOBYR[i] < 2000 & sample$AFB[i] + sample$DOBYR[i] >= 1998 & sum(is.na(sample[i,c("ttsacks95","ttsacks98")])==T) == 2){
          sample$cumsdafb[i] <- NA
        }else
          if(sample$AFB[i] + sample$DOBYR[i] < 2002 & sample$AFB[i] + sample$DOBYR[i] >= 2000 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00")])==F) > 1){
            sample$cumsdafb[i] <- sample$cumsd00[i]
          }else
            if(sample$AFB[i] + sample$DOBYR[i] < 2002 & sample$AFB[i] + sample$DOBYR[i] >= 2000 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00")])==F) == 1){
              sample$cumsdafb[i] <- 0
            }else
              if (sample$AFB[i] + sample$DOBYR[i] < 2002 & sample$AFB[i] + sample$DOBYR[i] >= 2000 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00")])==T) == 3){
                sample$cumsdafb[i] <- NA
              }else
                if(sample$AFB[i] + sample$DOBYR[i] < 2004 & sample$AFB[i] + sample$DOBYR[i] >= 2002 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02")])==F) > 1){
                  sample$cumsdafb[i] <- sample$cumsd02[i]
                }else
                  if(sample$AFB[i] + sample$DOBYR[i] < 2004 & sample$AFB[i] + sample$DOBYR[i] >= 2002 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02")])==F) == 1){
                    sample$cumsdafb[i] <- 0
                  }else
                    if (sample$AFB[i] + sample$DOBYR[i] < 2004 & sample$AFB[i] + sample$DOBYR[i] >= 2002 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02")])==T) == 4){
                      sample$cumsdafb[i] <- NA
                    }else
                      if(sample$AFB[i] + sample$DOBYR[i] < 2006 & sample$AFB[i] + sample$DOBYR[i] >= 2004 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04")])==F) > 1){
                        sample$cumsdafb[i] <- sample$cumsd04[i]
                      }else
                        if(sample$AFB[i] + sample$DOBYR[i] < 2006 & sample$AFB[i] + sample$DOBYR[i] >= 2004 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04")])==F) == 1){
                          sample$cumsdafb[i] <- 0
                        }else
                          if (sample$AFB[i] + sample$DOBYR[i] < 2006 & sample$AFB[i] + sample$DOBYR[i] >= 2004 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04")])==T) == 5){
                            sample$cumsdafb[i] <- NA
                          }else
                            if(sample$AFB[i] + sample$DOBYR[i] < 2010 & sample$AFB[i] + sample$DOBYR[i] >= 2006 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06")])==F) > 1){
                              sample$cumsdafb[i] <- sample$cumsd06[i]
                            }else
                              if(sample$AFB[i] + sample$DOBYR[i] < 2010 & sample$AFB[i] + sample$DOBYR[i] >= 2006 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06")])==F) == 1){
                                sample$cumsdafb[i] <- 0
                              }else
                                if (sample$AFB[i] + sample$DOBYR[i] < 2010 & sample$AFB[i] + sample$DOBYR[i] >= 2006 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06")])==T) == 6){
                                  sample$cumsdafb[i] <- NA
                                }else
                                  if(sample$AFB[i] + sample$DOBYR[i] >= 2010 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==F) > 1){
                                    sample$cumsdafb[i] <- sample$cumsd10[i]
                                  }else
                                    if(sample$AFB[i] + sample$DOBYR[i] >= 2010 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==F) == 1){
                                      sample$cumsdafb[i] <- 0
                                    }else
                                      if (sample$AFB[i] + sample$DOBYR[i] >= 2010 & sum(is.na(sample[i,c("ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10")])==T) == 7){
                                        sample$cumsdafb[i] <- NA
                                      }
}
#check it out
sample[which(is.na(sample$cumsdafb)==TRUE),c("t15nnn","AFB","DOBYR","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cumsdafb")]
nrow(sample[which(is.na(sample$cumsdafb)==TRUE),c("t15nnn","AFB","DOBYR","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cumsdafb")])

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

#ridgeline plot
z <- data.frame(acused=sample$acused95,year=rep(1995,nrow(sample)))
a <- data.frame(acused=sample$acused98,year=rep(1998,nrow(sample)))
r <- rbind(z,a)
a <- data.frame(acused=sample$acused00,year=rep(2000,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(acused=sample$acused02,year=rep(2002,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(acused=sample$acused04,year=rep(2004,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(acused=sample$acused06,year=rep(2006,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(acused=sample$acused10,year=rep(2010,nrow(sample)))
r <- rbind(r,a)
r <- r[!is.na(r$acused),]
pdf(file="C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/acused_ridge.pdf",15,12)
ridgeline(r$acused,r$year,palette=hcl.colors(7,"spectral",alpha=0.5),border="black",main="Farming land")
dev.off()

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
colors <- c("black","white")
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

#ridgeline plot
z <- data.frame(jjunezero=sample$jjunezero95,year=rep(1995,nrow(sample)))
#a <- data.frame(jjunezero=sample$jjunezero98,year=rep(1998,nrow(sample)))
#r <- rbind(z,a)
a <- data.frame(jjunezero=sample$jjunezero00,year=rep(2000,nrow(sample)))
r <- rbind(z,a)
a <- data.frame(jjunezero=sample$jjunezero02,year=rep(2002,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(jjunezero=sample$jjunezero04,year=rep(2004,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(jjunezero=sample$jjunezero06,year=rep(2006,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(jjunezero=sample$jjunezero10,year=rep(2010,nrow(sample)))
r <- rbind(r,a)
r <- r[!is.na(r$jjunezero),]
pdf(file="C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/jjunezero_ridge.pdf",15,12)
ridgeline(r$jjunezero,r$year,palette=hcl.colors(6,"spectral",alpha=0.5),border="black",main="Months without grain")
dev.off()

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
colors <- c("black","white")
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

#ridgeline plot
#prepare data
z <- data.frame(SumValue_rdKts=sample$SumValue_rdKts95,year=rep(1995,nrow(sample)))
a <- data.frame(SumValue_rdKts=sample$SumValue_rdKts98,year=rep(1998,nrow(sample)))
r <- rbind(z,a)
a <- data.frame(SumValue_rdKts=sample$SumValue_rdKts00,year=rep(2000,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(SumValue_rdKts=sample$SumValue_rdKts02,year=rep(2002,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(SumValue_rdKts=sample$SumValue_rdKts04,year=rep(2004,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(SumValue_rdKts=sample$SumValue_rdKts06,year=rep(2006,nrow(sample)))
r <- rbind(r,a)
a <- data.frame(SumValue_rdKts=sample$SumValue_rdKts10,year=rep(2010,nrow(sample)))
r <- rbind(r,a)
r <- r[!is.na(r$SumValue_rdKts),]
#plot it!
pdf(file="C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/sumvalue_ridge.pdf",15,12)
ridgeline(r$SumValue_rdKts,r$year,palette=hcl.colors(7,"spectral",alpha=0.5),border="black",main="Cash crops")
dev.off()

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
colors <- c("black","white")
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

### All together ----

#plot it!
colors <- c("black","white")
par(mfrow=c(2,2))
plot(NULL,xlim=c(0,ncol(y_ttsacks)),ylim=c(0,nrow(y_ttsacks)),xlab=c("Census year"),ylab=c("ID"),xaxt="n",main="Cash crops")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_ttsacks)), c(col(y_ttsacks))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_ttsacks)]
)
plot(NULL,xlim=c(0,ncol(y_acused)),ylim=c(0,nrow(y_acused)),xlab=c("Census year"),ylab=c("ID"),xaxt="n",main="Farming land")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_acused)), c(col(y_acused))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_acused)]
)
plot(NULL,xlim=c(0,ncol(y_jjunezero)),ylim=c(0,nrow(y_jjunezero)),xlab=c("Census year"),ylab=c("ID"),xaxt="n",main="Month without grain")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5),labels=c(1995,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_jjunezero)), c(col(y_jjunezero))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_jjunezero)]
)
plot(NULL,xlim=c(0,ncol(y_sumvalue)),ylim=c(0,nrow(y_sumvalue)),xlab=c("Census year"),ylab=c("ID"),xaxt="n",main="House assets")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(1995,1998,2000,2002,2004,2006,2010))
o <- cbind(c(row(y_sumvalue)), c(col(y_sumvalue))) - 1
rect(o[, 2], #xleft
     o[, 1], #ybottom
     o[, 2] + 1, #xright
     o[, 1] + 1, #ytop
     col=colors[as.factor(y_sumvalue)]
)

## Age at first birth ~ Year of birth of the mother ----

#whole data versus sample
palette(hcl.colors(2,"Geyser",alpha=0.5))
pdf(file="C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/afr_dobyr.pdf",15,12)
layout( matrix(c(1,1,3,3,3,2,2,3,3,3), nrow=2, byrow=TRUE))
plot(merge_1$AFB~merge_1$DOBYR,col=as.factor(merge_1$in_sampled_window),pch=16,ylim=c(10,51),xlim=c(1910,2000),xlab="Year of birth",ylab="Age at first reproduction",cex=3, cex.lab=1.5)
#only sample
plot(sample$AFB~sample$DOBYR,col=2,pch=16,ylim=c(10,51),xlim=c(1910,2000),xlab="Year of mother's birth",ylab="Age at first reproduction",main="Sample",cex=3)
plot(sample$AFB~sample$DOBYR,col=2,pch=16,xlab="Year of birth",ylab="Age at first reproduction",main="Sample",cex=3)
dev.off()
palette("default")

## Complete cases sample ----

#Based on having cumulative absolute, mean, and standard deviation
#check all together
sample[which(is.na(sample$cummeanafb)==FALSE | is.na(sample$cumsdafb)==FALSE | is.na(sample$cumabsafb)==FALSE),c("t15nnn","AFB","DOBYR","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cumabsafb","cummeanafb","cumsdafb")]
nrow(sample[which(is.na(sample$cummeanafb)==FALSE | is.na(sample$cumsdafb)==FALSE | is.na(sample$cumabsafb)==FALSE),c("t15nnn","AFB","DOBYR","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cumabsafb","cummeanafb","cumsdafb")])
length(is.na(sample[which(is.na(sample$cummeanafb)==FALSE | is.na(sample$cumsdafb)==FALSE | is.na(sample$cumabsafb)==FALSE),]$cummeanafb))
#check absolute
is.na(sample[which(is.na(sample$cumabsafb)==FALSE),]$cumabsafb)
length(is.na(sample[which(is.na(sample$cumabsafb)==FALSE),]$cumabsafb))
#check mean
is.na(sample[which(is.na(sample$cummeanafb)==FALSE),]$cummeanafb)
length(is.na(sample[which(is.na(sample$cummeanafb)==FALSE),]$cummeanafb))
#check standard deviation
is.na(sample[which(is.na(sample$cumabsafb)==FALSE),]$cumsdafb)
length(is.na(sample[which(is.na(sample$cumabsafb)==FALSE),]$cumsdafb))

sample_1 <- sample[which(is.na(sample$cummeanafb)==FALSE | is.na(sample$cumsdafb)==FALSE | is.na(sample$cumabsafb)==FALSE),]
#n=238

#Based on presence in the household ("hxxa"="p" or "m")

#check those with NA in households and cash crops
sample_1[which(sample_1$t15nnn %in% nasample$t15nnn),c("t15nnn","AFB","DOBYR","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cumabsafb","cummeanafb","cumsdafb")]
nrow(sample_1[which(sample_1$t15nnn %in% nasample$t15nnn),c("t15nnn","AFB","DOBYR","h95a","h98a","h00a","h02a","h04a","h06a","h10a","ttsacks95","ttsacks98","ttsacks00","ttsacks02","ttsacks04","ttsacks06","ttsacks10","cumabsafb","cummeanafb","cumsdafb")])
#subset
sub_sample_1a <- sample_1[which(sample_1$t15nnn %in% nasample$t15nnn),]
#subset those who don't have IDs=2958, 2997, 3518, 386
sub_sample_1b <- sub_sample_1a[which(sub_sample_1a$t15nnn != 301 & sub_sample_1a$t15nnn != 2958 & sub_sample_1a$t15nnn != 2997 & sub_sample_1a$t15nnn != 3518 & sub_sample_1a$t15nnn != 4034 & sub_sample_1a$t15nnn != 4397 & sub_sample_1a$t15nnn != 4473 & sub_sample_1a$t15nnn != 89 & sub_sample_1a$t15nnn != 386),]
#delete those in sub_sample_1b from sample_1
sample_2 <- sample_1[-which(sample_1$t15nnn %in% sub_sample_1b$t15nnn),]

#check for NAs
#AFB
sum(is.na(sample_2$AFB))
#DOBYR
sum(is.na(sample_2$DOBYR))
#cumabsafb
sum(is.na(sample_2$cumabsafb))
#cummeanafb
sum(is.na(sample_2$cummeanafb))
#cumsdafb
sum(is.na(sample_2$cumsdafb))
#no NAs

#summary statistics
#AFB
summary(sample_2$AFB)
sd(sample_2$AFB)
#mean=18.81
#sd=2.63
#median=18.47
#min=13.56
#max=30.42
hist(sample_2$AFB)

#DOBYR
summary(sample_2$DOBYR)
sd(sample_2$DOBYR)
#mean=1985
#sd=6.122
#median=1985
#min=1969
#max=1998
hist(sample_2$DOBYR)

#cumabsafb
summary(sample_2$cumabsafb)
sd(sample_2$cumabsafb)
#mean=29.78
#sd=36.305
#median=16
#min=0
#max=183.5
hist(sample_2$cumabsafb)

#cummeanafb
summary(sample_2$cummeanafb)
sd(sample_2$cummeanafb)
#mean=8.395
#sd=8.953
#median=6.062
#min=0
#max=52
hist(sample_2$cummeanafb)

#cumsdafb
summary(sample_2$cumsdafb)
sd(sample_2$cumsdafb)
#mean=4.033
#sd=5.46
#median=2.607
#min=0
#max=27.02
hist(sample_2$cumsdafb)

write.csv(sample_2,"C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/Wealth_AFR/sample2.csv",na="NA",row.names = FALSE)

