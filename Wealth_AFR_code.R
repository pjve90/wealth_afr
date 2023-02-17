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

### Farming land ----

### Cash crops ----

### Months without grain ----

### Holdings, house value, and assets ----