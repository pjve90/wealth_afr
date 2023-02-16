# PhD project on wealth and age at first reproduction ----

# This script is meant to collect all the necessary code to build up the models and explore the data necessary to understand the relationship between wealth and age at first reproduction.

## Data exploration ----

### Import data ----
#install package to import excel file
install.packages("readxl")
library(readxl)

### Demographic data ----
data_demo <- read_excel("C:/Users/pablo_varas/Nextcloud/PhD/Chapter 3/data/pablo_demog_ui_dec 23 2022.xlsx",sheet = "cases with AFB & UI sent")

#sample size
#check the number of women who gave birth after the data collection started
table(data_demo$in_sampled_window)
#n=335

#age at first reproduction
#check their age at first reproduction (AFR)
summary(data_demo$AFB[data_demo$in_sampled_window==1])
#min=12.58
#median=18.43
#mean=18.98
#max=32
#check for NAs
sum(is.na(data_demo$AFB[data_demo$in_sampled_window==1]))
#n=0
#plot it!
hist(data_demo$AFB[data_demo$in_sampled_window==1],xlab="AFR",breaks=32)

#age
#check the year of birth
summary(data_demo$DOBYR[data_demo$in_sampled_window==1])
#min=1966
#median=1984
#mean=1984
#max=1999
#check for NAs
sum(is.na(data_demo$DOBYR[data_demo$in_sampled_window==1]))
#n=0
#plot it!
hist(data_demo$DOBYR[data_demo$in_sampled_window==1],xlab="Year of birth",breaks=33)

#Relationship with household head in 1995 (britstatus95)
#check britstatus05
table(data_demo$BritStatus95[data_demo$in_sampled_window==1])
#check for NAs
sum(is.na(data_demo$BritStatus95[data_demo$in_sampled_window==1]))
#n=136
#plot it!
plot(table(data_demo$BritStatus95[data_demo$in_sampled_window==1]),xlab="Relationship with household")

#Relationship with household head in 1998 (britstatus98)
#check britstatus05
table(data_demo$britstatus98[data_demo$in_sampled_window==1])
#check for NAs
sum(is.na(data_demo$britstatus98[data_demo$in_sampled_window==1]))
#n=136
#plot it!
plot(table(data_demo$britstatus98[data_demo$in_sampled_window==1]),xlab="Relationship with household")

#Relationship with household head in 2000 (britstatus00)
#check britstatus05
table(data_demo$britstatus00[data_demo$in_sampled_window==1])
#check for NAs
sum(is.na(data_demo$britstatus00[data_demo$in_sampled_window==1]))
#n=86
#plot it!
plot(table(data_demo$britstatus00[data_demo$in_sampled_window==1]),xlab="Relationship with household")

#Relationship with household head in 2002 (britstatus02)
#check britstatus05
table(data_demo$britstatus02[data_demo$in_sampled_window==1])
#check for NAs
sum(is.na(data_demo$britstatus02[data_demo$in_sampled_window==1]))
#n=74
#plot it!
plot(table(data_demo$britstatus02[data_demo$in_sampled_window==1]),xlab="Relationship with household")

#Relationship with household head in 2004 (britstatus04)
#check britstatus05
table(data_demo$britstatus04[data_demo$in_sampled_window==1])
#check for NAs
sum(is.na(data_demo$britstatus04[data_demo$in_sampled_window==1]))
#n=67
#plot it!
plot(table(data_demo$britstatus04[data_demo$in_sampled_window==1]),xlab="Relationship with household")

#Relationship with household head in 2006 (britstatus06)
#check britstatus05
table(data_demo$britstatus06[data_demo$in_sampled_window==1])
#check for NAs
sum(is.na(data_demo$britstatus06[data_demo$in_sampled_window==1]))
#n=63
#plot it!
plot(table(data_demo$britstatus06[data_demo$in_sampled_window==1]),xlab="Relationship with household")

#Relationship with household head in 2010 (britstatus10)
#check britstatus05
table(data_demo$britstatus10[data_demo$in_sampled_window==1])
#check for NAs
sum(is.na(data_demo$britstatus10[data_demo$in_sampled_window==1]))
#n=52
#plot it!
plot(table(data_demo$britstatus10[data_demo$in_sampled_window==1]),xlab="Relationship with household")

#Household/farm that focal is associated with in 1995 (h95n)
#check h95n
table(data_demo$h95n[data_demo$in_sampled_window==1])
#check for NAs
sum(is.na(data_demo$h95n[data_demo$in_sampled_window==1]))
#n=136
#plot it!
plot(table(data_demo$h95n[data_demo$in_sampled_window==1]),xlab="Household focal is associated with")

