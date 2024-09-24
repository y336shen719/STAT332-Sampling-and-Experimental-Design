## Hospital data
## Data extracted from Applied Regression Models, (4th edition), Kutner, Neter, and Nachtsheim

# Stay: average length of hospital stay (in days)
# Age: average patient age
# Infctrsk = probability of acquiring infection in hospital
# Culture = number of cultures performed / number of patients without signs or symptoms of hospital-acquired infection, times 100
# Xray = # xrays/# patients without signs or symptoms of pneumonia, times 100
# Beds = average # beds during study period
# MedSchool = Medical school affliation (1=Yes, 2=No)
# Census = Average number of patients in hospital per day during study
# Region = Geographic region (1=NE, 2=NC, 3=S, 4=W)

library(data.table); library(dplyr)

## Read data
hospital_dat = fread("hospital_dat.csv", header=T)

## create some indicators
hospital_dat$regionNC = ifelse(hospital_dat$Region==2, 1, 0)
hospital_dat$regionS = ifelse(hospital_dat$Region==3, 1, 0)
hospital_dat$regionW = ifelse(hospital_dat$Region==4, 1, 0)
hospital_dat$Region = NULL

## Data exploration
cor(hospital_dat)
pairs(hospital_dat)

## Check VIF for Stay
stay = lm(Stay ~ Age + Culture + Xray + Beds + MedSchool + regionNC + regionS + regionW + Census, data = hospital_dat)
r2_stay = summary(stay)$r.squared; r2_stay;
VIF_stay = 1/(1-r2_stay); VIF_stay

## Check VIF for Beds
beds = lm(Beds ~ Stay + Age + Culture + Xray + MedSchool + regionNC + regionS + regionW + Census, data = hospital_dat)
r2_beds = summary(beds)$r.squared; r2_beds;
VIF_beds = 1/(1-r2_beds); VIF_beds

## Check VIF for all variables
## Fit a full simple linear regression
myfit_full = lm(InfctRsk ~ Stay + Age + Culture + Xray + Beds + MedSchool + regionNC + regionS + regionW + Census, data = hospital_dat)

library(car)
vif(myfit_full)

## remove Census based on largest VIF
myfit_reduced = lm(InfctRsk ~ Stay + Age + Culture + Xray + Beds + MedSchool + regionNC + regionS + regionW, data = hospital_dat)
vif(myfit_reduced)

summary(myfit_full)
summary(myfit_reduced) 

