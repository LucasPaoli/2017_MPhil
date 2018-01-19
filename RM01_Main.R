##################################################
# RM01 for Mphil in Environmental Policy
# Is there a Housing Wealth Effect on Energy Consumption?
# @ University of Cambridge
# Lucas Paoli, lp485
##################################################

###########
# Libraries
library(tidyverse)
library(readxl)
library(cowplot)
library(corrplot)
library(car)
library(cluster)
library(Rtsne)
library(mixOmics)
library(lmtest)
library(MASS)
library(PerformanceAnalytics)
###########

############
rm(list=ls())
# Directory
setwd("/Users/Lucas/Documents/MPhil_Cambridge/RM01/Exam")
# Data
data.raw.df = read_excel('RM01 Option A course work data 2017-2018.xls', sheet = 1, na = '.')
var.def = read_excel('RM01 Option A course work data 2017-2018.xls', sheet = 2, na = '.')
# Variables
source('RM01_Variables.R')
# Functions
source('RM01_Functions.R')
###########

#The housing wealth effect often manifests as a positive relationship between consumption 
#and perceived housing wealth (e.g., the perceived value of houses). When the perceived value 
#of a property rises, homeowners may feel more comfortable and secure about their wealth, 
#causing them to spend more. Does this apply to energy consumption in the UK? Choose a subset 
#of variables from the project dataset to find answers to this question.

#You should focus on verifying the existence of the relationship between housing wealth and 
#energy consumption, while controlling for property characteristics as well as a large number 
#of demographic, socio-economic and energy-use behaviour variables. You may also test other 
#relevant hypotheses, for example, whether there is a non- linear relationship; or whether the 
#relationship is different in London compared to other areas.

#################
# Computing indexes
#################
names(data.correct)

## Environment
lm.env.full = lm(FUELANNUAL ~ . ,data = data.correct[,names(data.correct) %in% c(Y,Environmental)])
summary(lm.env.full)
Anova(lm.env.full)
env.red = c('NCARS','ENVHABIT1_A','ENVHABIT8_A')
lm.env.red = lm(FUELANNUAL ~ . ,data = data.correct[,c(Y,env.red)])
summary(lm.env.red)
Anova(lm.env.red)

# Environment Cluster
dist.env <- daisy(data.correct[,env.red],metric = "gower")
summary(dist.env)
clust.env = get.pam.cluster(dist.env, data.correct, env.red)
lm.env.clust = lm(FUELANNUAL ~ as.factor(clust.env) ,data = data.correct[,c(Y,env.red)])
summary(lm.env.clust)
Anova(lm.env.clust)

# Environment Cluter (full)
dist.env.f <- daisy(data.correct[,names(data.correct) %in% Environmental],metric = "gower")
summary(dist.env.f)
clust.env.f = get.pam.cluster(dist.env.f, data.correct, names(data.correct)[names(data.correct) %in% Environmental])
lm.env.clust.f = lm(FUELANNUAL ~ as.factor(clust.env.f) ,data = data.correct[,c(Y,env.red)])
summary(lm.env.clust.f)
Anova(lm.env.clust.f)

## Economics
lm.econ.full = lm(FUELANNUAL ~ . ,data = data.correct[,names(data.correct) %in% c(Y,Economics)])
summary(lm.econ.full)
Anova(lm.econ.full)
econ.red = c('FIHHMNGRS_DV','FINNOW','SAVE','JBSTAT')
lm.econ.red = lm(FUELANNUAL ~ . ,data = data.correct[,c(Y,econ.red)])
summary(lm.econ.red)
Anova(lm.econ.red)

# Economics Cluter
dist.econ <- daisy(data.correct[,econ.red],metric = "gower")
summary(dist.econ)
clust.econ = get.pam.cluster(dist.econ, data.correct, econ.red)
lm.econ.clust = lm(FUELANNUAL ~ as.factor(clust.econ) ,data = data.correct)
summary(lm.econ.clust)
Anova(lm.econ.clust)

# Economics Cluter (full)
dist.econ.f <- daisy(data.correct[,names(data.correct) %in% Economics],metric = "gower")
summary(dist.econ.f)
clust.econ.f = get.pam.cluster(dist.econ.f, data.correct, names(data.correct)[names(data.correct) %in% Economics])
lm.econ.clust.f = lm(FUELANNUAL ~ as.factor(clust.econ.f) ,data = data.correct)
summary(lm.econ.clust.f)
Anova(lm.econ.clust.f)

# Cultural
Cultural[which(Cultural=='BIRTHY')]='AGE'
lm.cult.full = lm(FUELANNUAL ~ . ,data = data.correct[,names(data.correct) %in% c(Y,Cultural)])
summary(lm.cult.full)
Anova(lm.cult.full)
cult.red = c('RACEL_DV', 'AGE')
lm.cult.red = lm(FUELANNUAL ~ . ,data = data.correct[,c(Y,cult.red)])
summary(lm.cult.red)
Anova(lm.cult.red)

dist.cult <- daisy(data.correct[,cult.red],metric = "gower")
summary(dist.cult)
clust.cult = get.pam.cluster(dist.cult, data.correct, cult.red)

# Household
lm.hh.full = lm(FUELANNUAL ~ . ,data = data.correct[,names(data.correct) %in% c(Y,Household)])
summary(lm.hh.full)
Anova(lm.hh.full)
hh.red = c('HHTYPE_DV','HSBEDS','TENURE_DV')
lm.hh.red = lm(FUELANNUAL ~ . ,data = data.correct[,c(Y,hh.red)])
summary(lm.hh.red)
Anova(lm.hh.red)

dist.hh <- daisy(data.correct[,hh.red],metric = "gower")
summary(dist.cult)
clust.hh = get.pam.cluster(dist.hh, data.correct, hh.red)

# Fuel Incentives
lm.fuel.full = lm(FUELANNUAL ~ . ,data = data.correct[,names(data.correct) %in% c(Y,Fuel.incentives)])
summary(lm.fuel.full)
Anova(lm.fuel.full)
fuel.red = c('FUELDUEL')
lm.fuel.red = lm(FUELANNUAL ~ . ,data = data.correct[,c(Y,fuel.red)])
summary(lm.fuel.red)
Anova(lm.fuel.red)
#################

#################
# Computing Full model
#################
lm.full = lm(FUELANNUAL ~ . ,data = data.correct[,names(data.correct) %in% c('HSVAL',Y,env.red,econ.red,cult.red,hh.red,fuel.red)])
summary(lm.full)
Anova(lm.full)

model2<-lm(FUELANNUAL~., data=lm.full$model)
step(model2, direction = 'both')

lm.red = lm(formula = FUELANNUAL ~ HSVAL + HHTYPE_DV + HSBEDS + TENURE_DV + 
              FINNOW + SAVE + ENVHABIT1_A + FUELDUEL + AGE, data = data.correct)
summary(lm.red)
Anova(lm.red)
plot(lm.red)

bptest(lm.red)
#################

#################
# Geographical effects ?
#################
source('RM01_GB_Map.R')
names(plot.positions)=c('REGION', 'LAT', 'LONG', 'NSAMPLES')
data.correct=left_join(data.correct, plot.positions)
data.correct$LONDON = as.factor(ifelse(data.correct$REGION == 'London', 'London', 'Not London'))
glimpse(data.correct)

lm.lat = lm(FUELANNUAL ~ LONDON+LAT, data = data.correct)
summary(lm.lat)
Anova(lm.lat)

lm.lat.res = lm(lm.red$residuals ~ LONDON+LAT, data = data.correct)
summary(lm.lat.res)
Anova(lm.lat.res)

ggplot(data.correct)+geom_boxplot(aes(y=FUELANNUAL,x=factor(LAT)))+theme_grey()
ggplot(data.correct)+geom_boxplot(aes(y=FUELANNUAL,x=LONDON))+theme_grey()

lm.lat.full = lm(formula = FUELANNUAL ~ HSVAL + HHTYPE_DV + HSBEDS + TENURE_DV + 
                   FINNOW + SAVE + ENVHABIT1_A + FUELDUEL + AGE + LAT, data = data.correct)
summary(lm.lat.full)
Anova(lm.lat.full)
plot(lm.lat.full)
#################

#################
# Non-Linear ?
#################
source('RM01_non-linear.R')
#################

#################
# Identical relations in and out of London ?
#################
source('RM01_London.R')
#################

write_tsv(data.correct, path = 'RM01_processed_dataset.tsv')
