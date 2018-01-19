##################################################
# RM01 for Mphil in Environmental Policy
# Is there a Housing Wealth Effect on Energy Consumption?
# @ University of Cambridge
# Lucas Paoli, lp485
##################################################

#################
# Modelling LONDON
#################
data.london = subset(data.correct, LONDON == 'London')

## Environment
ldn.env.full = lm(T.FUEL ~ . ,data = data.london[,names(data.london) %in% c('T.FUEL',Environmental)])
summary(ldn.env.full)
Anova(ldn.env.full)
env.red = c('NCARS','ENVHABIT1_A')
ldn.env.red = lm(T.FUEL ~ . ,data = data.london[,c('T.FUEL',env.red)])
summary(ldn.env.red)
Anova(ldn.env.red)

## Economics
ldn.econ.full = lm(T.FUEL ~ . ,data = data.london[,names(data.london) %in% c('T.FUEL','T.HHINCOME','T.HHSAV',Economics)])
summary(ldn.econ.full)
Anova(ldn.econ.full)
econ.red = c('SAVE')
ldn.econ.red = lm(T.FUEL ~ . ,data = data.london[,c('T.FUEL',econ.red)])
summary(ldn.econ.red)
Anova(ldn.econ.red)

# Cultural
Cultural[which(Cultural=='BIRTHY')]='AGE'
ldn.cult.full = lm(T.FUEL ~ . ,data = data.london[,names(data.london) %in% c('T.FUEL',Cultural)])
summary(ldn.cult.full)
Anova(ldn.cult.full)
cult.red = c('RACEL_DV', 'AGE')
ldn.cult.red = lm(T.FUEL ~ . ,data = data.london[,c('T.FUEL',cult.red)])
summary(ldn.cult.red)
Anova(ldn.cult.red)

# Household
ldn.hh.full = lm(T.FUEL ~ . ,data = data.london[,names(data.london) %in% c('T.FUEL',Household)])
summary(ldn.hh.full)
Anova(ldn.hh.full)
hh.red = c('HHTYPE_DV')
ldn.hh.red = lm(T.FUEL ~ . ,data = data.london[,c('T.FUEL',hh.red)])
summary(ldn.hh.red)
Anova(ldn.hh.red)

# Fuel Incentives
ldn.fuel.full = lm(T.FUEL ~ . ,data = data.london[,names(data.london) %in% c('T.FUEL',Fuel.incentives)])
summary(ldn.fuel.full)
Anova(ldn.fuel.full)
#################

#################
# Modelling LONDON (FULLY)
#################
lm.london.full = lm(T.FUEL ~ . ,data = data.london[,names(data.correct) %in% c('HSVAL','T.FUEL','T.HVAL',env.red,econ.red,cult.red,hh.red)])
summary(lm.london.full)
Anova(lm.london.full)

london.model2<-lm(T.FUEL~., data=lm.london.full$model)
step(london.model2, direction = 'both')

lm.london.red = lm(formula = T.FUEL ~ HHTYPE_DV + SAVE + ENVHABIT1_A,
                   data = data.london)
summary(lm.london.red)
Anova(lm.london.red)
plot(lm.london.red)

bptest(lm.london.red)
#################

#################
# Modelling NOT LONDON
#################
data.not.london = subset(data.correct, LONDON == 'Not London')

## Environment
nldn.env.full = lm(T.FUEL ~ . ,data = data.not.london[,names(data.not.london) %in% c('T.FUEL',Environmental)])
summary(nldn.env.full)
Anova(nldn.env.full)
env.red = c('NCARS','ENVHABIT1_A')
nldn.env.red = lm(T.FUEL ~ . ,data = data.not.london[,c('T.FUEL',env.red)])
summary(nldn.env.red)
Anova(nldn.env.red)

## Economics
nldn.econ.full = lm(T.FUEL ~ . ,data = data.not.london[,names(data.not.london) %in% c('T.FUEL','T.HHSAV','T.HHINCOME',Economics)])
summary(nldn.econ.full)
Anova(nldn.econ.full)
econ.red = c('SAVE', 'T.HHINCOME', 'FINNOW', 'FIYRDIC_DV')
nldn.econ.red = lm(T.FUEL ~ . ,data = data.not.london[,c('T.FUEL',econ.red)])
summary(nldn.econ.red)
Anova(nldn.econ.red)

# Cultural
Cultural[which(Cultural=='BIRTHY')]='AGE'
nldn.cult.full = lm(T.FUEL ~ . ,data = data.not.london[,names(data.not.london) %in% c('T.FUEL',Cultural)])
summary(nldn.cult.full)
Anova(nldn.cult.full)
cult.red = c('RACEL_DV', 'AGE')
nldn.cult.red = lm(T.FUEL ~ . ,data = data.not.london[,c('T.FUEL',cult.red)])
summary(nldn.cult.red)
Anova(nldn.cult.red)

# Household
nldn.hh.full = lm(T.FUEL ~ . ,data = data.not.london[,names(data.not.london) %in% c('T.FUEL',Household)])
summary(nldn.hh.full)
Anova(nldn.hh.full)
hh.red = c('HHTYPE_DV','HSBEDS','TENURE_DV')
nldn.hh.red = lm(T.FUEL ~ . ,data = data.not.london[,c('T.FUEL',hh.red)])
summary(nldn.hh.red)
Anova(nldn.hh.red)

# Fuel Incentives
nldn.fuel.full = lm(T.FUEL ~ . ,data = data.not.london[,names(data.not.london) %in% c('T.FUEL',Fuel.incentives)])
summary(nldn.fuel.full)
Anova(nldn.fuel.full)
fuel.red = c('FUELDUEL')
nldn.fuel.red = lm(T.FUEL ~ . ,data = data.not.london[,c('T.FUEL',fuel.red)])
summary(nldn.fuel.red)
Anova(nldn.fuel.red)
#################

#################
# Modelling NOT LONDON (FULLY)
#################
lm.nlon.full = lm(T.FUEL ~ . ,data = data.not.london[,names(data.correct) %in% c('HSVAL','LAT','T.FUEL','T.HVAL',env.red,econ.red,cult.red,hh.red,fuel.red)])
summary(lm.nlon.full)
Anova(lm.nlon.full)

nlon.model2<-lm(T.FUEL~., data=lm.nlon.full$model)
step(nlon.model2, direction = 'both')

lm.nlon.red = lm(formula = T.FUEL ~ HSVAL + HHTYPE_DV + HSBEDS + TENURE_DV + 
                   FINNOW + RACEL_DV + NCARS + ENVHABIT1_A + FUELDUEL + 
                   AGE + LAT + T.HHINCOME, data = data.not.london)
summary(lm.nlon.red)
Anova(lm.nlon.red)
plot(lm.nlon.red)

bptest(lm.nlon.red)
#################

