##################################################
# RM01 for Mphil in Environmental Policy
# Is there a Housing Wealth Effect on Energy Consumption?
# @ University of Cambridge
# Lucas Paoli, lp485
##################################################

#################
# Non-Linear relationships ?
#################

data.correct$T.FUEL = log(data.correct$FUELANNUAL+mean(data.correct$FUELANNUAL))/
  (max(log(data.correct$FUELANNUAL+mean(data.correct$FUELANNUAL))))
p.F.1 = ggplot(data.correct)+geom_density(aes(x=FUELANNUAL)) + theme_grey() + 
  ggtitle('Density of FUELANNUAL') + theme(axis.title.y = element_blank())
p.F.2 = ggplot(data.correct)+geom_density(aes(x=T.FUEL)) + theme_grey() +
  ggtitle('Density of log-ratio transformed FUELANNUAL') + theme(axis.title.y = element_blank())

data.correct$T.HVAL = log(data.correct$HSVAL+mean(data.correct$HSVAL))/
  (max(log(data.correct$HSVAL+mean(data.correct$HSVAL))))
p.V.1 = ggplot(data.correct)+geom_density(aes(x=HSVAL)) + theme_grey() + 
  ggtitle('Density of HSVAL') + theme(axis.title.y = element_blank())
p.V.2 = ggplot(data.correct)+geom_density(aes(x=T.HVAL)) + theme_grey() + 
  ggtitle('Density of log-ratio transformed HSVAL') + theme(axis.title.y = element_blank())

data.correct$T.HHINCOME = log(data.correct$FIHHMNGRS_DV+mean(data.correct$FIHHMNGRS_DV))/
  (max(log(data.correct$FIHHMNGRS_DV+mean(data.correct$FIHHMNGRS_DV))))
p.I.1 = ggplot(data.correct)+geom_density(aes(x=FIHHMNGRS_DV)) + theme_grey() + 
  ggtitle('Density of FIHHMNGRS_DV') + theme(axis.title.y = element_blank())
p.I.2 = ggplot(data.correct)+geom_density(aes(x=T.HHINCOME)) + theme_grey() + 
  ggtitle('Density of log-ratio transformed FIHHMNGRS_DV') + theme(axis.title.y = element_blank())

data.correct$T.HHSAV = log(data.correct$FIYRDIC_DV+mean(data.correct$FIYRDIC_DV))/
  (max(log(data.correct$FIYRDIC_DV+mean(data.correct$FIYRDIC_DV))))
p.H.1 = ggplot(data.correct)+geom_density(aes(x=FIYRDIC_DV)) + theme_grey() + 
  ggtitle('Density of FIYRDIC_DV') + theme(axis.title.y = element_blank())
p.H.2 = ggplot(data.correct)+geom_density(aes(x=T.HHSAV)) + theme_grey() + 
  ggtitle('Density of log-ratio transformed FIYRDIC_DV') + theme(axis.title.y = element_blank())

p_logratio = plot_grid(p.F.1,p.V.1,p.I.1,p.H.1,p.F.2,p.V.2,p.I.2,p.H.2,
                       labels='AUTO',nrow = 2)
ggsave('Figure_logratio.pdf',p_logratio,width = 18, height = 10)

names(data.correct)

## Economics
ggplot(data.correct)+geom_point(aes(x=T.HVAL,y=FUELANNUAL))
ggplot(data.correct)+geom_point(aes(x=T.HVAL,y=T.FUEL))
ggplot(data.correct)+geom_point(aes(x=T.HHSAV,y=FUELANNUAL))
ggplot(data.correct)+geom_point(aes(x=T.HHSAV,y=T.FUEL))
ggplot(data.correct)+geom_point(aes(x=T.HHINCOME,y=FUELANNUAL))
ggplot(data.correct)+geom_point(aes(x=T.HHINCOME,y=T.FUEL))

summary(lm.econ.full)
Anova(lm.econ.full)
lm.econ.tr = lm(FUELANNUAL ~ T.HHINCOME + T.HHSAV + JBSTAT + SAVE + FINNOW + FINFUT,data = data.correct)
summary(lm.econ.tr)
Anova(lm.econ.tr)

lm.econ.tr.red = lm(FUELANNUAL ~ T.HHINCOME + SAVE + FINNOW,data = data.correct)
summary(lm.econ.tr.red)
Anova(lm.econ.tr.red)
summary(lm.econ.red)
lm.econ.f.tr = lm(T.FUEL ~ T.HHINCOME + SAVE + FINNOW,data = data.correct)
summary(lm.econ.f.tr)
Anova(lm.econ.f.tr)
#################


#################
# Computing Full model
#################
# Taking stock
summary(lm.full)
Anova(lm.full)
summary(lm.red)
Anova(lm.red)

lm.full.tr = lm(T.FUEL ~ HSVAL + T.HVAL + HHTYPE_DV + HSBEDS + TENURE_DV + T.HHINCOME +
                  FINNOW + SAVE + JBSTAT + RACEL_DV + NCARS + FIHHMNGRS_DV + 
                  ENVHABIT1_A + ENVHABIT8_A + FUELDUEL + AGE + LAT,data = data.correct)
summary(lm.full.tr)
Anova(lm.full.tr)

model2.tr<-lm(T.FUEL ~ ., data=lm.full.tr$model)
step(model2.tr, direction = 'both')

lm.red.tr = lm(T.FUEL ~ HSVAL + HHTYPE_DV + HSBEDS + TENURE_DV + T.HHINCOME + 
                 FINNOW + SAVE + RACEL_DV + NCARS + ENVHABIT1_A + 
                 FUELDUEL + AGE + LAT, data = data.correct)
summary(lm.red.tr)
Anova(lm.red.tr)
plot(lm.red.tr)

bptest(lm.red.tr)

# Adding geo
summary(lm.lat.full)

lm.geo.tr = lm(T.FUEL ~ HSVAL + HHTYPE_DV + HSBEDS + TENURE_DV + T.HHINCOME + 
                 FINNOW + SAVE + RACEL_DV + NCARS + FIHHMNGRS_DV + ENVHABIT1_A + 
                 FUELDUEL + AGE + LAT, data = data.correct)
summary(lm.geo.tr)
Anova(lm.geo.tr)
plot(lm.geo.tr)

bptest(lm.geo.tr)

# Adding London
lm.ldon.tr = lm(T.FUEL ~ LONDON*(HSVAL + HHTYPE_DV + HSBEDS + TENURE_DV + T.HHINCOME + 
                 FINNOW + SAVE + RACEL_DV + NCARS + FIHHMNGRS_DV + ENVHABIT1_A + 
                 FUELDUEL + AGE + LAT), data = data.correct)
summary(lm.ldon.tr)
Anova(lm.ldon.tr)
plot(lm.ldon.tr)

bptest(lm.ldon.tr)
#################