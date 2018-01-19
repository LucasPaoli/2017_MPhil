##################################################
# RM01 for Mphil in Environmental Policy
# Is there a Housing Wealth Effect on Energy Consumption?
# @ University of Cambridge
# Lucas Paoli, lp485
##################################################

###################
# Sorting variables
###################
Y = 'FUELANNUAL' # Annual fuel consumption
X = 'HSVAL' # House value: If you were to sell house, how much would it be

Dynamics = c('GOR_DV', # Region
             'YEAR') # Year of data collection

Household = c('AGECHY_DV', # Age younger child
              'HHSIZE', # Num. people in household
              'HHTYPE_DV', # Composition of houshold LFS-version
              'HSBEDS', # Num. bedrooms in household
              'TENURE_DV', # Housing Tenure
              'HSOWND', # Own accomodation ?
              'HSYRBUY', # Year you become owner 
              'HSCOST', # How much did it cost
              'MLSTAT', # Marital Status
              'NEMP_DV', # Num. Employed
              'NKIDS_DV', # Num. Kids
              'NPENS_DV', # Num. people pensionnable
              'NUE_DV', # Num. Unemployed
              'MVYR', # Year moved in ?
              'MVEVER', # Always lived there ?
              'LKMOVE', # Would you like to move ?
              'XPMOVE') # Expect to move ?

Economics = c('MRJSEG_DV', # Socio-economic group
              'FIHHMNGRS_DV', # Last month gross household income
              'PAYGU_DV', # Gross pay per month (current)
              'FIYRDIC_DV', # Incomes from savings and invest (annual)
              'FINNOW', # Financial situation (how well)
              'FINFUT', # How you expect it in the future
              'SAVE', # How much of the income do you save
              'HSYR04', # When did you start paying mortgage
              'MGOLD', # How much did you borrow
              'MGLIFE', # How many years mortage been running
              'MGTYPE', # Mortgage or loan ?
              'MGEXTRA', # Have you taken additional mortgage
              'MGNEW', # How much is the addtional ?
              'XPMG', # Last monthly installement on mortgages/loans
              'XPHSDB', # Been behind mortgage payment in the last 12 months ?
              'RENTF', # Do you rent ?
              'RENTLL', # Who is it rended from ?
              'RENT', # How much was the last rent ?
              'JBSTAT', # Current economic activity
              'JBSAT_D1', # Job Satisfaction
              'JBPERFP', # Performance related pay ?
              'JBONUS', # Bonus in the last 12 months ?
              'JBRISE', # Expect a rise ?
              'JBPEN', # Job pension scheme ?
              'JBPENM', # Belong to employer pension scheme ?
              'JBFXINF', # Accomodable work hours
              'JBLKCHB', # Would you like work related trainings ?
              'JBSEC', # Employement prospect
              'TUJBPL', # Trade Union exist ?
              'TUIN1', # Member of it ?
              'DEPENTH1', # Job feel tensed ?
              'WKTIME', # Working times
              'WKENDS', # Work weekends ?
              'WORKDIS', # Dist from work ?
              'WKAUT1', # Work autonomy...
              'WKAUT2',
              'WKAUT3',
              'WKAUT4',
              'WKAUT5') # ...

Cultural = c('QFHIGH', # Education
             'AHVWELL', # Importance of education
             'SF1', # Health ?
             'RACEL_DV', # Ethnic group
             'UKBORN', # Born in UK or not
             'PAJU', # Father working when 14 yo?
             'MAJU', # Mother working when 14 yo?
             'PCHAS', # Has PC ?
             'PCNET', # Has internet ?
             'PCBROAD', # Boradband ?
             'MOBUSE', # Using mobile ?
             'NETPUSE', # Often us internet
             'BIRTHY', # Birth year
             'AGEGR10_DV', # Current Age (10 years)
             'SEX')

Environmental = c('NCARS', # How lany cars in household
                  'CARSHARE', # Ready to carshare to work ?
                  'WKHOME', # Work from home ?
                  'COMBIKE', # Bike to work ?
                  'COMBUS', # Take the bus to work ?
                  'COMTRAIN', # Take the train to work ?
                  'COMWALK', # Walk to work ?
                  'ENVHABIT1_A', # TV standby for the night ?
                  'ENVHABIT3_A', # Tap running while brushing teeth ?
                  'ENVHABIT4_A', # More cloths on or increase heat ?
                  'ENVHABIT6_A', # Recycled paper ?
                  'ENVHABIT8_A') # Public transport ?

Fuel.incentives = c('FUELDUEL', # Pay gas and elec together or sep ?
                    'HEATCH', # Central heating ?
                    'HHEAT') # Can you keep accomodation warm ?

names.sorted = c(Y,X,Dynamics,Household,Economics,Cultural,Environmental,Fuel.incentives)
if (all(names(data.raw.df) %in% names.sorted)) print('All good to go!') else print('Missing variable... check something.')
###################

###################
# Evaluate variables
###################
# Create compagnon table
class_variable=NULL
for (i in c('Y','X','Dynamics','Household','Economics','Cultural','Environmental','Fuel.incentives')) {
  temp_df = tibble(Variables = get(i), Category = i)
  class_variable=rbind(class_variable, temp_df)
}

# Remove missing values from variables of interest: Y, X
i.na=as.data.frame(colSums(is.na(data.raw.df[,c(X,Y)]))/nrow(data.raw.df))*100
names(i.na)='% NAs in variables of interest'
print(i.na)
data.df = data.raw.df[!is.na(data.raw.df$HSVAL)&!is.na(data.raw.df$FUELANNUAL),]

na.df = as.data.frame(colSums(is.na(data.df))/nrow(data.df))*100
na.df = cbind(row.names(na.df),na.df)
names(na.df) = c('Variables','NAs')
na.df = arrange(na.df,desc(NAs))
class_variable = left_join(class_variable, na.df)

# Removing variables with too many missing values
pNA = ggplot(data=class_variable)+geom_histogram(aes(x=NAs)) + theme_grey() +
  xlab('Number of NAs in variable') + ggtitle('Histogram of the number of NAs per variables')
pNA
Variables_sub = class_variable[class_variable$NAs < 15,]

# Selecting relevant variable
Variables_sub$Variables
if (nrow(Variables_sub) == 39) {
  Relevance = c(
    T, # "FUELANNUAL"
    T, # "HSVAL"
    T, # "GOR_DV"
    T, # "YEAR"
    T, # "HHSIZE" Num person
    T, # "HHTYPE_DV" Composition household, LFS version
    T, # "HSBEDS" Num bed
    T, # "TENURE_DV"
    F, # "HSOWND" -> Same as TENURE_DV, less precise
    T, # "NEMP_DV" # Num employ
    T, # "NKIDS_DV"
    T, # "NPENS_DV"
    T, # "NUE_DV"
    T, # "LKMOVE"
    T, # "XPMOVE"
    T, # "FIHHMNGRS_DV" Gross household income
    T, # "FIYRDIC_DV" Income from savings
    T, # "FINNOW" Current econ situation
    T, # "FINFUT" Expected econ situation
    T, # "SAVE" How much saved monthly
    T, # "JBSTAT" Job status
    T, # "SF1" Health
    T, # "RACEL_DV" Ethnicity
    F, # "PCNET"
    F, # "PCBROAD"
    F, # "MOBUSE"
    F, # "NETPUSE"
    T, # "BIRTHY"
    F, # "AGEGR10_DV"
    T, # "SEX"
    T, # "NCARS"
    T, # "ENVHABIT1_A"
    T, # "ENVHABIT3_A"
    T, # "ENVHABIT4_A"
    T, # "ENVHABIT6_A"
    T, # "ENVHABIT8_A"
    T, # "FUELDUEL"
    T, # "HEATCH"
    T # "HHEAT"
  )
  
  Variables_sub$Relevance = Relevance
  class_variable = left_join(class_variable,Variables_sub[,c('Variables','Relevance')],by='Variables',)
  write_tsv(class_variable, "RM01_variables_selection.tsv")
  Variables_sub = Variables_sub[Relevance,]
}

data.sub = data.df[,Variables_sub$Variables]
data.sub = na.omit(data.sub)
print('% of data having the variables of interest also having corrective variables')
nrow(data.sub)/nrow(data.df)*100
nrow(data.sub)/nrow(data.raw.df)*100
###################

###################
# Inspect variables
###################
data.sub

# Variables of interest
summary(data.sub$HSVAL)
summary(data.sub$FUELANNUAL)

p1 = ggplot(data.sub) +
  geom_density(aes(x=HSVAL,fill='HSVAL',color='HSVAL'), alpha = .4) + theme_grey()+
  ggtitle('HSVAL density function') + theme(legend.position='none')
p2 = ggplot(data.sub) +
  geom_density(aes(x=FUELANNUAL,fill='FUELANNUAL',color='FUELANNUAL'), alpha = .4) + theme_grey() +
  ggtitle('FUELANNUAL density function') + theme(legend.position='none')
p3 = ggplot(data.sub) +
  geom_point(aes(x=HSVAL,y=FUELANNUAL)) + theme_grey()+ggtitle('FUELANNUAL ~ HSVAL')

fuel.fit = fitdistr(data.sub$FUELANNUAL, densfun = 'normal')

colors.pair = c("#9b599e","#95894d")
p4 = ggplot() + 
  geom_density(aes(x=rnorm(10000, mean = fuel.fit$estimate[[1]], sd = fuel.fit$estimate[[2]]),fill='Model'),color="#9b599e",alpha=.2)+
  geom_density(aes(x=data.sub$FUELANNUAL,fill='Observed'),color="#95894d",alpha=.2)+
  theme_grey() + xlab('FUELANNUAL') + ggtitle('FUELANNUAL density and fitted normal function') +
  scale_fill_manual(name = '',values = colors.pair) +
  theme(legend.position=c(.8,.8),legend.title = element_blank())
p4
pnorm(max(data.sub$FUELANNUAL),mean = fuel.fit$estimate[[1]],sd=fuel.fit$estimate[[2]],lower.tail = F)

data.correct = data.sub[-which(data.sub$FUELANNUAL == max(data.sub$FUELANNUAL)),]
p5 = ggplot(data.correct) +
  geom_density(aes(x=FUELANNUAL,fill='FUELANNUAL',color='FUELANNUAL'), alpha = .4) + theme_grey() +
  ggtitle('Corrected FUELANNUAL density function') + theme(legend.position='none')
p5

p_plot1 = plot_grid(pNA,p3,p4,nrow=1,labels = 'AUTO')
ggsave('Figure_NAs.pdf',p_plot1,width=17,height=5)

# Other quantitative variables:
data.correct
# FIHHMNGRS character ?
data.correct$FIHHMNGRS_DV[is.na(as.numeric(data.correct$FIHHMNGRS_DV))]
data.correct$FIHHMNGRS_DV = as.numeric(data.correct$FIHHMNGRS_DV)
data.correct = na.omit(data.correct)
p6 = ggplot(data.correct) +
  geom_density(aes(x=FIHHMNGRS_DV), alpha = .4) + theme_grey()
p6
p7 = ggplot(data.correct) +
  geom_density(aes(x=FIYRDIC_DV), alpha = .4) + theme_grey()
p7
###################

###################
# Transform variables
###################
# Add age
data.correct$AGE = data.correct$YEAR - data.correct$BIRTHY

# Regions
dic_regions = c('North East', 'North West', 'Yorkshire and the Humber', 'East Midlands', 
                'West Midlands', 'East of England', 'London', 'South East', 'South West',
                'Wales', 'Scotland')
names(dic_regions)=c('1','2','3','4','5','6','7','8','9','10','11')
data.correct$REGION = as.factor(dic_regions[data.correct$GOR_DV])

# Change encoding:
#####
# HHTYPE_DV
hhtype.dic = c('1 male, aged 65+, no children',
               '1 female, age 60+, no children',
               '1 adult under pensionable age, no children',
               '1 adult, 1 child',
               '1 adult, 2 or more children',
               'Couple both under pensionable age, no children',
               'Couple 1 or more over pensionable age,no children',
               'Couple with 1 child',
               'Couple with 2 children',
               'Couple with 3 or more children',
               '2 adults, not a couple, both under pensionable age, no children',
               '2 adults, not a couple, one or more over pensionable age, no children',
               '2 adults, not a couple, 1 or more children',
               '3 or more adults, no children, incl. at least one couple',
               '3 or more adults, 1-2 children, incl. at least one couple',
               '3 or more adults, >2 children, incl. at least one couple',
               '3 or more adults, no children, excl. any couples',
               '3 or more adults, 1 or more children, excl. any couples')
names(hhtype.dic) = c('1', '2', '3', '4', '5', '6', '8', '10', '11', '12', '16', '17', '18', '19', '20', '21', '22', '23')
data.correct$HHTYPE_DV=as.factor(hhtype.dic[as.character(data.correct$HHTYPE_DV)])
# TENURE_DV
tenure.dic = c('Owned outright','Owned with mortgage', 'Local authority rent',
               'Housing assoc rented', 'Rented from employer', 'Rented private unfurnished',
               'Rented private furnished','Other')
data.correct$TENURE_DV=as.factor(tenure.dic[data.correct$TENURE_DV])
# LKMOVE
lkmove.dic = c('Wants to stay', 'Wants to move')
data.correct$LKMOVE=as.factor(lkmove.dic[data.correct$LKMOVE])
# XPMOVE
xpmove.dic = c('Expect moving','Not expecting to move')
data.correct$XPMOVE=as.factor(xpmove.dic[data.correct$XPMOVE])
# FINNOW, FINFUT
# Ordinal, with 1: confortable, ..., 5: difficult
data.correct$FINNOW=as.ordered(data.correct$FINNOW)
data.correct$FINFUT=as.ordered(data.correct$FINFUT)
# SAVE
save.dic = c('Monthly savings', 'No monthly savings')
data.correct$SAVE=as.factor(save.dic[data.correct$SAVE])
# JBSTAT
jbstat.dic = c('Self employed', 'In paid employment (full or part-time)', 'Unemployed', 'Retired',
               'On maternity leave', 'Looking after family or home', 'Full-time student',
               'Long-term sick or disabled', 'On a government training scheme',
               'Unpaid worker in family business', 'Working in an apprenticeship',
               'Doing something else')
names(jbstat.dic)=as.character(c(1,2,3,4,5,6,7,8,9,10,11,97))
data.correct$JBSTAT=as.factor(jbstat.dic[as.character(data.correct$JBSTAT)])
# SF1
# Ordinal, with 1: Excellent, ..., 5: Poor
data.correct$SF1=as.ordered(data.correct$SF1)
# RACEL_DV
racel.dic = c('British/English/Scottish/Welsh/Northern Irish/White British/White Scottish (White)',
              'Irish (White)', 'Any Other White Background (White)',
              'Mixed: White and Black Caribbean', 'Mixed: White and Black African',
              'Mixed: White and Asian', 'Mixed: Any Other Mixed Background',
              'Indian (Asian or Asian British)', 'Pakistani (Asian or Asian British)',
              'Bangladeshi (Asian or Asian British)', 'Chinese (Asian or Asian British)',
              'Any Other Asian Background (Asian or Asian British)',
              'Caribbean (Black/Africa/Caribbean/Black British)',
              'Any Other Ethnic Group (Other Ethnic Group)')
names(racel.dic)=as.character(c(1,2,4,5,6,7,8,9,10,11,12,13,14,97))
data.correct$RACEL_DV=as.factor(racel.dic[as.character(data.correct$RACEL_DV)])
# SEX
sex.dic = c('Male','Female')
data.correct$SEX=as.factor(sex.dic[data.correct$SEX])
# ENVHABITS
# Ordinal values that we want scaled as increasing with env. awareness.
# ENVHABIT1: scale alrgiht, keeping non-applicable at 6.
unique(data.correct$ENVHABIT1_A); sum(data.correct$ENVHABIT1_A==6)
data.correct$ENVHABIT1_A=as.ordered(data.correct$ENVHABIT1_A)
# ENVHABIT3: scale alrgiht, keeping non-applicable at 6.
unique(data.correct$ENVHABIT3_A); sum(data.correct$ENVHABIT3_A==6)
data.correct$ENVHABIT3_A=as.ordered(data.correct$ENVHABIT3_A)
# ENVHABIT4: invert scale, keeping non-applicable at 0.
unique(data.correct$ENVHABIT4_A); sum(data.correct$ENVHABIT4_A==6)
env.dict = c(5,4,3,2,1,0)
data.correct$ENVHABIT4_A=as.ordered(env.dict[data.correct$ENVHABIT4_A])
# ENVHABIT6: invert scale, keeping non-applicable at 0.
unique(data.correct$ENVHABIT6_A); sum(data.correct$ENVHABIT6_A==6)
data.correct$ENVHABIT6_A=as.ordered(env.dict[data.correct$ENVHABIT6_A])
# ENVHABIT8: invert scale, keeping non-applicable at 0.
unique(data.correct$ENVHABIT8_A); sum(data.correct$ENVHABIT8_A==6)
data.correct$ENVHABIT8_A=as.ordered(env.dict[data.correct$ENVHABIT8_A])
# FUELDUEL
fuelduel.dic = c('One bill', 'Separate')
data.correct$FUELDUEL=as.factor(fuelduel.dic[data.correct$FUELDUEL])
# HEATCH
hheat.dic = c('Some form of central heating', 'No central heating')
data.correct$HEATCH=as.factor(hheat.dic[data.correct$HEATCH])
# HHEAT
hheat.dic = c('Yes', 'No', 'NA')
data.correct$HHEAT=as.factor(hheat.dic[data.correct$HHEAT])
#####
glimpse(data.correct)
###################
