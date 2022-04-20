#import packages ---------------------------------------------------------------------------------------------
library(lme4)
library(merTools)
library(foreign)
library(psych)
library(userfriendlyscience)

#load data ---------------------------------------------------------------------------------------------
data<-read.csv(file.choose())
head(data)
names(data)[1]<-"id" 

View(data)
names(data)


#change country and region to factor 
data$cntry<-as.factor(data$cntry)
data$nuts1<-as.factor(data$nuts1)
data$ach <-mean(c(data$pres1, data$pres2, data$pres3)) #achievement 



# scale reliability check for achievement and other variables--------------------------------------------------

# Achievement
scaleReliability(dat = data, items = c('ipshabt','ipsuces', 'iprspot'))

# Achievement (organized data)
scaleReliability(dat = data, items = c('pres1','pres2', 'pres3'))

# Satisfaction with Life Scale
scaleReliability(dat = data, items = c('stflife','happy'))

# Security
scaleReliability(dat = data, items = c('impsafe','imptrad'))


# Analysis prep----------------------------------------------------------------------------------------------
data$gndr<-(data$gndr-1) # 0 = male, 1 = female
data$gndr<-as.integer(data$gndr)

# standardize all Level-1 predictors
data$gender.z<-scale(data$gndr, scale =F)

data$age.z<-scale(data$age, scale =F)

data$eduyrs.z<-scale(data$eduyrs,scale = F)

data$child.z<-scale(data$chld,scale = F)

data$domicile.z<-scale(data$domicile,scale = F)

# standardize all Level-2 predictors

data$popdens.z<-scale(log(data$popdens_2015),scale =F)

data$popsz.z<-scale(log(data$popsz_2015),scale=T)

data$gdp.z<-scale(log(data$gdp_2015),scale=F)

data$popgrow.z<-scale(log(data$popgrow_2016),scale =F)

data$longunemp.z<-scale(log(data$longunemp_2015),scale =F)

data$lifexp.z<-scale(log(data$lfexp_2015),scale =F)

# Main analysis: Country Level & Nuts 1 Level -----------------------------------------------------------------------------------

## Random Intercept Model --------------------------------------------------------------------------------------------------------

#random intercept model (Level 2: country)
model.null<-lmer(ach~1+(1|cntry),REML =FALSE, data = data)
summary(model.null)
confint(model.null)


#random intercept model (Level 2: Regions-Nuts1)
model.null<-lmer(ach~1+(1|nuts1),REML =FALSE, data = data)
summary(model.null)
confint(model.null)

#calculating ICC
ICC(outcome ="ach", group = "country", data = data)



## Model with level 1 predictors (Level 2: country) ----------------------------------------------------------------------------

#achievement
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|cntry),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


#other variables

# Into measure: Satisfaction with Life Scale
data$swls<-mean(c(data$stflife,data$happy))

# Into measure: Security
data$sec<-mean(c(data$impsafe,data$imptrad))

# Satisfaction with Life Scale
model.1<-lmer(swls~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|cntry),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#security
model.1<-lmer(sec~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|cntry),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#impfun
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|cntry),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#impfun
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|cntry),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


#model with level 1 predictors (Level 2: nuts1) --------------------------------------------------------------------------------
#achievement
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)



## Model with level 2 predictors (Level 2: country) ----------------------------------------------------------------------------

model.1<-lmer(stflife~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+popdens.z*ipshabt+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

model.1<-lmer(stflife~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+popdens.z*ipshabt+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

model.1<-lmer(stflife~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+popdens.z*ipshabt+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

## Model with level 2 predictors (Level 2: Nuts 1 region) ----------------------------------------------------------------------

model.1<-lmer(impdiff~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+longunemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

model.1<-lmer(ipadvnt~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+longunemp.z+popgrow.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)






#others  --- exploratory ----- 



#impfun
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#ipgdtim
model.1<-lmer(ipgdtim~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+unemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#ipadvnt
model.1<-lmer(ipadvnt~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+popsz.z+gdp.z+unemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#iprspot
model.1<-lmer(iprspot~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+unemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#impdiff
model.1<-lmer(impdiff~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+unemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#ipcrtiv
model.1<-lmer(impdiff~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+unemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


