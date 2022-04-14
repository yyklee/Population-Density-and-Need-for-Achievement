library(lme4)
library(merTools)

data<-read.csv(file.choose())
head(data)
names(data)[1]<-"id"
data$gndr<-(data$gndr-1)

View(data)
names(data)


data$cntry<-as.factor(data$cntry)
data$gndr<-as.integer(data$gndr)
data$nuts1<-as.factor(data$nuts1)






library(foreign)
library(psych)
library(userfriendlyscience)


# Conformity
scaleReliability(dat = data, items = c('swpi_1','swpi_11'))

# Tradition
scaleReliability(dat = data, items = c('swpi_2','swpi_12'))

# Benevolence
scaleReliability(dat = data, items = c('swpi_3','swpi_13'))

# Universalism
scaleReliability(dat = data, items = c('swpi_4','swpi_14'))

# Self-Direction
scaleReliability(dat = data, items = c('swpi_5','swpi_15'))

# Stimulation
scaleReliability(dat = data, items = c('swpi_6','swpi_16'))

# Hedonism
scaleReliability(dat = data, items = c('swpi_7','swpi_17'))

# Achievement
scaleReliability(dat = data, items = c('swpi_8','swpi_18'))

# Power
scaleReliability(dat = data, items = c('swpi_9','swpi_19'))

# Security
scaleReliability(dat = data, items = c('swpi_10','swpi_20'))


# prep-----------------------------------------------------------------------------------

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

popgrow.z<-scale(log(data$popgrow_2016),scale =F)

data$longunemp.z<-scale(log(data$longunemp_2015),scale =F)

data$lifexp.z<-scale(log(data$lfexp_2015),scale =F)

#analysis: Country Level -----------------------------------------------------------------------------------

#random intercept model (Level 2: Regions-Nuts1)
model.null<-lmer(imprich~1+(1|nuts1),REML =FALSE, data = data)
summary(model.null)
confint(model.null)

#calculating ICC
ICC(outcome ="trustsystem", group = "country", data = data)


model.1<-lmer(stflife~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+popdens.z*ipshabt+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


########################model with level 1 predictors########################
#impfun
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


#other variables
#impfun
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#impfun
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#impfun
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


########################model with level 2 predictors########################



#analysis: NUTS 1-------------------------------------------------------------------------------------------

#random intercept model (Level 2: Nuts1 Regions)

model.null<-lmer(imprich~1+(1|nuts1),REML =FALSE, data = data)
summary(model.null)
confint(model.null)

#calculating ICC
ICC(outcome ="trustsystem", group = "country", data = data)


model.1<-lmer(stflife~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+popdens.z*ipshabt+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


########################model with level 1 predictors########################
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


#ipsuces
model.1<-lmer(ipsuces~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+popsz.z+gdp.z+unemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#ipshabt
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+popsz.z+gdp.z+unemp.z+(1|nuts1),REML = FALSE, data = data)
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

model.1<-lmer(impdiff~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+longunemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

model.1<-lmer(ipadvnt~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+longunemp.z+popgrow.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

########################model with level 2 predictors########################

#impfun
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+popsz.z+gdp.z+unemp.z+lifexp.z+longunemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


#ipadvnt
model.1<-lmer(impfun~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+unemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


#ipsuces
model.1<-lmer(ipsuces~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+unemp.z+lifexp.z+longunemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)


#ipshabt
model.2<-lmer(ipshabt~1+gender.z+age.z+eduyrs.z+child.z+domicile.z+popdens.z+gdp.z+unemp.z+lifexp.z+longunemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.2)
confint(model.2)


#impdiff
model.1<-lmer(impdiff~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+popdens.z+gdp.z+unemp.z+lifexp.z+longunemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)
.
#ipcrtiv
model.1<-lmer(ipcrtiv~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+popdens.z+gdp.z+unemp.z+lifexp.z+longunemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.1)
confint(model.1)

#iprspot
model.2<-lmer(iprspot~1+gender.z+age.z+eduyrs.z+child.z+popdens.z+gdp.z+unemp.z+longunemp.z+(1|nuts1),REML = FALSE, data = data)
summary(model.2)
confint(model.2)
