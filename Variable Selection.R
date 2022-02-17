library(gamlss)
library(MASS)
library(AGD)
library(gamlss.add)

prop<-na.omit(Terraced_Property) #Dataset

propt<- subset(prop, prop$YEAR<2017) #Train Dataset
props<- subset(prop, prop$YEAR==2017) #Score Dataset
#####Variable Selection#####
vari<- gamlss(roi ~costr+ryds+mrgi+mtax+tass+dwelld+wearn, data= test_prop, family = DBI)
varic<-chooseDist(vari, type = "binom", parallel = "snow")
getOrder(varic,1)[1:3]
vari_u<-update(vari, family=DBI)


#First order Interactions
#mu
vari_mu_order <-stepGAIC(vari, parameter="mu",scope=list(lower=~1, upper=~(costr+ryds+mrgi+mtax+tass+dwelld+wearn)^2))
vari_mu_order$anova

#sigma
vari_sigma_order <-stepGAIC(vari, parameter="sigma",scope=list(lower=~1, upper=~(costr+ryds+mrgi+mtax+tass+dwelld+wearn)^2))
vari_sigma_order$anova

#################
#For mu
vari_mu<- stepGAIC(vari, parameter = "mu", scope=~costr+ryds+mrgi+mtax+tass+dwelld+wearn)
vari_mu$anova

#For sigma
vari_sig<- stepGAIC(vari, parameter = "sigma", scope=~costr+ryds+mrgi+mtax+tass+dwelld+wearn)
vari_sig$anova



#Selection of explanatory variable for all of the distribution parameters simultanously
#Strategy A
vari_1a<- stepGAICAll.A(vari, scope=list(lower=~1, upper=~(costr+ryds+mrgi+mtax+tass+dwelld+wearn)), parallel= "snow", k=log(length(test_prop$roi)))
vari_2a<- stepGAICAll.A(vari, scope=list(lower=~1, upper=~(pb(costr)+pb(ryds)+pb(mrgi)+pb(mtax)+pb(tass)+pb(dwelld)+pb(wearn)),parallel= "snow", k=log(length(test_prop$roi))))
vari_3a<- stepGAICAll.A(vari, scope=list(lower=~1, upper=~(cs(costr)+cs(ryds)+cs(mrgi)+cs(mtax)+cs(tass)+cs(dwelld)+cs(wearn)), parallel= "snow",k=log(length(test_prop$roi))))
vari_4a<- stepGAICAll.A(vari, scope=list(lower=~1, upper=~(cy(costr)+cy(ryds)+cy(mrgi)+cy(mtax)+cy(tass)+cy(dwelld)+cy(wearn)), parallel= "snow", k=log(length(test_prop$roi))))
vari_5a<- stepGAICAll.A(vari, scope=list(lower=~1, upper=~(scs(costr)+scs(ryds)+scs(mrgi)+scs(mtax)+scs(tass)+scs(dwelld)+scs(wearn)), parallel= "snow", k=log(length(test_prop$roi))))
vari_6a<- stepGAICAll.A(vari, scope=list(lower=~1, upper=~(tr(costr)+tr(ryds)+tr(mrgi)+tr(mtax)+tr(tass)+tr(dwelld)+tr(wearn)), parallel= "snow",k=log(length(test_prop$roi))))
vari_7a<- stepGAICAll.A(vari, scope=list(lower=~1, upper=~(nn(~costr+ryds+mrgi+mtax+tass+dwelld+wearn, size= 2, decay=0.01)), parallel= "snow", k=log(length(test_prop$roi))))
vari_8a<- stepGAICAll.A(vari, scope=list(lower=~1, upper=~(lo(~costr)+lo(~ryds)+lo(~mrgi)+lo(~mtax)+lo(~tass)+lo(~dwelld)+lo(~wearn)),parallel= "snow", k=log(length(test_prop$roi))))

####################################################################################################################
#Final additive model using first order interactions between variables
vari_f1<- gamlss(roi ~ ryds + mrgi + mtax + tass + wearn + ryds:mrgi + mrgi:tass,sigma.formula = ~ryds + mrgi + mtax + tass + wearn + ryds:mrgi + mrgi:tass,family=BB, data=test_prop)
wp(vari_f1)


#Final Additive Model using step GAIC
vari_f2 <- gamlss(roi ~ ryds + mrgi + mtax + tass + wearn, sigma.formula = ~mrgi + dwelld + ryds + costr + tass, data=test_prop, family = DBI)
wp(vari_f2)

#Final Additive Model using step GAIC strategy A
vari_f3<-gamlss(formula = roi ~ ryds + tass + wearn, sigma.formula = ~mrgi,  
                family = BB, data = test_prop)
wp(vari_f3)

#Neural Network
vari_fn <- gamlss(roi~nn(~costr+ryds+mrgi+mtax+tass+dwelld+wearn, size= 2, decay=0.01), sigma.formula =~nn(~costr+ryds+mrgi+mtax+tass+dwelld+wearn), data=test_prop, family = ZIBI)
wp(vari_fn)

#####################################################################################################################
#Diagnostics

wp(vari_f1)
plot(vari_f1)

wp(vari_f2)
plot(vari_f2)

wp(vari_f3)
plot(vari_f3)

wp(vari_fn)
plot(vari_fn)

wp.twin(vari_f1,vari_f3)

GAIC(vari_f1,vari_f3, k=2) #AIC
GAIC(vari_f1, vari_f3, k=log(length(test_prop$roi))) #SBC

