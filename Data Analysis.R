library(gamlss)
library(MASS)
library(AGD)
library(gamlss.add)

prop<-na.omit(Terraced_Property) #Dataset

propt<- subset(prop, prop$YEAR<2017) #Train Dataset
props<- subset(prop, prop$YEAR==2017) #Score Dataset
###############################################################################################################
#Train Variables
roi<-propt$ROI #Binomial Response Variable for returns (PROFIT (1) or LOSS(0)) in year t(x+1)
return<-propt$FORC #Continous Response Variable for returns in year t(x+1)
year<- propt$YEAR #Year t(x)
tass<-propt$TASS #Transport assessment of property
mrgi<- propt$MRGI #Change in mortgage interest rate for t(x)
mtax<-propt$MTAX #Change in median council tax for t(x)
dwelld<- propt$DWELLD #Chance in the amount of dwellings per hectare
costr<-propt$COSTR #Ratio of a property's price to average price within an area
ryds <-propt$RYDS #Ratio of average rent within an area to property price
wearn<-propt$WEARN #Change in the average workplace earnings within a property's area

test_prop <- data.frame(roi, return,year,tass,mrgi,mtax,dwelld, costr,ryds,wearn)

###############################################################################################################
#Analysis of Data
ccol <- par(mfrow=c(1,2))
plot(return[return<1]~costr[return<1]); title("Return agaisnt COSTR")
plot(return[return<1]~log(costr[return<1])); title("Return agaisnt log(COSTR)")
par(ccol)

#Log Transform required for COSTR
test_prop$costr <- log(test_prop$costr)

ret_cr<-gamlss(return~(costr),sigma.formula = ~(costr), nu.formula=~(costr),tau.formula = ~(costr), data=test_prop, family=NO) #Initial Fit
ret_cr$type
range(return)
ret_cd<- chooseDist(ret_cr, type="realline", parallel = "snow")
getOrder(ret_cd, 1)[1:3]
ret_cu<-update(ret_cr, family =ST5)
wp(ret_cu)
wp.twin(ret_cr, ret_cu)
wp(ret_cu, xvar=test_prop$costr)

centiles.fan(ret_cu, xvar=test_prop$costr, colors="cm", ylab="RETURN", xlab="COSTR") #Centile for Continous respone and COSTR

#Binomial Response against COSTR
roi_cr<-gamlss(roi~costr,sigma.formula = ~pb(costr), nu.formula = ~pb(costr),tau.formula = ~pb(costr), data=test_prop, family=BI)
roi_cd<-chooseDist(roi_cr, type="binom", parallel="snow")
getOrder(roi_cd, 1)[1:3]
roi_cu<-update(roi_cr, family=DBI)
wp.twin(roi_cr, roi_cu)
#Probability of PROFIT against Cost Ratios
plot(roi~costr, data=test_prop, pch="|"); title("Probabilty for PROFIT against COSTR")
lines(fitted(roi_cu)[order(test_prop$costr)]~test_prop$costr[order(test_prop$costr)], col="purple", pch=".")


#Rental Yield
rrol <- par(mfrow=c(1,2))
plot(return[return<1]~ryds[return<1]); title("Return agaisnt RYDS")
plot(return[return<1]~log(ryds[return<1])); title("Return agaisnt log(RYDS)")
par(rrol)

ret_rr<-gamlss(return~(ryds),sigma.formula = ~(ryds), nu.formula=~(ryds),tau.formula = ~(ryds), data=test_prop, family=NO) #Initial Fit
ret_rd<- chooseDist(ret_rr, type="realline", parallel = "snow")
getOrder(ret_rd, 1)[1:3]
ret_ru<-update(ret_rr, family =ST4)
wp.twin(ret_rr, ret_ru)
wp(ret_cu, xvar=test_prop$costr)

centiles.fan(ret_ru, xvar=test_prop$ryds, colors="cm", ylab="RETURN", xlab="RYDS") #Centile for Continous respone and COSTR

#Binomial Response against RYDS
roi_rr<-gamlss(roi~ryds,sigma.formula = ~pb(ryds), nu.formula = ~pb(ryds),tau.formula = ~pb(ryds), data=test_prop, family=BI)
roi_rd<-chooseDist(roi_rr, type="binom", parallel="snow")
getOrder(roi_rd, 1)[1:3]
roi_ru<-update(roi_rr, family=BI)
roi_ru$family
wp.twin(roi_cr, roi_cu)
#Probability of PROFIT against Rental Yields
plot(roi~ryds, data=test_prop, pch="|"); title("Probabilty for PROFIT against RYDS")
lines(fitted(roi_ru)[order(test_prop$ryds)]~test_prop$ryds[order(test_prop$ryds)], col="purple", pch=".")

#Mortgage Interest Rate
miol <- par(mfrow=c(1,2))
plot(return[return<1]~mrgi[return<1]); title("Return agaisnt MRGI")
plot(return[return<1]~log(mrgi[return<1])); title("Return agaisnt log(MGRI)")
par(miol)

#Binomial Response against MRGI
roi_mr<-gamlss(roi~mrgi,sigma.formula = ~pb(mrgi), nu.formula = ~pb(mrgi),tau.formula = ~pb(mrgi), data=test_prop, family=BI)
roi_md<-chooseDist(roi_mr, type="binom", parallel="snow")
getOrder(roi_md, 1)[1:3]
roi_mu<-update(roi_mr, family=ZIBI)
wp.twin(roi_mr, roi_mu)
#Probability of PROFIT against Change in Mortgage Interest Rate
plot(roi~mrgi, data=test_prop, pch="|"); title("Probabilty FOR PROFIT against MRGI")
lines(fitted(roi_mu)[order(test_prop$mrgi)]~test_prop$mrgi[order(test_prop$mrgi)], col="purple", pch=".")

#Transport Assessment 
tol <- par(mfrow=c(1,2))
plot(return[return<1]~tass[return<1]); title("Return against TASS")
plot(return[return<1]~log(tass[return<1])); title("Return against log(TASS)")
par(tol)

#Log Transform required for TASS
test_prop$tass <- log(test_prop$tass)

ret_tr<-gamlss(return~(tass),sigma.formula = ~(tass), nu.formula=~(tass),tau.formula = ~(tass), data=test_prop, family=NO) #Initial Fit
ret_cr$type
range(return)
ret_td<- chooseDist(ret_tr, type="realline", parallel = "snow")
getOrder(ret_td, 1)[1:3]
ret_tu<-update(ret_tr, family =ST1)
wp.twin(ret_tr, ret_tu)
wp(ret_tu, xvar=test_prop$tass)

centiles.fan(ret_tu, xvar=test_prop$tass, colors="cm", ylab="RETURN", xlab="TASS") #Centile for Continous respone and TASS


#Binomial Response against TASS
#roi_tr<-gamlss(roi~tass,sigma.formula = ~pb(tass), nu.formula = ~pb(tass),tau.formula = ~pb(tass), data=test_prop, family=BI)
#roi_td<-chooseDist(roi_tr, type="binom", parallel="snow")
#getOrder(roi_td, 1)[1:3]
#roi_tu<-update(roi_tr, family=DBI)
#roi_tu$family
#wp.twin(roi_tr, roi_tu)
#Probability of PROFIT against Change in Transport Assessment
#plot(roi~tass, data=test_prop, pch="|"); title("Probabilty of PROFIT against TASS")
#lines(fitted(roi_tu)[order(test_prop$tass)]~test_prop$tass[order(test_prop$tass)], col="purple", pch=".")


#Change in Median Council Tax
mtol <- par(mfrow=c(1,2))
plot(return[return<1]~mtax[return<1]); title("Return against MTAX")
plot(return[return<1]~log(mtax[return<1])); title("Return against log(MTAX)")
par(mtol)

#Binomial Response against MTAX
#roi_mtr<-gamlss(roi~tass,sigma.formula = ~pb(mtax), nu.formula = ~pb(mtax),tau.formula = ~pb(mtax), data=test_prop, family=BI)
#roi_mtd<-chooseDist(roi_mtr, type="binom", parallel="snow")
#getOrder(roi_mtd, 1)[1:3]
#roi_mtu<-update(roi_mtr, family=ZIBI)
#roi_mtu$family
#wp.twin(roi_mtr, roi_mtu)
#Probability of PROFIT against Change in Median Council Tax
#plot(roi~mtax, data=test_prop, pch="|"); title("Probabilty of PROFIT against MTAX")
#lines(fitted(roi_mtu)[order(test_prop$mtax)]~test_prop$mtax[order(test_prop$mtax)],  col="purple", pch=".")

#Change in Dwelling Density
ddol <- par(mfrow=c(1,2))
plot(return[return<1]~dwelld[return<1]); title("Return agaisnt DWELLD")
plot(return[return<1]~log(dwelld[return<1])); title("Return agaisnt log(DWELLD)")
par(ddol)

ret_dd<-gamlss(return~(dwelld),sigma.formula = ~(dwelld), nu.formula=~(dwelld),tau.formula = ~(dwelld), data=test_prop, family=NO) #Initial Fit
ret_ddd<- chooseDist(ret_dd, type="realline", parallel = "snow")
getOrder(ret_ddd, 1)[1:3]
ret_ddu<-update(ret_dd, family =JSU)
wp.twin(ret_dd, ret_ddu)
wp(ret_ddu, xvar=test_prop$dwelld)

centiles.fan(ret_ddu, xvar=test_prop$dwelld, colors="cm", ylab="RETURN", xlab="DWELLD") #Centile for Continous respone and DWELLD

#Binomial Response against DWELLD
roi_dd<-gamlss(roi~dwelld,sigma.formula = ~pb(dwelld), nu.formula = ~pb(dwelld),tau.formula = ~pb(dwelld), data=test_prop, family=BI)
roi_ddd<-chooseDist(roi_dd, type="binom", parallel="snow")
getOrder(roi_ddd, 1)[1:3]
roi_du<-update(roi_dd, family=DBI)
wp.twin(roi_cr, roi_du)
#Probability of PROFIT against DWELLD
plot(roi~dwelld, data=test_prop, pch="|"); title("Probabilty of PROFIT against DWELLD")
lines(fitted(roi_du)[order(test_prop$dwelld)]~test_prop$dwelld[order(test_prop$dwelld)], col="purple", pch=".")


#Workplace Earnings
ccol <- par(mfrow=c(1,2))
plot(return[return<1]~wearn[return<1]); title("Return agaisnt COSTR")
plot(return[return<1]~log(wearn[return<1])); title("Return agaisnt log(COSTR)")
par(ccol)
