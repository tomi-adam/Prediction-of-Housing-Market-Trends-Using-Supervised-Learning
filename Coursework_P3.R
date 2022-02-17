library(gamlsss)
library(MASS)
library(AGD)
library(gamlss.add)

prop<-na.omit(Terraced_Property) #Dataset

propt<- subset(prop, prop$YEAR<2017) #Train Dataset
props<- subset(prop, prop$YEAR==2017) #Score Dataset
###############################################################################################################
#Train Variables
roi<-propt$ROI #Binomial Response Variable for returns (PROFIT (1)====== or LOSS(0)) in year t(x+1)
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

###############################################################################################################
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



#####################################################################################################################
###################################
#Tes Dataset/Score
#Score Variables
roi<-props$ROI #Binomial Response Variable for returns (PROFIT (1) or LOSS(0)) in year t(x+1)
return<-props$FORC #Continous Response Variable for returns in year t(x+1)
year<- props$YEAR #Year t(x)
tass<-props$TASS #Transport assessment of property
mrgi<- props$MRGI #Change in mortgage interest rate for t(x)
mtax<-props$MTAX #Change in median council tax for t(x)
dwelld<- props$DWELLD #Chance in the amount of dwellings per hectare
costr<-props$COSTR #Ratio of a property's price to average price within an area
ryds <-props$RYDS #Ratio of average rent within an area to property price
wearn<-props$WEARN #Change in the average workplace earnings within a property's area


score_prop <- data.frame(roi, return,year,tass,mrgi,mtax,dwelld, costr,ryds,wearn) #Score Dataset

#Transforms
score_prop$costr <- log(score_prop$costr)
score_prop$tass <- log(score_prop$tass)

################################################
#Prediction

new_roi<- predict(vari_f3, newdata = score_prop, type="response", what="mu")

truehist(new_roi, nbins=60); title("Probability level for PROFIT")
pred_dist <- gamlssML(new_roi, family=GA)
pred_distc<- chooseDist(pred_dist, type = "real0to1" )
getOrder(pred_distc, 1)[1:3]
pred_distu<- gamlssML(new_roi, family=SIMPLEX)
wp(pred_distu)
pred_distmu<- fitted(pred_distu, "mu")[1]
pred_distsig<- fitted(pred_distu, "sigma")[1]
#pred_distnu<- fitted(pred_distu, "sigma")[1]
#pred_disttau<- fitted(pred_distu, "tau")[1]
pdf.plot(pred_distu, mu=pred_distmu,sigma=pred_distsig,nu=pred_distnu , min=min(new_roi),max=max(new_roi)); title("Probability level for PROFIT")
qSIMPLEX(0.95,mu=pred_distmu,sigma=pred_distsig)
#Benchmark of 0.9463786
################################################

return_dat <- data.frame(return,new_roi)

plot(return[return<1 ]~new_roi[return<1], data=return_dat)
return_roi<-gamlss(return~new_roi, sigma.formula = ~new_roi, nu.formula = ~new_roi, tau.formula = ~new_roi, data = return_dat, family=NO)
return_roid<- chooseDist(return_roi, type="realline")
getOrder(return_roid,1)[1:3]
return_roiu<-update(return_roi, family=ST4)
wp(return_roiu)


centiles.fan(return_roiu, xvar=return_dat$new_roi, colors="heat", ylab="RETURN", xlab="Probability for PROFIT") #Centile for Continous respone and COSTR
abline(h=0, col="purple")
abline(v=0.9463786, col="purple")

##ANALYSIS of OPTIMAL RESULTS ABOVE 0.9463786
suggest<- subset(return_dat, new_roi>0.9463786)
truehist(suggest$return, nbins=30)
sug_ret<-suggest$return
dist_ret <-gamlssML(sug_ret,family = NO)
dist_retc<-chooseDist(dist_ret)
getOrder(dist_retc,1)[1:3]
dist_retu<-update(dist_ret, family=SHASH)
wp(dist_retu)
dist_retmu<- fitted(dist_retu, "mu")[1]
dist_retsig<- fitted(dist_retu, "sigma")[1]
dist_retnu<- fitted(dist_retu, "nu")[1]
dist_rettau<- fitted(dist_retu, "tau")[1]
pdf.plot(dist_retu, mu=dist_retmu, sigma = dist_retsig, nu=dist_retnu,tau=dist_rettau, min = min(sug_ret), max=max(sug_ret))
abline(v=0, col="black")

1-pSHASH(0.1,mu=dist_retmu,sigma=dist_retsig,nu=dist_retnu,tau=dist_rettau)
median(sug_ret)


abline(v=0.1, col="blue")
abline(v=0.2, col="purple")
abline(v=-0.05, col="red")
histDist(sug_ret, family=SHASH, nbins=50, max=0.4, min=0.2)
