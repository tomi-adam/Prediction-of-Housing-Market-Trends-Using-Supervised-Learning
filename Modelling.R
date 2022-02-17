library(gamlss)
library(MASS)
library(AGD)
library(gamlss.add)

prop<-na.omit(Terraced_Property) #Dataset

propt<- subset(prop, prop$YEAR<2017) #Train Dataset
props<- subset(prop, prop$YEAR==2017) #Score Dataset

#TestDataset/Score
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
