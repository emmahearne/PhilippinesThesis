
####################################
# Chunk :   Model construction  time varying                
####################################

# ####################################

form.bp0<- counts ~ period7+IRA_depMean+ total_incomeMed+aqacultureC+
    fisheryC+ pov+com_fisheryC +
    f(period8, model='rw1')+
    f(ID_prov, model="bym2", graph="Phil.graph") 
 
form.bp1 <- counts ~ period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, IRA_depMean, model="rw1")
 
form.bp2 <- counts ~ period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, total_incomeMed, model="rw1")

form.bp3 <- counts ~ period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, aqacultureC, model="rw1")
 
 
form.bp4 <- counts ~ period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, fisheryC, model="rw1")
 
form.bp5 <- counts ~ period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, pov, model="rw1")
 
form.bp6<- counts ~ period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, com_fisheryC, model="rw1")
 
 
 
forms=list(form.bp0, form.bp1,form.bp2,form.bp3,form.bp4,form.bp5,form.bp6)
 
res.bpt=list()
 for( i in 1:length(forms)){
   res.bpt[[i]] <- inla(forms[[i]], family="poisson", data=myagg,offset=log(offset),
                        control.compute = list(dic = TRUE, waic=TRUE), control.predictor=list(compute=TRUE))
   }
 
lapply(res.bpt, function(x) summary(x))
lapply(res.bpt, function(x) data.frame(dic=x$dic$dic, waic=x$waic$waic))
 
 
 
#####################################
# Model construction  time varying and spatial varying                
#####################################

form.bs0 <-  counts~period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, pov, model="rw1")+
   f(ID_prov2, IRA_depMean , model="besagproper", graph="Phil.graph", group=period1, control.group=list(model="rw1"))
   
form.bs1<-  counts~period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, pov, model="rw1")+
   f(ID_prov2, total_incomeMed , model="besagproper", graph="Phil.graph", group=period1, control.group=list(model="rw1"))
 
   
form.bs2<-  counts~period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, pov, model="rw1")+
   f(ID_prov2, aqacultureC , model="besagproper", graph="Phil.graph", group=period1, control.group=list(model="rw1"))
 
 
form.bs3<-  counts~period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, pov, model="rw1")+
   f(ID_prov2, fisheryC  , model="besagproper", graph="Phil.graph", group=period1, control.group=list(model="rw1"))
 
form.bs4<-  counts~period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, pov, model="rw1")+
   f(ID_prov2, pov , model="besagproper", graph="Phil.graph", group=period1, control.group=list(model="rw1"))
 
form.bs5<-  counts~period7+IRA_depMean+ total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
   f(period8, model='rw1')+
   f(ID_prov, model="bym2", graph="Phil.graph") +
   f(period2, pov, model="rw1")+
   f(ID_prov2, com_fisheryC  , model="besagproper", graph="Phil.graph", group=period1, control.group=list(model="rw1"))
 
formSP=list(form.bs0, form.bs1, form.bs2, form.bs3, form.bs4, form.bs5, form.bs5)
 
 resSP=list()
 for (i in 1:length(formSP)){
   
   resSP[[i]] <- inla(formSP[[i]], family="poisson", data=myagg,offset=log(offset),
                        control.compute = list(dic = TRUE, waic=TRUE), control.predictor=list(compute=TRUE))
   
 }
 
 lapply(resSP, function(x) summary(x))
 lapply(resSP, function(x) data.frame(dic=x$dic$dic, waic=x$waic$waic))
 
 

####################################
# Final model 
###################################

form.bs0 <-  counts~period7+IRA_depMean+ 
  total_incomeMed+aqacultureC+fisheryC+ pov+com_fisheryC +
  f(period8, model='rw1')+
  f(ID_prov, model="bym2", graph="Phil.graph") +
  f(period2, pov, model="rw1")+
  f(ID_prov2, IRA_depMean , model="besagproper", graph="Phil.graph", group=period1, control.group=list(model="rw1"))

results <- inla(form.bs0, family="poisson", data=myagg,offset=log(offset),
                control.compute = list(dic = TRUE, waic=TRUE), control.predictor=list(compute=TRUE))

summary(results)

####################################
# Chunk :  estimate 'fixed effects'  
####################################

estdf=round(exp(results$summary.fixed[c(1,3,5)]), 3)
