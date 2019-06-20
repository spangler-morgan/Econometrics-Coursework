library(foreign)
library(haven)
library(sandwich)
library(lmtest)
library(naniar)
library(dplyr)
library(cobalt)
library(stringr)
library(stringi)
library(tidyr)
library(qdap)
library(naniar)
library(ggplot2)
library(reshape2)
library(ModelMetrics)
library(psych)
library(cobalt)
library(Synth)

dat <- read_dta("traffic_safety_v3_v12.dta")
View(dat)
dat$lfatal <- log(dat$fatalities)
dat$lfatal_percap <- dat$lfatal/dat$population
treat <- subset(dat,dat$state==99)
control <- subset(dat,dat$state!=99)
control <- subset(control,control$state!=6)
control <- subset(control,control$state!=10)
control <- subset(control,control$state!=30)
control <- subset(control,control$state!=41)
treat$t<-1
control$t<-0

treat_pre <- subset(treat,treat$primary==0)
treat_post <- subset(treat,treat$primary==1)
avgfatal_treatpre <- mean(treat_pre$lfatal_percap)
avgfatal_control <- mean(control$lfatal_percap)

preyrs<-unique(treat_pre$year)

avg81_t <- mean(subset(treat_pre,treat_pre$year==1981)$lfatal_percap)
avg81_c <- mean(subset(control,control$year==1981)$lfatal_percap)
avg82_t <- mean(subset(treat_pre,treat_pre$year==1982)$lfatal_percap)
avg82_c <- mean(subset(control,control$year==1982)$lfatal_percap)
avg83_t <- mean(subset(treat_pre,treat_pre$year==1983)$lfatal_percap)
avg83_c <- mean(subset(control,control$year==1983)$lfatal_percap)
avg84_t <- mean(subset(treat_pre,treat_pre$year==1984)$lfatal_percap)
avg84_c <- mean(subset(control,control$year==1984)$lfatal_percap)
avg85_t <- mean(subset(treat_pre,treat_pre$year==1985)$lfatal_percap)
avg85_c <- mean(subset(control,control$year==1985)$lfatal_percap)

df <- data.frame(time=preyrs,
                 t =c(avg81_t,avg82_t,avg83_t,avg84_t,avg85_t),
                 c =c(avg81_c,avg82_c,avg83_c,avg84_c,avg85_c))
df <- melt(df ,  id.vars = 'time', variable.name = 'treatment')
ggplot(df,aes(time,value)) + geom_line(aes(colour = treatment))

control_pre <- subset(control,control$year<1986)

ctrl_states <- unique(control$state)
mat<-matrix(data=NA, nrow=43, ncol=5)
i=1
t=1981 
iter=1
for(i in ctrl_states){
  a<-mean(subset(treat$fatalities,treat$year==t))
  b<-mean(subset(control$fatalities,control$year==t & control$state==i))
  mat[iter,1]=mse(a,b)
  i=i+1
  iter=iter+1
}
i=1
t=1982
iter=1
for(i in ctrl_states){
  a<-mean(subset(treat$fatalities,treat$year==t))
  b<-mean(subset(control$fatalities,control$year==t & control$state==i))
  mat[iter,2]=mse(a,b)
  i=i+1
  iter=iter+1
}
i=1
t=1983
iter=1
for(i in ctrl_states){
  a<-mean(subset(treat$fatalities,treat$year==t))
  b<-mean(subset(control$fatalities,control$year==t & control$state==i))
  mat[iter,3]=mse(a,b)
  i=i+1
  iter=iter+1
}
i=1
t=1984 
iter=1
for(i in ctrl_states){
  a<-mean(subset(treat$fatalities,treat$year==t))
  b<-mean(subset(control$fatalities,control$year==t & control$state==i))
  mat[iter,4]=mse(a,b)
  i=i+1
  iter=iter+1
}
i=1
t=1985 
iter=1
for(i in ctrl_states){
  a<-mean(subset(treat$fatalities,treat$year==t))
  b<-mean(subset(control$fatalities,control$year==t & control$state==i))
  mat[iter,5]=mse(a,b)
  i=i+1
  iter=iter+1
}

dfmse <- rowSums(mat)
df<-cbind(dfmse,ctrl_states)
opt_mse <- min(dfmse)
opt <- subset(df,dfmse==opt_mse)
covs <- c("college","beer","population","unemploy","totalvmt","precip","snow32","rural_speed","urban_speed")
opt_covs <- subset(dat,dat$state==8,select=covs)
treat_covs <- subset(dat,dat$state==99,select=covs)
summary(opt_covs)
summary(treat_covs)
opt_covs$t=0
treat_covs$t=1
C<- rbind(opt_covs,treat_covs)
bal.tab(C,treat=C$t)


dat<-as.data.frame(dat)

dataprep.out<-
  dataprep(
    foo = dat,
    predictors = c("college","beer","unemploy","totalvmt","precip","snow32"),
    predictors.op = c("mean"),
    dependent = "lfatal_percap",
    unit.variable = "state",
    time.variable = "year",
    treatment.identifier = 99,
    controls.identifier = c(1,2,3,4,5,7,8,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,31,32,33,34,35,36,37,38,39,40,42,43,44,45,46,47,48),
    time.predictors.prior = c(1981:1985),
    time.optimize.ssr = c(1981:1985),
    time.plot = 1981:2003
  )

synth.out <- synth(dataprep.out)
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
  ) ; gaps
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

