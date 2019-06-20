library(foreign)
library(haven)
library(sandwich)
library(lmtest)
library(naniar)
library(dplyr)
library(cobalt)
library(boot)
library(stats)
library(plm)
library(vcov)
library(multiwayvcov)
library(gdata)
library(car)

#Part 1
#Q1
dat <- read_dta("simulated0_2019.dta")
df<- data.frame(dat)
#a
cor(df$u0,df$group)
#b
A <- lm(y0~treat,data=df)
summary(A)
#c
coeftest(A,cluster.vcov(A,cluster=df$group))
#Q2
dat <- read_dta("simulated1_2019.dta")
df<- data.frame(dat)
#a
cor(df$u,df$group)
#b
A <- lm(y~treat,data=df)
summary(A)
#c
coeftest(A,cluster.vcov(A,cluster=df$group))
#d
#e
cov <- vector("list",1000)
i=1
B=1000
for(i in 1:B) {
  y <- df$y
  treat <- df$treat
  y_re <-sample(y,replace=TRUE)
  treat_re <- sample(treat,replace=TRUE)
  iter <- cbind(y_re,treat_re)
  iter <- as.data.frame(iter)
  reg <- lm(y_re~treat_re,data=iter)
  X<-summary(reg)$coefficients[, 2]
  cov[i]<-X[2]
  i=i+1
}
cov_boot <- as.data.frame(unlist(cov))
cov_boot.mean<- mean(unlist(cov))
cov_boot$dif <- cov_boot-cov_boot.mean
cov_boot$difsq <- (cov_boot[2])^2
cov_boot.sum <- colSums(cov_boot[3])
cov_boot.fin <- cov_boot.sum/(B-1)
cov_boot.fin
#f
B=1000
cov <- vector("list",B)
i=1
for(i in 1:B) {
  y <- df$y
  treat <- df$treat
  y_re <-sample(y,replace=TRUE)
  treat_re <- sample(treat,replace=TRUE)
  iter <- cbind(y_re,treat_re)
  iter <- as.data.frame(iter)
  reg <- lm(y_re~treat_re,data=iter)
  X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
  cov[i]<-X[2]
  i=i+1
}
cov_boot <- as.data.frame(unlist(cov))
cov_boot.mean<- mean(unlist(cov))
cov_boot$dif <- cov_boot-cov_boot.mean
cov_boot$difsq <- (cov_boot[2])^2
cov_boot.sum <- colSums(cov_boot[3])
cov_boot.fin <- cov_boot.sum/(B-1)
cov_boot.fin
#g
A <- data.frame(z1=double(),z2=double(),z3=double(),z4=double(),z5=double(),z6=double(),z7=double(),z8=double(),
                stringsAsFactors=FALSE)
j=1
for(j in 1:100) {
  B=5
  cov <- vector("list",B)
  i=1
  for(i in 1:B) {
    y <- df$y
    treat <- df$treat
    y_re <-sample(y,replace=TRUE)
    treat_re <- sample(treat,replace=TRUE)
    iter <- cbind(y_re,treat_re)
    iter <- as.data.frame(iter)
    reg <- lm(y_re~treat_re,data=iter)
    X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
    cov[i]<-X[2]
    i=i+1
  }
  cov_boot <- as.data.frame(unlist(cov))
  cov_boot.mean<- mean(unlist(cov))
  cov_boot$dif <- cov_boot-cov_boot.mean
  cov_boot$difsq <- (cov_boot[2])^2
  cov_boot.sum <- colSums(cov_boot[3])
  cov_boot.fin <- cov_boot.sum/(B-1)
  A[1,j] <- cov_boot.fin
  j=j+1
}

j=1
for(j in 1:100) {
  B=10
  cov <- vector("list",B)
  i=1
  for(i in 1:B) {
    y <- df$y
    treat <- df$treat
    y_re <-sample(y,replace=TRUE)
    treat_re <- sample(treat,replace=TRUE)
    iter <- cbind(y_re,treat_re)
    iter <- as.data.frame(iter)
    reg <- lm(y_re~treat_re,data=iter)
    X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
    cov[i]<-X[2]
    i=i+1
  }
  cov_boot <- as.data.frame(unlist(cov))
  cov_boot.mean<- mean(unlist(cov))
  cov_boot$dif <- cov_boot-cov_boot.mean
  cov_boot$difsq <- (cov_boot[2])^2
  cov_boot.sum <- colSums(cov_boot[3])
  cov_boot.fin <- cov_boot.sum/(B-1)
  A[2,j] <- cov_boot.fin
  j=j+1
}

j=1
for(j in 1:100) {
  B=15
  cov <- vector("list",B)
  i=1
  for(i in 1:B) {
    y <- df$y
    treat <- df$treat
    y_re <-sample(y,replace=TRUE)
    treat_re <- sample(treat,replace=TRUE)
    iter <- cbind(y_re,treat_re)
    iter <- as.data.frame(iter)
    reg <- lm(y_re~treat_re,data=iter)
    X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
    cov[i]<-X[2]
    i=i+1
  }
  cov_boot <- as.data.frame(unlist(cov))
  cov_boot.mean<- mean(unlist(cov))
  cov_boot$dif <- cov_boot-cov_boot.mean
  cov_boot$difsq <- (cov_boot[2])^2
  cov_boot.sum <- colSums(cov_boot[3])
  cov_boot.fin <- cov_boot.sum/(B-1)
  A[3,j] <- cov_boot.fin
  j=j+1
}

j=1
for(j in 1:100) {
  B=20
  cov <- vector("list",B)
  i=1
  for(i in 1:B) {
    y <- df$y
    treat <- df$treat
    y_re <-sample(y,replace=TRUE)
    treat_re <- sample(treat,replace=TRUE)
    iter <- cbind(y_re,treat_re)
    iter <- as.data.frame(iter)
    reg <- lm(y_re~treat_re,data=iter)
    X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
    cov[i]<-X[2]
    i=i+1
  }
  cov_boot <- as.data.frame(unlist(cov))
  cov_boot.mean<- mean(unlist(cov))
  cov_boot$dif <- cov_boot-cov_boot.mean
  cov_boot$difsq <- (cov_boot[2])^2
  cov_boot.sum <- colSums(cov_boot[3])
  cov_boot.fin <- cov_boot.sum/(B-1)
  A[4,j] <- cov_boot.fin
  j=j+1
}

j=1
for(j in 1:100) {
  B=30
  cov <- vector("list",B)
  i=1
  for(i in 1:B) {
    y <- df$y
    treat <- df$treat
    y_re <-sample(y,replace=TRUE)
    treat_re <- sample(treat,replace=TRUE)
    iter <- cbind(y_re,treat_re)
    iter <- as.data.frame(iter)
    reg <- lm(y_re~treat_re,data=iter)
    X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
    cov[i]<-X[2]
    i=i+1
  }
  cov_boot <- as.data.frame(unlist(cov))
  cov_boot.mean<- mean(unlist(cov))
  cov_boot$dif <- cov_boot-cov_boot.mean
  cov_boot$difsq <- (cov_boot[2])^2
  cov_boot.sum <- colSums(cov_boot[3])
  cov_boot.fin <- cov_boot.sum/(B-1)
  A[5,j] <- cov_boot.fin
  j=j+1
}

j=1
for(j in 1:100) {
  B=50
  cov <- vector("list",B)
  i=1
  for(i in 1:B) {
    y <- df$y
    treat <- df$treat
    y_re <-sample(y,replace=TRUE)
    treat_re <- sample(treat,replace=TRUE)
    iter <- cbind(y_re,treat_re)
    iter <- as.data.frame(iter)
    reg <- lm(y_re~treat_re,data=iter)
    X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
    cov[i]<-X[2]
    i=i+1
  }
  cov_boot <- as.data.frame(unlist(cov))
  cov_boot.mean<- mean(unlist(cov))
  cov_boot$dif <- cov_boot-cov_boot.mean
  cov_boot$difsq <- (cov_boot[2])^2
  cov_boot.sum <- colSums(cov_boot[3])
  cov_boot.fin <- cov_boot.sum/(B-1)
  A[6,j] <- cov_boot.fin
  j=j+1
}

j=1
for(j in 1:100) {
  B=80
  cov <- vector("list",B)
  i=1
  for(i in 1:B) {
    y <- df$y
    treat <- df$treat
    y_re <-sample(y,replace=TRUE)
    treat_re <- sample(treat,replace=TRUE)
    iter <- cbind(y_re,treat_re)
    iter <- as.data.frame(iter)
    reg <- lm(y_re~treat_re,data=iter)
    X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
    cov[i]<-X[2]
    i=i+1
  }
  cov_boot <- as.data.frame(unlist(cov))
  cov_boot.mean<- mean(unlist(cov))
  cov_boot$dif <- cov_boot-cov_boot.mean
  cov_boot$difsq <- (cov_boot[2])^2
  cov_boot.sum <- colSums(cov_boot[3])
  cov_boot.fin <- cov_boot.sum/(B-1)
  A[7,j] <- cov_boot.fin
  j=j+1
}

j=1
for(j in 1:100) {
  B=100
  cov <- vector("list",B)
  i=1
  for(i in 1:B) {
    y <- df$y
    treat <- df$treat
    y_re <-sample(y,replace=TRUE)
    treat_re <- sample(treat,replace=TRUE)
    iter <- cbind(y_re,treat_re)
    iter <- as.data.frame(iter)
    reg <- lm(y_re~treat_re,data=iter)
    X<-coeftest(reg,cluster.vcov(reg,cluster=df$group))
    cov[i]<-X[2]
    i=i+1
  }
  cov_boot <- as.data.frame(unlist(cov))
  cov_boot.mean<- mean(unlist(cov))
  cov_boot$dif <- cov_boot-cov_boot.mean
  cov_boot$difsq <- (cov_boot[2])^2
  cov_boot.sum <- colSums(cov_boot[3])
  cov_boot.fin <- cov_boot.sum/(B-1)
  A[8,j] <- cov_boot.fin
  j=j+1
}
A<-t(A)
boxplot(A,names=c("B=5","B=10","B=15","B=20","B=30","B=50","B=80","B=100"))

#Part 2
#Q1
#a
#b
#Q2
#a
#b
#c
#Q3
dat <- read_dta("traffic_safety2_2019.dta")
df<- data.frame(dat)
#a
reg1 <- lm(fatalities~primary,data=df)
summary(reg1)
df$time <- df$year-1981
df$time2<-df$time*df$time
df$time3<-df$time2*df$time
df$time4<-df$time3*df$time
reg2 <- lm(fatalities~primary+time+time2+time3+time4,data=df)
summary(reg2)
reg3 <- lm(fatalities~primary+time+time2+time3+time4+college+beer+precip+snow32+secondary+rural_speed+urban_speed+secondary+totalvmt,data=df)
summary(reg3)
#b
robust1<-hccm(reg1)
robust2<-hccm(reg2)
robust3<-hccm(reg3)
#manual clustering function
clustering   <- function(dat,fn,cluster){
  attach(dat, warn.conflicts = F)
  G <- length(unique(cluster))
  N <- length(cluster)
  K <- fn$rank
  uj  <- apply(estfun(fn),2, function(x) tapply(x, cluster, sum));
  vcovCL <- sandwich(fn, meat=crossprod(uj)/N)
  coeftest(fn, vcovCL) }

summary(reg1)
robust1
clustering(df,reg1,state)
coeftest(reg1,cluster.vcov(reg1,cluster=df$state))
summary(reg2)
robust2
clustering(df,reg2,state)
coeftest(reg2,cluster.vcov(reg2,cluster=df$state))
summary(reg3)
robust3
clustering(df,reg3,state)
coeftest(reg3,cluster.vcov(reg3,cluster=df$state))
#c
btwn1 <- plm(fatalities~primary,data=df,model="between")
btwn2 <- plm(fatalities~primary+time+time2+time3+time4,data=df,model="between")
btwn3 <- plm(fatalities~primary+time+time2+time3+time4+college+beer+precip+snow32+secondary+rural_speed+urban_speed+secondary+totalvmt,data=df,model="between")
summary(btwn1)
summary(btwn2)
summary(btwn3)
#d
re1 <- plm(fatalities~primary,data=df,model="random")
re2 <- plm(fatalities~primary+time+time2+time3+time4,data=df,model="random")
re3 <- plm(fatalities~primary+time+time2+time3+time4+college+beer+precip+snow32+secondary+rural_speed+urban_speed+secondary+totalvmt,data=df,model="random")
summary(re1)
summary(re2)
summary(re3)
#e
coeftest(re1,vcov.=vcovHC(re1,cluster="group"))
coeftest(re2,vcov.=vcovHC(re2,cluster="group"))
coeftest(re3,vcov.=vcovHC(re3,cluster="group"))
#f
fe <- plm(fatalities~primary+time4,data=df,model="within")
summary(fe)
coeftest(fe,vcov.=vcovHC(fe,cluster="group"))
#g
fe1 <- plm(fatalities~primary,data=df,model="within")
fe2 <- plm(fatalities~primary+time+time2+time3+time4,data=df,model="within")
fe3 <- plm(fatalities~primary+time+time2+time3+time4+college+beer+precip+snow32+secondary+rural_speed+urban_speed+secondary+totalvmt,data=df,model="within")
summary(fe1)
summary(fe2)
summary(fe3)
coeftest(fe1,vcov.=vcovHC(fe1,cluster="group"))
coeftest(fe2,vcov.=vcovHC(fe2,cluster="group"))
coeftest(fe3,vcov.=vcovHC(fe3,cluster="group"))
#appendix
plot(fatalities~primary,data=df)
plot(fatalities~primary+time+time2+time3+time4,data=df)
plot(fatalities~primary+time+time2+time3+time4+college+beer+precip+snow32+secondary+rural_speed+urban_speed+secondary+totalvmt,data=df)
