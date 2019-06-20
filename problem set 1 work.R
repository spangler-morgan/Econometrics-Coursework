
#Setup
dat <- pp420_data
earn <- dat$earn
welf <- dat$adcc
tot <- dat$tinc
educ <- dat$higrade 
age <- dat$age
agesq <- dat$agesq
#B1
summary(earn)
summary(welf)
summary(tot)
summary(educ)
summary(age)
summary(agesq)
n1 <- ncol(earn)
n2 <- ncol(welf)
n3 <- ncol(tot)
n4 <- ncol(educ)
n5 <- ncol(age)
n6 <- ncol(agesq)
#B2
lm(earn~1)
lm(welf~1)
lm(tot~1)
lm(educ~1)
lm(age~1)
lm(agesq~1)
#B3
b3 <- lm(earn~educ+age+agesq)
b3
anova(b3)
#B3a
n <- nrow(b3$model)
n
#B3b
x<- b3$effects
x
#B3c
res <- b3$residuals
y <- b3[["model"]][["earn"]]
ybar <- b3$fitted.values
TSS <- sum((y-ybar)^2)
ESS <- sum(res^2)
RSS <- TSS-RSS
TSS
ESS
RSS
#B3d
Rsq <- 1- (ESS/TSS)
Rsq
#B4
#B4a
check1<- sum(res)
check1
#B4b
check2<- mean(y)-mean(ybar)
check2
#B4c
check3a <- cor(res,x)
check3a
check3b<-cor(res,y)
check3b
#B4d
check4 <- (cor(y,ybar)^2)-Rsq
check4
#B5
b5 <- lm(earn~educ+age+agesq-1)
anova(b5)
#B5a
res <- b5$residuals
x<- b5$effects
y <- b5[["model"]][["earn"]]
ybar <- b5$fitted.values
TSS <- sum((y-ybar)^2)
ESS <- sum(res^2)
Rsq <- 1- (ESS/TSS)
b5rsq <- r^2
check1<- sum(res)
check1
check2<- mean(y)-mean(ybar)
check2
check3a <- cor(res,x)
check3a
check3b<-cor(res,y)
check3b
check4 <- (cor(y,ybar)^2)-Rsq
check4