#PPHA 41400 Problem Set 1 work
#Problem 1
library(psych)
library(knitr)
library(cobalt)
library(dplyr)
df_all <- read.csv('~/Desktop/JTPA.csv', header=TRUE, sep=',')
library(MatchIt); data("df_all", package = "cobalt")
#1.1
df_all_drop <-df_all %>%
  na.omit()		
dim(df_all_drop)
adj_df_all <- na.omit(df_all)
treatexp <- matchit(treated ~ afdc_ra + badenglh + fdst_ra + ged + kids + kidsud4 + longafdc + married + minor02 + minor03 + minor04 + neveradc + neverful + neverwrk + nodegree + single + numinhh + schlhgst + agesq + pre12ern + pre12wrk + bifid + age + caltime + site, data = adj_df_all)
bal.plot(treatexp, "afdc_ra", which = "unadjusted")
bal.plot(treatexp, "badenglh", which = "unadjusted")
bal.plot(treatexp, "fdst_ra", which = "unadjusted")
bal.plot(treatexp, "ged", which = "unadjusted")
bal.plot(treatexp, "kids", which = "unadjusted")
bal.plot(treatexp, "kidsud4", which = "unadjusted")
bal.plot(treatexp, "longafdc", which = "unadjusted")
bal.plot(treatexp, "married", which = "unadjusted")
bal.plot(treatexp, "minor02", which = "unadjusted")
bal.plot(treatexp, "minor03", which = "unadjusted")
bal.plot(treatexp, "minor04", which = "unadjusted")
bal.plot(treatexp, "neveradc", which = "unadjusted")
bal.plot(treatexp, "neverful", which = "unadjusted")
bal.plot(treatexp, "neverwrk", which = "unadjusted")
bal.plot(treatexp, "nodegree", which = "unadjusted")
bal.plot(treatexp, "single", which = "unadjusted")
bal.plot(treatexp, "numinhh", which = "unadjusted")
bal.plot(treatexp, "schlhgst", which = "unadjusted")
bal.plot(treatexp, "agesq", which = "unadjusted")
bal.plot(treatexp, "pre12ern", which = "unadjusted")
bal.plot(treatexp, "pre12wrk", which = "unadjusted")
bal.plot(treatexp, "bifid", which = "unadjusted")
bal.plot(treatexp, "age", which = "unadjusted")
bal.plot(treatexp, "caltime", which = "unadjusted")
bal.plot(treatexp, "site", which = "unadjusted")

trtgrpexp <- matchit(expstat ~ afdc_ra + badenglh + fdst_ra + ged + kids + kidsud4 + longafdc + married + minor02 + minor03 + minor04 + neveradc + neverful + neverwrk + nodegree + single + numinhh + schlhgst + agesq + pre12ern + pre12wrk + bifid + age + caltime + site, data = adj_df_all)
bal.plot(trtgrpexp, "afdc_ra", which = "unadjusted")
bal.plot(trtgrpexp, "badenglh", which = "unadjusted")
bal.plot(trtgrpexp, "fdst_ra", which = "unadjusted")
bal.plot(trtgrpexp, "ged", which = "unadjusted")
bal.plot(trtgrpexp, "kids", which = "unadjusted")
bal.plot(trtgrpexp, "kidsud4", which = "unadjusted")
bal.plot(trtgrpexp, "longafdc", which = "unadjusted")
bal.plot(trtgrpexp, "married", which = "unadjusted")
bal.plot(trtgrpexp, "minor02", which = "unadjusted")
bal.plot(trtgrpexp, "minor03", which = "unadjusted")
bal.plot(trtgrpexp, "minor04", which = "unadjusted")
bal.plot(trtgrpexp, "neveradc", which = "unadjusted")
bal.plot(trtgrpexp, "neverful", which = "unadjusted")
bal.plot(trtgrpexp, "neverwrk", which = "unadjusted")
bal.plot(trtgrpexp, "nodegree", which = "unadjusted")
bal.plot(trtgrpexp, "single", which = "unadjusted")
bal.plot(trtgrpexp, "numinhh", which = "unadjusted")
bal.plot(trtgrpexp, "schlhgst", which = "unadjusted")
bal.plot(trtgrpexp, "agesq", which = "unadjusted")
bal.plot(trtgrpexp, "pre12ern", which = "unadjusted")
bal.plot(trtgrpexp, "pre12wrk", which = "unadjusted")
bal.plot(trtgrpexp, "bifid", which = "unadjusted")
bal.plot(trtgrpexp, "age", which = "unadjusted")
bal.plot(trtgrpexp, "caltime", which = "unadjusted")
bal.plot(trtgrpexp, "site", which = "unadjusted")

densityBy(adj_df_all, var="expstat", grp="treated", main="Density of crossover of treatment groups")
densityBy(adj_df_all, var="expstat", grp="treated", freq=TRUE, main="Frequency of crossover of treatment groups")
densityBy(adj_df_all, var="expstat", grp=NULL, main="Density of placement in treatment group")
densityBy(adj_df_all, var="expstat", grp=NULL, freq=TRUE, main="Frequency of placement in treatment group")
densityBy(adj_df_all, var="treated", grp=NULL, main="Density of receiving treatment")
densityBy(adj_df_all, var="treated", grp=NULL, freq=TRUE, main="Frequency of receiving treatment")

#1.2
trtgrp <- subset(adj_df_all, expstat == 1, select=c(emp))
trtgrp_mean <- mean(trtgrp$emp)

ctrlgrp <- subset (adj_df_all, expstat == 0, select=c(emp))
ctrlgrp_mean <- mean(ctrlgrp$emp)

itt <- trtgrp_mean - ctrlgrp_mean
itt

#1.3
trt <- subset(adj_df_all, treated == 1, select=c(emp))
ctrl <- subset(adj_df_all, treated ==0, select=c(emp))

trtact_mean <- mean(trt$emp)
ctrlact_mean <- mean(ctrl$emp)

numer <- itt
denom <- trtact_mean - ctrlact_mean
denom

bloom <- numer/denom
bloom

#1.4
trt_itt <- subset(adj_df_all, expstat == 1, select = c(emp, afdc_ra, badenglh, fdst_ra, ged, kids, kidsud4, longafdc, married, minor02, minor03, minor04, neveradc, neverful, neverwrk, nodegree, single, numinhh, schlhgst, agesq, pre12ern, pre12wrk, bifid, age, caltime, site))
ctrl_itt <- subset(adj_df_all, expstat == 0, select = c(emp, afdc_ra, badenglh, fdst_ra, ged, kids, kidsud4, longafdc, married, minor02, minor03, minor04, neveradc, neverful, neverwrk, nodegree, single, numinhh, schlhgst, agesq, pre12ern, pre12wrk, bifid, age, caltime, site))

trt_ittmodel <- lm(emp ~ afdc_ra + badenglh + fdst_ra + ged + kids + kidsud4 + longafdc + married + minor02 + minor03 + minor04 + neveradc + neverful + neverwrk + nodegree + single + numinhh + schlhgst + agesq + pre12ern + pre12wrk + bifid + age + caltime + site, data = trt_itt)
ctrl_ittmodel <- lm(emp ~ afdc_ra + badenglh + fdst_ra + ged + kids + kidsud4 + longafdc + married + minor02 + minor03 + minor04 + neveradc + neverful + neverwrk + nodegree + single + numinhh + schlhgst + agesq + pre12ern + pre12wrk + bifid + age + caltime + site, data = ctrl_itt)

itt <- (coef(trt_ittmodel)[1] - coef(ctrl_ittmodel)[1])
itt

trt_bloom <- subset(adj_df_all, treated == 1, select = c(emp, afdc_ra, badenglh, fdst_ra, ged, kids, kidsud4, longafdc, married, minor02, minor03, minor04, neveradc, neverful, neverwrk, nodegree, single, numinhh, schlhgst, agesq, pre12ern, pre12wrk, bifid, age, caltime, site))
ctrl_bloom <- subset(adj_df_all, treated == 0, select = c(emp, afdc_ra, badenglh, fdst_ra, ged, kids, kidsud4, longafdc, married, minor02, minor03, minor04, neveradc, neverful, neverwrk, nodegree, single, numinhh, schlhgst, agesq, pre12ern, pre12wrk, bifid, age, caltime, site))

trt_bloommodel <- lm(emp ~ afdc_ra + badenglh + fdst_ra + ged + kids + kidsud4 + longafdc + married + minor02 + minor03 + minor04 + neveradc + neverful + neverwrk + nodegree + single + numinhh + schlhgst + agesq + pre12ern + pre12wrk + bifid + age + caltime + site, data = trt_bloom)
ctrl_bloommodel <- lm(emp ~ afdc_ra + badenglh + fdst_ra + ged + kids + kidsud4 + longafdc + married + minor02 + minor03 + minor04 + neveradc + neverful + neverwrk + nodegree + single + numinhh + schlhgst + agesq + pre12ern + pre12wrk + bifid + age + caltime + site, data = ctrl_bloom)
bloom <- (coef(trt_bloommodel)[1] - coef(ctrl_bloommodel)[1])/(itt)
bloom

#Problem 2
set.seed(0)
n <- 1000
mu0 <- 40000
mu1 <- 50000
s0 <- 10000
s1 <- 20000
rho <- 0
mu <- c(mu0,mu1)
sigma <- matrix(c(s0^2, s0*s1*rho, s0*s1*rho, s1^2),2)
#2.1
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col="red")
}
gibbs<-function (n, mu0, s0, mu1, s1, rho) 
{
  mat <- matrix(ncol = 4, nrow = n)
  y0 <- 0
  y1 <- 0
  D <- 0
  y <- 0
  mat[1, ] <- c(y0, y1, D, y)
  for (i in 1:n) {
    y0 <- rnorm(1, mu0 + 
                  (s0/s1) * rho * (y1 - mu1), sqrt((1 - rho^2)*s0^2))
    y1 <- rnorm(1, mu1 + 
                  (s1/s0) * rho * (y0 - mu0), sqrt((1 - rho^2)*s1^2))
    D <- ifelse(y1 - y0 >= 0, 1, 0)
    y <- y0*(1-D) + y1*D
    mat[i, ] <- c(y0, y1, D, y)
  }
  mat
}
bvn4 <- gibbs(n,mu0,s0,mu1,s1,rho)
colnames(bvn4) <- c("Y0","Y1", "D", "Y")
bvn4
pctcollege <- mean(bvn4[,3])
pctcollege
#2.2
naive_est <- mean(bvn4[,4])
naive_est
#2.3
expy0 <- mean(bvn4[,1])
expy1 <- mean(bvn4[,2])
ate <- expy1 - expy0
ate
#2.4
college <- subset(bvn4, bvn4[,4]==1, select=c("Y0", "Y1", "D", "Y"))
nocollege <- subset(bvn4, bvn4[,4]==0,select=c("Y0", "Y1", "D", "Y"))
college_y1 <- mean(college[,2])
college_y0 <- mean(college[,1])
att <- college_y1 - college_y0
nocollege_y1 <- mean(nocollege[,2])
nocollege_yo <- mean(nocollege[,1])
atn <- nocollege_y1 - nocollege_yo
att
atn
#2.5
set.seed(0)
n <- 1000
mu0 <- 40000
mu1 <- 50000
s0 <- 10000
s1 <- 20000
rho <- 0.6
mu <- c(mu0,mu1)
sigma <- matrix(c(s0^2, s0*s1*rho, s0*s1*rho, s1^2),2)
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col="red")
}
gibbs<-function (n, mu0, s0, mu1, s1, rho) 
{
  mat <- matrix(ncol = 4, nrow = n)
  y0 <- 0
  y1 <- 0
  D <- 0
  y <- 0
  mat[1, ] <- c(y0, y1, D, y)
  for (i in 1:n) {
    y0 <- rnorm(1, mu0 + 
                  (s0/s1) * rho * (y1 - mu1), sqrt((1 - rho^2)*s0^2))
    y1 <- rnorm(1, mu1 + 
                  (s1/s0) * rho * (y0 - mu0), sqrt((1 - rho^2)*s1^2))
    D <- ifelse(y1 - y0 >= 0, 1, 0)
    y <- y0*(1-D) + y1*D
    mat[i, ] <- c(y0, y1, D, y)
  }
  mat
}
bvn4 <- gibbs(n,mu0,s0,mu1,s1,rho)
colnames(bvn4) <- c("Y0","Y1", "D", "Y")
bvn4
pctcollege <- mean(bvn4[,3])
pctcollege
naive_est <- mean(bvn4[,4])
naive_est
expy0 <- mean(bvn4[,1])
expy1 <- mean(bvn4[,2])
ate <- expy1 - expy0
ate

#2.6
set.seed(0)
n <- 1000
mu0 <- 40000
mu1 <- 50000
s0 <- 10000
s1 <- 20000
rho <- -0.5
mu <- c(mu0,mu1)
sigma <- matrix(c(s0^2, s0*s1*rho, s0*s1*rho, s1^2),2)
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col="red")
}
gibbs<-function (n, mu0, s0, mu1, s1, rho) 
{
  mat <- matrix(ncol = 4, nrow = n)
  y0 <- 0
  y1 <- 0
  D <- 0
  y <- 0
  mat[1, ] <- c(y0, y1, D, y)
  for (i in 1:n) {
    y0 <- rnorm(1, mu0 + 
                  (s0/s1) * rho * (y1 - mu1), sqrt((1 - rho^2)*s0^2))
    y1 <- rnorm(1, mu1 + 
                  (s1/s0) * rho * (y0 - mu0), sqrt((1 - rho^2)*s1^2))
    D <- ifelse(y1 - y0 >= 0, 1, 0)
    y <- y0*(1-D) + y1*D
    mat[i, ] <- c(y0, y1, D, y)
  }
  mat
}
bvn4 <- gibbs(n,mu0,s0,mu1,s1,rho)
colnames(bvn4) <- c("Y0","Y1", "D", "Y")
bvn4
pctcollege <- mean(bvn4[,3])
pctcollege
naive_est <- mean(bvn4[,4])
naive_est
expy0 <- mean(bvn4[,1])
expy1 <- mean(bvn4[,2])
ate <- expy1 - expy0
ate