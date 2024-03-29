
library(foreign)
library(haven)
library(sandwich)
library(lmtest)
allsites <- read_dta("allsites_2019.dta")

lm1 = lm(lnmdvalhs0 ~ npl2000 + lnmeanhs8, data= allsites)
coeftest(lm1, vcov = vcovHC(lm1, "HC1"))

lm2 = lm(lnmdvalhs0 ~ npl2000 + lnmeanhs8 + firestoveheat80 + noaircond80 + nofullkitchen80 + bedrms0_80occ + bedrms1_80occ + bedrms2_80occ + bedrms3_80occ + bedrms4_80occ + bedrms5_80occ, data= allsites)
coeftest(lm2, vcov = vcovHC(lm2, "HC1"))

lm3 = lm(lnmdvalhs0 ~ npl2000 + lnmeanhs8 + firestoveheat80 + noaircond80 + nofullkitchen80 + bedrms0_80occ + bedrms1_80occ + bedrms2_80occ + bedrms3_80occ + bedrms4_80occ + bedrms5_80occ + shrblk8 + shrhsp8 + child8 + old8 + shrfor8 + ffh8 + hsdrop8 + no_hs_diploma8 + ba_or_better8 + unemprt8+povrat8+welfare8+avhhin8, data= allsites)
coeftest(lm3, vcov = vcovHC(lm3, "HC1"))

lm4 = lm(lnmdvalhs0 ~ npl2000 + lnmeanhs8 + firestoveheat80 + noaircond80 + nofullkitchen80 + bedrms0_80occ + bedrms1_80occ + bedrms2_80occ + bedrms3_80occ + bedrms4_80occ + bedrms5_80occ + shrblk8 + shrhsp8 + child8 + old8 + shrfor8 + ffh8 + hsdrop8 + no_hs_diploma8 + ba_or_better8 + unemprt8+povrat8+welfare8+avhhin8 + factor(statefips), data= allsites)
coeftest(lm4, vcov = vcovHC(lm4, "HC1"))


library(cobalt)
library(Hmisc)
allcovariates <- read_dta("allcovariates_2019.dta")
firestoveheat80_t <- subset(allcovariates$firestoveheat80, allcovariates$nbr_dummy==1)
firestoveheat80_c <- subset(allcovariates$firestoveheat80, allcovariates$nbr_dummy==0)
noaircond80_t <- subset(allcovariates$noaircond80, allcovariates$nbr_dummy==1)
noaircond80_c <- subset(allcovariates$noaircond80, allcovariates$nbr_dummy==0)
nofullkitchen80_t <- subset(allcovariates$nofullkitchen80, allcovariates$nbr_dummy==1)
nofullkitchen80_c <- subset(allcovariates$nofullkitchen80, allcovariates$nbr_dummy==0)
zerofullbath80_t <- subset(allcovariates$zerofullbath80, allcovariates$nbr_dummy==1)
zerofullbath80_c <- subset(allcovariates$zerofullbath80, allcovariates$nbr_dummy==0)
bedrms0_80occ_t <- subset(allcovariates$bedrms0_80occ, allcovariates$nbr_dummy==1)
bedrms0_80occ_c <- subset(allcovariates$bedrms0_80occ, allcovariates$nbr_dummy==0)
bedrms1_80occ_t <- subset(allcovariates$bedrms1_80occ, allcovariates$nbr_dummy==1)
bedrms1_80occ_c <- subset(allcovariates$bedrms1_80occ, allcovariates$nbr_dummy==0)
bedrms2_80occ_t <- subset(allcovariates$bedrms2_80occ, allcovariates$nbr_dummy==1)
bedrms2_80occ_c <- subset(allcovariates$bedrms2_80occ, allcovariates$nbr_dummy==0)
bedrms3_80occ_t <- subset(allcovariates$bedrms3_80occ, allcovariates$nbr_dummy==1)
bedrms3_80occ_c <- subset(allcovariates$bedrms3_80occ, allcovariates$nbr_dummy==0)
bedrms4_80occ_t <- subset(allcovariates$bedrms4_80occ, allcovariates$nbr_dummy==1)
bedrms4_80occ_c <- subset(allcovariates$bedrms4_80occ, allcovariates$nbr_dummy==0)
bedrms5_80occ_t <- subset(allcovariates$bedrms5_80occ, allcovariates$nbr_dummy==1)
bedrms5_80occ_c <- subset(allcovariates$bedrms5_80occ, allcovariates$nbr_dummy==0)
blt0_1yrs80occ_t <- subset(allcovariates$blt0_1yrs80occ, allcovariates$nbr_dummy==1)
blt0_1yrs80occ_c <- subset(allcovariates$blt0_1yrs80occ, allcovariates$nbr_dummy==0)
blt2_5yrs80occ_t <- subset(allcovariates$blt2_5yrs80occ, allcovariates$nbr_dummy==1)
blt2_5yrs80occ_c <- subset(allcovariates$blt2_5yrs80occ, allcovariates$nbr_dummy==0)
blt6_10yrs80occ_t <- subset(allcovariates$blt6_10yrs80occ, allcovariates$nbr_dummy==1)
blt6_10yrs80occ_c <- subset(allcovariates$blt6_10yrs80occ, allcovariates$nbr_dummy==0)
blt10_20yrs80occ_t <- subset(allcovariates$blt10_20yrs80occ, allcovariates$nbr_dummy==1)
blt10_20yrs80occ_c <- subset(allcovariates$blt10_20yrs80occ, allcovariates$nbr_dummy==0)
blt20_30yrs80occ_t <- subset(allcovariates$blt20_30yrs80occ, allcovariates$nbr_dummy==1)
blt20_30yrs80occ_c <- subset(allcovariates$blt20_30yrs80occ, allcovariates$nbr_dummy==0)
blt30_40yrs80occ_t <- subset(allcovariates$blt30_40yrs80occ, allcovariates$nbr_dummy==1)
blt30_40yrs80occ_c <- subset(allcovariates$blt30_40yrs80occ, allcovariates$nbr_dummy==0)
blt40_yrs80occ_t <- subset(allcovariates$blt40_yrs80occ, allcovariates$nbr_dummy==1)
blt40_yrs80occ_c <- subset(allcovariates$blt40_yrs80occ, allcovariates$nbr_dummy==0)
detach80occ_t <- subset(allcovariates$detach80occ, allcovariates$nbr_dummy==1)
detach80occ_c <- subset(allcovariates$detach80occ, allcovariates$nbr_dummy==0)
mobile80occ_t <- subset(allcovariates$mobile80occ, allcovariates$nbr_dummy==1)
mobile80occ_c <- subset(allcovariates$mobile80occ, allcovariates$nbr_dummy==0)
pop_den8_t <- subset(allcovariates$pop_den8, allcovariates$nbr_dummy==1)
pop_den8_c <- subset(allcovariates$pop_den8, allcovariates$nbr_dummy==0)
shrblk8_t <- subset(allcovariates$shrblk8, allcovariates$nbr_dummy==1)
shrblk8_c <- subset(allcovariates$shrblk8, allcovariates$nbr_dummy==0)
shrhsp8_t <- subset(allcovariates$shrhsp8, allcovariates$nbr_dummy==1)
shrhsp8_c <- subset(allcovariates$shrhsp8, allcovariates$nbr_dummy==0)
child8_t <- subset(allcovariates$child8, allcovariates$nbr_dummy==1)
child8_c <- subset(allcovariates$child8, allcovariates$nbr_dummy==0)
shrfor8_t <- subset(allcovariates$shrfor8, allcovariates$nbr_dummy==1)
shrfor8_c <- subset(allcovariates$shrfor8, allcovariates$nbr_dummy==0)
ffh8_t <- subset(allcovariates$ffh8, allcovariates$nbr_dummy==1)
ffh8_c <- subset(allcovariates$ffh8, allcovariates$nbr_dummy==0)
smhse8_t <- subset(allcovariates$smhse8, allcovariates$nbr_dummy==1)
smhse8_c <- subset(allcovariates$smhse8, allcovariates$nbr_dummy==0)
hsdrop8_t <- subset(allcovariates$hsdrop8, allcovariates$nbr_dummy==1)
hsdrop8_c <- subset(allcovariates$hsdrop8, allcovariates$nbr_dummy==0)
no_hs_diploma8_t <- subset(allcovariates$no_hs_diploma8, allcovariates$nbr_dummy==1)
no_hs_diploma8_c <- subset(allcovariates$no_hs_diploma8, allcovariates$nbr_dummy==0)
ba_or_better8_t <- subset(allcovariates$ba_or_better8, allcovariates$nbr_dummy==1)
ba_or_better8_c <- subset(allcovariates$ba_or_better8, allcovariates$nbr_dummy==0)
unemprt8_t <- subset(allcovariates$unemprt8, allcovariates$nbr_dummy==1)
unemprt8_c <- subset(allcovariates$unemprt8, allcovariates$nbr_dummy==0)
povrat8_t <- subset(allcovariates$povrat8, allcovariates$nbr_dummy==1)
povrat8_c <- subset(allcovariates$povrat8, allcovariates$nbr_dummy==0)
welfare8_t <- subset(allcovariates$welfare8, allcovariates$nbr_dummy==1)
welfare8_c <- subset(allcovariates$welfare8, allcovariates$nbr_dummy==0)
avhhin8_t <- subset(allcovariates$avhhin8, allcovariates$nbr_dummy==1)
avhhin8_c <- subset(allcovariates$avhhin8, allcovariates$nbr_dummy==0)
tothsun8_t <- subset(allcovariates$tothsun8, allcovariates$nbr_dummy==1)
tothsun8_c <- subset(allcovariates$tothsun8, allcovariates$nbr_dummy==0)
ownocc8_t <- subset(allcovariates$ownocc8, allcovariates$nbr_dummy==1)
ownocc8_c <- subset(allcovariates$ownocc8, allcovariates$nbr_dummy==0)
occupied80_t <- subset(allcovariates$occupied80, allcovariates$nbr_dummy==1)
occupied80_c <- subset(allcovariates$occupied80, allcovariates$nbr_dummy==0)

firestoveheat80 <- list(firestoveheat80_t,firestoveheat80_c)
noaircond80 <- list(noaircond80_t, noaircond80_c)
nofullkitchen80 <- list(nofullkitchen80_t,nofullkitchen80_c)
zerofullbath80 <- list(zerofullbath80_t,zerofullbath80_c)
bedrms0_80occ <- list(bedrms0_80occ_t, bedrms0_80occ_c)
bedrms1_80occ <- list(bedrms1_80occ_t, bedrms1_80occ_c)
bedrms2_80occ <- list(bedrms2_80occ_t, bedrms2_80occ_c)
bedrms3_80occ <- list(bedrms3_80occ_t, bedrms3_80occ_c)
bedrms4_80occ <- list(bedrms4_80occ_t, bedrms4_80occ_c)
bedrms5_80occ <- list(bedrms5_80occ_t, bedrms5_80occ_c)
blt0_1yrs80occ <- list(blt0_1yrs80occ_t, blt0_1yrs80occ_c)
blt2_5yrs80occ <- list(blt2_5yrs80occ_t, blt2_5yrs80occ_c)
blt6_10yrs80occ <- list(blt6_10yrs80occ_t, blt6_10yrs80occ_c)
blt10_20yrs80occ <- list(blt10_20yrs80occ_t, blt10_20yrs80occ_c)
blt20_30yrs80occ <- list(blt20_30yrs80occ_t, blt20_30yrs80occ_c)
blt30_40yrs80occ <- list(blt30_40yrs80occ_t, blt30_40yrs80occ_c)
blt40_yrs80occ <- list(blt40_yrs80occ_t, blt40_yrs80occ_c)
detach80occ <- list(detach80occ_t, detach80occ_c)
mobile80occ <- list(mobile80occ_t, mobile80occ_c)
pop_den8 <- list(pop_den8_t, pop_den8_c)
shrblk8 <- list(shrblk8_t, shrblk8_c)
shrhsp8 <- list(shrhsp8_t, shrhsp8_c)
child8 <- list(child8_t, child8_c)
shrfor8 <- list(shrfor8_t, shrfor8_c)
ffh8 <- list(ffh8_t, ffh8_c)
smhse8 <- list(smhse8_t, smhse8_c)
hsdrop8 <- list(hsdrop8_t, hsdrop8_c)
no_hs_diploma8 <- list(no_hs_diploma8_t, no_hs_diploma8_c)
ba_or_better8 <- list(ba_or_better8_t, ba_or_better8_c)
unemprt8 <- list(unemprt8_t, unemprt8_c)
povrat8 <- list(povrat8_t, povrat8_c)
welfare8 <- list(welfare8_t, welfare8_c)
avhhin8 <- list(avhhin8_t, avhhin8_c)
tothsun8 <- list(tothsun8_t, tothsun8_c)
ownocc8 <- list(ownocc8_t, ownocc8_c)
occupied80 <- list(occupied80_t, occupied80_c)

histbackback(firestoveheat80, main="firestoveheat80", xlab=c("Treat","Control"))
histbackback(noaircond80, main="noaircond80", xlab=c("Treat","Control"))
histbackback(nofullkitchen80, main="nofullkitchen80", xlab=c("Treat","Control"))
histbackback(zerofullbath80, main="zerofullbath80", xlab=c("Treat","Control"))
histbackback(bedrms0_80occ, main="bedrms0_80occ", xlab=c("Treat","Control"))
histbackback(bedrms1_80occ, main="bedrms1_80occ", xlab=c("Treat","Control"))
histbackback(bedrms2_80occ, main="bedrms2_80occ", xlab=c("Treat","Control"))
histbackback(bedrms3_80occ, main="bedrms3_80occ", xlab=c("Treat","Control"))
histbackback(bedrms4_80occ, main="bedrms4_80occ", xlab=c("Treat","Control"))
histbackback(bedrms5_80occ, main="bedrms5_80occ", xlab=c("Treat","Control"))
histbackback(blt0_1yrs80occ, main="blt0_1yrs80occ", xlab=c("Treat","Control"))
histbackback(blt2_5yrs80occ, main="blt2_5yrs80occ", xlab=c("Treat","Control"))
histbackback(blt6_10yrs80occ, main="blt6_10yrs80occ", xlab=c("Treat","Control"))
histbackback(blt10_20yrs80occ, main="blt10_20yrs80occ", xlab=c("Treat","Control"))
histbackback(blt20_30yrs80occ, main="blt20_30yrs80occ", xlab=c("Treat","Control"))
histbackback(blt30_40yrs80occ, main="blt30_40yrs80occ", xlab=c("Treat","Control"))
histbackback(blt40_yrs80occ, main="blt40_yrs80occ", xlab=c("Treat","Control"))
histbackback(detach80occ, main="detach80occ", xlab=c("Treat","Control"))
histbackback(mobile80occ, main="mobile80occ", xlab=c("Treat","Control"))
histbackback(pop_den8, main="pop_den8", xlab=c("Treat","Control"))
histbackback(shrblk8, main="shrblk8", xlab=c("Treat","Control"))
histbackback(shrhsp8, main="shrhsp8", xlab=c("Treat","Control"))
histbackback(child8, main="child8", xlab=c("Treat","Control"))
histbackback(shrfor8, main="shrfor8", xlab=c("Treat","Control"))
histbackback(ffh8, main="ffh8", xlab=c("Treat","Control"))
histbackback(smhse8, main="smhse8", xlab=c("Treat","Control"))
histbackback(hsdrop8, main="hsdrop8", xlab=c("Treat","Control"))
histbackback(no_hs_diploma8, main="no_hs_diploma8", xlab=c("Treat","Control"))
histbackback(ba_or_better8, main="ba_or_better8", xlab=c("Treat","Control"))
histbackback(unemprt8, main="unemprt8", xlab=c("Treat","Control"))
histbackback(povrat8, main="povrat8", xlab=c("Treat","Control"))
histbackback(welfare8, main="welfare8", xlab=c("Treat","Control"))
histbackback(avhhin8, main="avhhin8", xlab=c("Treat","Control"))
histbackback(tothsun8, main="tothsun8", xlab=c("Treat","Control"))
histbackback(ownocc8, main="ownocc8", xlab=c("Treat","Control"))
histbackback(occupied80,main="occupied80", xlab=c("Treat","Control"))

library(haven)
sitecovariates <- read_dta("sitecovariates_2019.dta")
library(cobalt)

library(Hmisc)
firestoveheat80_t <- subset(sitecovariates$firestoveheat80, sitecovariates$hrs_82 >= 28.5)
firestoveheat80_c <- subset(sitecovariates$firestoveheat80, sitecovariates$hrs_82 < 28.5)
noaircond80_t <- subset(sitecovariates$noaircond80, sitecovariates$hrs_82 >= 28.5)
noaircond80_c <- subset(sitecovariates$noaircond80, sitecovariates$hrs_82 < 28.5)
nofullkitchen80_t <- subset(sitecovariates$nofullkitchen80, sitecovariates$hrs_82 >= 28.5)
nofullkitchen80_c <- subset(sitecovariates$nofullkitchen80, sitecovariates$hrs_82 < 28.5)
zerofullbath80_t <- subset(sitecovariates$zerofullbath80, sitecovariates$hrs_82 >= 28.5)
zerofullbath80_c <- subset(sitecovariates$zerofullbath80, sitecovariates$hrs_82 < 28.5)
bedrms0_80occ_t <- subset(sitecovariates$bedrms0_80occ, sitecovariates$hrs_82 >= 28.5)
bedrms0_80occ_c <- subset(sitecovariates$bedrms0_80occ, sitecovariates$hrs_82 < 28.5)
bedrms1_80occ_t <- subset(sitecovariates$bedrms1_80occ, sitecovariates$hrs_82 >= 28.5)
bedrms1_80occ_c <- subset(sitecovariates$bedrms1_80occ, sitecovariates$hrs_82 < 28.5)
bedrms2_80occ_t <- subset(sitecovariates$bedrms2_80occ, sitecovariates$hrs_82 >= 28.5)
bedrms2_80occ_c <- subset(sitecovariates$bedrms2_80occ, sitecovariates$hrs_82 < 28.5)
bedrms3_80occ_t <- subset(sitecovariates$bedrms3_80occ, sitecovariates$hrs_82 >= 28.5)
bedrms3_80occ_c <- subset(sitecovariates$bedrms3_80occ, sitecovariates$hrs_82 < 28.5)
bedrms4_80occ_t <- subset(sitecovariates$bedrms4_80occ, sitecovariates$hrs_82 >= 28.5)
bedrms4_80occ_c <- subset(sitecovariates$bedrms4_80occ, sitecovariates$hrs_82 < 28.5)
bedrms5_80occ_t <- subset(sitecovariates$bedrms5_80occ, sitecovariates$hrs_82 >= 28.5)
bedrms5_80occ_c <- subset(sitecovariates$bedrms5_80occ, sitecovariates$hrs_82 < 28.5)
blt0_1yrs80occ_t <- subset(sitecovariates$blt0_1yrs80occ, sitecovariates$hrs_82 >= 28.5)
blt0_1yrs80occ_c <- subset(sitecovariates$blt0_1yrs80occ, sitecovariates$hrs_82 < 28.5)
blt2_5yrs80occ_t <- subset(sitecovariates$blt2_5yrs80occ, sitecovariates$hrs_82 >= 28.5)
blt2_5yrs80occ_c <- subset(sitecovariates$blt2_5yrs80occ, sitecovariates$hrs_82 < 28.5)
blt6_10yrs80occ_t <- subset(sitecovariates$blt6_10yrs80occ, sitecovariates$hrs_82 >= 28.5)
blt6_10yrs80occ_c <- subset(sitecovariates$blt6_10yrs80occ, sitecovariates$hrs_82 < 28.5)
blt10_20yrs80occ_t <- subset(sitecovariates$blt10_20yrs80occ, sitecovariates$hrs_82 >= 28.5)
blt10_20yrs80occ_c <- subset(sitecovariates$blt10_20yrs80occ, sitecovariates$hrs_82 < 28.5)
blt20_30yrs80occ_t <- subset(sitecovariates$blt20_30yrs80occ, sitecovariates$hrs_82 >= 28.5)
blt20_30yrs80occ_c <- subset(sitecovariates$blt20_30yrs80occ, sitecovariates$hrs_82 < 28.5)
blt30_40yrs80occ_t <- subset(sitecovariates$blt30_40yrs80occ, sitecovariates$hrs_82 >= 28.5)
blt30_40yrs80occ_c <- subset(sitecovariates$blt30_40yrs80occ, sitecovariates$hrs_82 < 28.5)
blt40_yrs80occ_t <- subset(sitecovariates$blt40_yrs80occ, sitecovariates$hrs_82 >= 28.5)
blt40_yrs80occ_c <- subset(sitecovariates$blt40_yrs80occ, sitecovariates$hrs_82 < 28.5)
detach80occ_t <- subset(sitecovariates$detach80occ, sitecovariates$hrs_82 >= 28.5)
detach80occ_c <- subset(sitecovariates$detach80occ, sitecovariates$hrs_82 < 28.5)
mobile80occ_t <- subset(sitecovariates$mobile80occ, sitecovariates$hrs_82 >= 28.5)
mobile80occ_c <- subset(sitecovariates$mobile80occ, sitecovariates$hrs_82 < 28.5)
pop_den8_t <- subset(sitecovariates$pop_den8, sitecovariates$hrs_82 >= 28.5)
pop_den8_c <- subset(sitecovariates$pop_den8, sitecovariates$hrs_82 < 28.5)
shrblk8_t <- subset(sitecovariates$shrblk8, sitecovariates$hrs_82 >= 28.5)
shrblk8_c <- subset(sitecovariates$shrblk8, sitecovariates$hrs_82 < 28.5)
shrhsp8_t <- subset(sitecovariates$shrhsp8, sitecovariates$hrs_82 >= 28.5)
shrhsp8_c <- subset(sitecovariates$shrhsp8, sitecovariates$hrs_82 < 28.5)
child8_t <- subset(sitecovariates$child8, sitecovariates$hrs_82 >= 28.5)
child8_c <- subset(sitecovariates$child8, sitecovariates$hrs_82 < 28.5)
shrfor8_t <- subset(sitecovariates$shrfor8, sitecovariates$hrs_82 >= 28.5)
shrfor8_c <- subset(sitecovariates$shrfor8, sitecovariates$hrs_82 < 28.5)
ffh8_t <- subset(sitecovariates$ffh8, sitecovariates$hrs_82 >= 28.5)
ffh8_c <- subset(sitecovariates$ffh8, sitecovariates$hrs_82 < 28.5)
smhse8_t <- subset(sitecovariates$smhse8, sitecovariates$hrs_82 >= 28.5)
smhse8_c <- subset(sitecovariates$smhse8, sitecovariates$hrs_82 < 28.5)
hsdrop8_t <- subset(sitecovariates$hsdrop8, sitecovariates$hrs_82 >= 28.5)
hsdrop8_c <- subset(sitecovariates$hsdrop8, sitecovariates$hrs_82 < 28.5)
no_hs_diploma8_t <- subset(sitecovariates$no_hs_diploma8, sitecovariates$hrs_82 >= 28.5)
no_hs_diploma8_c <- subset(sitecovariates$no_hs_diploma8, sitecovariates$hrs_82 < 28.5)
ba_or_better8_t <- subset(sitecovariates$ba_or_better8, sitecovariates$hrs_82 >= 28.5)
ba_or_better8_c <- subset(sitecovariates$ba_or_better8, sitecovariates$hrs_82 < 28.5)
unemprt8_t <- subset(sitecovariates$unemprt8, sitecovariates$hrs_82 >= 28.5)
unemprt8_c <- subset(sitecovariates$unemprt8, sitecovariates$hrs_82 < 28.5)
povrat8_t <- subset(sitecovariates$povrat8, sitecovariates$hrs_82 >= 28.5)
povrat8_c <- subset(sitecovariates$povrat8, sitecovariates$hrs_82 < 28.5)
welfare8_t <- subset(sitecovariates$welfare8, sitecovariates$hrs_82 >= 28.5)
welfare8_c <- subset(sitecovariates$welfare8, sitecovariates$hrs_82 < 28.5)
avhhin8_t <- subset(sitecovariates$avhhin8, sitecovariates$hrs_82 >= 28.5)
avhhin8_c <- subset(sitecovariates$avhhin8, sitecovariates$hrs_82 < 28.5)
tothsun8_t <- subset(sitecovariates$tothsun8, sitecovariates$hrs_82 >= 28.5)
tothsun8_c <- subset(sitecovariates$tothsun8, sitecovariates$hrs_82 < 28.5)
ownocc8_t <- subset(sitecovariates$ownocc8, sitecovariates$hrs_82 >= 28.5)
ownocc8_c <- subset(sitecovariates$ownocc8, sitecovariates$hrs_82 < 28.5)
occupied80_t <- subset(sitecovariates$occupied80, sitecovariates$hrs_82 >= 28.5)
occupied80_c <- subset(sitecovariates$occupied80, sitecovariates$hrs_82 < 28.5)

firestoveheat80 <- list(firestoveheat80_t,firestoveheat80_c)
noaircond80 <- list(noaircond80_t, noaircond80_c)
nofullkitchen80 <- list(nofullkitchen80_t,nofullkitchen80_c)
zerofullbath80 <- list(zerofullbath80_t,zerofullbath80_c)
bedrms0_80occ <- list(bedrms0_80occ_t, bedrms0_80occ_c)
bedrms1_80occ <- list(bedrms1_80occ_t, bedrms1_80occ_c)
bedrms2_80occ <- list(bedrms2_80occ_t, bedrms2_80occ_c)
bedrms3_80occ <- list(bedrms3_80occ_t, bedrms3_80occ_c)
bedrms4_80occ <- list(bedrms4_80occ_t, bedrms4_80occ_c)
bedrms5_80occ <- list(bedrms5_80occ_t, bedrms5_80occ_c)
blt0_1yrs80occ <- list(blt0_1yrs80occ_t, blt0_1yrs80occ_c)
blt2_5yrs80occ <- list(blt2_5yrs80occ_t, blt2_5yrs80occ_c)
blt6_10yrs80occ <- list(blt6_10yrs80occ_t, blt6_10yrs80occ_c)
blt10_20yrs80occ <- list(blt10_20yrs80occ_t, blt10_20yrs80occ_c)
blt20_30yrs80occ <- list(blt20_30yrs80occ_t, blt20_30yrs80occ_c)
blt30_40yrs80occ <- list(blt30_40yrs80occ_t, blt30_40yrs80occ_c)
blt40_yrs80occ <- list(blt40_yrs80occ_t, blt40_yrs80occ_c)
detach80occ <- list(detach80occ_t, detach80occ_c)
mobile80occ <- list(mobile80occ_t, mobile80occ_c)
pop_den8 <- list(pop_den8_t, pop_den8_c)
shrblk8 <- list(shrblk8_t, shrblk8_c)
shrhsp8 <- list(shrhsp8_t, shrhsp8_c)
child8 <- list(child8_t, child8_c)
shrfor8 <- list(shrfor8_t, shrfor8_c)
ffh8 <- list(ffh8_t, ffh8_c)
smhse8 <- list(smhse8_t, smhse8_c)
hsdrop8 <- list(hsdrop8_t, hsdrop8_c)
no_hs_diploma8 <- list(no_hs_diploma8_t, no_hs_diploma8_c)
ba_or_better8 <- list(ba_or_better8_t, ba_or_better8_c)
unemprt8 <- list(unemprt8_t, unemprt8_c)
povrat8 <- list(povrat8_t, povrat8_c)
welfare8 <- list(welfare8_t, welfare8_c)
avhhin8 <- list(avhhin8_t, avhhin8_c)
tothsun8 <- list(tothsun8_t, tothsun8_c)
ownocc8 <- list(ownocc8_t, ownocc8_c)
occupied80 <- list(occupied80_t, occupied80_c)

histbackback(firestoveheat80, main="firestoveheat80", xlab=c("Treat","Control"))
histbackback(noaircond80, main="noaircond80", xlab=c("Treat","Control"))
histbackback(nofullkitchen80, main="nofullkitchen80", xlab=c("Treat","Control"))
histbackback(zerofullbath80, main="zerofullbath80", xlab=c("Treat","Control"))
histbackback(bedrms0_80occ, main="bedrms0_80occ", xlab=c("Treat","Control"))
histbackback(bedrms1_80occ, main="bedrms1_80occ", xlab=c("Treat","Control"))
histbackback(bedrms2_80occ, main="bedrms2_80occ", xlab=c("Treat","Control"))
histbackback(bedrms3_80occ, main="bedrms3_80occ", xlab=c("Treat","Control"))
histbackback(bedrms4_80occ, main="bedrms4_80occ", xlab=c("Treat","Control"))
histbackback(bedrms5_80occ, main="bedrms5_80occ", xlab=c("Treat","Control"))
histbackback(blt0_1yrs80occ, main="blt0_1yrs80occ", xlab=c("Treat","Control"))
histbackback(blt2_5yrs80occ, main="blt2_5yrs80occ", xlab=c("Treat","Control"))
histbackback(blt6_10yrs80occ, main="blt6_10yrs80occ", xlab=c("Treat","Control"))
histbackback(blt10_20yrs80occ, main="blt10_20yrs80occ", xlab=c("Treat","Control"))
histbackback(blt20_30yrs80occ, main="blt20_30yrs80occ", xlab=c("Treat","Control"))
histbackback(blt30_40yrs80occ, main="blt30_40yrs80occ", xlab=c("Treat","Control"))
histbackback(blt40_yrs80occ, main="blt40_yrs80occ", xlab=c("Treat","Control"))
histbackback(detach80occ, main="detach80occ", xlab=c("Treat","Control"))
histbackback(mobile80occ, main="mobile80occ", xlab=c("Treat","Control"))
histbackback(pop_den8, main="pop_den8", xlab=c("Treat","Control"))
histbackback(shrblk8, main="shrblk8", xlab=c("Treat","Control"))
histbackback(shrhsp8, main="shrhsp8", xlab=c("Treat","Control"))
histbackback(child8, main="child8", xlab=c("Treat","Control"))
histbackback(shrfor8, main="shrfor8", xlab=c("Treat","Control"))
histbackback(ffh8, main="ffh8", xlab=c("Treat","Control"))
histbackback(smhse8, main="smhse8", xlab=c("Treat","Control"))
histbackback(hsdrop8, main="hsdrop8", xlab=c("Treat","Control"))
histbackback(no_hs_diploma8, main="no_hs_diploma8", xlab=c("Treat","Control"))
histbackback(ba_or_better8, main="ba_or_better8", xlab=c("Treat","Control"))
histbackback(unemprt8, main="unemprt8", xlab=c("Treat","Control"))
histbackback(povrat8, main="povrat8", xlab=c("Treat","Control"))
histbackback(welfare8, main="welfare8", xlab=c("Treat","Control"))
histbackback(avhhin8, main="avhhin8", xlab=c("Treat","Control"))
histbackback(tothsun8, main="tothsun8", xlab=c("Treat","Control"))
histbackback(ownocc8, main="ownocc8", xlab=c("Treat","Control"))
histbackback(occupied80,main="occupied80", xlab=c("Treat","Control"))

firestoveheat80_t <- subset(firestoveheat80_t, sitecovariates$hrs_82 < 40.5)
firestoveheat80_c <- subset(firestoveheat80_c, sitecovariates$hrs_82 > 16.5)
noaircond80_t <- subset(noaircond80_t, sitecovariates$hrs_82 < 40.5)
noaircond80_c <- subset(noaircond80_c, sitecovariates$hrs_82 > 16.5)
nofullkitchen80_t <- subset(nofullkitchen80_t, sitecovariates$hrs_82 < 40.5)
nofullkitchen80_c <- subset(nofullkitchen80_c, sitecovariates$hrs_82 > 16.5)
zerofullbath80_t <- subset(zerofullbath80_t, sitecovariates$hrs_82 < 40.5)
zerofullbath80_c <- subset(zerofullbath80_c, sitecovariates$hrs_82 > 16.5)
bedrms0_80occ_t <- subset(bedrms0_80occ_t, sitecovariates$hrs_82 < 40.5)
bedrms0_80occ_c <- subset(bedrms0_80occ_c, sitecovariates$hrs_82 > 16.5)
bedrms1_80occ_t <- subset(bedrms1_80occ_t, sitecovariates$hrs_82 < 40.5)
bedrms1_80occ_c <- subset(bedrms1_80occ_c, sitecovariates$hrs_82 > 16.5)
bedrms2_80occ_t <- subset(bedrms2_80occ_t, sitecovariates$hrs_82 < 40.5)
bedrms2_80occ_c <- subset(bedrms2_80occ_c, sitecovariates$hrs_82 > 16.5)
bedrms3_80occ_t <- subset(bedrms3_80occ_t, sitecovariates$hrs_82 < 40.5)
bedrms3_80occ_c <- subset(bedrms3_80occ_c, sitecovariates$hrs_82 > 16.5)
bedrms4_80occ_t <- subset(bedrms4_80occ_t, sitecovariates$hrs_82 < 40.5)
bedrms4_80occ_c <- subset(bedrms4_80occ_c, sitecovariates$hrs_82 > 16.5)
bedrms5_80occ_t <- subset(bedrms5_80occ_t, sitecovariates$hrs_82 < 40.5)
bedrms5_80occ_c <- subset(bedrms5_80occ_c, sitecovariates$hrs_82 > 16.5)
blt0_1yrs80occ_t <- subset(blt0_1yrs80occ_t, sitecovariates$hrs_82 < 40.5)
blt0_1yrs80occ_c <- subset(blt0_1yrs80occ_c, sitecovariates$hrs_82 > 16.5)
blt2_5yrs80occ_t <- subset(blt2_5yrs80occ_t, sitecovariates$hrs_82 < 40.5)
blt2_5yrs80occ_c <- subset(blt2_5yrs80occ_c, sitecovariates$hrs_82 > 16.5)
blt6_10yrs80occ_t <- subset(blt6_10yrs80occ_t, sitecovariates$hrs_82 < 40.5)
blt6_10yrs80occ_c <- subset(blt6_10yrs80occ_c, sitecovariates$hrs_82 > 16.5)
blt10_20yrs80occ_t <- subset(blt10_20yrs80occ_t, sitecovariates$hrs_82 < 40.5)
blt10_20yrs80occ_c <- subset(blt10_20yrs80occ_c, sitecovariates$hrs_82 > 16.5)
blt20_30yrs80occ_t <- subset(blt20_30yrs80occ_t, sitecovariates$hrs_82 < 40.5)
blt20_30yrs80occ_c <- subset(blt20_30yrs80occ_c, sitecovariates$hrs_82 > 16.5)
blt30_40yrs80occ_t <- subset(blt30_40yrs80occ_t, sitecovariates$hrs_82 < 40.5)
blt30_40yrs80occ_c <- subset(blt30_40yrs80occ_c, sitecovariates$hrs_82 > 16.5)
blt40_yrs80occ_t <- subset(blt40_yrs80occ_t, sitecovariates$hrs_82 < 40.5)
blt40_yrs80occ_c <- subset(blt40_yrs80occ_c, sitecovariates$hrs_82 > 16.5)
detach80occ_t <- subset(detach80occ_t, sitecovariates$hrs_82 < 40.5)
detach80occ_c <- subset(detach80occ_c, sitecovariates$hrs_82 > 16.5)
mobile80occ_t <- subset(mobile80occ_t, sitecovariates$hrs_82 < 40.5)
mobile80occ_c <- subset(mobile80occ_c, sitecovariates$hrs_82 > 16.5)
pop_den8_t <- subset(pop_den8_t, sitecovariates$hrs_82 < 40.5)
pop_den8_c <- subset(pop_den8_c, sitecovariates$hrs_82 > 16.5)
shrblk8_t <- subset(shrblk8_t, sitecovariates$hrs_82 < 40.5)
shrblk8_c <- subset(shrblk8_c, sitecovariates$hrs_82 > 16.5)
shrhsp8_t <- subset(shrhsp8_t, sitecovariates$hrs_82 < 40.5)
shrhsp8_c <- subset(shrhsp8_c, sitecovariates$hrs_82 > 16.5)
child8_t <- subset(child8_t, sitecovariates$hrs_82 < 40.5)
child8_c <- subset(child8_c, sitecovariates$hrs_82 > 16.5)
shrfor8_t <- subset(shrfor8_t, sitecovariates$hrs_82 < 40.5)
shrfor8_c <- subset(shrfor8_c, sitecovariates$hrs_82 > 16.5)
ffh8_t <- subset(ffh8_t, sitecovariates$hrs_82 < 40.5)
ffh8_c <- subset(ffh8_c, sitecovariates$hrs_82 > 16.5)
smhse8_t <- subset(smhse8_t, sitecovariates$hrs_82 < 40.5)
smhse8_c <- subset(smhse8_c, sitecovariates$hrs_82 > 16.5)
hsdrop8_t <- subset(hsdrop8_t, sitecovariates$hrs_82 < 40.5)
hsdrop8_c <- subset(hsdrop8_c, sitecovariates$hrs_82 > 16.5)
no_hs_diploma8_t <- subset(no_hs_diploma8_t, sitecovariates$hrs_82 < 40.5)
no_hs_diploma8_c <- subset(no_hs_diploma8_c, sitecovariates$hrs_82 > 16.5)
ba_or_better8_t <- subset(ba_or_better8_t, sitecovariates$hrs_82 < 40.5)
ba_or_better8_c <- subset(ba_or_better8_c, sitecovariates$hrs_82 > 16.5)
unemprt8_t <- subset(unemprt8_t, sitecovariates$hrs_82 < 40.5)
unemprt8_c <- subset(unemprt8_c, sitecovariates$hrs_82 > 16.5)
povrat8_t <- subset(povrat8_t, sitecovariates$hrs_82 < 40.5)
povrat8_c <- subset(povrat8_c, sitecovariates$hrs_82 > 16.5)
welfare8_t <- subset(welfare8_t, sitecovariates$hrs_82 < 40.5)
welfare8_c <- subset(welfare8_c, sitecovariates$hrs_82 > 16.5)
avhhin8_t <- subset(avhhin8_t, sitecovariates$hrs_82 < 40.5)
avhhin8_c <- subset(avhhin8_c, sitecovariates$hrs_82 > 16.5)
tothsun8_t <- subset(tothsun8_t, sitecovariates$hrs_82 < 40.5)
tothsun8_c <- subset(tothsun8_c, sitecovariates$hrs_82 > 16.5)
ownocc8_t <- subset(ownocc8_t, sitecovariates$hrs_82 < 40.5)
ownocc8_c <- subset(ownocc8_c, sitecovariates$hrs_82 > 16.5)
occupied80_t <- subset(occupied80_t, sitecovariates$hrs_82 < 40.5)
occupied80_c <- subset(occupied80_c, sitecovariates$hrs_82 > 16.5)

firestoveheat80 <- list(firestoveheat80_t,firestoveheat80_c)
noaircond80 <- list(noaircond80_t, noaircond80_c)
nofullkitchen80 <- list(nofullkitchen80_t,nofullkitchen80_c)
zerofullbath80 <- list(zerofullbath80_t,zerofullbath80_c)
bedrms0_80occ <- list(bedrms0_80occ_t, bedrms0_80occ_c)
bedrms1_80occ <- list(bedrms1_80occ_t, bedrms1_80occ_c)
bedrms2_80occ <- list(bedrms2_80occ_t, bedrms2_80occ_c)
bedrms3_80occ <- list(bedrms3_80occ_t, bedrms3_80occ_c)
bedrms4_80occ <- list(bedrms4_80occ_t, bedrms4_80occ_c)
bedrms5_80occ <- list(bedrms5_80occ_t, bedrms5_80occ_c)
blt0_1yrs80occ <- list(blt0_1yrs80occ_t, blt0_1yrs80occ_c)
blt2_5yrs80occ <- list(blt2_5yrs80occ_t, blt2_5yrs80occ_c)
blt6_10yrs80occ <- list(blt6_10yrs80occ_t, blt6_10yrs80occ_c)
blt10_20yrs80occ <- list(blt10_20yrs80occ_t, blt10_20yrs80occ_c)
blt20_30yrs80occ <- list(blt20_30yrs80occ_t, blt20_30yrs80occ_c)
blt30_40yrs80occ <- list(blt30_40yrs80occ_t, blt30_40yrs80occ_c)
blt40_yrs80occ <- list(blt40_yrs80occ_t, blt40_yrs80occ_c)
detach80occ <- list(detach80occ_t, detach80occ_c)
mobile80occ <- list(mobile80occ_t, mobile80occ_c)
pop_den8 <- list(pop_den8_t, pop_den8_c)
shrblk8 <- list(shrblk8_t, shrblk8_c)
shrhsp8 <- list(shrhsp8_t, shrhsp8_c)
child8 <- list(child8_t, child8_c)
shrfor8 <- list(shrfor8_t, shrfor8_c)
ffh8 <- list(ffh8_t, ffh8_c)
smhse8 <- list(smhse8_t, smhse8_c)
hsdrop8 <- list(hsdrop8_t, hsdrop8_c)
no_hs_diploma8 <- list(no_hs_diploma8_t, no_hs_diploma8_c)
ba_or_better8 <- list(ba_or_better8_t, ba_or_better8_c)
unemprt8 <- list(unemprt8_t, unemprt8_c)
povrat8 <- list(povrat8_t, povrat8_c)
welfare8 <- list(welfare8_t, welfare8_c)
avhhin8 <- list(avhhin8_t, avhhin8_c)
tothsun8 <- list(tothsun8_t, tothsun8_c)
ownocc8 <- list(ownocc8_t, ownocc8_c)
occupied80 <- list(occupied80_t, occupied80_c)

histbackback(firestoveheat80, main="firestoveheat80", xlab=c("Treat","Control"))
histbackback(noaircond80, main="noaircond80", xlab=c("Treat","Control"))
histbackback(nofullkitchen80, main="nofullkitchen80", xlab=c("Treat","Control"))
histbackback(zerofullbath80, main="zerofullbath80", xlab=c("Treat","Control"))
histbackback(bedrms0_80occ, main="bedrms0_80occ", xlab=c("Treat","Control"))
histbackback(bedrms1_80occ, main="bedrms1_80occ", xlab=c("Treat","Control"))
histbackback(bedrms2_80occ, main="bedrms2_80occ", xlab=c("Treat","Control"))
histbackback(bedrms3_80occ, main="bedrms3_80occ", xlab=c("Treat","Control"))
histbackback(bedrms4_80occ, main="bedrms4_80occ", xlab=c("Treat","Control"))
histbackback(bedrms5_80occ, main="bedrms5_80occ", xlab=c("Treat","Control"))
histbackback(blt0_1yrs80occ, main="blt0_1yrs80occ", xlab=c("Treat","Control"))
histbackback(blt2_5yrs80occ, main="blt2_5yrs80occ", xlab=c("Treat","Control"))
histbackback(blt6_10yrs80occ, main="blt6_10yrs80occ", xlab=c("Treat","Control"))
histbackback(blt10_20yrs80occ, main="blt10_20yrs80occ", xlab=c("Treat","Control"))
histbackback(blt20_30yrs80occ, main="blt20_30yrs80occ", xlab=c("Treat","Control"))
histbackback(blt30_40yrs80occ, main="blt30_40yrs80occ", xlab=c("Treat","Control"))
histbackback(blt40_yrs80occ, main="blt40_yrs80occ", xlab=c("Treat","Control"))
histbackback(detach80occ, main="detach80occ", xlab=c("Treat","Control"))
histbackback(mobile80occ, main="mobile80occ", xlab=c("Treat","Control"))
histbackback(pop_den8, main="pop_den8", xlab=c("Treat","Control"))
histbackback(shrblk8, main="shrblk8", xlab=c("Treat","Control"))
histbackback(shrhsp8, main="shrhsp8", xlab=c("Treat","Control"))
histbackback(child8, main="child8", xlab=c("Treat","Control"))
histbackback(shrfor8, main="shrfor8", xlab=c("Treat","Control"))
histbackback(ffh8, main="ffh8", xlab=c("Treat","Control"))
histbackback(smhse8, main="smhse8", xlab=c("Treat","Control"))
histbackback(hsdrop8, main="hsdrop8", xlab=c("Treat","Control"))
histbackback(no_hs_diploma8, main="no_hs_diploma8", xlab=c("Treat","Control"))
histbackback(ba_or_better8, main="ba_or_better8", xlab=c("Treat","Control"))
histbackback(unemprt8, main="unemprt8", xlab=c("Treat","Control"))
histbackback(povrat8, main="povrat8", xlab=c("Treat","Control"))
histbackback(welfare8, main="welfare8", xlab=c("Treat","Control"))
histbackback(avhhin8, main="avhhin8", xlab=c("Treat","Control"))
histbackback(tothsun8, main="tothsun8", xlab=c("Treat","Control"))
histbackback(ownocc8, main="ownocc8", xlab=c("Treat","Control"))
histbackback(occupied80,main="occupied80", xlab=c("Treat","Control"))

twomile <- read_dta("2miledata_2019.dta")
HRShist <- hist(twomile$hrs_82,breaks=11,freq=FALSE,xlab="HRS Score - 1982",main="HRS Dist Hist")
abline(v=28.5,col="blue")


HRShist <- hist(twomile$hrs_82,breaks=11,freq=FALSE,xlab="HRS Score - 1982",main="HRS Hist with LLR")
abline(v=28.5,col="blue")
x<- HRShist$mids
y<- HRShist$density
dat <- cbind(x,y)
dat <- as.data.frame(dat)
datL <- subset(dat, x<28.5)
datR <- subset(dat, x>28.5)
xL <- datL$x
xR <- datR$x
yL <- datL$y
yR <- datR$y
datR <- subset(dat, x>28.5)
abline(lsfit(xL,yL), xlim=28.5, col="red")
abline(lsfit(xR,yR), col="red")


twomile$z <- ifelse(twomile$hrs_82>=28.5,1,0)

lm_1st <- lm(npl2000 ~ z + firestoveheat80_nbr + noaircond80_nbr + nofullkitchen80_nbr + zerofullbath80_nbr + bedrms0_80occ_nbr + bedrms1_80occ_nbr + bedrms2_80occ_nbr + bedrms3_80occ_nbr + bedrms4_80occ_nbr + bedrms5_80occ_nbr + blt0_1yrs80occ_nbr + 
               blt2_5yrs80occ_nbr + blt6_10yrs80occ_nbr + blt10_20yrs80occ_nbr + blt20_30yrs80occ_nbr + blt30_40yrs80occ_nbr + blt40_yrs80occ_nbr + detach80occ_nbr + mobile80occ_nbr + pop_den8_nbr + shrblk8_nbr + shrhsp8_nbr + child8_nbr + shrfor8_nbr + ffh8_nbr + smhse8_nbr + hsdrop8_nbr + no_hs_diploma8_nbr + ba_or_better8_nbr + unemprt8_nbr + povrat8_nbr + welfare8_nbr + avhhin8_nbr + tothsun8_nbr + ownocc8_nbr + occupied80_nbr, data=twomile)
npl2000_fit1 <- lm_1st$fitted.values
coeftest(lm_1st, vcov = vcovHC(lm_1st, "HC1"))

twomile_2 <- subset(twomile, twomile$hrs_82>= 16.5)
twomile_2 <- subset(twomile_2, twomile_2$hrs_82<= 40.5)

lm_1st_2 <- lm(npl2000 ~ z + firestoveheat80_nbr + noaircond80_nbr + nofullkitchen80_nbr + zerofullbath80_nbr + bedrms0_80occ_nbr + bedrms1_80occ_nbr + bedrms2_80occ_nbr + bedrms3_80occ_nbr + bedrms4_80occ_nbr + bedrms5_80occ_nbr + blt0_1yrs80occ_nbr + 
                 blt2_5yrs80occ_nbr + blt6_10yrs80occ_nbr + blt10_20yrs80occ_nbr + blt20_30yrs80occ_nbr + blt30_40yrs80occ_nbr + blt40_yrs80occ_nbr + detach80occ_nbr + mobile80occ_nbr + pop_den8_nbr + shrblk8_nbr + shrhsp8_nbr + child8_nbr + shrfor8_nbr + ffh8_nbr + smhse8_nbr + hsdrop8_nbr + no_hs_diploma8_nbr + ba_or_better8_nbr + unemprt8_nbr + povrat8_nbr + welfare8_nbr + avhhin8_nbr + tothsun8_nbr + ownocc8_nbr + occupied80_nbr, data=twomile_2)
npl2000_fit2 <- lm_1st_2$fitted.values
coeftest(lm_1st_2, vcov = vcovHC(lm_1st_2, "HC1"))

plot(twomile$hrs_82,twomile$npl2000)
plot(twomile$hrs_82, twomile$meanhs8)

lm_2nd <- lm(meanhs8 ~ npl2000_fit1 + firestoveheat80_nbr + noaircond80_nbr + nofullkitchen80_nbr + zerofullbath80_nbr + bedrms0_80occ_nbr + bedrms1_80occ_nbr + bedrms2_80occ_nbr + bedrms3_80occ_nbr + bedrms4_80occ_nbr + bedrms5_80occ_nbr + blt0_1yrs80occ_nbr + blt2_5yrs80occ_nbr + blt6_10yrs80occ_nbr + blt10_20yrs80occ_nbr + blt20_30yrs80occ_nbr + blt30_40yrs80occ_nbr + blt40_yrs80occ_nbr + detach80occ_nbr + mobile80occ_nbr + pop_den8_nbr + shrblk8_nbr + shrhsp8_nbr + child8_nbr + shrfor8_nbr + ffh8_nbr + smhse8_nbr + hsdrop8_nbr + no_hs_diploma8_nbr + ba_or_better8_nbr + unemprt8_nbr + povrat8_nbr + welfare8_nbr + avhhin8_nbr + tothsun8_nbr + ownocc8_nbr + occupied80_nbr, data=twomile)
coeftest(lm_2nd, vcov = vcovHC(lm_2nd, "HC1"))

twomile_2 <- subset(twomile, twomile$hrs_82>= 16.5)
twomile_2 <- subset(twomile_2, twomile_2$hrs_82<= 40.5)

lm_2nd_2 <- lm(npl2000 ~ npl2000_fit2 + firestoveheat80_nbr + noaircond80_nbr + nofullkitchen80_nbr + zerofullbath80_nbr + bedrms0_80occ_nbr + bedrms1_80occ_nbr + bedrms2_80occ_nbr + bedrms3_80occ_nbr + bedrms4_80occ_nbr + bedrms5_80occ_nbr + blt0_1yrs80occ_nbr + blt2_5yrs80occ_nbr + blt6_10yrs80occ_nbr + blt10_20yrs80occ_nbr + blt20_30yrs80occ_nbr + blt30_40yrs80occ_nbr + blt40_yrs80occ_nbr + detach80occ_nbr + mobile80occ_nbr + pop_den8_nbr + shrblk8_nbr + shrhsp8_nbr + child8_nbr + shrfor8_nbr + ffh8_nbr + smhse8_nbr + hsdrop8_nbr + no_hs_diploma8_nbr + ba_or_better8_nbr + unemprt8_nbr + povrat8_nbr + welfare8_nbr + avhhin8_nbr + tothsun8_nbr + ownocc8_nbr + occupied80_nbr, data=twomile_2)
coeftest(lm_2nd_2, vcov = vcovHC(lm_2nd_2, "HC1"))
