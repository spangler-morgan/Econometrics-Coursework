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
#What is the causal effect of maternal smoking during pregnancy on infant birthweight and other health outcomes?
#Data from 1993 National Natality Detail Files for Pennsylvania
#One obs is an infant-mother match

#Cleaning data, should get 103,112 observations at the end
dat <- read_dta("ps3_2019.dta")
df<- data.frame(dat)
#rectype: 1(res), 2(non res)
#pldel3: place of delivery, hospital (1), not hospital (2)
#birattnd: MD (1), DO (2), Cert Nurse Midwife (3), Midwife (4), Other (5)
#cntocpop: pop of county, 1mil+ (0), 500K-1mil (1), 250K-500k (2), 100k-250k(3), less than 100k (9)
#stresfip: state of residence, foreign (0), state fips code (1-54)
#dmage: age of mother (12-49 years)
#ormoth: hispanic origin of mother, nonhisp (0), mex (1), PR (2), cuban (3), central or SA (4), other (5)
#mrace3: race of mother, white (1), other (2), black (3)
#dmeduc: mothers ed, none (0), 1-8 yrs of elementary (1-8), 1-4 yrs of HS (9-12), 1-4 yrs of col (13-16), 5+ col (17)
#dmar: mother marital status, married (1), unmarried (2)
#adequacy: adequacy of care, adq (1), int (2), inadq (3)
#nlbnl: # of live births living, 0-29
#dlivord: live birth order
#dtotord: total birth order
#totord9: birth order (1st - 8th+)
#monpre: month in preg prenatal care began, none (0), 1st-9th (1-9)
#nprevist: number of prenatal visits
#disllb: interval since last live birth, 0 ind plural birth
#isllb10: interval since last live birth, no prev birth (0), plural birth (1), 1-11mos (2), 12-17mos (3), 18-23mos (4), 24-35mos (5), 36-47mos (6), 48-59mos (7), 60-71mos (8), 72+mos (9)
#dfage: father's age
#orfath: hispanic origin of father, nonhisp (0), mex (1), PR (2), cuban (3), C or SA (4), other/unknown hisp (5)
#dfeduc: fatherd ed, none (0), 1-8 yrs of elementary (1-8), 1-4 yrs of HS (9-12), 1-4 yrs of col (13-16), 5+ col (17)
#birmon: birth month, jan-dec (1-12)
#weekday: day of week of birth, sun-sat (1-7)
#dgestat: gestation in weeks
#csex: sex of child, M (1), F (2)
#dbrwt: birth weight in grams
#dplural: single (1), twin (2), triplet (3), quadruplet (4)
#omaps: one min apgar 
#fmaps: five min apgar
#clingest: clinical est of gest
#delmeth5: method of delivery, vag (1), vag post csection (2), csection (3), repeat csection (4), not stated (5)
#anemia: reported (1), not reported (2)
#cardiac: reported (1), not reported (2)
#lung: reported (1), not reported (2)
#diabetes: reported (1), not reported (2)
#herpes: reported (1), not reported (2)
#chyper: reported (1), not reported (2)
#phyper: reported(1), not reported (2)
#pre4000: previous infant 4000+ grams, reported (1), not reported (2)
#preterm: reported (1), not reported (2)
#tobacco: yes(1), no (2)
#cigar: avg # of cigs per day, detailed
#cigar6: avg # of cigs per day
#alcohol: use during pregnancy, Y (1), N (0)
#drink: avg # drinks per week, detailed
#drink5: avg # of drinks per week
#wgain: weight gain

df <- replace_with_na(df, replace=list(herpes=8))
df <- replace_with_na(df,replace = list(tobacco = 9))
df <- replace_with_na(df, replace=list(cigar=99))
df<- replace_with_na(df,replace = list(cigar6 = 6))
df <- replace_with_na(df,replace = list(alcohol = 9))
df <- replace_with_na(df,replace = list(drink = 99))
df <- replace_with_na(df,replace = list(drink5 = 5))
df <- replace_with_na(df, replace=list(wgain=99))
summary(df)
finaldf <- select(df, c("pldel3", "birattnd", "cntocpop", "stresfip", "dmage", "ormoth", "mrace3", "dmeduc", "dtotord", "totord9", "monpre", "nprevist", "dfage", "orfath", "dfeduc", "birmon", "weekday", "dgestat", "csex", "dbrwt", "dplural", "omaps", "fmaps", "clingest", "delmeth5", "anemia", "cardiac", "lung", "diabetes", "herpes", "chyper", "phyper", "pre4000", "preterm", "tobacco", "cigar6", "alcohol", "drink5", "wgain", "dmar", "adequacy", "nlbnl", "dlivord"))
finaldf <- na.omit(df)

finaldf$pldel3 <- ifelse(finaldf$pldel3==1, 0, 1)
#pldel3: place of delivery, hospital (0), not hospital (1)
finaldf$do <- ifelse(finaldf$birattnd==2, 1,0)
finaldf$cnm <- ifelse(finaldf$birattnd==3,1,0)
finaldf$md <- ifelse(finaldf$birattnd==1,1,0)
finaldf$midwife <- ifelse(finaldf$birattnd==4,1,0)
finaldf <- subset(finaldf, select=-c(birattnd))
#birattnd: MD (1), DO (2), Cert Nurse Midwife (3), Midwife (4), Other (5)
finaldf$county.size <- ifelse(finaldf$cntocpop==2,0,1)
finaldf <- subset(finaldf, select=-c(cntocpop))
#cntocpop: pop of county, 1mil+ (0), 500K-1mil (1), 250K-500k (2), 100k-250k(3), less than 100k (9)
finaldf$stresfip <- factor(finaldf$stresfip,0:55,0:55)
#stresfip: state of residence, foreign (0), state fips code (1-54)
#dmage: age of mother (12-49 years)
finaldf$ormoth <- factor(finaldf$ormoth, 0:5, labels=c("Non-Hispanic","Mexican","Puerto Rican", "Cuban","Central or South American","Other Hispanic"))
#ormoth: hispanic origin of mother, nonhisp (0), mex (1), PR (2), cuban (3), central or SA (4), other (5)
finaldf$mrace3 <- factor(finaldf$mrace3, 1:3, labels=c("White","Other","Black"))
#mrace3: race of mother, white (1), other (2), black (3)
finaldf$dmeduc <- factor(finaldf$dmeduc, 0:17, labels=c("No Schooling","1 year","2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years", "11 years", "High School Grad", "1 year college", "2 years college", "3 years college", "4 years college", "5+ years college"))
#dmeduc: mothers ed, none (0), 1-8 yrs of elementary (1-8), 1-4 yrs of HS (9-12), 1-4 yrs of col (13-16), 5+ col (17)
finaldf$dmar <- ifelse(finaldf$dmar==1, 1, 0)
#dmar: mother marital status, married (1), unmarried (2)
finaldf$adequacy <- factor(finaldf$adequacy, 1:3, labels=c("Adequate","Intermediate","Inadequate"))
#adequacy: adequacy of care, adq (1), int (2), inadq (3)
#nlbnl: # of live births living, 0-29
#dlivord: live birth order
#dtotord: total birth order
#totord9: birth order (1st - 8th+)
#monpre: month in preg prenatal care began, none (0), 1st-9th (1-9)
#nprevist: number of prenatal visits
#dfage: father's age
finaldf$orfath <- factor(finaldf$orfath,  0:5, labels=c("Non-Hispanic","Mexican","Puerto Rican", "Cuban","Central or South American","Other Hispanic"))
#orfath: hispanic origin of father, nonhisp (0), mex (1), PR (2), cuban (3), C or SA (4), other/unknown hisp (5)
finaldf$dfeduc <- factor(finaldf$dfeduc,  0:17, labels=c("No Schooling","1 year","2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years", "11 years", "High School Grad", "1 year college", "2 years college", "3 years college", "4 years college", "5+ years college"))
#dfeduc: fatherd ed, none (0), 1-8 yrs of elementary (1-8), 1-4 yrs of HS (9-12), 1-4 yrs of col (13-16), 5+ col (17)
#birmon: birth month, jan-dec (1-12)
#weekday: day of week of birth, sun-sat (1-7)
#dgestat: gestation in weeks
finaldf$csex <- ifelse(finaldf$csex=="M", 1, 0)
#csex: sex of child, M (1), F (2)
#dbrwt: birth weight in grams
#dplural: single (1), twin (2), triplet (3), quadruplet (4)
#omaps: one min apgar 
#fmaps: five min apgar
#clingest: clinical est of gest
finaldf$delmeth5 <- factor(finaldf$delmeth5, 1:5, labels=c("Vaginal","Vaginal Post C-Section","C-Section","Repeat C-Section","Not Stated"))
#delmeth5: method of delivery, vag (1), vag post csection (2), csection (3), repeat csection (4), not stated (5)
finaldf$anemia <- ifelse(finaldf$anemia==1, 1, 0)
#anemia: reported (1), not reported (2)
finaldf$cardiac <- ifelse(finaldf$cardiac==1, 1, 0)
#cardiac: reported (1), not reported (2)
finaldf$lung <- ifelse(finaldf$lung==1, 1, 0)
#lung: reported (1), not reported (2)
finaldf$diabetes <- ifelse(finaldf$diabetes==1, 1, 0)
#diabetes: reported (1), not reported (2)
finaldf$herpes <- ifelse(finaldf$herpes==1, 1, 0)
#herpes: reported (1), not reported (2)
finaldf$chyper <- ifelse(finaldf$chyper==1, 1, 0)
#chyper: reported (1), not reported (2)
finaldf$phyper <- ifelse(finaldf$phyper==1, 1, 0)
#phyper: reported(1), not reported (2)
finaldf$pre4000 <- ifelse(finaldf$pre4000==1, 1, 0)
#pre4000: previous infant 4000+ grams, reported (1), not reported (2)
finaldf$preterm <- ifelse(finaldf$preterm==1, 1, 0)
#preterm: reported (1), not reported (2)
finaldf$tobacco <- ifelse(finaldf$tobacco==1, 1, 0)
#tobacco: yes(1), no (2)
finaldf$cigar6 <- factor(finaldf$cigar6, levels=letters, labels=letters)
#cigar6: avg # of cigs per day
finaldf$alcohol <- ifelse(finaldf$alcohol==1,1,0)
#alcohol: use during pregnancy, Y (1), N (0)
finaldf$drink5 <- factor(finaldf$drink5, levels=0:4, labels=0:4)
#drink5: avg # of drinks per week
#wgain: weight gain
summary(finaldf)

a <- subset(finaldf, select=c(pldel3,nlbnl))
b <- subset(finaldf, select=c(drink5,monpre,alcohol))
c <- subset(finaldf, select=c(wgain,adequacy,dgestat,clingest,delmeth5))
d <- subset(finaldf,select=c(county.size,dmage,ormoth,mrace3,dmeduc,dmar,nprevist,dfage,orfath,dfeduc))
D1 <- finaldf$tobacco
D2 <- finaldf$cigar6
e <- subset(finaldf, select=c(do, md, cnm, midwife,dlivord,dtotord,totord9,birmon,weekday,csex))
y1 <- finaldf$dbrwt
y2 <- finaldf$omaps
y3 <- finaldf$fmaps
y4 <- finaldf$anemia
y5 <- finaldf$cardiac
y6 <- finaldf$lung
y7 <- finaldf$diabetes
y8 <- finaldf$herpes
y9 <- finaldf$chyper
y10 <- finaldf$phyper

gg_miss_which(df)
missing<- subset(df,select=c(wgain,drink5,drink,alcohol,cigar,cigar6,tobacco,herpes,dbrwt))
gg_miss_var(missing)
gg_miss_fct(missing,dbrwt)

q <- subset(finaldf, select=c(preterm,dplural,pre4000))
y1bin <- ifelse(y1<=2500,1,0)

#treat and control meaned differences
treat_y <- subset(finaldf, D1==1, select=c(dbrwt,omaps,fmaps,tobacco) )
ctrl_y <- subset(finaldf, D1==0, select=c(dbrwt,omaps,fmaps,tobacco))
trt_omaps <- mean(treat_y$omaps)
trt_fmaps <- mean(treat_y$fmaps)
trt_dbrwt <- mean(treat_y$dbrwt)
ctrl_omaps <- mean(ctrl_y$omaps)
ctrl_fmaps <- mean(ctrl_y$fmaps)
ctrl_dbrwt <- mean(ctrl_y$dbrwt)
dif_omaps <- ctrl_omaps-trt_omaps
dif_fmaps <- ctrl_fmaps-trt_fmaps
dif_dbrwt <- ctrl_dbrwt-trt_dbrwt

X<- finaldf %>%
  group_by(tobacco) %>%
  summarise_all(mean)
X<-as.matrix(X)
X
as.matrix(cbind(trt_omaps,ctrl_omaps,dif_omaps,trt_fmaps,ctrl_fmaps,dif_fmaps,trt_dbrwt,ctrl_dbrwt,dif_dbrwt))

#balance table
summary(subset(finaldf, tobacco==1))
summary(subset(finaldf, tobacco==0))

#predetermined vars
#rectype: 1(res), 2(non res)
#pldel3: place of delivery, hospital (1), not hospital (2)
#birattnd: MD (1), DO (2), Cert Nurse Midwife (3), Midwife (4), Other (5)
#cntocpop: pop of county, 1mil+ (0), 500K-1mil (1), 250K-500k (2), 100k-250k(3), less than 100k (9)
#stresfip: state of residence, foreign (0), state fips code (1-54)
#dmage: age of mother (12-49 years)
#ormoth: hispanic origin of mother, nonhisp (0), mex (1), PR (2), cuban (3), central or SA (4), other (5)
#mrace3: race of mother, white (1), other (2), black (3)
#dmeduc: mothers ed, none (0), 1-8 yrs of elementary (1-8), 1-4 yrs of HS (9-12), 1-4 yrs of col (13-16), 5+ col (17)
#dmar: mother marital status, married (1), unmarried (2)
#adequacy: adequacy of care, adq (1), int (2), inadq (3)
#nlbl: # of live births living, 0-29
#dlivord: live birth order
#dtotord: total birth order
#totord9: birth order (1st - 8th+)
#monpre: month in preg prenatal care began, none (0), 1st-9th (1-9)
#nprevist: number of prenatal visits
#disllb: interval since last live birth, 0 ind plural birth
#isllb10: interval since last live birth, no prev birth (0), plural birth (1), 1-11mos (2), 12-17mos (3), 18-23mos (4), 24-35mos (5), 36-47mos (6), 48-59mos (7), 60-71mos (8), 72+mos (9)
#dfage: father's age
#orfath: hispanic origin of father, nonhisp (0), mex (1), PR (2), cuban (3), C or SA (4), other/unknown hisp (5)
#dfeduc: fatherd ed, none (0), 1-8 yrs of elementary (1-8), 1-4 yrs of HS (9-12), 1-4 yrs of col (13-16), 5+ col (17)
#birmon: birth month, jan-dec (1-12)
#weekday: day of week of birth, sun-sat (1-7)
#dgestat: gestation in weeks
#csex: sex of child, M (1), F (2)
#dplural: single (1), twin (2), triplet (3), quadruplet (4)
#clingest: clinical est of gest
#delmeth5: method of delivery, vag (1), vag post csection (2), csection (3), repeat csection (4), not stated (5)
#pre4000: previous infant 4000+ grams, reported (1), not reported (2)
#preterm: reported (1), not reported (2)
#cigar6: avg # of cigs per day
#alcohol: use during pregnancy, Y (1), N (0)
#drink5: avg # of drinks per week
#wgain: weight gain

pre <- cbind(b,d,q,y1,D1)
summary(pre)

#basic linear regression
reg0 <- lm(y1 ~ D1+drink5+monpre+alcohol+county.size+dmage+ormoth+mrace3+dmeduc+dmar+nprevist+dfage+orfath+dfeduc+preterm+dplural+pre4000, data=pre)
summary(reg0)

#propensity score
px <- glm(D1 ~ drink5+county.size+dmage+ormoth+mrace3+dmeduc+dmar+nprevist+dfage+orfath+dfeduc+preterm+dplural+pre4000, data=pre, family=binomial(link="logit"))
summary(px)
px_sig <- glm(D1 ~ drink5+monpre+alcohol+county.size+dmage+ormoth+mrace3+dmar+nprevist+dfage+orfath+dfeduc+preterm+pre4000, data=pre, family=binomial(link="logit"))
pre$px_val <- predict.glm(px_sig, type="response")
summary(px_sig)

#including a propensity score as a covariate
reg1 <- lm(y1 ~ D1+ px_val, data=pre)
summary(reg1)

#weighting with a propensity score
reg2 <- lm(y1 ~ D1, data=pre, weights=px_val)
summary(reg2)

#blocking on the propensity score
prop_treat <- subset(pre, pre$D1==1)
prop_ctrl <- subset(pre, pre$D1==0)

px0<-subset(pre,px_val<=(0))
px1<-subset(pre,px_val<=(.01)&px_val>(0))
px2<-subset(pre,px_val<=(.02) & px_val>(.01))
px3<-subset(pre,px_val<=(.03)& px_val>(.02))
px4<-subset(pre,px_val<=(.04)& px_val>(.03))
px5<-subset(pre,px_val<=(.05)& px_val>(.04))
px6<-subset(pre,px_val<=(.06)& px_val>(.05))
px7<-subset(pre,px_val<=(.07)& px_val>(.06))
px8<-subset(pre,px_val<=(.08)& px_val>(.07))
px9<-subset(pre,px_val<=(.09)& px_val>(.08))
px10<-subset(pre,px_val<=(.10)& px_val>(.09))
px11<-subset(pre,px_val<=(.11)& px_val>(.1))
px12<-subset(pre,px_val<=(.12)& px_val>(.11))
px13<-subset(pre,px_val<=(.13)& px_val>(.12))
px14<-subset(pre,px_val<=(.14)& px_val>(.13))
px15<-subset(pre,px_val<=(.15)& px_val>(.14))
px16<-subset(pre,px_val<=(.16)& px_val>(.15))
px17<-subset(pre,px_val<=(.17)& px_val>(.16))
px18<-subset(pre,px_val<=(.18)& px_val>(.17))
px19<-subset(pre,px_val<=(.19)& px_val>(.18))
px20<-subset(pre,px_val<=(.20)& px_val>(.19))
px21<-subset(pre,px_val<=(.21)& px_val>(.2))
px22<-subset(pre,px_val<=(.22)& px_val>(.21))
px23<-subset(pre,px_val<=(.23)& px_val>(.22))
px24<-subset(pre,px_val<=(.24)& px_val>(.23))
px25<-subset(pre,px_val<=(.25)& px_val>(.24))
px26<-subset(pre,px_val<=(.26)& px_val>(.25))
px27<-subset(pre,px_val<=(.27)& px_val>(.26))
px28<-subset(pre,px_val<=(.28)& px_val>(.27))
px29<-subset(pre,px_val<=(.29)& px_val>(.28))
px30<-subset(pre,px_val<=(.30)& px_val>(.29))
px31<-subset(pre,px_val<=(.31)& px_val>(.3))
px32<-subset(pre,px_val<=(.32)& px_val>(.31))
px33<-subset(pre,px_val<=(.33)& px_val>(.32))
px34<-subset(pre,px_val<=(.34)& px_val>(.33))
px35<-subset(pre,px_val<=(.35)& px_val>(.34))
px36<-subset(pre,px_val<=(.36)& px_val>(.35))
px37<-subset(pre,px_val<=(.37)& px_val>(.36))
px38<-subset(pre,px_val<=(.38)& px_val>(.37))
px39<-subset(pre,px_val<=(.39)& px_val>(.38))
px40<-subset(pre,px_val<=(.40)& px_val>(.39))
px41<-subset(pre,px_val<=(.41)& px_val>(.40))
px42<-subset(pre,px_val<=(.42)& px_val>(.41))
px43<-subset(pre,px_val<=(.43)& px_val>(.42))
px44<-subset(pre,px_val<=(.44)& px_val>(.43))
px45<-subset(pre,px_val<=(.45)& px_val>(.44))
px46<-subset(pre,px_val<=(.46)& px_val>(.45))
px47<-subset(pre,px_val<=(.47)& px_val>(.46))
px48<-subset(pre,px_val<=(.48)& px_val>(.47))
px49<-subset(pre,px_val<=(.49)& px_val>(.48))
px50<-subset(pre,px_val<=(.50)& px_val>(.49))
px51<-subset(pre,px_val<=(.51)& px_val>(.5))
px52<-subset(pre,px_val<=(.52)& px_val>(.51))
px53<-subset(pre,px_val<=(.53)& px_val>(.52))
px54<-subset(pre,px_val<=(.54)& px_val>(.53))
px55<-subset(pre,px_val<=(.55)& px_val>(.54))
px56<-subset(pre,px_val<=(.56)& px_val>(.55))
px57<-subset(pre,px_val<=(.57)& px_val>(.56))
px58<-subset(pre,px_val<=(.58)& px_val>(.57))
px59<-subset(pre,px_val<=(.59)& px_val>(.58))
px60<-subset(pre,px_val<=(.60)& px_val>(.59))
px61<-subset(pre,px_val<=(.61)& px_val>(.6))
px62<-subset(pre,px_val<=(.62)& px_val>(.61))
px63<-subset(pre,px_val<=(.63)& px_val>(.62))
px64<-subset(pre,px_val<=(.64)& px_val>(.63))
px65<-subset(pre,px_val<=(.65)& px_val>(.64))
px66<-subset(pre,px_val<=(.66)& px_val>(.65))
px67<-subset(pre,px_val<=(.67)& px_val>(.66))
px68<-subset(pre,px_val<=(.68)& px_val>(.67))
px69<-subset(pre,px_val<=(.69)& px_val>(.68))
px70<-subset(pre,px_val<=(.70)& px_val>(.69))
px71<-subset(pre,px_val<=(.71)& px_val>(.7))
px72<-subset(pre,px_val<=(.72)& px_val>(.71))
px73<-subset(pre,px_val<=(.73)& px_val>(.72))
px74<-subset(pre,px_val<=(.74)& px_val>(.73))
px75<-subset(pre,px_val<=(.75)& px_val>(.74))
px76<-subset(pre,px_val<=(.76)& px_val>(.75))
px77<-subset(pre,px_val<=(.77)& px_val>(.76))
px78<-subset(pre,px_val<=(.78)& px_val>(.77))
px79<-subset(pre,px_val<=(.79)& px_val>(.78))
px80<-subset(pre,px_val<=(.80)& px_val>(.79))
px81<-subset(pre,px_val<=(.81)& px_val>(.80))
px82<-subset(pre,px_val<=(.82)& px_val>(.81))
px83<-subset(pre,px_val<=(.83)& px_val>(.82))
px84<-subset(pre,px_val<=(.84)& px_val>(.83))
px85<-subset(pre,px_val<=(.85)& px_val>(.84))
px86<-subset(pre,px_val<=(.86)& px_val>(.85))
px87<-subset(pre,px_val<=(.87)& px_val>(.86))
px88<-subset(pre,px_val<=(.88)& px_val>(.87))
px89<-subset(pre,px_val<=(.89)& px_val>(.88))
px90<-subset(pre,px_val<=(.90)& px_val>(.89))
px91<-subset(pre,px_val<=(.91)& px_val>(.9))
px92<-subset(pre,px_val<=(.92)& px_val>(.91))
px93<-subset(pre,px_val<=(.93)& px_val>(.92))
px94<-subset(pre,px_val<=(.94)& px_val>(.93))
px95<-subset(pre,px_val<=(.95)& px_val>(.94))
px96<-subset(pre,px_val<=(.96)& px_val>(.95))
px97<-subset(pre,px_val<=(.97)& px_val>(.96))
px98<-subset(pre,px_val<=(.98)& px_val>(.97))
px99<-subset(pre,px_val<=(.99)& px_val>(.98))
px100<-subset(pre,px_val<=(1)& px_val>(.99))
px101<-subset(pre,px_val<=(1.1)& px_val>(1))

rm(px0,px99,px98,px97,px96,px100,px101)

p1<-lm(y1~D1,data = px1)
p1e<-p1[["coefficients"]][["D1"]]
p2<-lm(y1~D1,data = px2)
p2e<-p2[["coefficients"]][["D1"]]
p3<-lm(y1~D1,data = px3)
p3e<-p3[["coefficients"]][["D1"]]
p4<-lm(y1~D1,data = px4)
p4e<-p4[["coefficients"]][["D1"]]
p5<-lm(y1~D1,data = px5)
p5e<-p5[["coefficients"]][["D1"]]
p6<-lm(y1~D1,data = px6)
p6e<-p6[["coefficients"]][["D1"]]
p7<-lm(y1~D1,data = px7)
p7e<-p7[["coefficients"]][["D1"]]
p8<-lm(y1~D1,data = px8)
p8e<-p8[["coefficients"]][["D1"]]
p9<-lm(y1~D1,data = px9)
p9e<-p9[["coefficients"]][["D1"]]

p10<-lm(y1~D1,data = px10)
p10e<-p10[["coefficients"]][["D1"]]
p11<-lm(y1~D1,data = px11)
p11e<-p11[["coefficients"]][["D1"]]
p12<-lm(y1~D1,data = px12)
p12e<-p12[["coefficients"]][["D1"]]
p13<-lm(y1~D1,data = px13)
p13e<-p13[["coefficients"]][["D1"]]
p14<-lm(y1~D1,data = px14)
p14e<-p14[["coefficients"]][["D1"]]
p15<-lm(y1~D1,data = px15)
p15e<-p15[["coefficients"]][["D1"]]
p16<-lm(y1~D1,data = px16)
p16e<-p16[["coefficients"]][["D1"]]
p17<-lm(y1~D1,data = px17)
p17e<-p17[["coefficients"]][["D1"]]
p18<-lm(y1~D1,data = px18)
p18e<-p18[["coefficients"]][["D1"]]
p19<-lm(y1~D1,data = px19)
p19e<-p19[["coefficients"]][["D1"]]

p20<-lm(y1~D1,data = px20)
p20e<-p20[["coefficients"]][["D1"]]
p21<-lm(y1~D1,data = px21)
p21e<-p21[["coefficients"]][["D1"]]
p22<-lm(y1~D1,data = px22)
p22e<-p22[["coefficients"]][["D1"]]
p23<-lm(y1~D1,data = px23)
p23e<-p23[["coefficients"]][["D1"]]
p24<-lm(y1~D1,data = px24)
p24e<-p24[["coefficients"]][["D1"]]
p25<-lm(y1~D1,data = px25)
p25e<-p25[["coefficients"]][["D1"]]
p26<-lm(y1~D1,data = px26)
p26e<-p26[["coefficients"]][["D1"]]
p27<-lm(y1~D1,data = px27)
p27e<-p27[["coefficients"]][["D1"]]
p28<-lm(y1~D1,data = px28)
p28e<-p28[["coefficients"]][["D1"]]
p29<-lm(y1~D1,data = px29)
p29e<-p29[["coefficients"]][["D1"]]

p30<-lm(y1~D1,data = px30)
p30e<-p30[["coefficients"]][["D1"]]
p31<-lm(y1~D1,data = px31)
p31e<-p31[["coefficients"]][["D1"]]
p32<-lm(y1~D1,data = px32)
p32e<-p32[["coefficients"]][["D1"]]
p33<-lm(y1~D1,data = px33)
p33e<-p33[["coefficients"]][["D1"]]
p34<-lm(y1~D1,data = px34)
p34e<-p34[["coefficients"]][["D1"]]
p35<-lm(y1~D1,data = px35)
p35e<-p35[["coefficients"]][["D1"]]
p36<-lm(y1~D1,data = px36)
p36e<-p36[["coefficients"]][["D1"]]
p37<-lm(y1~D1,data = px37)
p37e<-p37[["coefficients"]][["D1"]]
p38<-lm(y1~D1,data = px38)
p38e<-p38[["coefficients"]][["D1"]]
p39<-lm(y1~D1,data = px39)
p39e<-p39[["coefficients"]][["D1"]]

p40<-lm(y1~D1,data = px40)
p40e<-p40[["coefficients"]][["D1"]]
p41<-lm(y1~D1,data = px41)
p41e<-p41[["coefficients"]][["D1"]]
p42<-lm(y1~D1,data = px42)
p42e<-p42[["coefficients"]][["D1"]]
p43<-lm(y1~D1,data = px43)
p43e<-p43[["coefficients"]][["D1"]]
p44<-lm(y1~D1,data = px44)
p44e<-p44[["coefficients"]][["D1"]]
p45<-lm(y1~D1,data = px45)
p45e<-p45[["coefficients"]][["D1"]]
p46<-lm(y1~D1,data = px46)
p46e<-p46[["coefficients"]][["D1"]]
p47<-lm(y1~D1,data = px47)
p47e<-p47[["coefficients"]][["D1"]]
p48<-lm(y1~D1,data = px48)
p48e<-p48[["coefficients"]][["D1"]]
p49<-lm(y1~D1,data = px49)
p49e<-p49[["coefficients"]][["D1"]]

p50<-lm(y1~D1,data = px50)
p50e<-p50[["coefficients"]][["D1"]]
p51<-lm(y1~D1,data = px51)
p51e<-p51[["coefficients"]][["D1"]]
p52<-lm(y1~D1,data = px52)
p52e<-p52[["coefficients"]][["D1"]]
p53<-lm(y1~D1,data = px53)
p53e<-p53[["coefficients"]][["D1"]]
p54<-lm(y1~D1,data = px54)
p54e<-p54[["coefficients"]][["D1"]]
p55<-lm(y1~D1,data = px55)
p55e<-p55[["coefficients"]][["D1"]]
p56<-lm(y1~D1,data = px56)
p56e<-p56[["coefficients"]][["D1"]]
p57<-lm(y1~D1,data = px57)
p57e<-p57[["coefficients"]][["D1"]]
p58<-lm(y1~D1,data = px58)
p58e<-p58[["coefficients"]][["D1"]]
p59<-lm(y1~D1,data = px59)
p59e<-p59[["coefficients"]][["D1"]]

p60<-lm(y1~D1,data = px60)
p60e<-p60[["coefficients"]][["D1"]]
p61<-lm(y1~D1,data = px61)
p61e<-p61[["coefficients"]][["D1"]]
p62<-lm(y1~D1,data = px62)
p62e<-p62[["coefficients"]][["D1"]]
p63<-lm(y1~D1,data = px63)
p63e<-p63[["coefficients"]][["D1"]]
p64<-lm(y1~D1,data = px64)
p64e<-p64[["coefficients"]][["D1"]]
p65<-lm(y1~D1,data = px65)
p65e<-p65[["coefficients"]][["D1"]]
p66<-lm(y1~D1,data = px66)
p66e<-p66[["coefficients"]][["D1"]]
p67<-lm(y1~D1,data = px67)
p67e<-p67[["coefficients"]][["D1"]]
p68<-lm(y1~D1,data = px68)
p68e<-p68[["coefficients"]][["D1"]]
p69<-lm(y1~D1,data = px69)
p69e<-p69[["coefficients"]][["D1"]]

p70<-lm(y1~D1,data = px70)
p70e<-p70[["coefficients"]][["D1"]]
p71<-lm(y1~D1,data = px71)
p71e<-p71[["coefficients"]][["D1"]]
p72<-lm(y1~D1,data = px72)
p72e<-p72[["coefficients"]][["D1"]]
p73<-lm(y1~D1,data = px73)
p73e<-p73[["coefficients"]][["D1"]]
p74<-lm(y1~D1,data = px74)
p74e<-p74[["coefficients"]][["D1"]]
p75<-lm(y1~D1,data = px75)
p75e<-p75[["coefficients"]][["D1"]]
p76<-lm(y1~D1,data = px76)
p76e<-p76[["coefficients"]][["D1"]]
p77<-lm(y1~D1,data = px77)
p77e<-p77[["coefficients"]][["D1"]]
p78<-lm(y1~D1,data = px78)
p78e<-p78[["coefficients"]][["D1"]]
p79<-lm(y1~D1,data = px79)
p79e<-p79[["coefficients"]][["D1"]]

p80<-lm(y1~D1,data = px80)
p80e<-p80[["coefficients"]][["D1"]]
p81<-lm(y1~D1,data = px81)
p81e<-p81[["coefficients"]][["D1"]]
p82<-lm(y1~D1,data = px82)
p82e<-p82[["coefficients"]][["D1"]]
p83<-lm(y1~D1,data = px83)
p83e<-p83[["coefficients"]][["D1"]]
p84<-lm(y1~D1,data = px84)
p84e<-p84[["coefficients"]][["D1"]]
p85<-lm(y1~D1,data = px85)
p85e<-p85[["coefficients"]][["D1"]]
p86<-lm(y1~D1,data = px86)
p86e<-p86[["coefficients"]][["D1"]]
p87<-lm(y1~D1,data = px87)
p87e<-p87[["coefficients"]][["D1"]]
p88<-lm(y1~D1,data = px88)
p88e<-p88[["coefficients"]][["D1"]]
p89<-lm(y1~D1,data = px89)
p89e<-p89[["coefficients"]][["D1"]]

p90<-lm(y1~D1,data = px90)
p90e<-p90[["coefficients"]][["D1"]]
p91<-lm(y1~D1,data = px91)
p91e<-p91[["coefficients"]][["D1"]]
p92<-lm(y1~D1,data = px92)
p92e<-p92[["coefficients"]][["D1"]]
p93<-lm(y1~D1,data = px93)
p93e<-p93[["coefficients"]][["D1"]]
p94<-lm(y1~D1,data = px94)
p94e<-p94[["coefficients"]][["D1"]]
p95<-lm(y1~D1,data = px95)
p95e<-p95[["coefficients"]][["D1"]]

p<-rbind(p1e,p2e,p3e,p4e,p5e,p6e,p7e,p8e,p9e,p10e,p11e,p12e,p13e,p14e,p15e,p16e,p17e,p18e,p19e,p20e,p21e,p22e,p23e,p24e,p25e,p26e,p27e,p28e,p29e,p30e,p31e,p32e,p33e,p34e,p35e,p36e,p37e,p38e,p39e,p40e,p41e,p42e,p43e,p44e,p45e,p46e,p47e,p48e,p49e,p50e,p51e,p52e,p53e,p54e,p55e,p56e,p57e,p58e,p59e,p60e,p61e,p62e,p63e,p64e,p65e,p66e,p67e,p68e,p69e,p70e,p71e,p72e,p73e,p74e,p75e,p76e,p77e,p78e,p79e,p80e,p81e,p82e,p83e,p84e,p85e,p86e,p87e,p88e,p89e,p90e,p91e,p92e,p93e,p94e,p95e)
n<-rbind(nrow(px1),nrow(px2),nrow(px3),nrow(px4),nrow(px5),nrow(px6),nrow(px7),nrow(px8),nrow(px9),nrow(px10),nrow(px11),nrow(px12),nrow(px13),nrow(px14),nrow(px15),nrow(px16),nrow(px17),nrow(px18),nrow(px19),nrow(px20),nrow(px21),nrow(px22),nrow(px23),nrow(px24),nrow(px25),nrow(px26),nrow(px27),nrow(px28),nrow(px29),nrow(px30),nrow(px31),nrow(px32),nrow(px33),nrow(px34),nrow(px35),nrow(px36),nrow(px37),nrow(px38),nrow(px39),nrow(px40),nrow(px41),nrow(px42),nrow(px43),nrow(px44),nrow(px45),nrow(px46),nrow(px47),nrow(px48),nrow(px49),nrow(px50),nrow(px51),nrow(px52),nrow(px53),nrow(px54),nrow(px55),nrow(px56),nrow(px57),nrow(px58),nrow(px59),nrow(px60),nrow(px61),nrow(px62),nrow(px63),nrow(px64),nrow(px65),nrow(px66),nrow(px67),nrow(px68),nrow(px69),nrow(px70),nrow(px71),nrow(px72),nrow(px73),nrow(px74),nrow(px75),nrow(px76),nrow(px77),nrow(px78),nrow(px79),nrow(px80),nrow(px81),nrow(px82),nrow(px83),nrow(px84),nrow(px85),nrow(px86),nrow(px87),nrow(px88),nrow(px89),nrow(px90),nrow(px91),nrow(px92),nrow(px93),nrow(px94),nrow(px95))

wtot<-sum(n)
w<-n/wtot
w<-as.numeric(w)

mat <-cbind(p,n,w)

pe <- weighted.mean(p,w,na.rm=TRUE)

#low birth weight indicator
pre$y2 <- ifelse(pre$y1 <= 2500, 1, 0)

px0<-subset(pre,px_val<=(0))
px1<-subset(pre,px_val<=(.01)&px_val>(0))
px2<-subset(pre,px_val<=(.02) & px_val>(.01))
px3<-subset(pre,px_val<=(.03)& px_val>(.02))
px4<-subset(pre,px_val<=(.04)& px_val>(.03))
px5<-subset(pre,px_val<=(.05)& px_val>(.04))
px6<-subset(pre,px_val<=(.06)& px_val>(.05))
px7<-subset(pre,px_val<=(.07)& px_val>(.06))
px8<-subset(pre,px_val<=(.08)& px_val>(.07))
px9<-subset(pre,px_val<=(.09)& px_val>(.08))
px10<-subset(pre,px_val<=(.10)& px_val>(.09))
px11<-subset(pre,px_val<=(.11)& px_val>(.1))
px12<-subset(pre,px_val<=(.12)& px_val>(.11))
px13<-subset(pre,px_val<=(.13)& px_val>(.12))
px14<-subset(pre,px_val<=(.14)& px_val>(.13))
px15<-subset(pre,px_val<=(.15)& px_val>(.14))
px16<-subset(pre,px_val<=(.16)& px_val>(.15))
px17<-subset(pre,px_val<=(.17)& px_val>(.16))
px18<-subset(pre,px_val<=(.18)& px_val>(.17))
px19<-subset(pre,px_val<=(.19)& px_val>(.18))
px20<-subset(pre,px_val<=(.20)& px_val>(.19))
px21<-subset(pre,px_val<=(.21)& px_val>(.2))
px22<-subset(pre,px_val<=(.22)& px_val>(.21))
px23<-subset(pre,px_val<=(.23)& px_val>(.22))
px24<-subset(pre,px_val<=(.24)& px_val>(.23))
px25<-subset(pre,px_val<=(.25)& px_val>(.24))
px26<-subset(pre,px_val<=(.26)& px_val>(.25))
px27<-subset(pre,px_val<=(.27)& px_val>(.26))
px28<-subset(pre,px_val<=(.28)& px_val>(.27))
px29<-subset(pre,px_val<=(.29)& px_val>(.28))
px30<-subset(pre,px_val<=(.30)& px_val>(.29))
px31<-subset(pre,px_val<=(.31)& px_val>(.3))
px32<-subset(pre,px_val<=(.32)& px_val>(.31))
px33<-subset(pre,px_val<=(.33)& px_val>(.32))
px34<-subset(pre,px_val<=(.34)& px_val>(.33))
px35<-subset(pre,px_val<=(.35)& px_val>(.34))
px36<-subset(pre,px_val<=(.36)& px_val>(.35))
px37<-subset(pre,px_val<=(.37)& px_val>(.36))
px38<-subset(pre,px_val<=(.38)& px_val>(.37))
px39<-subset(pre,px_val<=(.39)& px_val>(.38))
px40<-subset(pre,px_val<=(.40)& px_val>(.39))
px41<-subset(pre,px_val<=(.41)& px_val>(.40))
px42<-subset(pre,px_val<=(.42)& px_val>(.41))
px43<-subset(pre,px_val<=(.43)& px_val>(.42))
px44<-subset(pre,px_val<=(.44)& px_val>(.43))
px45<-subset(pre,px_val<=(.45)& px_val>(.44))
px46<-subset(pre,px_val<=(.46)& px_val>(.45))
px47<-subset(pre,px_val<=(.47)& px_val>(.46))
px48<-subset(pre,px_val<=(.48)& px_val>(.47))
px49<-subset(pre,px_val<=(.49)& px_val>(.48))
px50<-subset(pre,px_val<=(.50)& px_val>(.49))
px51<-subset(pre,px_val<=(.51)& px_val>(.5))
px52<-subset(pre,px_val<=(.52)& px_val>(.51))
px53<-subset(pre,px_val<=(.53)& px_val>(.52))
px54<-subset(pre,px_val<=(.54)& px_val>(.53))
px55<-subset(pre,px_val<=(.55)& px_val>(.54))
px56<-subset(pre,px_val<=(.56)& px_val>(.55))
px57<-subset(pre,px_val<=(.57)& px_val>(.56))
px58<-subset(pre,px_val<=(.58)& px_val>(.57))
px59<-subset(pre,px_val<=(.59)& px_val>(.58))
px60<-subset(pre,px_val<=(.60)& px_val>(.59))
px61<-subset(pre,px_val<=(.61)& px_val>(.6))
px62<-subset(pre,px_val<=(.62)& px_val>(.61))
px63<-subset(pre,px_val<=(.63)& px_val>(.62))
px64<-subset(pre,px_val<=(.64)& px_val>(.63))
px65<-subset(pre,px_val<=(.65)& px_val>(.64))
px66<-subset(pre,px_val<=(.66)& px_val>(.65))
px67<-subset(pre,px_val<=(.67)& px_val>(.66))
px68<-subset(pre,px_val<=(.68)& px_val>(.67))
px69<-subset(pre,px_val<=(.69)& px_val>(.68))
px70<-subset(pre,px_val<=(.70)& px_val>(.69))
px71<-subset(pre,px_val<=(.71)& px_val>(.7))
px72<-subset(pre,px_val<=(.72)& px_val>(.71))
px73<-subset(pre,px_val<=(.73)& px_val>(.72))
px74<-subset(pre,px_val<=(.74)& px_val>(.73))
px75<-subset(pre,px_val<=(.75)& px_val>(.74))
px76<-subset(pre,px_val<=(.76)& px_val>(.75))
px77<-subset(pre,px_val<=(.77)& px_val>(.76))
px78<-subset(pre,px_val<=(.78)& px_val>(.77))
px79<-subset(pre,px_val<=(.79)& px_val>(.78))
px80<-subset(pre,px_val<=(.80)& px_val>(.79))
px81<-subset(pre,px_val<=(.81)& px_val>(.80))
px82<-subset(pre,px_val<=(.82)& px_val>(.81))
px83<-subset(pre,px_val<=(.83)& px_val>(.82))
px84<-subset(pre,px_val<=(.84)& px_val>(.83))
px85<-subset(pre,px_val<=(.85)& px_val>(.84))
px86<-subset(pre,px_val<=(.86)& px_val>(.85))
px87<-subset(pre,px_val<=(.87)& px_val>(.86))
px88<-subset(pre,px_val<=(.88)& px_val>(.87))
px89<-subset(pre,px_val<=(.89)& px_val>(.88))
px90<-subset(pre,px_val<=(.90)& px_val>(.89))
px91<-subset(pre,px_val<=(.91)& px_val>(.9))
px92<-subset(pre,px_val<=(.92)& px_val>(.91))
px93<-subset(pre,px_val<=(.93)& px_val>(.92))
px94<-subset(pre,px_val<=(.94)& px_val>(.93))
px95<-subset(pre,px_val<=(.95)& px_val>(.94))
px96<-subset(pre,px_val<=(.96)& px_val>(.95))
px97<-subset(pre,px_val<=(.97)& px_val>(.96))
px98<-subset(pre,px_val<=(.98)& px_val>(.97))
px99<-subset(pre,px_val<=(.99)& px_val>(.98))
px100<-subset(pre,px_val<=(1)& px_val>(.99))
px101<-subset(pre,px_val<=(1.1)& px_val>(1))

rm(px0,px99,px98,px97,px96,px100,px101)

p1<-lm(y2~D1,data = px1)
p1e<-p1[["coefficients"]][["D1"]]
p2<-lm(y2~D1,data = px2)
p2e<-p2[["coefficients"]][["D1"]]
p3<-lm(y2~D1,data = px3)
p3e<-p3[["coefficients"]][["D1"]]
p4<-lm(y2~D1,data = px4)
p4e<-p4[["coefficients"]][["D1"]]
p5<-lm(y2~D1,data = px5)
p5e<-p5[["coefficients"]][["D1"]]
p6<-lm(y2~D1,data = px6)
p6e<-p6[["coefficients"]][["D1"]]
p7<-lm(y2~D1,data = px7)
p7e<-p7[["coefficients"]][["D1"]]
p8<-lm(y2~D1,data = px8)
p8e<-p8[["coefficients"]][["D1"]]
p9<-lm(y2~D1,data = px9)
p9e<-p9[["coefficients"]][["D1"]]

p10<-lm(y2~D1,data = px10)
p10e<-p10[["coefficients"]][["D1"]]
p11<-lm(y2~D1,data = px11)
p11e<-p11[["coefficients"]][["D1"]]
p12<-lm(y2~D1,data = px12)
p12e<-p12[["coefficients"]][["D1"]]
p13<-lm(y2~D1,data = px13)
p13e<-p13[["coefficients"]][["D1"]]
p14<-lm(y2~D1,data = px14)
p14e<-p14[["coefficients"]][["D1"]]
p15<-lm(y2~D1,data = px15)
p15e<-p15[["coefficients"]][["D1"]]
p16<-lm(y2~D1,data = px16)
p16e<-p16[["coefficients"]][["D1"]]
p17<-lm(y2~D1,data = px17)
p17e<-p17[["coefficients"]][["D1"]]
p18<-lm(y2~D1,data = px18)
p18e<-p18[["coefficients"]][["D1"]]
p19<-lm(y2~D1,data = px19)
p19e<-p19[["coefficients"]][["D1"]]

p20<-lm(y2~D1,data = px20)
p20e<-p20[["coefficients"]][["D1"]]
p21<-lm(y2~D1,data = px21)
p21e<-p21[["coefficients"]][["D1"]]
p22<-lm(y2~D1,data = px22)
p22e<-p22[["coefficients"]][["D1"]]
p23<-lm(y2~D1,data = px23)
p23e<-p23[["coefficients"]][["D1"]]
p24<-lm(y2~D1,data = px24)
p24e<-p24[["coefficients"]][["D1"]]
p25<-lm(y2~D1,data = px25)
p25e<-p25[["coefficients"]][["D1"]]
p26<-lm(y2~D1,data = px26)
p26e<-p26[["coefficients"]][["D1"]]
p27<-lm(y2~D1,data = px27)
p27e<-p27[["coefficients"]][["D1"]]
p28<-lm(y2~D1,data = px28)
p28e<-p28[["coefficients"]][["D1"]]
p29<-lm(y2~D1,data = px29)
p29e<-p29[["coefficients"]][["D1"]]

p30<-lm(y2~D1,data = px30)
p30e<-p30[["coefficients"]][["D1"]]
p31<-lm(y2~D1,data = px31)
p31e<-p31[["coefficients"]][["D1"]]
p32<-lm(y2~D1,data = px32)
p32e<-p32[["coefficients"]][["D1"]]
p33<-lm(y2~D1,data = px33)
p33e<-p33[["coefficients"]][["D1"]]
p34<-lm(y2~D1,data = px34)
p34e<-p34[["coefficients"]][["D1"]]
p35<-lm(y2~D1,data = px35)
p35e<-p35[["coefficients"]][["D1"]]
p36<-lm(y2~D1,data = px36)
p36e<-p36[["coefficients"]][["D1"]]
p37<-lm(y2~D1,data = px37)
p37e<-p37[["coefficients"]][["D1"]]
p38<-lm(y2~D1,data = px38)
p38e<-p38[["coefficients"]][["D1"]]
p39<-lm(y2~D1,data = px39)
p39e<-p39[["coefficients"]][["D1"]]

p40<-lm(y2~D1,data = px40)
p40e<-p40[["coefficients"]][["D1"]]
p41<-lm(y2~D1,data = px41)
p41e<-p41[["coefficients"]][["D1"]]
p42<-lm(y2~D1,data = px42)
p42e<-p42[["coefficients"]][["D1"]]
p43<-lm(y2~D1,data = px43)
p43e<-p43[["coefficients"]][["D1"]]
p44<-lm(y2~D1,data = px44)
p44e<-p44[["coefficients"]][["D1"]]
p45<-lm(y2~D1,data = px45)
p45e<-p45[["coefficients"]][["D1"]]
p46<-lm(y2~D1,data = px46)
p46e<-p46[["coefficients"]][["D1"]]
p47<-lm(y2~D1,data = px47)
p47e<-p47[["coefficients"]][["D1"]]
p48<-lm(y2~D1,data = px48)
p48e<-p48[["coefficients"]][["D1"]]
p49<-lm(y2~D1,data = px49)
p49e<-p49[["coefficients"]][["D1"]]

p50<-lm(y2~D1,data = px50)
p50e<-p50[["coefficients"]][["D1"]]
p51<-lm(y2~D1,data = px51)
p51e<-p51[["coefficients"]][["D1"]]
p52<-lm(y2~D1,data = px52)
p52e<-p52[["coefficients"]][["D1"]]
p53<-lm(y2~D1,data = px53)
p53e<-p53[["coefficients"]][["D1"]]
p54<-lm(y2~D1,data = px54)
p54e<-p54[["coefficients"]][["D1"]]
p55<-lm(y2~D1,data = px55)
p55e<-p55[["coefficients"]][["D1"]]
p56<-lm(y2~D1,data = px56)
p56e<-p56[["coefficients"]][["D1"]]
p57<-lm(y2~D1,data = px57)
p57e<-p57[["coefficients"]][["D1"]]
p58<-lm(y2~D1,data = px58)
p58e<-p58[["coefficients"]][["D1"]]
p59<-lm(y2~D1,data = px59)
p59e<-p59[["coefficients"]][["D1"]]

p60<-lm(y2~D1,data = px60)
p60e<-p60[["coefficients"]][["D1"]]
p61<-lm(y2~D1,data = px61)
p61e<-p61[["coefficients"]][["D1"]]
p62<-lm(y2~D1,data = px62)
p62e<-p62[["coefficients"]][["D1"]]
p63<-lm(y2~D1,data = px63)
p63e<-p63[["coefficients"]][["D1"]]
p64<-lm(y2~D1,data = px64)
p64e<-p64[["coefficients"]][["D1"]]
p65<-lm(y2~D1,data = px65)
p65e<-p65[["coefficients"]][["D1"]]
p66<-lm(y2~D1,data = px66)
p66e<-p66[["coefficients"]][["D1"]]
p67<-lm(y2~D1,data = px67)
p67e<-p67[["coefficients"]][["D1"]]
p68<-lm(y2~D1,data = px68)
p68e<-p68[["coefficients"]][["D1"]]
p69<-lm(y2~D1,data = px69)
p69e<-p69[["coefficients"]][["D1"]]

p70<-lm(y2~D1,data = px70)
p70e<-p70[["coefficients"]][["D1"]]
p71<-lm(y2~D1,data = px71)
p71e<-p71[["coefficients"]][["D1"]]
p72<-lm(y2~D1,data = px72)
p72e<-p72[["coefficients"]][["D1"]]
p73<-lm(y2~D1,data = px73)
p73e<-p73[["coefficients"]][["D1"]]
p74<-lm(y2~D1,data = px74)
p74e<-p74[["coefficients"]][["D1"]]
p75<-lm(y2~D1,data = px75)
p75e<-p75[["coefficients"]][["D1"]]
p76<-lm(y2~D1,data = px76)
p76e<-p76[["coefficients"]][["D1"]]
p77<-lm(y2~D1,data = px77)
p77e<-p77[["coefficients"]][["D1"]]
p78<-lm(y2~D1,data = px78)
p78e<-p78[["coefficients"]][["D1"]]
p79<-lm(y2~D1,data = px79)
p79e<-p79[["coefficients"]][["D1"]]

p80<-lm(y2~D1,data = px80)
p80e<-p80[["coefficients"]][["D1"]]
p81<-lm(y2~D1,data = px81)
p81e<-p81[["coefficients"]][["D1"]]
p82<-lm(y2~D1,data = px82)
p82e<-p82[["coefficients"]][["D1"]]
p83<-lm(y2~D1,data = px83)
p83e<-p83[["coefficients"]][["D1"]]
p84<-lm(y2~D1,data = px84)
p84e<-p84[["coefficients"]][["D1"]]
p85<-lm(y2~D1,data = px85)
p85e<-p85[["coefficients"]][["D1"]]
p86<-lm(y2~D1,data = px86)
p86e<-p86[["coefficients"]][["D1"]]
p87<-lm(y2~D1,data = px87)
p87e<-p87[["coefficients"]][["D1"]]
p88<-lm(y2~D1,data = px88)
p88e<-p88[["coefficients"]][["D1"]]
p89<-lm(y2~D1,data = px89)
p89e<-p89[["coefficients"]][["D1"]]

p90<-lm(y2~D1,data = px90)
p90e<-p90[["coefficients"]][["D1"]]
p91<-lm(y2~D1,data = px91)
p91e<-p91[["coefficients"]][["D1"]]
p92<-lm(y2~D1,data = px92)
p92e<-p92[["coefficients"]][["D1"]]
p93<-lm(y2~D1,data = px93)
p93e<-p93[["coefficients"]][["D1"]]
p94<-lm(y2~D1,data = px94)
p94e<-p94[["coefficients"]][["D1"]]
p95<-lm(y2~D1,data = px95)
p95e<-p95[["coefficients"]][["D1"]]

p<-rbind(p1e,p2e,p3e,p4e,p5e,p6e,p7e,p8e,p9e,p10e,p11e,p12e,p13e,p14e,p15e,p16e,p17e,p18e,p19e,p20e,p21e,p22e,p23e,p24e,p25e,p26e,p27e,p28e,p29e,p30e,p31e,p32e,p33e,p34e,p35e,p36e,p37e,p38e,p39e,p40e,p41e,p42e,p43e,p44e,p45e,p46e,p47e,p48e,p49e,p50e,p51e,p52e,p53e,p54e,p55e,p56e,p57e,p58e,p59e,p60e,p61e,p62e,p63e,p64e,p65e,p66e,p67e,p68e,p69e,p70e,p71e,p72e,p73e,p74e,p75e,p76e,p77e,p78e,p79e,p80e,p81e,p82e,p83e,p84e,p85e,p86e,p87e,p88e,p89e,p90e,p91e,p92e,p93e,p94e,p95e)
n<-rbind(nrow(px1),nrow(px2),nrow(px3),nrow(px4),nrow(px5),nrow(px6),nrow(px7),nrow(px8),nrow(px9),nrow(px10),nrow(px11),nrow(px12),nrow(px13),nrow(px14),nrow(px15),nrow(px16),nrow(px17),nrow(px18),nrow(px19),nrow(px20),nrow(px21),nrow(px22),nrow(px23),nrow(px24),nrow(px25),nrow(px26),nrow(px27),nrow(px28),nrow(px29),nrow(px30),nrow(px31),nrow(px32),nrow(px33),nrow(px34),nrow(px35),nrow(px36),nrow(px37),nrow(px38),nrow(px39),nrow(px40),nrow(px41),nrow(px42),nrow(px43),nrow(px44),nrow(px45),nrow(px46),nrow(px47),nrow(px48),nrow(px49),nrow(px50),nrow(px51),nrow(px52),nrow(px53),nrow(px54),nrow(px55),nrow(px56),nrow(px57),nrow(px58),nrow(px59),nrow(px60),nrow(px61),nrow(px62),nrow(px63),nrow(px64),nrow(px65),nrow(px66),nrow(px67),nrow(px68),nrow(px69),nrow(px70),nrow(px71),nrow(px72),nrow(px73),nrow(px74),nrow(px75),nrow(px76),nrow(px77),nrow(px78),nrow(px79),nrow(px80),nrow(px81),nrow(px82),nrow(px83),nrow(px84),nrow(px85),nrow(px86),nrow(px87),nrow(px88),nrow(px89),nrow(px90),nrow(px91),nrow(px92),nrow(px93),nrow(px94),nrow(px95))

wtot<-sum(n)
w<-n/wtot
w<-as.numeric(w)

mat <-cbind(p,n,w)

pe_2 <- weighted.mean(p,w,na.rm=TRUE)
