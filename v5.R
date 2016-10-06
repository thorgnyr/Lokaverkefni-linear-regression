#Libraries----
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(GGally)
library(car)
library(stats)
#Gögn tekin inn og deilt niður á matssvæði.---------------
fasteignir <- read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ",", fileEncoding = "latin1")
fasteignir_sorted <- filter(fasteignir, matssvaedi %in% c(11, 31, 80, 120, 150))
#Breytur fjarlægðar----
fasteignir_sorted <- fasteignir_sorted[, -c(1, 5, 21)]
#Breytum lyftu og stig10 í 0/1 factora.----
fasteignir_sorted$lyfta[fasteignir_sorted$lyfta > 0] <- 1
fasteignir_sorted$stig10[fasteignir_sorted$stig10 < 10] <- 0
fasteignir_sorted$stig10[fasteignir_sorted$stig10 == 10] <- 1
fasteignir_sorted$fjbilast[fasteignir_sorted$fjbilast > 0] <- 1

fasteignir_sorted$lyfta <- as.factor(fasteignir_sorted$lyfta)
fasteignir_sorted$stig10 <- as.factor(fasteignir_sorted$stig10)
fasteignir_sorted$fjbilast <- as.factor(fasteignir_sorted$fjbilast)
#Búin til ný breyta með ártölum kaupa og tíföldum ibm2------------------------
fasteignir_sorted$kaup_ar <- as.Date(fasteignir_sorted$kdagur, format = "%Y-%m-%d")
fasteignir_sorted$kaup_ar <- year(fasteignir_sorted$kaup_ar)
fasteignir_sorted <- fasteignir_sorted[-1]
fasteignir_sorted$kaup_ar <- as.factor(fasteignir_sorted$kaup_ar)
fasteignir_sorted$ibm2 <- fasteignir_sorted$ibm2*10
#Slembiúrtök úr gögnunum. Búið til sett til að train'a módelið og annað til að að prufa módelið á.------
set.seed(5)
n <- dim(fasteignir_sorted)[1]
third <- sample(1:n, n/3)
testdata <- fasteignir_sorted[third,]
traindata <- fasteignir_sorted[-third,]

#################HINGAÐ ER ALLT EINS###########################

#Gerum módel og steppum það.----
stort_model <- lm(nuvirdi ~ ., data=traindata)
stort.st <- step(stort_model)
summary(stort.st)
plot(stort.st)    #Sjáum að 1865, 1405 og 999 eru stórir.

#Manually að taka út breytur, slást við Steppið----
#Niðurstaðan er sú sama. Um að gera að keyra báðar aðferðir og sýna summary'in (en ekki reikninginn)
traindata.manual.st <- traindata
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-13]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-12]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-14]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-10]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-12]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-8]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-8]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-3]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

traindata.manual.st <- traindata.manual.st[-7]
stort_model.2 <- lm(nuvirdi ~ ., data = traindata.manual.st)
summary(stort_model.2)

plot(stort_model.2)

#Reyni að taka út breytur til að laga Q-Q plottið.
trainmodel_fixrow <- trainmodel_ms
trainmodel_fixrow <- trainmodel_fixrow[-c(999, 1405), ]
frummodel3 <- lm(nuvirdi ~ ., data = trainmodel_fixrow)
plot(frummodel3)

#Skoðum víxlhrif á verði og tegund eigna----
vixlhrif <- lm(nuvirdi ~ teg_eign * ibm2, data = trainmodel)
plot(

#Prufa að búa til módel úr kaupárum á núvirði.----


#Athugasemdir-------

#Prufukóðar---------
m1<-lm(nuvirdi ~ ., na.action = "na.exclude", data = train)
sum(is.na(residuals(m1)))
sm1<-summary(m1)
sm1
####diagnostic
m2<-lm(nuvirdi~teg_eign+byggar+efnu+ibm2+fjherb, train)
#skoða þætti sem líklegir eru til að hafa áhrif á verð 
#sm2<-summary(lm(nuvirdi~kaupverd+grfast+fjmib+fjhaed, train))
sm2<-summary(m2)
m3<-update(m1, .~.-faerslunumer)
m4<-update(m3, .~. -rfastnum)
#m5<-update(m4, .~. -kaupverd)
m6<-update(m4, .~. -svfn)
m7<-update(m6, .~. -byggd)
m8<-update(m7, .~. -adferd)
m9<-update(m8, .~. -ib3m2)

lm_herb_serherb <- lm(fjherb ~ I(fjbkar + fjsturt + fjklos + fjeld + fjstof), data = trainmodel)
summary(lm_herb_serherb)
