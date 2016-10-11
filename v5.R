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
#Byggar breytingin----
fasteignir_sorted$byggar <- cut(fasteignir_sorted$byggar,c(0, 1938,1960,1983, 2016), right=F)
levels(fasteignir_sorted$byggar)<-c("gamalt","midlungs", "lanabreyting", "nytt")

#Trans fasteignir----
#fasteignir_sorted$nuvirdi <- sqrt(fasteignir_sorted$nuvirdi)

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

#Skerum út jaðarmælingar úr stóra módelinu.----
diag1 <- fortify(stort.st)
diag1 <- diag1[diag1$.stdresid < 6.5, ]
diag1.m <- lm(nuvirdi ~ teg_eign + byggar + haednr + lyfta + ibm2 + fjhaed + fjbilast + fjbkar + fjstof + matssvaedi + ibteg + kaup_ar, data = diag1)
summary(diag1.m)


#Transform----
boxCox(diag1.m)
diag1.byggar <- lm(nuvirdi ~ teg_eign + byggar + haednr + lyfta + ibm2 + fjhaed + fjklos +fjstof + matssvaedi + ibteg + kaup_ar, data = diag1)
summary(diag1.byggar)
#Hér taka fram að við breytum breytunum í upphafsgögnum.

#Keyrt á testdata
thefinalcountdown <- predict.lm(diag1.m, newdata = testdata)


e <- testdata$nuvirdi - thefinalcountdown
RSS <- sum(e^2)
TSS <- sum((testdata$nuvirdi - mean(testdata$nuvirdi))^2)
rAdj <- 1 - ((RSS/725)/(TSS/741))



