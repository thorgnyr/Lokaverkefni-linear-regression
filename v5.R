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

#Skerum út jaðarmælingar úr stóra módelinu.----
diag1 <- fortify(stort.st)
diag1 <- diag1[diag1$.stdresid < 6.5, ]
diag1.m <- lm(nuvirdi ~ teg_eign + byggar + haednr + lyfta + ibm2 + fjhaed + fjklos +fjstof + matssvaedi + ibteg + kaup_ar, data = diag1)
summary(diag1.m)

#Skipta byggingarári í þrjár breytur og transform----
diag1$byggar <- cut(diag1$byggar,c(0, 1938,1960,1983, 2016), right=F)
levels(diag1$byggar)<-c("gamalt","midlungs", "lanabreyting", "nytt")
diag1.byggar <- lm(sqrt(nuvirdi) ~ teg_eign + byggar + haednr + lyfta + ibm2 + fjhaed + fjklos +fjstof + matssvaedi + ibteg + kaup_ar, data = diag1)
summary(diag1.byggar)


#########TESTMODEL VINNA###############

#Gerum módel og steppum það.
litid_model <- lm(nuvirdi ~ ., data=testdata)
extr.byggar <- as.data.frame(testdata$byggar)
litid.st <- step(litid_model)
summary(litid_model)

#Skerum út jaðarmælingar úr stóra módelinu.
diag2 <- fortify(litid.st)
diag2 <- cbind(diag2, extr.byggar)
diag2 <- diag2[diag2$.stdresid < 6.5, ]
diag2.m <- lm(nuvirdi ~ teg_eign + `testdata$byggar` + haednr + lyfta + ibm2 + fjhaed + fjklos +fjstof + matssvaedi + ibteg + kaup_ar, data = diag2)
summary(diag2.m)

#Skipta byggingarári í þrjár breytur og transform
diag2$`testdata$byggar` <- cut(diag2$`testdata$byggar`,c(0,1938,1960,1983, 2016), right=F)
levels(diag2$`testdata$byggar`)<-c("gamalt","midlungs", "lanabreyting", "nytt")
diag2.byggar <- lm(sqrt(nuvirdi) ~ teg_eign + `testdata$byggar` + haednr + lyfta + ibm2 + fjhaed + fjklos +fjstof + matssvaedi + ibteg + kaup_ar, data = diag2)
summary(diag2.byggar)
plot(diag2.byggar)

#Þetta----

litid_model <- lm(nuvirdi ~ ., data=testdata)
extr.byggar <- as.data.frame(testdata$byggar)
litid.st <- step(litid_model)
diag2 <- fortify(litid.st)
diag2 <- cbind(diag2, extr.byggar)
diag2$byggar <- diag2$`testdata$byggar`
diag2 <- diag2[diag2$.stdresid < 6.5, ]
diag2$byggar <- cut(diag2$byggar, c(0,1938,1960,1983, 2016), right=F)
levels(diag2$byggar)<-c("gamalt","midlungs", "lanabreyting", "nytt")




#Keyra þetta á testmódelið.----
thefinalcountdown <- predict(diag1.byggar, newdata = diag2)
