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
testdata$byggar <- as.factor(testdata$byggar)