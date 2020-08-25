#1
#(a)
mosquito <- read_excel("C:/Users/patrickverdun/Desktop/Exam2/mosquito.xls")
View(mosquito)
sum(mosquito$N)
sum(mosquito$Surv)
1000-330
#(b)
log(0.33/0.67)
null<-glm(cbind(Surv,N-Surv)~1,family=binomial,data=mosquito)
summary(null)
IP <- NULL
IP[which(mosquito$Treatment=='Pour')] <- 1
IP[which(mosquito$Treatment=="Spray")] <- 0
mosquito$IP <-IP
mosquito$IX <-mosquito$IP*mosquito$Dose
Flask <- NULL
Flask[which(mosquito$Flask=='F1')] <- 1
Flask[which(mosquito$Flask=="F2")] <- 2
Flask[which(mosquito$Flask=="F3")] <- 3
Flask[which(mosquito$Flask=="F4")] <- 4
Flask[which(mosquito$Flask=="F5")] <- 5
Flask[which(mosquito$Flask=="F6")] <- 6
Flask[which(mosquito$Flask=="F7")] <- 7
Flask[which(mosquito$Flask=="F8")] <- 8
Flask[which(mosquito$Flask=="F9")] <- 9
Flask[which(mosquito$Flask=="F10")] <- 10
mosquito$Flasknum <-Flask
full<-glm(cbind(Surv,N-Surv)~Dose+IP+IX+Flasknum,family=binomial,data=mosquito)
summary(full)
red <-glm(cbind(Surv,N-Surv)~~Dose+IP+IX,family=binomial,data=mosquito)
summary(red)
Red <- AIC(red,k=0)
Full <- AIC(full,k=0)
chisq <-Red-Full
pchisq <- pchisq(chisq,1)
1-pchisq 
#(d)
mod1<-glm(cbind(Surv,N-Surv)~Dose+IP+IX,family=binomial,data=mosquito)
mod2<-glm(cbind(Surv,N-Surv)~Dose+IP,family=binomial,data=mosquito)
mod3<-glm(cbind(Surv,N-Surv)~Dose+IX,family=binomial,data=mosquito)
mod4<-glm(cbind(Surv,N-Surv)~Dose,family=binomial,data=mosquito)
mod5<-glm(cbind(Surv,N-Surv)~1,family=binomial,data=mosquito)
AIC(mod5)
m1=AIC(mod1,k=0)
m2=AIC(mod2,k=0)
AIC(mod3,k=log(1000))
m3=AIC(mod4,k=0)
m4=AIC(mod5,k=0)
chisq <-m4-m3
pchisq <- pchisq(chisq,1)
1-pchisq
#(e)
summary(mod4)
(0.12207+1.098612)/21.35886  
calc$Exp.sur <- calc$p.hat*calc$T
calc$Exp.fail <- calc$T - calc$Exp.pass
calc$pearson.Chisq <-((calc$P-calc$Exp.pass)^2)/calc$Exp.pass + ((calc$T-calc$P-calc$Exp.fail)^2)/calc$Exp.fail
sum(calc$pearson.Chisq)
calc$Deviance.Chisq <-2*((calc$P*log(calc$P/calc$Exp.pass)) + (calc$T-calc$P)*log((calc$T-calc$P)/calc$Exp.fail))
sum(calc$Deviance.Chisq)
1-pchisq(8.046143, df=7) #deviance p value
1-pchisq(7.836313, df=7) #pearson p value

 
#b
ratdrug18 <- read.csv("C:/Users/patrickverdun/Desktop/Exam2/ratdrug18.txt", sep="")
IB <- NULL
IB[which(ratdrug18$Drug=='B')] <- 1
IB[which(ratdrug18$Drug=='A')] <- 0
ratdrug18$IB <-IB
ratdrug18$IX <-ratdrug18$IB*ratdrug18$Dose
modA <-lm(Anxiety~Dose+IB+IX,data=ratdrug18)
modB <-lm(Anxiety~Dose+IB,data=ratdrug18)
modC <-lm(Anxiety~Dose+IX,data=ratdrug18)
modD <-lm(Anxiety~Dose,data=ratdrug18)
modE <-lm(Anxiety~1,data=ratdrug18)
summary(modA)
summary(modB)
summary(modC)
summary(modD)
summary(modE)
AIC(modA)
AIC(modA, k=log(48))
anova(modA)
AIC(modB)
AIC(modB, k=log(48))
anova(modB)
AIC(modC)
AIC(modC, k=log(48))
anova(modC)
AIC(modD)
AIC(modD, k=log(48))
anova(modD)
AIC(modE)
AIC(modE, k=log(48))
anova(modE)
(242.16-104.59)/(104.59/44)
anova(modA,modC)
pf(q=57.87437, df1=1, df2=44,lower.tail=FALSE)
#3
-11.08314+0.15658*65+0.20625*29+0.09178*58
exp(1)
2.718282^10.39905
exp(1)^0.18356
#4
GAGOV18
dat <- GAGOV18[1:14,]
mod1<-glm(cbind(ETOT,TOTV-ETOT)~ETOT+PW+PB+PO,family=binomial, data=dat)
summary(mod1)
mod2<-glm(cbind(ETOT,TOTV-ETOT)~ETOT+PW+PB,family=binomial, data=dat)
summary(mod2)
mod2
mod3<-glm(cbind(ETOT,TOTV-ETOT)~ETOT+PW,family=binomial, data=dat)
summary(mod3)
chisq <- AIC(mod2,k=0)-AIC(mod1,k=0)
pchisq <- pchisq(chisq,1)
1-pchisq
chisqn <- AIC(mod3,k=0)-AIC(mod2,k=0)
pchisqn <- pchisq(chisqn,1)
1-pchisqn
mod1a<-glm(cbind(SAV,TOTV-SAV)~PW+PB+PO,family=binomial, data=dat)
summary(mod1a)
mod2a<-glm(cbind(SAV,TOTV-SAV)~PW+PO,family=binomial, data=dat)
summary(mod2a)
mod3a<-glm(cbind(SAV,TOTV-SAV)~PB+PO,family=binomial, data=dat)
summary(mod3a)
chisq1=AIC(mod2a,k=0)-AIC(mod1a,k=0)
pchisq1 <- pchisq(chisq1,1)
1-pchisq1
chisq2=AIC(mod3a,k=0)-AIC(mod1a,k=0)
pchisq2 <- pchisq(chisq2,1)
1-pchisq2
0.0051*65
0.0051*29
0.0051*58
1.984*0.6661*sqrt(42.9981)
10.39905-8.665742
exp(1)^1.733308
