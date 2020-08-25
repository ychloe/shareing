#(a)
mosquito <- read_excel("C:/Users/Desktop/mosquito.xls")
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

 
