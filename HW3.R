install.packages("gsDesign")
library(gsDesign)
myof<-gsDesign(k=8, test.type = 2, sfu="OF",alpha = 0.1)
myof$upper$bound
myof$lower$bound
mypo = gsDesign(k=8,test.type=2,sfu="Pocock",alpha = 0.1) 
mypo$upper$bound
mypo$lower$bound
plot(c(1:8/8,1:8/8),c(myof$upper$bound,myof$lower$bound),ylab = "Z Value", xlab="Information Time") 
lines(1:8/8,myof$upper$bound) 
lines(1:8/8,myof$lower$bound) 
lines(1:8/8,mypo$upper$bound,lty=2) 
lines(1:8/8,mypo$lower$bound,lty=2) 
legend(locator(1),lty=1:2,legend=c("O'Brien Fleming","Pocock"))
myof
mypo

