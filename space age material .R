library(gdata)
library(readxl)
# data import
space<- read_xlsx("C:\\Users\\patrickverdun\\Desktop\\Space_Age_Material_Project.xlsx",
                sheet = "Larger range")
Height<-as.vector(as.matrix(space[,1]))
Prob<-as.vector(as.matrix(space[,12]))
breaks<-as.vector(as.matrix(space[,13]))
Res<-as.vector(as.matrix(space[,14]))
N<-as.vector(as.matrix(space[,15]))
########################################################################
#Step I: Simple Linear Regression

# 1. Line graph for Probability & Height
plot(Prob~Height,pch=16,cex=0.5,xlab = "Height", ylab = "Probability",main = "Line graph for Probability & Height")
abline(lm(Prob~Height,weight=N), col= 'blue')
op <- par(cex = 1)
legend('bottomright',c("Data Point","Regression Line"),
       pch=c(16,-1),col=c("black","blue"),cex = 0.5,lty = c(0,1)) 


# 2. Do weighted linear regression test
LR <- lm(Prob~Height,weights = N)
summary(LR)

# Residual Plot for weighted Linear Regression
plot(LR$fitted.values,rstandard(LR),pch=15,cex=0.5,col="black",
     xlab = "fitted", ylab = "Residual of Prob",
     main = "Residual Plot of weighted linear regression ")
abline(0,0)
lines(lowess(LR$fitted.values,ress),col="blue") 


########################################################################
#Step II: Logistic Regression

# 1. Run logistic model
logi<-glm(cbind(breaks, Res)~Height, family=quasibinomial)
summary(logi)


# probability prediction
prediction<-predict(logi, list(x=Height),type="response")
prediction
# plot observed and predicted probabilities vs. Height
plot(Prob~Height,pch=16,col="black",
     xlab="Height",ylab="Probability",
     main="Data points and predicted lines of linear and logistic regression")
lines(prediction~Height,col="red")
abline(lm(Prob~Height), col= 'blue')
legend('bottomright',c("Data point", "Logistic Predicted", "Linear Predicted"),
       pch=c(15,-1,-1),col=c("black","red","blue"),cex = 0.3,lty = c(0,1,1)) 


# Residual Plot of Logi Regression
res=(Prob-prediction)/sqrt((prediction*(1-prediction))/N) #pearson res for logi 
plot(prediction,res,pch=15,cex=0.5,
     xlab = "Fitted", ylab = "Residual",ylim=c(-2.0,2.0), 
     main = "Residual Plot of Logistic Regression ",col="black")
abline(0,0)
lines(lowess(prediction,res),col="red") 

#calcualte pearson residual for both model in order to compare them in same scale
pred<-predict(LR,list(x=Height),type="response")
1-(pred[15:17]-1) #since last three values for lr exceeded 1, to keep the same deviation, we reset those values
pred[15:17]=c(0.9990858, 0.9263866, 0.8536873 )
resi=(Prob-pred)/sqrt((pred*(1-pred))/N) #pearson res for Linear  
sum(res^2)
sum(resi^2) 
Total=c("Total","100","66","NAN","NAN","NAN","6.749419",
        "6.459927")
tb=data.frame(Height,N,breaks,Prob,prediction,pred,res,resi)
tb=rbind(tb,Total)
tb
plot(LR$fitted.values,resi,col="black",xlab="fitted values", ylab="residual",
     main="residual plot of logistic and linear regression",pch=15) 
points(logi$fitted.values,res,col="red",pch=15)
abline(0,0)
legend('bottomright',c("Linear residual","Logistic residual"),
       pch=c(15,15),col=c("black","red"),cex = 0.3) 


p=exp(LR$coefficients[1]+LR$coefficients[2]*Height)/(1+exp(LR$coefficients[1]+LR$coefficients[2]*Height))
p

