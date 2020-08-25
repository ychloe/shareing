##### Import Data ####
library(ggplot2)
library(dbplyr)
library(readr) # importing csv files
library(MASS) # Boxcox function
library(car) # qqPlot function
library(moments) # skeweness and kurtosis functions
library(stringr)
library(corrplot)
library(tidyverse)
library(knitr)
library(gridExtra)
library(Metrics)
Car<- read.csv("C:/Users/chloe/Desktop/R project/test folder/QEM Aug 2019/QEM JAN 2019/CarPrice.csv")

###### Data Cleaning #####
summary(Car)
Cdata=Car[Car$price>1000 & Car$price<100000,] #Remove Outlier in response
Cdata<- Cdata[!(is.na(Cdata$enginelocation) | Cdata$enginelocation==""), ] #Remove missing values enginetype
Cdata<-na.omit(Cdata) #Remove missing values in horsepower
Cdata<-Cdata[!(is.na(Cdata$enginetype) | Cdata$enginetype=="FALSE"),] # remove mis-entered values
remove<-c(206:214)
Cdata = Cdata[!Cdata$car_ID%in%remove, ] #remove duplicates 
r1<-c("mfi","spfi")
Cdata = Cdata[!Cdata$fuelsystem%in%r1, ] #remove 1 observation  
r2<-c("three","twelve")
Cdata = Cdata[!Cdata$cylindernumber%in%r2, ] 
Cdata = Cdata[,-1] #Car_ID 
summary(Cdata)


###### Replace Value of CarName #####
alfaromero<-Cdata %>%
  filter(str_detect(CarName, "alfa-romero"))
mean(alfaromero$price) #14997.5
audi<-Cdata %>%
  filter(str_detect(CarName, "audi"))
mean(audi$price) #17631.25
bmw<-Cdata %>%
  filter(str_detect(CarName, "bmw"))
mean(bmw$price) #26203.33
dodge<-Cdata %>%
  filter(str_detect(CarName, "dodge"))
mean(dodge$price) #9272
honda <-Cdata %>%
  filter(str_detect(CarName, "honda"))
mean(honda$price) #8288.5
isuzu <-Cdata %>%
  filter(str_detect(CarName, "isuzu"))
mean(isuzu$price) #15233.38
jaguar <-Cdata %>%
  filter(str_detect(CarName, "jaguar"))
mean(jaguar$price) #34600
mazda <-Cdata %>%
  filter(str_detect(CarName, "mazda"))
mean(mazda$price) #11210.33
buick <-Cdata %>%
  filter(str_detect(CarName, "buick"))
mean(buick$price) #34312
mitsubishi<-Cdata %>%
  filter(str_detect(CarName, "mitsubishi"))
mean(mitsubishi$price) #9956
nissan<-Cdata %>%
  filter(str_detect(CarName, "nissan"))
mean(nissan$price) #11188.29
peugeot<-Cdata %>%
  filter(str_detect(CarName, "peugeot"))
mean(peugeot$price) #15877.78
plymouth<-Cdata %>%
  filter(str_detect(CarName, "plymouth"))
mean(plymouth$price) #7285.5
porsche<-Cdata %>%
  filter(str_detect(CarName, "porsche"))
mean(porsche$price) #31118.62
renault<-Cdata %>%
  filter(str_detect(CarName, "renault"))
mean(renault$price) #9595
saab<-Cdata %>%
  filter(str_detect(CarName, "saab"))
mean(saab$price) #14638
subaru<-Cdata %>%
  filter(str_detect(CarName, "subaru"))
mean(subaru$price) #8140.333
toyota<-Cdata %>%
  filter(str_detect(CarName, "toyo"))
mean(toyota$price) #10110.42
volkswagen<-Cdata %>%
  filter(str_detect(CarName, "volk"))
mean(volkswagen$price) #10466
volvo<-Cdata %>%
  filter(str_detect(CarName, "volvo"))
mean(volvo$price) #17760.71
Cdata$CarName<- gsub("alfa.*","alfa-romero", Cdata$CarName)
Cdata$CarName<- gsub("audi.*","audi", Cdata$CarName)
Cdata$CarName<- gsub("bmw.*","bmw", Cdata$CarName)
Cdata$CarName<- gsub("buick.*","buick", Cdata$CarName)
Cdata$CarName<- gsub("chevrolet.*","chevrolet", Cdata$CarName)
Cdata$CarName<- gsub("dodge.*","dodge", Cdata$CarName)
Cdata$CarName<- gsub("honda.*","honda", Cdata$CarName)
Cdata$CarName<- gsub("isuzu.*","isuzu", Cdata$CarName)
Cdata$CarName<- gsub("jaguar.*","jaguar", Cdata$CarName)
Cdata$CarName<- gsub("mazda.*","mazda", Cdata$CarName)
Cdata$CarName<- gsub("mercury.*","mercury", Cdata$CarName)
Cdata$CarName<- gsub("mitsubishi.*","mitsubishi", Cdata$CarName)
Cdata$CarName<- gsub("nissan.*","nissan", Cdata$CarName)
Cdata$CarName<- gsub("peugeot.*","peugeot", Cdata$CarName)
Cdata$CarName<- gsub("plymouth.*","plymouth", Cdata$CarName)
Cdata$CarName<- gsub("porsche.*","porsche", Cdata$CarName)
Cdata$CarName<- gsub("renault.*","renault", Cdata$CarName)
Cdata$CarName<- gsub("saab.*","saab", Cdata$CarName)
Cdata$CarName<- gsub("subaru.*","subaru", Cdata$CarName)
Cdata$CarName<- gsub("toyo.*","toyota", Cdata$CarName)
Cdata$CarName<- gsub("volk.*","volkswagen", Cdata$CarName)
Cdata$CarName<- gsub("volvo.*","volvo", Cdata$CarName)
e<-c("chevrolet","dodge","honda","mitsubishi","plymouth","renault","subaru")
m<-c("alfa-romero","audi","mazda","isuzu","mercury","nissan","peugeot","saab","volkswagen","volvo","toyota")
l<-c("bmw","buick","jaguar","porsche")
Cdata$CarName[Cdata$CarName%in%e]<-"economy"
Cdata$CarName[Cdata$CarName%in%m]<-"middle"
Cdata$CarName[Cdata$CarName%in%l]<-"luxury"
Cdata$CarCategory=Cdata$CarName
Cdata=Cdata[,-2]
summary(Cdata)

###### Recode variables#####

Cdata<-Cdata %>% mutate_at(c("CarCategory","fueltype","aspiration","doornumber",
                             "carbody","drivewheel","enginelocation","enginetype",
                             "cylindernumber","fuelsystem"), as_factor)

###### EDA #####
hist(Cdata$price,prob=TRUE,main = "Car Price Distribution",xlab="Car price",ylab="Count")#highly screwed positive data
mx=mean(Cdata$price)
lines(density(Cdata$price),col="grey") 
abline(v = mx, col = "blue", lwd = 1)
md=median(Cdata$price)
abline(v = md, col = "red", lwd = 1)
text(locator(), labels = c("median", "mean"),col=c("red","blue"))
summary(Cdata$price)


p1<-ggplot(data.frame(Cdata$CarCategory), aes(x=Cdata$CarCategory)) +
  geom_bar()
g1=print(p1 + labs(y="count", x = "CarCategory")
      +ggtitle("CarCategory distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p2<-ggplot(data.frame(Cdata$fueltype), aes(x=Cdata$fueltype)) +
  geom_bar()
g2=print(p2 + labs(y="count", x = "fueltype")
      +ggtitle("fueltype distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p3<-ggplot(data.frame(Cdata$aspiration), aes(x=Cdata$aspiration)) +
  geom_bar()
g3=print(p3 + labs(y="count", x = "aspiration")
      +ggtitle("aspiration distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p4<-ggplot(data.frame(Cdata$doornumber), aes(x=Cdata$doornumber)) +
  geom_bar()
g4=print(p4 + labs(y="count", x = "doornumber")
      +ggtitle("doornumber distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p5<-ggplot(data.frame(Cdata$carbody), aes(x=Cdata$carbody)) +
  geom_bar()
g5=print(p5 + labs(y="count", x = "carbody")
      +ggtitle("carbody distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p6<-ggplot(data.frame(Cdata$drivewheel), aes(x=Cdata$drivewheel)) +
  geom_bar()
g6=print(p6 + labs(y="count", x = "drivewheel")
      +ggtitle("drivewheel distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p7<-ggplot(data.frame(Cdata$enginelocation), aes(x=Cdata$enginelocation)) +
  geom_bar()
g7=print(p7 + labs(y="count", x = "enginelocation")
      +ggtitle("enginelocation distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p8<-ggplot(data.frame(Cdata$enginetype), aes(x=Cdata$enginetype)) +
  geom_bar()
g8=print(p8 + labs(y="count", x = "enginetype")
      +ggtitle("enginetype distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p9<-ggplot(data.frame(Cdata$cylindernumber), aes(x=Cdata$cylindernumber)) +
  geom_bar()
g9=print(p9 + labs(y="count", x = "cylindernumber")
      +ggtitle("cylindernumber distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
p10<-ggplot(data.frame(Cdata$fuelsystem), aes(x=Cdata$fuelsystem)) +
  geom_bar()
g10=print(p10 + labs(y="count", x = "fuelsystem")
      +ggtitle("fuelsystem distribution")
      +theme(plot.title = element_text(hjust = 0.5)))
grid.arrange(g1, g2,g3,g4,g5,g6,g7,g8,g9,g10, ncol=5)

plot(price~factor(CarCategory),Cdata,main="Car price for CarCategory",
     xlab="CarCategory",col="grey")
plot(price~factor(fueltype),Cdata,main="Car price for fueltype",
     xlab="fueltype",col="grey")
plot(price~factor(aspiration),Cdata,main="Car price for aspiration",
     xlab="aspiration",col="grey")
plot(price~factor(doornumber),Cdata,main="Car price for doornumber",
     xlab="doornumber",col="grey")
plot(price~factor(carbody),Cdata,main="Car price for carbody",
     xlab="carbody",col="grey")
plot(price~factor(drivewheel),Cdata,main="Car price for drivewheel",
     xlab="drivewheel",col="grey")
plot(price~factor(enginelocation),Cdata,main="Car price for enginelocation",
     xlab="enginelocation",col="grey")
plot(price~factor(enginetype),Cdata,main="Car price for enginetype",
     xlab="enginetype",col="grey")
plot(price~factor(cylindernumber),Cdata,main="Car price for cylindernumber",
     xlab="cylindernumber",col="grey")
plot(price~factor(fuelsystem),Cdata,main="Car price for fuelsystem",
     xlab="fuelsystem",col="grey")

par(mfrow=c(1,1))
plot(price~wheelbase,Cdata,main="Car price vs.Wheelbase")
plot(price~carlength,Cdata,main="Car price vs.Carlength")
plot(price~carwidth,Cdata,main="Car price vs.Carwidth")
plot(price~carheight,Cdata,main="Car price vs.Carheight")
plot(price~curbweight,Cdata,main="Car price vs.Curbweight")
plot(price~enginesize,Cdata,main="Car price vs.Enginesize")
plot(price~boreratio,Cdata,main="Car price vs.Boreratio")
plot(price~stroke,Cdata,main="Car price vs.Stroke")
plot(price~compressionratio,Cdata,main="Car price vs.Compressionratio")
plot(price~horsepower,Cdata,main="Car price vs.Horsepower")
plot(price~peakrpm,Cdata,main="Car price vs.Peakrpm")
plot(price~citympg,Cdata,main="Car price vs.Citympg")
plot(price~highwaympg,Cdata,main="Car price vs.Highwaympg")
plot(price~symboling,Cdata,main="Car price vs.Symboling")
Ccon=Cdata[,c(1,8:12,15,17:24)]
c=cor(Ccon,method="pearson")
corrplot(c, method="color")
######### Modeling #######
f<-lm(price~.,data=Cdata)
s=summary(f)
AIC(f)
BIC(f)
step.model <- stepAIC(f, direction = "backward", 
                      trace = TRUE)
summary(step.model)
anova(step.model)
f2=lm(price~factor(CarCategory)+factor(aspiration)     
     +factor(carbody)+factor(enginelocation)+carwidth
     +curbweight+factor(enginetype)+factor(cylindernumber)
     +enginesize+factor(fuelsystem)+stroke+compressionratio+peakrpm,Cdata)
summary(f2)
AIC(f2)
BIC(f2)
plot(step.model)
shapiro.test(step.model$residuals)
durbinWatsonTest(step.model)
vif(step.model)
bc=boxcox(step.model)
bc$x[which(bc$y==max(bc$y))]
text(locator(), labels ="best lambda=0.2",col="black")


f3=lm(log10(price)~factor(CarCategory)+factor(aspiration)     
      +factor(carbody)+factor(enginelocation)+carwidth
      +curbweight+factor(enginetype)+factor(cylindernumber)
      +boreratio+factor(fuelsystem)+stroke+compressionratio+peakrpm,Cdata)

step<- stepAIC(f3, direction = "backward", 
                      trace = TRUE)
summary(step)
Anova(step)
AIC(step)
BIC(step)


f4=lm(log10(price)~factor(CarCategory)+factor(carbody)+factor(enginelocation)+carwidth
      +curbweight+factor(enginetype)+factor(fuelsystem)+stroke+compressionratio+peakrpm,Cdata)
summary(f4)
Anova(f4)
AIC(f4)
BIC(f4)
plot(f4)
shapiro.test(f4$residuals)
vif(f4)

f5=lm(log10(price)~factor(CarCategory)+factor(carbody)+factor(enginelocation)
      +curbweight+factor(enginetype)+factor(fuelsystem)+stroke+peakrpm,Cdata)
summary(f5)
Anova(f5)
AIC(f5)
BIC(f5)
plot(f5)
shapiro.test(f5$residuals)
vif(f5)

Cdata$pred=(10)^predict(f5)
plot(pred~peakrpm,Cdata,main="Car price vs.peakrpm")
plot(pred~stroke,Cdata,main="Car price vs.stroke")
plot(pred~curbweight,Cdata,main="Car price vs.curbweight")
plot(pred~factor(carbody),Cdata,main="Car price vs.carbody",xlab="carbody",col="grey")
plot(pred~factor(enginelocation),Cdata,main="Car price vs.enginelocation",xlab="enginelocation",col="grey")
plot(pred~factor(enginetype),Cdata,main="Car price vs.enginetype",xlab="enginetype",col="grey")
plot(pred~factor(fuelsystem),Cdata,main="Car price vs.fuelsystem",xlab="fuelsystem",col="grey")
plot(pred~factor(CarCategory),Cdata,main="Car price for CarCategory",
     xlab="CarCategory",col="grey")

#err <- 10^(f5$fitted.values) - Cdata$price
#err2 <- err^2
#rmse <- sqrt(mean(err2))

################# Cross Validation
set.seed(123)
samplesize<-round(0.7 * nrow(Cdata),0)
index<-sample(seq_len(nrow(Cdata)),size=samplesize)
data_train<-Cdata[index,]
data_test<-Cdata[-index,]
f5=lm(log10(price)~factor(CarCategory)+factor(carbody)+factor(enginelocation)
      +curbweight+factor(enginetype)+factor(fuelsystem)+stroke+peakrpm,data_train)
summary(f5)
pred5<-predict(f5,newdata=data_test %>% select(-price))
rmse(10^(f5$fitted.values),data_train$price) #train rmse 1847.
rmse(10^(pred5),data_test$price) #test rmse 3630.1,since test data is larger than traning data, then our model is overfit the trianing dataset
