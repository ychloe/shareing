library(tidyverse)
library(readxl)
library(lubridate)
library(splines)
library(boot)
library(pls)
####################
## Importing Data ##
####################

## input the relvant data set as birthdata
## or use this function
## make sure it is named as birthdata
## make sure it has months in the data
birthdata <- ssdm_post_1916 
View(birthdata)

#####################
## Variable Adding ##
#####################

## adds the day of the year
for(i in 1:nrow(birthdata))
{
  birthdata$totday[i] = i 
}

## adds day of the month
birthdata$day <- 0
day=1
birthdata$births[birthdata$month==2]
count=1
for(i in 1:12)
{
  day=1
  for(g in 1:length(birthdata$births[birthdata$month==i]))
  {
    birthdata$day[count] = g
    day = day + 1
    count=count+1
  }
}

#####################
### Day Variation ###
#####################

## initialize the repeating digits
birthdata$rpt <- 0
for(i in 1:nrow(birthdata))
{
  if(birthdata$month[i]==birthdata$day[i])
  {
    birthdata$rpt[i] <- 1
  }
}

## initialize the holidays 
## as factor and boolean variables
birthdata$holiday <- 0
birthdata$is_holiday <- 0
for(i in 1:nrow(birthdata))
{
  if((i==1)|
     (i==45)|
     (i==53)|
     (i==77)|
     (i==186)|
     (i==360)|
     (i==366))
  {
    birthdata$holiday[i] <- i
    birthdata$is_holiday[i] <- 1
  }
}
birthdata$holiday <- as.factor(birthdata$holiday)


#####################
##### Modeling ######
#####################

## set the value for holidays as the month average
daymodel <- lm(births~as.factor(day) + holiday + rpt,data=birthdata)
#### summary(daymodel)
#### plot(birthdata$totday,(daymodel$residuals+mean(birthdata$births)), ylim=c(100000,160000))

## plots the reisiduals without outliers
## this is the data for the spline regression
#### plot(birthdata$totday,(birthdata$births), ylim=c(100000,160000))
#### plot(birthdata$totday,(daymodel$residuals+mean(birthdata$births)), ylim=c(100000,160000))

## sets the unex var as the residuals of the previous regression
## adds the mean, for plot scaling, used for only the seasonal trendlines
birthdata$unex <- (daymodel$residuals+mean(birthdata$births))

## for post-1916 use 12 df 
mdl <- glm(unex~bs(totday, df = 12),data=birthdata) #fitting spline
table=summary(mdl)
library(xtable)
xtable(table, type = "latex", file = "table.tex")
whic.max(mdl$fitted.values)
which.min(mdl$fitted.values)
sort(mdl$fitted.values)
##peaks:247(max),64;  vallys: 331(min), 136
##Sep.3, March.4; Nov.26, May.15

#####################
##### Graphing ######
#####################

## graph the results
plot(birthdata$totday,mdl$fitted.values, ylim=c(100000,160000),xlab = "birthdate",ylab="fitted population ",main="Seasonal Trendline")
plot(birthdata$totday,mdl$fitted.values, ylim=c(100000,160000),
     xlab = "birthdate",ylab="fitted population ",main="Seasonal Trendline",type="n")
lo <- loess(mdl$fitted.values~birthdata$totday)
lines(lo, col='blue', lwd=2)

plot(birthdata$totday,(birthdata$births-mdl$fitted.values), ylim=c(-8000,8000),ylab="residual",xlab="birthdate",main="Residual vs Birthdate")
abline(0,0)
hist((birthdata$births-mdl$fitted.values), breaks=60,xlim=c(-20000,20000),xlab="residual",main="Distribution of Residual")
mean(birthdata$births-mdl$fitted.values)
which.max(mdl$fitted.values)
sort(mdl$fitted.values)


## get the full model
## for pre-1916 use 12 df 
mdl <- glm(births~bs(totday, df = 12) + as.factor(day) + holiday + rpt,data=birthdata) #fitting spline
table1=summary(mdl)
xtable(table1, type = "latex", file = "table1.tex")

plot(birthdata$totday,(birthdata$births - mdl$fitted.values), ylim=c(-20000,20000),xlab = "birthdate",ylab="residual ",main="Residual vs Birthdate")
abline(0,0)
plot(birthdata$totday,mdl$fitted.values, ylim=c(100000,160000),xlab = "birthdate",ylab="fitted population ",main="Fitted Population vs Birthdate")
which.max(mdl$fitted.values)
which.max(mdl$residuals)
sort(mdl$residuals)
which.min(mdl$fitted.values)
findPeaks=function(x,thresh=0)
{
  pks<-which(diff(sign(diff(x,na.pad=FALSE)),na.pad=FALSE)<0)+2
                 if(!missing(thresh)){pks[x[pks-1]-x[pks]>thresh]
                 }
                   else pks
}
findPeaks(mdl$fitted.values)
findValleys(mdl$fitted.values,thresh=0)


birthdata$is12 <- birthdata$month <=

mdl <- glm(births~(day<=12),data=birthdata) #fitting spline
table3=summary(mdl)
xtable(table3, type = "latex", file = "table3.tex")
which.max(mdl$fitted.values)



############percentage of fake#######
birthdata$projdiff <- 0
for(i in 1:366)
{
  if(birthdata$births[i] > (mdl$fitted.values[i] - 3483.82))
  {
    birthdata$projdiff[i] = birthdata$births[i] - (mdl$fitted.values[i] - 3483.82)
  }
}
sum(birthdata$projdiff)/sum(birthdata$births)
round(birthdata$projdiff,0)
