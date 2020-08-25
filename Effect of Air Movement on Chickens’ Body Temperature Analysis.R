library(readxl)
data <- read_excel("C:/Users/chloe/Desktop/R project/8001/Projects/Brian Fairchild/cleaned data -chicken.xlsx")
data$id=paste(data$sensor,data$rep)
View(data)
r85data=data[data$loc=="0"& data$room_temp=="0",]
r90data=data[data$loc=="0"& data$room_temp=="1",]
r95data=data[data$loc=="0"& data$room_temp=="2",]
f85data=data[data$loc=="1"& data$room_temp=="0",]
f90data=data[data$loc=="1"& data$room_temp=="1",]
f95data=data[data$loc=="1"& data$room_temp=="2",]
install.packages("lme4")
library(lme4) 
m<-lm(body_temp_diff ~ factor(rep) + factor(day)+factor(group)+factor(time_of_day), data =r85data)
summary(m)
anova(m)


k<-lm(cell_mean_temp  ~pen_mean_temp, data =r85data)
summary(k)

k<-lm(cell_mean_temp  ~pen_mean_temp+factor(group), data =r85data)
summary(k)

k<-lm(cell_mean_temp  ~pen_mean_temp+factor(rep) + factor(day)+factor(group)+factor(time_of_day), data =r85data)
summary(k)

k<-lmer(cell_mean_temp  ~factor(rep)+(1|rep/sensor), data =r85data)
summary(k)

k<-lmer(cell_mean_temp  ~factor(rep)+(1|rep/sensor)+factor(group) , data =r85data)
summary(k)



#########################final model###################
install.packages("nlme")
library(nlme)

m1<-lme(cell_mean_temp ~ factor(rep)+factor(day)+factor(group)+factor(time_of_day), random=~1|id, data =r85data)
anova(m1)
summary(m1)
r85data$fit=predict(m1)

m2<-lme(cell_mean_temp ~ factor(rep) + factor(day)+factor(group)+factor(time_of_day),random=~1|id, data =r90data)
summary(m2)
anova(m2)
r90data$fit=predict(m2)

m3<-lme(cell_mean_temp ~ factor(rep) +factor(day)+factor(group)+factor(time_of_day), random=~1|id, data =r95data)
summary(m3)
anova(m3)
r95data$fit=predict(m3)

m4<-lme(cell_mean_temp ~ factor(rep)+factor(day)+factor(group)+factor(time_of_day), random=~1|id, data =f85data)
anova(m4)
summary(m4)
f85data$fit=predict(m4)
m5<-lme(cell_mean_temp ~ factor(rep) + factor(day)+factor(group)+factor(time_of_day),random=~1|id, data =f90data)
summary(m5)
anova(m5)
f90data$fit=predict(m5)
m6<-lme(cell_mean_temp ~ factor(rep) +factor(day)+factor(group)+factor(time_of_day), random=~1|id, data =f95data)
summary(m6)
anova(m6)
f95data$fit=predict(m6)
r=rbind(r85data,r90data,r95data)
f=rbind(f85data,f90data,f95data)

#####################plot#######
library(ggplot2)
r$room_temp <- factor(r$room_temp, levels =0:2, 
                      labels = c("85\u00B0F","90\u00B0F","95\u00B0F"))
f$room_temp <- factor(f$room_temp, levels =0:2, 
                      labels = c("85\u00B0F","90\u00B0F","95\u00B0F"))
##day effect 
r$day <- factor(r$day, levels = 1:7, 
                       labels = c("day1", "day2", "day3","day4","day5","day6","day7"))
rdayplot<-ggplot(data = r, aes(y = fit,x=day,fill=room_temp)) + 
  geom_boxplot()
print(rdayplot + labs(y="Rectal Temperature", x = "Ageday")
      +ggtitle("Ageday Effect on Rectal Temperature")
      +theme(plot.title = element_text(hjust = 0.5))
      +labs(fill= "Room\nTemperature"))


f$day <- factor(f$day, levels = 1:7, 
                labels = c("day1", "day2", "day3","day4","day5","day6","day7"))
fdayplot<-ggplot(data = f, aes(y = fit,x=day,fill=room_temp)) + 
  geom_boxplot()
print(fdayplot + labs(y="Flank Temperature", x = "Ageday")
      +ggtitle("Ageday Effect on Flank Temperature")
      +theme(plot.title = element_text(hjust = 0.5))
      +labs(fill= "Room\nTemperature"))
###time effect 
r$time_of_day <- factor(r$time_of_day, levels =0:1, 
                labels = c("AM","PM"))
rtimeplot<-ggplot(data = r, aes(y = fit,x=time_of_day,fill=room_temp)) + 
  geom_boxplot()
print(rtimeplot + labs(y="Rectal Temperature", x = "Time of the Day")
      +ggtitle("Time of the Day Effect on Rectal Temperature")
      +theme(plot.title = element_text(hjust = 0.5))
      +labs(fill= "Room\nTemperature"))

f$time_of_day <- factor(f$time_of_day, levels =0:1, 
                        labels = c("AM","PM"))
ftimeplot<- ggplot(data = f, aes(y = fit,x=time_of_day,fill=room_temp)) + 
  geom_boxplot()
print(ftimeplot + labs(y="Flank Temperature", x = "Time of the Day")
      +ggtitle("Time of the Day Effect on Flank Temperature")
      +theme(plot.title = element_text(hjust = 0.5))
      +labs(fill= "Room\nTemperature"))
####group effect 
r$group <- factor(r$group, levels =0:1, 
                        labels = c("Control(no air movement)","Treatment(w/ air movement)"))
rgpplot<-ggplot(data = r, aes(y = fit,x=group,fill=room_temp)) + 
  geom_boxplot()
print(rgpplot + labs(y="Rectal Temperature", x = "Group")
      +ggtitle("Group Effect(air movement) on Rectal Temperature")
      +theme(plot.title = element_text(hjust = 0.5))
      +labs(fill= "Room\nTemperature"))

f$group <- factor(f$group, levels =0:1, 
                        labels = c("Control(no air movement)","Treatment(w/ air movement)"))
fgpplot<-ggplot(data = f, aes(y = fit,x=group,fill=room_temp)) + 
  geom_boxplot()
print(fgpplot + labs(y="Flank Temperature", x = "Group")
      +ggtitle("Group Effect(air movement) on Flank Temperature")
      +theme(plot.title = element_text(hjust = 0.5))
      +labs(fill= "Room\nTemperature"))

#######################investigate covariate######################### 
rdata=data[data$loc=="0",]
fdata=data[data$loc=="1",]
boxplot(cell_mean_temp~group*room_temp, data=rdata)
library(dplyr)
rdata %>% 
  group_by(group,room_temp) %>% 
  summarise(n=n(),mean=mean(cell_mean_temp),sd=sd(cell_mean_temp))

fdata %>% 
  group_by(group,room_temp) %>% 
  summarise(n=n(),mean=mean(body_temp_diff),sd=sd(body_temp_diff))




l<-lme(pen_mean_temp ~ factor(rep) + factor(day)+factor(group), random=~1|rep/sensor, data =r85data)
anova(l) #group not significant
summary(l) 

l1<-lme(pen_mean_temp ~ factor(rep) + factor(day)+factor(group), random=~1|rep/sensor, data =f85data)
anova(l1) #group not significant
summary(l1) 

data$id=paste(data$sensor,data$rep)
# flank going from cell to the pen, chicks' temp (control group, no speed)is affected by room temp,different for control and treatment 
# rectal is the same 



