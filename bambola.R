library(readxl)
bambola <- read_excel("C:/Users/patrickverdun/Desktop/bambola.xlsx")
View(bambola)
library(leaps)
dat=bambola[bambola$LIVE==1,]
dat<-dat[,c(-1,-4,-7,-8,-9)]
View(dat)
##################
#(e)model 
hist(bambola$HT)
hist(dat$HT) #height is not normal distributted, it has long tail, need transformation 
hist(log10(dat$HT)) #normally distributted 

#model selection(backward)
aggregate(log10(dat$HT),list(location=dat$LOC,variety=dat$VAR,color=dat$CLR),length)
aggregate(log10(dat$HT),list(location=dat$LOC,variety=dat$VAR,color=dat$CLR),mean)
#since each of the combine doesn't have exactly the same sample size. However, the design is almost balanced, as can be seen via the MEANS option
#typeIII anova significant level=0.05
full = lm(log10(HT)~factor(LOC)*factor(VAR)*factor(CLR), data=dat)
anova(lm(log10(HT)~factor(LOC)*factor(VAR)*factor(CLR), data=dat,type=3))
anova(lm(log10(HT)~factor(LOC)+factor(VAR)+factor(CLR), data=dat,type=3))#color is not significant 
anova(lm(log10(HT)~factor(LOC)*factor(VAR), data=dat,type=3)) # interaction is not significant 
anova(lm(log10(HT)~factor(LOC)+factor(VAR), data=dat,type=3))
#final model
model=lm(log10(HT)~factor(LOC)+factor(VAR),dat)
summary(model) #RMSE=0.419

#most favoriate by grand mean model 
mean(log10(dat$HT)) ## grand mean
aggregate(log10(dat$HT),list(location=dat$LOC),length)
aggregate(log10(dat$HT),list(location=dat$LOC),mean)
aggregate(log10(dat$HT),list(location=dat$LOC),sd)
aggregate(log10(dat$HT),list(variety=dat$VAR),length)
aggregate(log10(dat$HT),list(variety=dat$VAR),mean)
aggregate(log10(dat$HT),list(variety=dat$VAR),sd)
#grand=2.451681
#loc=y_loc-grand
#FL=2.511202-2.451681=0.059521 & LA=2.396883-2.451681=-0.054798
#Var=y_var-grand
#A=2.470413-2.451681=0.018732 & B=2.508327-2.451681=0.056646
#C=2.399527-2.451681=-0.052154 & D=2.427863-2.451681=-0.023818
#inter=y_in-yloc-yvar-grand
#FL:A=2.493189-2.511202-2.470413+2.451681=-0.036745
#FL:B=2.525780-2.511202-2.508327+2.451681=-0.042068
#FL:C=2.501912-2.511202-2.399527+2.451681=0.042864
#FL:D=2.522641-2.511202-2.427863+2.451681=0.035257
#LA:A=2.450272-2.396883-2.470413+2.451681=0.034657
#LA:B=2.492460-2.396883-2.508327+2.451681=-0.038931
#LA:C=2.305017-2.396883-2.399527+2.451681=-0.039712
#LA:D=2.336245-2.396883-2.427863+2.451681=-0.03682

#log10(HT)=grand+loc+var+RMSE, transform HT back to real height
#HT(FL:A)=2.451681+0.059521+0.018732=2.529934, realHT=10^2.529934=338.7927
#HT(FL:B)=2.451681+0.059521+0.056646=2.567848, realHT=10^2.567848=369.6988 ***
#HT(FL:C)=2.451681+0.059521-0.052154=2.459048, realHT=10^2.459048=287.7716
#HT(FL:D)=2.451681+0.059521-0.023818=2.487384, realHT=10^2.487384=307.1737
#HT(LA:A)=2.451681-0.054798+0.018732=2.415615, realHT=10^2.415615=260.3844
#HT(LA:B)=2.451681-0.054798+0.056646=2.453529, realHT=10^2.453529=284.1378
#HT(LA:C)=2.451681-0.054798-0.052154=2.344729, realHT=10^2.344729=221.1714
#HT(LA:D)=2.451681-0.054798-0.023818=2.373065, realHT=10^2.373065=236.0832
#Most favoriable Variety-Location combination is (B,FL), since the estimate Height of cane is the greatest.

#(a)color ditribution
sum(dat$CLR=="W") #566
sum(dat$CLR=="P") #279
sum(dat$CLR=="R") #75
chisq.test(c(566,279,75), p=c(57,36,7)/100) #chisq test for Goodness of Fit
#p-value = 0.001312, color distribution is signifiacntly different from stated


#(b)non-germinating
datn<-bambola[,c(-1,-2,-5,-6,-7,-8,-9)]
sum(datn$VAR=='A' &datn$LIVE==0) #28
sum(datn$VAR=='B' &datn$LIVE==0) #25
sum(datn$VAR=='C' &datn$LIVE==0) #31
sum(datn$VAR=='D' &datn$LIVE==0) #20
table1=matrix(c(28,25,31,20,228,231,225,236),ncol=2)
colnames(table1)=c("No","Yes")
rownames(table1)=c("A","B","C","D")
table1
chisq.test(table1) 
prop.test(table1)
#H0:A=B=C=D, p-value =0.4193, cannot reject, all of them are the same.
prob=data.frame(var=c('A','B','C','D'),
                 N=c(256,256,256,256),no=c(28,25,31,20))
modn<- glm(cbind(no, N-no)~factor(var),family = binomial, data = prob)
summary(modn) 
#ln(p_no/1-p_no)=-2.0971-0.1264*I(var=B)+0.1150*I(var=C)-0.3710*I(D)
#p_no=e^(-2.0971-0.1264*I(var=B)+0.1150*I(var=C)-0.3710*I(D))/1+e^(-2.0971-0.1264*I(var=B)+0.1150*I(var=C)-0.3710*I(D))
#prob no for A=e^(-2.0971)/1+e^(-2.0971)=0.109379
#prob no for B=e^(-2.0971-0.1264)/1+e^(-2.0971-0.1264)=0.09765994
#prob no for C=e^(-2.0971+0.1150)/1+e^(-2.0971+0.1150)=0.1210952
#prob no for D=e^(-2.0971-0.3710)/1+e^(-2.0971-0.3710)=0.07812497

#(c)
datn1<-bambola[,c(-1,-3,-5,-6,-7,-8,-9)]
sum(datn1$LOC=='FL' &datn$LIVE==1) #441
sum(datn1$LOC=='LA' &datn$LIVE==1) #479
sum(datn1$LOC=='FL') #512
sum(datn1$LOC=='LA') #512
table2=matrix(c(441,479,71,33),ncol=2)
colnames(table2)=c("Yes","No")
rownames(table2)=c("FL","LA")
table2
chisq.test(table2)
prop.test(table2)
#H0:FL=LA, p-value = 0.0001293, , they are not same, significant difference
prob1=data.frame(loc=c('FL','LA'),
                n=c(512,512),yes=c(441,479))
mody<- glm(cbind(yes, n-yes)~factor(loc),family = binomial, data = prob1)
summary(mody)
#ln(p_yes/1-p_yes)=1.8264+0.8488*I(loc=LA)
#p_yes=e^(1.8264+0.8488*I(loc=LA))/1+e^(1.8264+0.8488*I(loc=LA))
#prob yes for FL=e^(1.8264)/1+e^(1.8264)=0.8613323
#prob yes for LA=e^(1.8264+0.8488)/1+e^(1.8264+0.8488)=0.9355473 ->higher germinateion rate

#(d)
install.packages("vcd")
install.packages("grid")
library(vcd)
FLA=dat[dat$LOC=="FL"&dat$VAR=="A",]

FLB=dat[dat$LOC=="FL"&dat$VAR=="B",]

FLC=dat[dat$LOC=="FL"&dat$VAR=="C",]

FLD=dat[dat$LOC=="FL"&dat$VAR=="D",]

LAA=dat[dat$LOC=="LA"&dat$VAR=="A",]

LAB=dat[dat$LOC=="LA"&dat$VAR=="B",]

LAC=dat[dat$LOC=="LA"&dat$VAR=="C",]

LAD=dat[dat$LOC=="LA"&dat$VAR=="D",]

tableloc=matrix(c(41,34,400,445),ncol=2)
colnames(tableloc)=c("R","NonR")
rownames(tableloc)=c("FL","LA")
tableloc
chisq.test(tableloc) #p-value = 0.2726 cannot reject, location red are same
prop.test(tableloc) #FL=0.09297052 LA=0.07098121 

tablevar=matrix(c(14,13,27,21,214,218,198,215),ncol=2)
colnames(tablevar)=c("R","NonR")
rownames(tablevar)=c("A","B","C","D")
tablevar
chisq.test(tablevar) #p-value = 0.04981 reject, var significant different
prop.test(tablevar) #A=0.06140351 B=0.05627706 C=0.12000000 D=0.08898305, C is higher 

colordist = c(62,85,59,56,70,89,77,68,40,18,33,47,42,26,29,44,5,7,16,13,9,6,11,8)
dim(colordist) = c(4, 2, 3)
dimnames(colordist) =
  list(Variety = c("A", "B","C","D"),
       Location = c("Florida", "Louisiana"),
       Color = c("White", "Pink","Red"))
mosaicplot(colordist, col = c("white","Pink","Red"),
           main = "Color distribution across Variety and Location")

#So, from graph (FL,C) produces more red canes, but from test FL produce significantly more than LA.

tablelocvar=matrix(c(5,7,16,13,9,6,11,8,102,103,92,103,112,115,106,112),ncol=2,nrow=8)
colnames(tablelocvar)=c("R","NonR")
rownames(tablelocvar)=c("FLA","FLB","FLC","FLD","LAA","LAB","LAC","LAD")
tablelocvar
chisq.test(tablelocvar) #p-value = 0.08884 cannot reject, no significant difference for each combine 
prop.test(tablelocvar) #A=0.06140351 B=0.05627706 C=0.12000000 D=0.08898305, C is higher 



#######
#(f) yields highest total value
aggregate(bambola$TOTAL,list(location=bambola$LOC,variety=bambola$VAR),length)
aggregate(bambola$TOTAL,list(location=bambola$LOC,variety=bambola$VAR),mean) #highest=(FL,D)
value=matrix(c(102.48984,108.15469,110.70078,128.45391,121.79531, 106.98516,70.29063,76.65937),ncol=2)
colnames(value)=c("FL","LA")
rownames(value)=c("A","B","C","D")
value
chisq.test(value) #p-value = 0.000624
sd=aggregate(bambola$TOTAL,list(location=bambola$LOC,variety=bambola$VAR),sd)
SE=sd$x/sqrt(128) #SE is from $11-15 are large, most of them are not significant different, 
#mean for LAC and LAD is small and Se are small, they are significant worser than other.