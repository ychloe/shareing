gm=rep(1,16)
bl=c(rep(-1,16))
B=c(rep(c(-1,1),8))
DQ=c(rep(-1,4),rep(1,8),rep(-1,4))
BCQ=c(1,-1,-1,1,1,-1,-1,1,-1,1,1,-1,-1,1,1,-1)
DEQ=c(1,-1,-1,1,1,-1,-1,1,-1,1,1,-1,-1,1,1,-1)
X1=data.frame(gm,bl,B,DQ)
X2=data.frame(BCQ,DEQ)
Xd=data.frame(gm,bl,B,DQ,BCQ,DEQ)

####################
B=rep(c("a","b","c","d"),each=256)
C=rep(rep(c("a","b","c","d"),each=64),4)
D=rep(rep(c("a","b","c","d"),each=16),16)
E=rep(rep(c("a","b","c","d"),each=4),64)
Q=rep(c("a","b","c","d"),256)
d=cbind(A,B,C,D,E)
View(d)
for(i in 1:nrow(d))
{
  print(d[i,])
}

B17=rep(c(1,1,-1,-1),each=256)
C17=rep(rep(c(1,1,-1,-1),each=64),4)
D17=rep(rep(c(1,1,-1,-1),each=16),16)
E17=rep(rep(c(1,1,-1,-1),each=4),64)
Q17=rep(c(1,1,-1,-1),256)
d17=data.frame(B17,C17,D17,E17,Q17)
d17$DQ17=d17$D17*d17$Q17
d17$BCQ17=d17$B17*d17$C17*d17$Q17
d17$DEQ17=d17$D17*d17$E17*d17$Q17
d17$bl17=rep(1,1024)
d17$gm17=rep(1,1024)
r17=data.frame(d17$gm17,d17$bl17,d17$B17,d17$DQ17,d17$BCQ17,d17$DEQ17)
colnames(r17)=c("gm","bl","B","DQ","BCQ","DEQ")
dd17=data.frame(B17,C17,D17,E17,Q17)
colnames(dd17)=c("B","C","D","E","Q")
View(r17)
r17a=data.frame(d17$gm17,d17$bl17,d17$B17,d17$DQ17)
r17b=data.frame(d17$BCQ17,d17$DEQ17)
colnames(r17a)=c("gm","bl","B","DQ")
colnames(r17b)=c("BCQ","DEQ")


B18=rep(c(1,-1,1,-1),each=256)
C18=rep(rep(c(1,-1,1,-1),each=64),4)
D18=rep(rep(c(1,-1,1,-1),each=16),16)
E18=rep(rep(c(1,-1,1,-1),each=4),64)
Q18=rep(c(1,-1,1,-1),256)
bl18=rep(1,1024)
d18=data.frame(B18,C18,D18,E18,Q18)
d18$DQ18=d18$D18*d18$Q18
d18$BCQ18=d18$B18*d18$C18*d18$Q18
d18$DEQ18=d18$D18*d18$E18*d18$Q18
d18$bl18=rep(1,1024)
d18$gm18=rep(1,1024)
r18=data.frame(d18$gm18,d18$bl18,d18$B18,d18$DQ18,d18$BCQ18,d18$DEQ18)
colnames(r18)=c("gm","bl","B","DQ","BCQ","DEQ")
View(r18)
r18a=data.frame(d18$gm18,d18$bl18,d18$B18,d18$DQ18)
r18b=data.frame(d18$BCQ18,d18$DEQ18)
colnames(r18a)=c("gm","bl","B","DQ")
colnames(r18b)=c("BCQ","DEQ")
dd18=data.frame(B18,C18,D18,E18,Q18)
colnames(dd18)=c("B","C","D","E","Q")

##########D#######
for (i in 1:1024)
{
  XD=rbind(Xd,r17[i,],r18[i,])
  XD=as.matrix(XD)
  a=t(XD)%*%XD
  det=det(a)
  output[[i]]=det
}
d=data.frame(dd17,dd18,output)
maxd=d[d$output==max(d$output),]
library(xtable)
xtable(maxd,caption = "optimal D setting")
##########Ds#######
for (i in 1:1024)
{
  X11=rbind(X1,r17a[i,],r18a[i,])
  X21=rbind(X2,r17b[i,],r18b[i,])
  X11=as.matrix(X11)
  X21=as.matrix(X21)
  dt=det(t(X21)%*%X21-t(X21)%*%X11%*%solve((t(X11)%*%X11))%*%t(X11)%*%X21)
  output[[i]]=dt
}
output
ds=data.frame(dd17,dd18,output)
maxds=ds[ds$output==max(ds$output),]
xtable(maxds,caption = "optimal Ds setting")