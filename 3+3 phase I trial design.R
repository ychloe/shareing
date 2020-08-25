Do3plus3 = function ( pDLT, currentdose, lastdose ) {
  myreturn = c(NA,NA,NA,NA); # MTD, samples used, DLTs, escalation (1=yes, 0=no)
  outcome = 0;
  x1 = rbinom(1,3,pDLT);
  x2 = rbinom(1,3,pDLT);
  ####4 cases 1 tocxity, stop, continouse..
  if (x1>1) { myreturn = c(lastdose,3,x1,0); }
  else if (x1==0) { myreturn = c(currentdose,3,0,1); }
  else if (x1==1 && x2==0) { myreturn = c(currentdose,6,x1+x2,1); }
  else if (x1==1 && x2>0) { myreturn = c(lastdose,6,x1+x2,0); }
} 

#3mg
mcruns = 100000
myresmat = matrix(rep(0,mcruns*4),ncol=4);

for (i in 1:mcruns) {
  myresmat[i,] = Do3plus3(0.01,3,0);
}

# MTD
summary(myresmat[,1])

# Number of samples used
summary(myresmat[,2])

# Number of DLTs
summary(myresmat[,3])

# Dose escalation y/n
summary(myresmat[,4])

#6mg
for (i in 1:mcruns) {
  if (myresmat[i,4]==1) {
    newres = Do3plus3(0.03,6,3);
    myresmat[i,1] = newres[1];
    myresmat[i,4] = newres[4];
    myresmat[i,2:3] = myresmat[i,2:3] + newres[2:3];
  }
}

# MTD est
summary(myresmat[,1])

# Number of samples used
summary(myresmat[,2])

# Number of DLTs
summary(myresmat[,3])

# Dose escalation y/n
summary(myresmat[,4])

#9mg
for (i in 1:mcruns) {
  if (myresmat[i,4]==1) {
    newres = Do3plus3(0.25,9,6);
    myresmat[i,1] = newres[1];
    myresmat[i,4] = newres[4];
    myresmat[i,2:3] = myresmat[i,2:3] + newres[2:3];
  }
}

# MTD est
summary(myresmat[,1])

# Number of samples used
summary(myresmat[,2])

# Number of DLTs
summary(myresmat[,3])

# Dose escalation y/n
summary(myresmat[,4])

#12mg
for (i in 1:mcruns) {
  if (myresmat[i,4]==1) {
    newres = Do3plus3(0.99,12,9);
    myresmat[i,1] = newres[1];
    myresmat[i,4] = newres[4];
    myresmat[i,2:3] = myresmat[i,2:3] + newres[2:3];
  }
}

# MTD est
summary(myresmat[,1])

# Number of samples used
summary(myresmat[,2])

# Number of DLTs
summary(myresmat[,3])

# Dose escalation y/n
summary(myresmat[,4])

















