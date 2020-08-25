library(rjags)
library(MCMCvis)
setwd("C:/Users/chloe/Desktop/final-6350")
salaries <- read.table("salaries.txt", header=TRUE)

######Exploratory data analysis
hist(salaries$Salary, main="Salary of Engineering Department Professors", 
     xlab = "Salaries (in 1000's of dollars)")
# Table with sample sizes, means, and standard deviations by school
# (could also use tapply function very similarly)
attach(salaries)
by(Salary,Dept,function(x) { c(SAMPSIZE=length(x), MEAN=mean(x), SD=sd(x) )})
# Create boxplot of time spent on homework by school
boxplot(Salary ~ Dept, xlab="Department", ylab="Salary (in 1000's of dollars)",
        main="Salary of Engineering Professors by Department")

######Data Analysis
###recode the dept
salaries$dept=NULL
salaries$dept[salaries$Dept == "BM"] <- 1
salaries$dept[salaries$Dept == "CH"] <- 2
salaries$dept[salaries$Dept == "CS"] <- 3
salaries$dept[salaries$Dept == "CV"] <- 4
salaries$dept[salaries$Dept == "EE"] <- 5
salaries$dept[salaries$Dept == "IN"] <- 6
salaries$dept[salaries$Dept == "MC"] <- 7
salaries$dept[salaries$Dept == "MS"] <- 8
salaries<-salaries[,-2]

## Save sample statistics as variables
# Overall data
n <- dim(salaries)[1]      # overall sample size
y_bar <- mean(Salary)   # overall sample mean
s <- sd(Salary)         # overall sample sd

# By group
J <- length(unique(dept))    # number of groups
nj <- rep(0,J)
yj_bar <- rep(0,J)
sj <- rep(0,J)
for(j in 1:J){
  nj[j] <- sum(dept==j)                # group sample size
  yj_bar[j] <- mean(Salary[dept==j])   # group sample mean
  sj[j] <- sd(Salary[dept==j])         # group sample sd
}


##################################
## Model 1 (no group structure) ##
##################################
## (1) Create data list
dataList1 <- list(
  "Salary" = Salary,
  "n" = n)

## (2) Specify list of parameter(s) to be monitored
parameters1 <- c("theta","sig2")

## (3) Specify initial values for parameter(s) in Metropolis-Hastings algorithm
initsValues1 <- list(
  "theta" = y_bar,
  "inv_sig2" = 1/s^2
)

## (4) Specify parameters for running Metropolis-Hastings algorithm
adaptSteps <- 10000              # number of steps to "tune" the samplers
burnInSteps <- 20000             # number of steps to "burn-in" the samplers
nChains <- 5                    # number of chains to run
numSavedSteps <- 50000          # total number of steps in chains to save
thinSteps <- 10                  # number of steps to "thin" (1 = keep every step)
nIter <- ceiling((numSavedSteps*thinSteps)/nChains) 	# steps per chain

## (5) Create, initialize, and adapt the model
# This will require you to create a separate .txt file which specifies
# the model
jagsModel1 <- jags.model("model1.txt", 
                         data = dataList1, 
                         inits = initsValues1, 
                         n.chains = nChains, 
                         n.adapt = adaptSteps)

## (6) Burn-in the algorithm
if(burnInSteps>0){
  cat( "Burning in the MCMC chain...\n")
  update(jagsModel1, n.iter = burnInSteps)
}

## (7) Run MCMC algorithm
cat("Sampling final MCMC chain...\n" )
codaSamples1 <- coda.samples(jagsModel1, 
                             variable.names = parameters1, 
                             n.iter = nIter, 
                             thin = thinSteps)

## (8) Diagnose convergence and plot posterior densities
par(ask=T)
plot(codaSamples1)

## (9) Calculate numerical summaries for the posterior samples
summary(codaSamples1)

###############################################
## Model 2 (models each group independently) ##
###############################################
## (1) Create data list
dataList2 <- list(
  "Salary" = Salary,
  "dept" = dept,
  "n" = n,
  "J" = J)

## (2) Specify list of parameter(s) to be monitored
parameters2 <- c("theta","sig2")

## (3) Specify initial values for parameter(s) in Metropolis-Hastings algorithm
initsValues2 <- list(
  "theta" = rep(y_bar,J),
  "inv_sig2" = rep(1/s^2,J)
)

## (4) Specify parameters for running Metropolis-Hastings algorithm
adaptSteps <- 10000              # number of steps to "tune" the samplers
burnInSteps <- 20000             # number of steps to "burn-in" the samplers
nChains <- 1                    # number of chains to run
numSavedSteps <- 50000          # total number of steps in chains to save
thinSteps <- 10                  # number of steps to "thin" (1 = keep every step)
nIter <- ceiling((numSavedSteps*thinSteps)/nChains) 	# steps per chain

## (5) Create, initialize, and adapt the model
# This will require you to create a separate .txt file which specifies
# the model
jagsModel2 <- jags.model("model2.txt", 
                         data = dataList2, 
                         inits = initsValues2, 
                         n.chains = nChains, 
                         n.adapt = adaptSteps)

## (6) Burn-in the algorithm
if(burnInSteps>0){
  cat( "Burning in the MCMC chain...\n")
  update(jagsModel2, n.iter = burnInSteps)
}

## (7) Run MCMC algorithm
cat("Sampling final MCMC chain...\n" )
codaSamples2 <- coda.samples(jagsModel2, 
                             variable.names = parameters2, 
                             n.iter = nIter, 
                             thin = thinSteps)

## (8) Diagnose convergence and plot posterior densities
# This can be tedious for complex models, since we have 16 
# parameters to monitor
par(ask=T)
plot(codaSamples2)

## (9) Calculate numerical summaries for the posterior samples
summary(codaSamples2)

## (10) Retrieve posterior samples for later use
mcmcChain2 <- as.matrix(codaSamples2)

## (11) Other plots
# Boxplot of theta posterior samples
thetaSamples2 <- matrix(NA, numSavedSteps, J)
for(j in 1:J){
  thetaSamples2[,j] <- mcmcChain2[, paste("theta[",j,"]", sep="")]
}

par(mfrow=c(1,1), ask=F)
boxplot(as.data.frame(thetaSamples2),
        names = as.character(1:J),
        xlab = "Department",
        main = "Posterior samples of theta")
abline(h=0)

# 50% (thin line) and 95% (thick line) credible interval "caterpillar" plots
# dot is the posterior median
MCMCplot(codaSamples2, params = "theta",
         main = "Posterior CIs for theta", ref = NULL)
MCMCplot(codaSamples2, params = "sig2", 
         main = "Posterior CIs for sig2", ref = NULL)


##################################
## Model 3 (hierarchical model) ##
##################################
## (1) Create data list
dataList3 <- list(
  "Salary" = Salary,
  "dept" = dept,
  "n" = n,
  "J" = J)

## (2) Specify list of parameter(s) to be monitored
parameters3 <- c("theta","mu","sig2","tau2")

## (3) Specify initial values for parameter(s) in Metropolis-Hastings algorithm
initsValues3 <- list(
  "theta" = rep(y_bar,J),
  "mu" = y_bar,
  "sig" = s,
  "tau" = 1
)

## (4) Specify parameters for running Metropolis-Hastings algorithm
adaptSteps <- 10000              # number of steps to "tune" the samplers
burnInSteps <- 20000             # number of steps to "burn-in" the samplers
nChains <- 3                    # number of chains to run
numSavedSteps <- 50000          # total number of steps in chains to save
thinSteps <- 10                  # number of steps to "thin" (1 = keep every step)
nIter <- ceiling((numSavedSteps*thinSteps)/nChains) 	# steps per chain

## (5) Create, initialize, and adapt the model
# This will require you to create a separate .txt file which specifies
# the model
jagsModel3 <- jags.model("model.txt", 
                         data = dataList3, 
                         inits = initsValues3, 
                         n.chains = nChains, 
                         n.adapt = adaptSteps)

## (6) Burn-in the algorithm
if(burnInSteps>0){
  cat( "Burning in the MCMC chain...\n")
  update(jagsModel3, n.iter = burnInSteps)
}

## (7) Run MCMC algorithm
cat("Sampling final MCMC chain...\n" )
codaSamples3 <- coda.samples(jagsModel3, 
                             variable.names = parameters3, 
                             n.iter = nIter, 
                             thin = thinSteps)

## (8) Diagnose convergence and plot posterior densities
# This can be tedious for complex models, since we have 16 
# parameters to monitor
par(ask=T)
plot(codaSamples3)

## (9) Calculate numerical summaries for the posterior samples
summary(codaSamples3)

## (10) Retrieve posterior samples for later use
mcmcChain3 <- as.matrix(codaSamples3)

## (11) Other plots
# 50% (thin line) and 95% (thick line) credible interval "caterpillar" plots
# dot is the posterior median
MCMCplot(codaSamples3, params = c("theta","mu"),
         main = "Posterior CIs for mean parameters", ref = NULL)
MCMCplot(codaSamples3, params = c("sig2","tau2"), 
         main = "Posterior CIs for variance parameters", ref = NULL)

