library(R2jags)
library(tidyverse)
LaLigaData <- read.csv("la_liga_dataset.csv", header = TRUE, sep = ",")

Formation <-LaLigaData$Formation
TeamNum <- LaLigaData$Team.ID
N <- LaLigaData$X
MeanXG <- LaLigaData$Mean_xg
TotalXG <- LaLigaData$Total_xg
PassAccuracy <- LaLigaData$pass_accuracy
Goals <- LaLigaData$Goals
PossRatio <- LaLigaData$possession_ratio
TargetSt <- LaLigaData$Shots_tg

mean(MeanXG)
length(N)

La1 <- LaLigaData %>% 
  filter(Formation == "433")

La2 <- LaLigaData %>% 
  filter(Formation == "442")

La3 <- LaLigaData %>% 
  filter(Formation == "4321")

MeanXG1 <- La1$Mean_xg
TotalXG1 <- La1$Total_xg
Goals1 <- La1$Goals
PassAcc1 <- La1$pass_accuracy
Poss1 <- La1$possession_ratio
TargetSt1 <- La1$Shots_tg
MinuteXG1 <- La1$xg_per_minute

MeanXG2 <- La2$Mean_xg
TotalXG2 <- La2$Total_xg
Goals2 <- La2$Goals
PassAcc2 <- La2$pass_accuracy
Poss2 <- La2$possession_ratio
TargetSt2 <- La2$Shots_tg
MinuteXG2 <- La2$xg_per_minute

MeanXG3 <- La3$Mean_xg
TotalXG3 <- La3$Total_xg
Goals3 <- La3$Goals
PassAcc3 <- La3$pass_accuracy
Poss3 <- La3$possession_ratio
TargetSt3 <- La3$Shots_tg
MinuteXG3 <- La3$xg_per_minute
##################
mean(quantile((LaLigaData$Goals),c(0.05,0.95)))

mean(Goals)
##################
LAModel1 <- "model {
  for(i in 1:199){
    Goals2[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * TargetSt2[i] + beta2 * MeanXG2[i]  + beta3 * Poss[i] + beta4 * PassAcc2[i]
mean.default()
  }
  beta0 ~ dexp(1)
  beta1 ~ dnorm(0,1/100)
  beta2 ~ dnorm(0,1/100)
  beta3 ~ dnorm(0,1/100)
  beta4 ~ dnorm(0,1/100)
}
"

LA.sim1 <- jags(
  data=c('Goals2','TargetSt2','MeanXG2','Poss2','PassAcc2'),
  parameters.to.save=c('beta0','ppd','beta1','beta2','beta3','beta4'),
  model.file=textConnection(LAModel1),
  n.iter=12000,
  n.burnin=2000,
  n.chains=2,
  n.thin=2
)

plot(LA.sim1$BUGSoutput$sims.matrix[,"beta1"],type='l')
plot(LA.sim1$BUGSoutput$sims.matrix[,"beta2"],type='l')
plot(LA.sim1$BUGSoutput$sims.matrix[,"beta3"],type='l')
plot(LA.sim1$BUGSoutput$sims.matrix[,"beta4"],type='l')

acf(LA.sim1$BUGSoutput$sims.matrix[,"beta1"])
acf(LA.sim1$BUGSoutput$sims.matrix[,"beta2"])
acf(LA.sim1$BUGSoutput$sims.matrix[,"beta3"])
acf(LA.sim1$BUGSoutput$sims.matrix[,"beta4"])
###################
LAModel <- "model {
  for(i in 1:480){
    Goals1[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * TargetSt1[i] + beta2 * MeanXG1[i]  + beta3 * Poss1[i] + beta4 * PassAcc1[i]
    ppd[i] ~ dpois(lambda[i]) 
  }
  beta0 ~ dexp(1)
  beta1 ~ dnorm(0,1/100)
  beta2 ~ dnorm(0,1/100)
  beta3 ~ dnorm(0,1/100)
  beta4 ~ dnorm(0,1/100)
}
"

LA.sim <- jags(
  data=c('Goals1','TargetSt1','MeanXG1','Poss1','PassAcc1'),
  parameters.to.save=c('beta0','ppd','beta1','beta2','beta3','beta4'),
  model.file=textConnection(LAModel),
  n.iter=12000,
  n.burnin=2000,
  n.chains=2,
  n.thin=2
)

plot(LA.sim$BUGSoutput$sims.matrix[,"beta1"],type='l')
plot(LA.sim$BUGSoutput$sims.matrix[,"beta2"],type='l')
plot(LA.sim$BUGSoutput$sims.matrix[,"beta3"],type='l')
plot(LA.sim$BUGSoutput$sims.matrix[,"beta4"],type='l')

acf(LA.sim$BUGSoutput$sims.matrix[,"beta1"])
acf(LA.sim$BUGSoutput$sims.matrix[,"beta2"])
acf(LA.sim$BUGSoutput$sims.matrix[,"beta3"])
acf(LA.sim$BUGSoutput$sims.matrix[,"beta4"])

mean(LA.sim1$BUGSoutput$sims.matrix[,7:205])
mean(LA.sim$BUGSoutput$sims.matrix[,7:486])
mean(LA.sim2$BUGSoutput$sims.matrix[,7:15])

mean(quantile((LA.sim$BUGSoutput$sims.matrix[,7:205]),c(0.05,0.95)))
####################
LAModel2 <- "model {
  for(i in 1:9){
    Goals3[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * TargetSt3[i] + beta2 * MeanXG3[i]  + beta3 * Poss3[i] + beta4 * PassAcc3[i]
    ppd[i] ~ dpois(lambda[i]) 
  }
  beta0 ~ dexp(1)
  beta1 ~ dnorm(0,1/100)
  beta2 ~ dnorm(0,1/100)
  beta3 ~ dnorm(0,1/100)
  beta4 ~ dnorm(0,1/100)
}
"

LA.sim2 <- jags(
  data=c('Goals3','TargetSt3','MeanXG3','Poss3','PassAcc3'),
  parameters.to.save=c('beta0','ppd','beta1','beta2','beta3','beta4'),
  model.file=textConnection(LAModel2),
  n.iter=102000,
  n.burnin=2000,
  n.chains=2,
  n.thin=2
)

plot(LA.sim2$BUGSoutput$sims.matrix[,"beta1"],type='l')
plot(LA.sim2$BUGSoutput$sims.matrix[,"beta2"],type='l')
plot(LA.sim2$BUGSoutput$sims.matrix[,"beta3"],type='l')
plot(LA.sim2$BUGSoutput$sims.matrix[,"beta4"],type='l')

acf(LA.sim2$BUGSoutput$sims.matrix[,"beta1"])
acf(LA.sim2$BUGSoutput$sims.matrix[,"beta2"])
acf(LA.sim2$BUGSoutput$sims.matrix[,"beta3"])
acf(LA.sim2$BUGSoutput$sims.matrix[,"beta4"])
#################
min(LA.sim$BUGSoutput$sims.matrix[,'deviance']) + 5 * log(480)
min(LA.sim1$BUGSoutput$sims.matrix[,'deviance']) + 5 * log(199)
min(LA.sim2$BUGSoutput$sims.matrix[,'deviance']) + 5 * log(9)
