###################################
### Autor codigo: Nelson Brito
### 2019
###################################

rm(list=ls())
setwd("C:/Users/BYRON/Desktop/Investigación Caputo Duch/Pivotality Duch Caputo/Pivotality Experiment/script/Replication")
data <- read.dta("long_data.dta")
library(reshape2)
library(ggplot2)
library(plyr)
library(foreign)
library(pscl)

data <- read.dta("long_data.dta")
data <- data[!is.na(data$proposer_choice),]
data$proposer_choice3 <- factor(data$proposer_choice,labels=c("((9,1),(7,3))","((9,1),(5,5))","((7,3),(5,5))"))
#table(data$proposer_choice2,data$proposer_choice3)

plot.data <- data.frame(tapply(data$value,list(data$variable,data$proposer_choice3),mean))
plot.data$A <- as.numeric(gsub("BChoice(\\d)\\..+","\\1",row.names(plot.data)))
plot.data$situation <- as.numeric(gsub(".+(\\d)$","\\1",row.names(plot.data)))

#plot.data$A <- factor(plot.data$A,labels=c('A1','A2','A3'))
#plot.data$situation <- factor(plot.data$situation,labels=c('(1,1,1)','(1,1,2)','(1,2,1)','(2,1,1)','(1,2,2)','(2,1,2)','(2,2,1)','(2,2,2)'))
vote.record <- c('u-u-u','u-u-e','u-e-u','e-u-u','u-e-e','e-u-e','e-e-u','e-e-e')
plot.data$situation2 <- factor(plot.data$situation, labels=vote.record)
plot.data$voter <- factor(plot.data$A,labels=paste("Voter",1:3))
names(plot.data)[1:3] <- c("avg.punish97","avg.punish95","avg.punish75")

tab.data <- data.frame(proposal = c("((9,1),(7,3))",rep(NA,7),"((9,1),(5,5))",rep(NA,7),"((7,3),(5,5))",rep(NA,7)),
                       vote.record=rep(vote.record,3),
                       Voter1 = c(plot.data$avg.punish97[ plot.data$voter=="Voter 1"],
                                  plot.data$avg.punish95[ plot.data$voter=="Voter 1"],
                                  plot.data$avg.punish75[ plot.data$voter=="Voter 1"]),
                       Voter2 = c(plot.data$avg.punish97[ plot.data$voter=="Voter 2"],
                                  plot.data$avg.punish95[ plot.data$voter=="Voter 2"],
                                  plot.data$avg.punish75[ plot.data$voter=="Voter 2"]),
                       Voter3 =c(plot.data$avg.punish97[ plot.data$voter=="Voter 3"],
                                 plot.data$avg.punish95[ plot.data$voter=="Voter 3"],
                                 plot.data$avg.punish75[ plot.data$voter=="Voter 3"]))

library(xtable)
print(xtable(tab.data),include.rownames=F)



#proposer punishment
p.data <- data[ data$proposer_dummy==1,]
xtable(tapply(p.data$value,list(p.data$proposer_choice3,p.data$dm_choice),mean))

#logit
data <- read.dta("long_data.dta")
data$punish.binary <- ifelse(data$value>0,1,0)
data$pivotal_to_unequal <- data$pivotal*(1-data$equitable_outcome)
data$unequal_outcome <- 1 - data$equitable_outcome
data$choice_unequal <- as.numeric(data$dm_choice==1)


models <- NULL
models[[1]] <- glm(punish.binary ~  choice_unequal ,
                   data=data[ is.na(data$proposer_choice),], family="binomial" )  
for(i in 1:3){
  models[[i+1]] <- glm(punish.binary ~ proposer_dummy + choice_unequal,
                       data=data[ data$proposer_choice==i,], family="binomial" )  
}
models[[5]] <- glm(punish.binary ~  choice_unequal + pivotal_to_unequal,
                   data=data[ is.na(data$proposer_choice),], family="binomial" )  
for(i in 1:3){
  models[[i+5]] <- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal,
                       data=data[ data$proposer_choice==i,], family="binomial" )
}

library(texreg)
texreg(models,booktabs=T, 
       custom.coef.names = c("Constant","Choice Uequal","Proposer","Pivotal to Unequal"),
       reorder.coef = c(2:4,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment",
       caption.above=T) #dcolumn=T,

## 
models.2 <- NULL
for(i in 1:4) models.2[[i]] <- models[[i+4]]
texreg(models.2,booktabs=T, 
       custom.coef.names = c("Constant","Choice Uequal","Proposer","Pivotal to Unequal"),
       reorder.coef = c(2:4,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment",
       caption.above=T)


## include intention_unkind
data$situation <-as.numeric(gsub(".+(\\d)$","\\1",as.character(data$variable)))
unkind.data <- data.frame(situation = rep(1:8,3),
                          dm_number = rep(1:3,each=8),
                          intention_unkind = c(1,1,1,0,1,0,0,0,
                                               1,1,0,1,0,1,0,0,
                                               0,0,1,1,0,0,0,0))
data2 <- merge(data, unkind.data,by=c("situation","dm_number"))

for(i in 1:3){
  print(summary(glm(punish.binary ~ proposer_dummy + choice_unequal +intention_unkind+ pivotal_to_unequal,
                    data=data2[ data2$proposer_choice==i,], family="binomial" )))
}
# not cool result...
