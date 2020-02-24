###################################
### Autor codigo: Nelson Brito
### 2019
###################################

rm(list=ls())
setwd("C:/Users/BYRON/Desktop/Investigación Caputo Duch/Pivotality Duch Caputo 2019/Pivotality Experiment 2019 modificado/Data")

data <- read.csv('subject.table.Pilot_Session123.csv',as.is=T)

library(ggplot2)
library(reshape2)
library(foreign)
library(texreg)

data$chosen.proposal <- factor(data$proposal,labels=c("((9,1),(7,3))","((9,1),(5,5))","((7,3),(5,5))"))
data$winning_allocation_labeled <- factor(data$winning_allocation_real,labels=c("(9,1)","(7,3)","(5,5)"))
#pivotality 
names(data)
data$situation <- NA
data$situation <- with(data,(a1choice-1)*4+(a2choice-1)*2+(a3choice-1))+1
table(data$situation)
data$situation <- ifelse(data$situation==4,5,ifelse(data$situation==5,4,data$situation))
table(data$situation)
pivot.table <- c(2,2,3,3,3,3,2,2)
data$pivot <- pivot.table[data$situation]


b.data <- subset(data,subset = role==1)
b.data$proposer.punishment <-0
for(i in 1:nrow(b.data))b.data$proposer.punishment[i] <- b.data[i,21+b.data$proposer[i]]
b.data$proposer.is.pivot <- factor(as.numeric(b.data$proposer==b.data$pivot))
b.data$proposer.is <- factor(b.data$proposer,labels=c("A1","A2","A3"))
b.data$proposer.vote <- NA
for(i in 1:nrow(b.data))b.data$proposer.vote[i] <- b.data[i,18+b.data$proposer[i]]
b.data$proposer.vote <- factor(b.data$proposer.vote,labels=c("(9,1)","(7,3)","(5,5)"))

vote.record <- c('u-u-u','u-u-e','u-e-u','e-u-u','u-e-e','e-u-e','e-e-u','e-e-e')
b.data$situation2 <- factor(b.data$situation, labels=vote.record)



#proposer punishment
b.data$proposer.vote2 <- NA
for(i in 1:nrow(b.data))b.data$proposer.vote2[i] <- b.data[i,15+b.data$proposer[i]]

sub.data <- b.data[,c(22:24,11,13,14,163,164,166)]
sub.data$id <- 1:nrow(sub.data)

long.data <- melt(sub.data,measure.vars = names(sub.data)[1:3])
long.data$punish.binary <- as.numeric(long.data$value>0)
long.data$target <- as.numeric(gsub('\\D',"",long.data$variable))
long.data$is.proposer <- as.numeric(long.data$target==long.data$proposer)
long.data$is.pivot <- as.numeric(long.data$target==long.data$pivot)
long.data$pivotal_to_unequal <- long.data$is.pivot*(2-long.data$winning_allocation)
long.data$outcome_unequal<-(2-long.data$winning_allocation)



sub.data.2 <- b.data[,c(16:18)]
sub.data.2$id <- 1:nrow(sub.data.2)
long.data.2 <- melt(sub.data.2, id.vars = "id")
long.data.2$target <- as.numeric(gsub('\\D',"",long.data.2$variable))
names(long.data.2)[3] <- "achoice"
long.data.comp <- merge(long.data,long.data.2,by=c("id","target"))
long.data.comp$choice_unequal <- as.numeric(long.data.comp$achoice==1)


## New regresions

models <- NULL

models[[1]] <- glm(punish.binary ~ is.proposer,
                   data=long.data.comp[ long.data.comp$proposal==1,], family="binomial" )

models[[2]] <- glm(punish.binary ~ is.proposer + choice_unequal,
                   data=long.data.comp[ long.data.comp$proposal==1,], family="binomial" )

models[[3]] <- glm(punish.binary ~ is.proposer + choice_unequal + pivotal_to_unequal,
                   data=long.data.comp[ long.data.comp$proposal==1,], family="binomial" )

models[[4]]  <- glm(punish.binary ~ is.proposer*pivotal_to_unequal + choice_unequal,
                    data=long.data.comp[ long.data.comp$proposal==1,], family="binomial" )

models[[5]]  <- glm(punish.binary ~ is.proposer+pivotal_to_unequal + choice_unequal+ outcome_unequal,
                    data=long.data.comp[ long.data.comp$proposal==1,], family="binomial" )

models[[6]]  <- glm(punish.binary ~ is.proposer*pivotal_to_unequal + choice_unequal+ outcome_unequal,
                    data=long.data.comp[ long.data.comp$proposal==1,], family="binomial" )


texreg(booktabs=T, l = list(models[[1]],models[[2]],models[[3]],models[[4]],models[[5]],models[[6]]),
       custom.model.names = c("Model 1","Model 2", "Model 3","Model 4","Model 5","Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,6,5,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal A: ((9,1),(7,3))",
       caption.above=T) #dcolumn=T,

r2_1<-(1-(models[[1]]$deviance/models[[1]]$null.deviance))
r2_2<-(1-(models[[2]]$deviance/models[[2]]$null.deviance))
r2_3<-(1-(models[[3]]$deviance/models[[3]]$null.deviance))
r2_4<-(1-(models[[4]]$deviance/models[[4]]$null.deviance))
r2_5<-(1-(models[[5]]$deviance/models[[5]]$null.deviance))
r2_6<-(1-(models[[6]]$deviance/models[[6]]$null.deviance))

#r^2 (9,1),(7,3)
r2_936<-cbind(r2_1,r2_2,r2_3,r2_4,r2_5,r2_6)
xtable(r2_936,digits = 3)




models[[7]] <- glm(punish.binary ~ is.proposer,
                   data=long.data.comp[ long.data.comp$proposal==2,], family="binomial" )

models[[8]] <- glm(punish.binary ~ is.proposer + choice_unequal,
                   data=long.data.comp[ long.data.comp$proposal==2,], family="binomial" )

models[[9]] <- glm(punish.binary ~ is.proposer + choice_unequal + pivotal_to_unequal,
                   data=long.data.comp[ long.data.comp$proposal==2,], family="binomial" )

models[[10]]  <- glm(punish.binary ~ is.proposer*pivotal_to_unequal + choice_unequal,
                     data=long.data.comp[ long.data.comp$proposal==2,], family="binomial" )

models[[11]]  <- glm(punish.binary ~ is.proposer+pivotal_to_unequal + choice_unequal+ outcome_unequal,
                     data=long.data.comp[ long.data.comp$proposal==2,], family="binomial" )

models[[12]]  <- glm(punish.binary ~ is.proposer*pivotal_to_unequal + choice_unequal+ outcome_unequal,
                     data=long.data.comp[ long.data.comp$proposal==2,], family="binomial" )

texreg(booktabs=T, l = list(models[[7]],models[[8]],models[[9]],models[[10]],models[[11]],models[[12]]),
       custom.model.names = c("Model 1","Model 2", "Model 3","Model 4","Model 5","Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,6,5,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal B: ((9,1),(5,5))",
       caption.above=T) #dcolumn=T,

r2_7<-(1-(models[[7]]$deviance/models[[7]]$null.deviance))
r2_8<-(1-(models[[8]]$deviance/models[[8]]$null.deviance))
r2_9<-(1-(models[[9]]$deviance/models[[9]]$null.deviance))
r2_10<-(1-(models[[10]]$deviance/models[[10]]$null.deviance))
r2_11<-(1-(models[[11]]$deviance/models[[11]]$null.deviance))
r2_12<-(1-(models[[12]]$deviance/models[[12]]$null.deviance))

#r^2 (9,1),(5,5)
r2_819<-cbind(r2_7,r2_8,r2_9,r2_10,r2_11,r2_12)
xtable(r2_819,digits = 3)




models[[13]] <- glm(punish.binary ~ is.proposer,
                    data=long.data.comp[ long.data.comp$proposal==3,], family="binomial" )

models[[14]] <- glm(punish.binary ~ is.proposer + choice_unequal,
                    data=long.data.comp[ long.data.comp$proposal==3,], family="binomial" )

models[[15]] <- glm(punish.binary ~ is.proposer + choice_unequal + pivotal_to_unequal,
                    data=long.data.comp[ long.data.comp$proposal==3,], family="binomial" )

models[[16]]  <- glm(punish.binary ~ is.proposer*pivotal_to_unequal + choice_unequal,
                     data=long.data.comp[ long.data.comp$proposal==3,], family="binomial" )

models[[17]]  <- glm(punish.binary ~ is.proposer+pivotal_to_unequal + choice_unequal+ outcome_unequal,
                     data=long.data.comp[ long.data.comp$proposal==3,], family="binomial" )

models[[18]]  <- glm(punish.binary ~ is.proposer*pivotal_to_unequal + choice_unequal+ outcome_unequal,
                     data=long.data.comp[ long.data.comp$proposal==3,], family="binomial" )

texreg(booktabs=T, l = list(models[[13]],models[[14]],models[[15]],models[[16]],models[[17]],models[[18]]),
       custom.model.names = c("Model 1", "Model 2","Model 3","Model 4","Model 5", "Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,6,5,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal C: ((7,3),(5,5))",
       caption.above=T) #dcolumn=T,

r2_13<-(1-(models[[13]]$deviance/models[[13]]$null.deviance))
r2_14<-(1-(models[[14]]$deviance/models[[14]]$null.deviance))
r2_15<-(1-(models[[15]]$deviance/models[[15]]$null.deviance))
r2_16<-(1-(models[[16]]$deviance/models[[16]]$null.deviance))
r2_17<-(1-(models[[17]]$deviance/models[[17]]$null.deviance))
r2_18<-(1-(models[[18]]$deviance/models[[18]]$null.deviance))

#r^2 (7,3),(5,5)
r2_945<-cbind(r2_13,r2_14,r2_15,r2_16,r2_17,r2_18)
xtable(r2_945,digits = 3)




models[[19]] <- glm(punish.binary ~ is.proposer*pivotal_to_unequal + choice_unequal+ outcome_unequal,
                    data=long.data.comp, family="binomial" )

texreg(booktabs=T, l = list(models[[6]],models[[12]],models[[18]],models[[19]]),
       custom.model.names = c("Model 6 A", "Model 6 B","Model 6 C","Model A+B+C"),
       custom.coef.names = c("Constant","Proposer","Pivotal to Unequal","Choice Unequal","Unequal Outcome","Proposer * Pivotal to Unequal"),
       reorder.coef = c(2,4,3,5,6,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal A,B,C",
       caption.above=T) #dcolumn=T,

r2_19<-(1-(models[[19]]$deviance/models[[19]]$null.deviance))

#r^2 2700 obs
r2_2700<-cbind(r2_6,r2_12,r2_18,r2_19)
xtable(r2_2700,digits = 3)




#long.data.comp$unkind <- NA
#for (i in 1:nrow(long.data.comp)){
#  if (long.data.comp$achoice[i]==1){
#    long.data.comp$unkind[i]=1
#  }else{
#    long.data.comp$unkind[i]=0
#  }
#}


# Guardar bases
#write.csv(data, file = 'data.csv')
#write.csv(b.data, file = 'b.data.csv')
#write.csv(long.data, file = 'long_data.csv')
#write.csv(long.data.com, file = 'long_data.com.csv')
#write.csv(long.data.comp, file = 'long_data.comp.csv')

#texreg(booktabs=T, l = list(models[[12]],models[[1]],models[[4]],models[[10]],models[[11]],models[[2]],models[[5]],models[[8]],models[[3]],models[[6]],models[[9]]),
#       custom.model.names = c("Model 0","Model 1", "Model 4","Model 10","Model 11","Model 2", "Model 5", "Model 8", "Model 3", "Model 6", "Model 9"),
#       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
#      reorder.coef = c(2,3,4,6,5,1),stars = c(0.1,0.05,0.01),
#      caption="Logistic Regression Models of DM Punishment",
#      caption.above=T) #dcolumn=T,




### OLS Regression

models_2 <- NULL

models_2[[1]] <- lm(value ~ is.proposer,
                    data=long.data.comp[ long.data.comp$proposal==1,])

models_2[[2]] <- lm(value ~ is.proposer + choice_unequal,
                    data=long.data.comp[ long.data.comp$proposal==1,])

models_2[[3]] <- lm(value ~ is.proposer + choice_unequal + pivotal_to_unequal,
                    data=long.data.comp[ long.data.comp$proposal==1,])

models_2[[4]]  <- lm(value ~ is.proposer*pivotal_to_unequal + choice_unequal,
                     data=long.data.comp[ long.data.comp$proposal==1,])

models_2[[5]]  <- lm(value ~ is.proposer+pivotal_to_unequal + choice_unequal+ outcome_unequal,
                     data=long.data.comp[ long.data.comp$proposal==1,])

models_2[[6]]  <- lm(value ~ is.proposer*pivotal_to_unequal + choice_unequal+ outcome_unequal,
                     data=long.data.comp[ long.data.comp$proposal==1,])

texreg(booktabs=T, l = list(models_2[[1]],models_2[[2]],models_2[[3]],models_2[[4]],models_2[[5]],models_2[[6]]),
       custom.model.names = c("Model 1","Model 2", "Model 3","Model 4","Model 5","Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="OLS Regression Models of DM Punishment proposal A: ((9,1),(7,3))",
       caption.above=T) #dcolumn=T,


models_2[[7]] <- lm(value ~ is.proposer,
                    data=long.data.comp[ long.data.comp$proposal==2,])

models_2[[8]] <- lm(value ~ is.proposer + choice_unequal,
                    data=long.data.comp[ long.data.comp$proposal==2,])

models_2[[9]] <- lm(value ~ is.proposer + choice_unequal + pivotal_to_unequal,
                    data=long.data.comp[ long.data.comp$proposal==2,])

models_2[[10]]  <- lm(value ~ is.proposer*pivotal_to_unequal + choice_unequal,
                      data=long.data.comp[ long.data.comp$proposal==2,])

models_2[[11]]  <- lm(value ~ is.proposer+pivotal_to_unequal + choice_unequal+ outcome_unequal,
                      data=long.data.comp[ long.data.comp$proposal==2,])

models_2[[12]]  <- lm(value ~ is.proposer*pivotal_to_unequal + choice_unequal+ outcome_unequal,
                      data=long.data.comp[ long.data.comp$proposal==2,])

texreg(booktabs=T, l = list(models_2[[7]],models_2[[8]],models_2[[9]],models_2[[10]],models_2[[11]],models_2[[12]]),
       custom.model.names = c("Model 1","Model 2", "Model 3","Model 4","Model 5","Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="OLS Regression Models of DM Punishment proposal B: ((9,1),(5,5))",
       caption.above=T) #dcolumn=T,


models_2[[13]] <- lm(value ~ is.proposer,
                     data=long.data.comp[ long.data.comp$proposal==3,])

models_2[[14]] <- lm(value ~ is.proposer + choice_unequal,
                     data=long.data.comp[ long.data.comp$proposal==3,])

models_2[[15]] <- lm(value ~ is.proposer + choice_unequal + pivotal_to_unequal,
                     data=long.data.comp[ long.data.comp$proposal==3,])

models_2[[16]]  <- lm(value ~ is.proposer*pivotal_to_unequal + choice_unequal,
                      data=long.data.comp[ long.data.comp$proposal==3,])

models_2[[17]]  <- lm(value ~ is.proposer+pivotal_to_unequal + choice_unequal+ outcome_unequal,
                      data=long.data.comp[ long.data.comp$proposal==3,])

models_2[[18]]  <- lm(value ~ is.proposer*pivotal_to_unequal + choice_unequal+ outcome_unequal,
                      data=long.data.comp[ long.data.comp$proposal==3,])

texreg(booktabs=T, l = list(models_2[[13]],models_2[[14]],models_2[[15]],models_2[[16]],models_2[[17]],models_2[[18]]),
       custom.model.names = c("Model 1", "Model 2","Model 3","Model 4","Model 5", "Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="OLS Regression Models of DM Punishment proposal C: ((7,3),(5,5))",
       caption.above=T) #dcolumn=T,


models_2[[19]] <- lm(value ~ is.proposer*pivotal_to_unequal + choice_unequal+ outcome_unequal,
                     data=long.data.comp)

texreg(booktabs=T, l = list(models_2[[6]],models_2[[12]],models_2[[18]],models_2[[19]]),
       custom.model.names = c("Model 6 A", "Model 6 B","Model 6 C","Model A+B+C"),
       custom.coef.names = c("Constant","Proposer","Pivotal to Unequal","Choice Unequal","Unequal Outcome","Proposer * Pivotal to Unequal"),
       reorder.coef = c(2,4,3,5,6,1),stars = c(0.1,0.05,0.01),
       caption="OLS Regression Models of DM Punishment proposal A,B,C",
       caption.above=T) #dcolumn=T,
