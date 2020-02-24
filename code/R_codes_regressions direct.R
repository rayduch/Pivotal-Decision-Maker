###################################
### Autor codigo: Nelson Brito
### 2019
###################################

rm(list=ls())
setwd("C:/Users/BYRON/Desktop/Investigación Caputo Duch/Pivotality Duch Caputo 2019/Pivotality Experiment 2019 original/script/Replication")

library(reshape2)
library(ggplot2)
library(plyr)
library(foreign)
library(pscl)


#logit
data <- read.dta("long_data.dta")
data$punish.binary <- ifelse(data$value>0,1,0)
data$outcome_unequal<-(1-data$equitable_outcome)
data$pivotal_to_unequal <- data$pivotal*data$outcome_unequal
data$choice_unequal <- as.numeric(data$dm_choice==1)

data_prueba=data[ data$proposer_dummy==1,]
data_prueba=data_prueba[complete.cases(data_prueba[,"proposer_dummy"]),]
mean(data$value)

### Check outcome_unequal

#data$equitable_outcome_test<-NA

#for (i in 1:1368) {
#  if (data$situation[i]>4){
#    data$equitable_outcome_test[i]=1
#  }else{
#    data$equitable_outcome_test[i]=0
#  }
#}

#data$outcome_unequal <- (1-data$equitable_outcome_test)
#data$check=(data$equitable_outcome-data$equitable_outcome_test)
#data$pivotal - (1-data$equitable_outcome)

#data$intention_unkind <- NA
#for (i in 1:1368) {
#  if (data$situation[i]>7){
#    data$intention_unkind[i]=0
#  }else{
#    data$intention_unkind[i]=1
#  }
#}


## New regresions


data1=data[ data$proposer_choice==1,]
data1=data1[complete.cases(data1[,"proposer_choice"]),]

data2=data[ data$proposer_choice==2,]
data2=data2[complete.cases(data2[,"proposer_choice"]),]

data3=data[ data$proposer_choice==3,]
data3=data3[complete.cases(data3[,"proposer_choice"]),]


models <- NULL

models[[1]] <- glm(punish.binary ~ proposer_dummy ,
                   data=data1, family="binomial" )  

models[[2]]<- glm(punish.binary ~ proposer_dummy + choice_unequal,
                  data=data1, family="binomial" )

models[[3]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal,
                  data=data1, family="binomial" )

models[[4]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + pivotal_to_unequal,
                  data=data1, family="binomial" )

models[[5]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal ,
                  data=data1, family="binomial" )

models[[6]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal,
                  data=data1, family="binomial" )

library(texreg)
texreg(booktabs=T,l = list(models[[1]],models[[2]],models[[3]],models[[4]],models[[5]],models[[6]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4", "Model 5", "Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice unequal","Pivotal to unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal A: ((9,1),(7,3))",
       caption.above=T) #dcolumn=T,

# R squared

r2_1<-(1-(models[[1]]$deviance/models[[1]]$null.deviance))
r2_2<-(1-(models[[2]]$deviance/models[[2]]$null.deviance))
r2_3<-(1-(models[[3]]$deviance/models[[3]]$null.deviance))
r2_4<-(1-(models[[4]]$deviance/models[[4]]$null.deviance))
r2_5<-(1-(models[[5]]$deviance/models[[5]]$null.deviance))
r2_6<-(1-(models[[6]]$deviance/models[[6]]$null.deviance))

#r^2 (9,1),(7,3)
r2_504<-cbind(r2_1,r2_2,r2_3,r2_4,r2_5,r2_6)
xtable(r2_504,digits = 3)




models[[7]] <- glm(punish.binary ~ proposer_dummy,
                   data=data2, family="binomial" )  

models[[8]]<- glm(punish.binary ~ proposer_dummy + choice_unequal,
                  data=data2, family="binomial" )

models[[9]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal,
                  data=data2, family="binomial" )

models[[10]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + pivotal_to_unequal,
                   data=data2, family="binomial" )

models[[11]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal ,
                   data=data2, family="binomial" )

models[[12]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal,
                   data=data2, family="binomial" )


texreg(booktabs=T,l = list(models[[7]],models[[8]],models[[9]],models[[10]],models[[11]],models[[12]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4", "Model 5", "Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice unequal","Pivotal to unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal B: ((9,1),(5,5))",
       caption.above=T) #dcolumn=T,

# R squared

r2_7<-(1-(models[[7]]$deviance/models[[7]]$null.deviance))
r2_8<-(1-(models[[8]]$deviance/models[[8]]$null.deviance))
r2_9<-(1-(models[[9]]$deviance/models[[9]]$null.deviance))
r2_10<-(1-(models[[10]]$deviance/models[[10]]$null.deviance))
r2_11<-(1-(models[[11]]$deviance/models[[11]]$null.deviance))
r2_12<-(1-(models[[12]]$deviance/models[[12]]$null.deviance))

#r^2 (9,1),(5,5)
r2_144<-cbind(r2_7,r2_8,r2_9,r2_10,r2_11,r2_12)
xtable(r2_144,digits = 3)




models[[13]] <- glm(punish.binary ~ proposer_dummy,
                    data=data3, family="binomial" )  

models[[14]]<- glm(punish.binary ~ proposer_dummy + choice_unequal,
                   data=data3, family="binomial" )

models[[15]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal,
                   data=data3, family="binomial" )

models[[16]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + pivotal_to_unequal,
                   data=data3, family="binomial" )

models[[17]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal ,
                   data=data3, family="binomial" )

models[[18]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal,
                   data=data3, family="binomial" )

texreg(booktabs=T,l = list(models[[13]],models[[14]],models[[15]],models[[16]],models[[17]],models[[18]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4", "Model 5", "Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice unequal","Pivotal to unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal C: ((7,3),(5,5))",
       caption.above=T) #dcolumn=T,

# R squared

r2_13<-(1-(models[[13]]$deviance/models[[13]]$null.deviance))
r2_14<-(1-(models[[14]]$deviance/models[[14]]$null.deviance))
r2_15<-(1-(models[[15]]$deviance/models[[15]]$null.deviance))
r2_16<-(1-(models[[16]]$deviance/models[[16]]$null.deviance))
r2_17<-(1-(models[[17]]$deviance/models[[17]]$null.deviance))
r2_18<-(1-(models[[18]]$deviance/models[[18]]$null.deviance))

#r^2 (7,3),(5,5)
r2_72<-cbind(r2_13,r2_14,r2_15,r2_16,r2_17,r2_18)
xtable(r2_72,digits = 3)




models[[19]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal,
                   data=data[!is.na(data$proposer_choice),], family="binomial" )

texreg(booktabs=T, l = list(models[[6]],models[[12]],models[[18]],models[[19]]),
       custom.model.names = c("Model 6 A", "Model 6 B","Model 6 C","Model A+B+C"),
       custom.coef.names = c("Constant","Proposer","Pivotal to Unequal","Choice Unequal","Unequal Outcome","Proposer * Pivotal to Unequal"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal A,B,C",
       caption.above=T) #dcolumn=T,

# R squared

r2_19<-(1-(models[[19]]$deviance/models[[19]]$null.deviance))

#r^2 720 obs
r2_720<-cbind(r2_6,r2_12,r2_18,r2_19)
xtable(r2_720,digits = 3)



## New regresions without constant


data1=data[ data$proposer_choice==1,]
data1=data1[complete.cases(data1[,"proposer_choice"]),]

data2=data[ data$proposer_choice==2,]
data2=data2[complete.cases(data2[,"proposer_choice"]),]

data3=data[ data$proposer_choice==3,]
data3=data3[complete.cases(data3[,"proposer_choice"]),]


models_2 <- NULL

models_2[[1]] <- glm(punish.binary ~ proposer_dummy - 1,
                     data=data1, family="binomial" )  

models_2[[2]]<- glm(punish.binary ~ proposer_dummy + choice_unequal - 1,
                    data=data1, family="binomial" )

models_2[[3]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal - 1,
                    data=data1, family="binomial" )

models_2[[4]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + pivotal_to_unequal - 1,
                    data=data1, family="binomial" )

models_2[[5]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal - 1,
                    data=data1, family="binomial" )

models_2[[6]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal - 1,
                    data=data1, family="binomial" )


texreg(booktabs=T,l = list(models_2[[1]],models_2[[2]],models_2[[3]],models_2[[4]],models_2[[5]],models_2[[6]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4", "Model 5", "Model 6"),
       custom.coef.names = c("Proposer","Choice unequal","Pivotal to unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(1,2,3,4,5),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal A: ((9,1),(7,3))",
       caption.above=T) #dcolumn=T,

# R squared

r2_1_2<-(1-(models_2[[1]]$deviance/models_2[[1]]$null.deviance))
r2_2_2<-(1-(models_2[[2]]$deviance/models_2[[2]]$null.deviance))
r2_3_2<-(1-(models_2[[3]]$deviance/models_2[[3]]$null.deviance))
r2_4_2<-(1-(models_2[[4]]$deviance/models_2[[4]]$null.deviance))
r2_5_2<-(1-(models_2[[5]]$deviance/models_2[[5]]$null.deviance))
r2_6_2<-(1-(models_2[[6]]$deviance/models_2[[6]]$null.deviance))

#r^2 (9,1),(7,3)
r2_504_2<-cbind(r2_1_2,r2_2_2,r2_3_2,r2_4_2,r2_5_2,r2_6_2)
xtable(r2_504_2,digits = 3)



models_2[[7]] <- glm(punish.binary ~ proposer_dummy -1,
                     data=data2, family="binomial" )  

models_2[[8]]<- glm(punish.binary ~ proposer_dummy + choice_unequal -1,
                    data=data2, family="binomial" )

models_2[[9]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal -1,
                    data=data2, family="binomial" )

models_2[[10]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + pivotal_to_unequal -1,
                     data=data2, family="binomial" )

models_2[[11]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal -1,
                     data=data2, family="binomial" )

models_2[[12]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal -1,
                     data=data2, family="binomial" )

texreg(booktabs=T,l = list(models_2[[7]],models_2[[8]],models_2[[9]],models_2[[10]],models_2[[11]],models_2[[12]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4", "Model 5", "Model 6"),
       custom.coef.names = c("Proposer","Choice unequal","Pivotal to unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(1,2,3,4,5),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal B: ((9,1),(5,5))",
       caption.above=T) #dcolumn=T,

# R squared

r2_7_2<-(1-(models_2[[7]]$deviance/models_2[[7]]$null.deviance))
r2_8_2<-(1-(models_2[[8]]$deviance/models_2[[8]]$null.deviance))
r2_9_2<-(1-(models_2[[9]]$deviance/models_2[[9]]$null.deviance))
r2_10_2<-(1-(models_2[[10]]$deviance/models_2[[10]]$null.deviance))
r2_11_2<-(1-(models_2[[11]]$deviance/models_2[[11]]$null.deviance))
r2_12_2<-(1-(models_2[[12]]$deviance/models_2[[12]]$null.deviance))

#r^2 (9,1),(5,5)
r2_144_2<-cbind(r2_7_2,r2_8_2,r2_9_2,r2_10_2,r2_11_2,r2_12_2)
xtable(r2_144_2,digits = 3)


models_2[[13]] <- glm(punish.binary ~ proposer_dummy -1,
                      data=data3, family="binomial" )  

models_2[[14]]<- glm(punish.binary ~ proposer_dummy + choice_unequal -1,
                     data=data3, family="binomial" )

models_2[[15]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal -1,
                     data=data3, family="binomial" )

models_2[[16]]<- glm(punish.binary ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal -1,
                     data=data3, family="binomial" )

texreg(booktabs=T,l = list(models_2[[13]],models_2[[14]],models_2[[15]],models_2[[16]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4"),
       custom.coef.names = c("Proposer","Choice unequal","Pivotal to unequal","Unequal Outcome"),
       reorder.coef = c(1,2,3,4),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal C: ((7,3),(5,5))",
       caption.above=T) #dcolumn=T,

# R squared

r2_13_2<-(1-(models_2[[13]]$deviance/models_2[[13]]$null.deviance))
r2_14_2<-(1-(models_2[[14]]$deviance/models_2[[14]]$null.deviance))
r2_15_2<-(1-(models_2[[15]]$deviance/models_2[[15]]$null.deviance))
r2_16_2<-(1-(models_2[[16]]$deviance/models_2[[16]]$null.deviance))


#r^2 (7,3),(5,5)
r2_72_2<-cbind(r2_13_2,r2_14_2,r2_15_2,r2_16_2)
xtable(r2_72_2,digits = 3)



models_2[[17]]<- glm(punish.binary ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal-1,
                     data=data[!is.na(data$proposer_choice),], family="binomial" )


texreg(booktabs=T, l = list(models_2[[6]],models_2[[12]],models_2[[16]],models_2[[17]]),
       custom.model.names = c("Model 6 A", "Model 6 B","Model 4 C","Model A+B+C"),
       custom.coef.names = c("Proposer","Pivotal to Unequal","Choice Unequal","Unequal Outcome","Proposer * Pivotal to Unequal"),
       reorder.coef = c(1,2,3,4,5),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment proposal A,B,C",
       caption.above=T) #dcolumn=T,

# R squared

r2_17_2<-(1-(models_2[[17]]$deviance/models_2[[17]]$null.deviance))

#r^2 720 obs
r2_720_2<-cbind(r2_6_2,r2_12_2,r2_16_2,r2_17_2)
xtable(r2_720_2,digits = 3)


## New regresions with OLS


data1=data[ data$proposer_choice==1,]
data1=data1[complete.cases(data1[,"proposer_choice"]),]

data2=data[ data$proposer_choice==2,]
data2=data2[complete.cases(data2[,"proposer_choice"]),]

data3=data[ data$proposer_choice==3,]
data3=data3[complete.cases(data3[,"proposer_choice"]),]


models_3 <- NULL

models_3[[1]] <- lm(value ~ proposer_dummy ,
                    data=data1)  

models_3[[2]]<- lm(value ~ proposer_dummy + choice_unequal,
                   data=data1)

models_3[[3]]<- lm(value ~ proposer_dummy + choice_unequal + pivotal_to_unequal,
                   data=data1)

models_3[[4]]<- lm(value ~ proposer_dummy*pivotal_to_unequal + choice_unequal + pivotal_to_unequal,
                   data=data1)

models_3[[5]]<- lm(value ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal ,
                   data=data1)

models_3[[6]]<- lm(value ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal,
                   data=data1)


texreg(booktabs=T,l = list(models_3[[1]],models_3[[2]],models_3[[3]],models_3[[4]],models_3[[5]],models_3[[6]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4", "Model 5", "Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice unequal","Pivotal to unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="OLS Regression Models of DM Punishment proposal A: ((9,1),(7,3))",
       caption.above=T) #dcolumn=T,


models_3[[7]] <- lm(value ~ proposer_dummy,
                    data=data2 )  

models_3[[8]]<- lm(value ~ proposer_dummy + choice_unequal,
                   data=data2 )

models_3[[9]]<- lm(value ~ proposer_dummy + choice_unequal + pivotal_to_unequal,
                   data=data2)

models_3[[10]]<- lm(value ~ proposer_dummy*pivotal_to_unequal + choice_unequal + pivotal_to_unequal,
                    data=data2)

models_3[[11]]<- lm(value ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal ,
                    data=data2)

models_3[[12]]<- lm(value ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal,
                    data=data2)
cor(data3$value,data3$choice_unequal)
summary(as.factor(data1$value))
texreg(booktabs=T,l = list(models_3[[7]],models_3[[8]],models_3[[9]],models_3[[10]],models_3[[11]],models_3[[12]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4", "Model 5", "Model 6"),
       custom.coef.names = c("Constant","Proposer","Choice unequal","Pivotal to unequal","Proposer * Pivotal to Unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="OLS Regression Models of DM Punishment proposal B: ((9,1),(5,5))",
       caption.above=T) #dcolumn=T,


models_3[[13]] <- lm(value ~ proposer_dummy,
                     data=data3)  

models_3[[14]]<- lm(value ~ proposer_dummy + choice_unequal,
                    data=data3)

models_3[[15]]<- lm(value ~ proposer_dummy + choice_unequal + pivotal_to_unequal,
                    data=data3)

models_3[[16]]<- lm(value ~ proposer_dummy + choice_unequal + pivotal_to_unequal + outcome_unequal ,
                    data=data3)


texreg(booktabs=T,l = list(models_3[[13]],models_3[[14]],models_3[[15]],models_3[[16]]),
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4"),
       custom.coef.names = c("Constant","Proposer","Choice unequal","Pivotal to unequal","Unequal Outcome"),
       reorder.coef = c(2,3,4,5,1),stars = c(0.1,0.05,0.01),
       caption="OLS Regression Models of DM Punishment proposal C: ((7,3),(5,5))",
       caption.above=T) #dcolumn=T,



models_3[[17]]<- lm(value ~ proposer_dummy*pivotal_to_unequal + choice_unequal + outcome_unequal,
                    data=data[!is.na(data$proposer_choice),])

texreg(booktabs=T, l = list(models_3[[6]],models_3[[12]],models_3[[16]],models_3[[17]]),
       custom.model.names = c("Model 6 A", "Model 6 B","Model 4 C","Model A+B+C"),
       custom.coef.names = c("Constant","Proposer","Pivotal to Unequal","Choice Unequal","Unequal Outcome","Proposer * Pivotal to Unequal"),
       reorder.coef = c(2,3,4,5,6,1),stars = c(0.1,0.05,0.01),
       caption="OLS Regression Models of DM Punishment proposal A,B,C",
       caption.above=T) #dcolumn=T,


## 
#models.2 <- NULL
#for(i in 1:4) models.2[[i]] <- models[[i+4]]
#texreg(models.2,booktabs=T, 
#       custom.coef.names = c("Constant","Choice Uequal","Proposer","Pivotal to Unequal"),
#       reorder.coef = c(2:4,1),stars = c(0.1,0.05,0.01),
#       caption="Logistic Regression Models of DM Punishment",
#       caption.above=T)


## include intention_unkind
#data$situation <-as.numeric(gsub(".+(\\d)$","\\1",as.character(data$variable)))
#unkind.data <- data.frame(situation = rep(1:8,3),
#                          dm_number = rep(1:3,each=8),
#                          intention_unkind = c(1,1,1,0,1,0,0,0,
#                                               1,1,0,1,0,1,0,0,
#                                               0,0,1,1,0,0,0,0))
#data2 <- merge(data, unkind.data,by=c("situation","dm_number"))
#
#
#for(i in 1:3){
#  print(summary(glm(punish.binary ~ proposer_dummy + choice_unequal +intention_unkind+ pivotal_to_unequal,
#                    data=data2[ data2$proposer_choice==i,], family="binomial" )))
#}
# not cool result...