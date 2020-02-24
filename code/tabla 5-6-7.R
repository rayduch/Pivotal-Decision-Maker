###################################
### Autor codigo: Nelson Brito
### 2019
###################################

rm(list=ls())
setwd("C:/Users/BYRON/Desktop/Investigación Caputo Duch/Pivotality Duch Caputo/Pivotality Experiment/Data")

data <- read.csv('subject.table.Pilot_Session123.csv',as.is=T)

library(ggplot2)
library(reshape2)
library(foreign)
install.packages('texreg')
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
theme_set(theme_bw())
qplot(x = factor( apply(b.data[,22:24],1,sum)),geom='histogram',xlab="Total Deduction",ylab="Count")
ggsave("Total_deduction.pdf",width=5,height=5)
b.data$proposer.punishment <-0
for(i in 1:nrow(b.data))b.data$proposer.punishment[i] <- b.data[i,21+b.data$proposer[i]]
b.data$proposer.is.pivot <- factor(as.numeric(b.data$proposer==b.data$pivot))
b.data$proposer.is <- factor(b.data$proposer,labels=c("A1","A2","A3"))
b.data$proposer.vote <- NA
for(i in 1:nrow(b.data))b.data$proposer.vote[i] <- b.data[i,18+b.data$proposer[i]]
b.data$proposer.vote <- factor(b.data$proposer.vote,labels=c("(9,1)","(7,3)","(5,5)"))

table(b.data$proposer.punishment)
tapply(b.data$proposer.punishment,b.data$chosen.proposal,mean)
tapply(b.data$proposer.punishment,list(b.data$chosen.proposal,b.data$proposer.vote),mean)
library(xtable)
xtable(tapply(b.data$proposer.punishment,list(b.data$chosen.proposal,b.data$proposer.vote),mean))
vote.record <- c('u-u-u','u-u-e','u-e-u','e-u-u','u-e-e','e-u-e','e-e-u','e-e-e')
b.data$situation2 <- factor(b.data$situation, labels=vote.record)

plot.data <- b.data[,grep("bchoice\\d|chosen.proposal|situation2|^proposer$",names(b.data))]
plot.data <- melt(plot.data,id.vars=c("chosen.proposal","situation2","proposer"))
plot.data$voter <- as.numeric(gsub("\\D+","",as.character(plot.data$variable)))
#plot.data <- plot.data[ plot.data$proposer!=plot.data$voter,]


plot.data <- data.frame(tapply(plot.data$value,list(plot.data$situation2,plot.data$chosen.proposal,plot.data$variable),mean))

temp.data <- b.data[ b.data$subrole==1,]
plot.data2 <-tapply(rep(1, nrow(temp.data)),list(temp.data$situation2,temp.data$chosen.proposal),sum)
nobs <- as.vector(plot.data2)

plot.long <- NULL
for( i in 1:3){
  temp <- plot.data[,1:3*3-3+i]
  names(temp) <- c("Voter1","Voter2","Voter3")
  plot.long <- rbind(plot.long,temp)
}
#plot.long <- data.frame(,plot.long)
plot.long <- data.frame(proposal = c("((9,1),(7,3))",rep(NA,7),"((9,1),(5,5))",rep(NA,7),"((7,3),(5,5))",rep(NA,7)),
                        vote.record=rep(vote.record,3),
                        N = as.character(nobs),
                        plot.long)
boldify <-function(x,bfidx){
  x <- round(x,digits = 2)
  x <- ifelse(bfidx,sprintf("{\\bf %0.2f}",x),sprintf("%0.2f",x))
  return(x)
}
plot.long$Voter2 <- 
  boldify(plot.long$Voter2, rep(c(1,1,0,0,0,0,0,0),3))
plot.long$Voter3 <- boldify(plot.long$Voter3, rep(c(0,0,1,1,0,0,0,0),3))

print(xtable(plot.long),include.rownames=F,file="temp.table.txt",sanitize.text.function = function(x) x)

#proposer punishment
b.data$proposer.vote2 <- NA
for(i in 1:nrow(b.data))b.data$proposer.vote2[i] <- b.data[i,15+b.data$proposer[i]]
xtable(tapply(b.data$proposer.punishment,list(b.data$chosen.proposal,b.data$proposer.vote2),mean))

# logit
names(b.data)
sub.data <- b.data[,c(22:24,11,13,14,163,164,166)]
sub.data$id <- 1:nrow(sub.data)

long.data <- melt(sub.data,measure.vars = names(sub.data)[1:3])
long.data$punish.binary <- as.numeric(long.data$value>0)
long.data$target <- as.numeric(gsub('\\D',"",long.data$variable))
long.data$is.proposer <- as.numeric(long.data$target==long.data$proposer)
long.data$is.pivot <- as.numeric(long.data$target==long.data$pivot)
long.data$pivotal_to_unequal <- long.data$is.pivot*(2-long.data$winning_allocation)

sub.data.2 <- b.data[,c(16:18)]
sub.data.2$id <- 1:nrow(sub.data.2)
long.data.2 <- melt(sub.data.2, id.vars = "id")
long.data.2$target <- as.numeric(gsub('\\D',"",long.data.2$variable))
names(long.data.2)[3] <- "achoice"
long.data.comp <- merge(long.data,long.data.2,by=c("id","target"))

long.data.comp$choice_unequal <- as.numeric(long.data.comp$achoice==1)


models <- NULL
for(i in 1:3){
  models[[i]] <- glm(punish.binary ~ is.proposer + choice_unequal,
                     data=long.data.comp[ long.data.comp$proposal==i,], family="binomial" )
}

for(i in 1:3){
  models[[i+3]] <- glm(punish.binary ~ is.proposer + choice_unequal + pivotal_to_unequal,
                       data=long.data.comp[ long.data.comp$proposal==i,], family="binomial" )
}

library(texreg)
texreg(models,booktabs=T, 
       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal"),
       reorder.coef = c(2:4,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment",
       caption.above=T) #dcolumn=T,

models.2 <- NULL
for(i in 1:3) models.2[[i]] <- models[[i+3]]
texreg(models.2,booktabs=T, 
       custom.coef.names = c("Constant","Proposer","Choice Unequal","Pivotal to Unequal"),
       reorder.coef = c(3,2,4,1),stars = c(0.1,0.05,0.01),
       caption="Logistic Regression Models of DM Punishment",
       caption.above=T)
