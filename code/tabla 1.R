###################################
### Autor codigo: Nelson Brito
### 2019
###################################

rm(list=ls())
setwd("C:/Users/BYRON/Desktop/Investigación Caputo Duch/Pivotality Duch Caputo/Pivotality Experiment/script/Replication")
data <- read.delim("data_p12.txt")

library(reshape2)
library(ggplot2)

names(data) <- gsub("\\.$","",names(data))

str(data$X140311_1026)
levels(data$X140311_1026) <- c("Pilot","Session 1","Session 2")
names(data)[1] <- "Session"

plot.data <- data[,c(1,17:40)]
plot.data <- melt(plot.data,id.vars="Session")
plot.data <- plot.data[which(plot.data$value!=-1),]
plot.data <- data.frame(tapply(plot.data$value,plot.data$variable,mean))
plot.data$A <- as.numeric(gsub("BChoice(\\d)\\..+","\\1",row.names(plot.data)))
plot.data$situation <- as.numeric(gsub(".+(\\d)$","\\1",row.names(plot.data)))

#plot.data$A <- factor(plot.data$A,labels=c('A1','A2','A3'))
#plot.data$situation <- factor(plot.data$situation,labels=c('(1,1,1)','(1,1,2)','(1,2,1)','(2,1,1)','(1,2,2)','(2,1,2)','(2,2,1)','(2,2,2)'))
vote.record <- c('u-u-u','u-u-e','u-e-u','e-u-u','u-e-e','e-u-e','e-e-u','e-e-e')
plot.data$situation2 <- factor(plot.data$situation, labels=vote.record)
plot.data$voter <- factor(plot.data$A,labels=paste("Voter",1:3))
names(plot.data)[1] <- "avg.punish"

tab.data <- data.frame(allocation = c("Unequal",rep("",3),"Equal",rep("",3)),
                       vote.record,
                       Voter1 = plot.data$avg.punish[ plot.data$voter=="Voter 1"],
                       Voter2 = plot.data$avg.punish[ plot.data$voter=="Voter 2"],
                       Voter3 = plot.data$avg.punish[ plot.data$voter=="Voter 3"])

library(xtable)
print(xtable(tab.data),include.rownames=F)


# names(plot.data)[1] <- 'value'
# g <- ggplot(plot.data, aes(x=A,y=value))
# g + geom_histogram(stat='identity')+facet_wrap(~situation)+ylab("Average Deduction Points")+xlab("Player")
 