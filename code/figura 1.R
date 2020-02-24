###################################
### Autor codigo: Nelson Brito
### 2019
###################################

rm(list=ls())
setwd("C:/Users/BYRON/Desktop/Investigación Caputo Duch/Pivotality Duch Caputo/Pivotality Experiment/script/Replication")
data <- read.delim("data_p12.txt")

library(reshape2)
library(ggplot2)
library(plyr)
library(foreign)

# out <- read.dta("data_all.dta")
# #write(names(out),file="varnames.txt")
# ## update 03/20
# 
# dm.data <- out[which(out$role==1),]
# dm.data <- dm.data[which(!is.na(dm.data$proposer)),]
# 
# #table(apply(dm.data[,122:124],1,sd)==0)
# 
# plot.data <- dm.data[,c(1,6,122:124,65,134)]
# plot.data <- melt(plot.data,id.vars=c("Session","proposer","subrole","Group"))
# plot.data$A <- as.numeric(gsub("PChoice\\.(\\d)","\\1",plot.data$variable))
# # real.prop <- plot.data[which(plot.data$proposer==plot.data$subrole&plot.data$subrole==plot.data$A),]
# # real.prop$proposal <- factor(real.prop$value,labels=c("9 & 7","9 & 5", "7 & 5"))
# # write.csv(table(real.prop$proposer,real.prop$proposal),file="proposer_choice.csv")
# 
# 
# plot.data <- plot.data[which(plot.data$proposer==plot.data$A&plot.data$subrole==plot.data$proposer),]
# plot.data <- plot.data[which(plot.data$value!=-1),]
# plot.data <- data.frame(tapply(plot.data$value,plot.data$variable,mean))
# #plot.data$A <- as.numeric(gsub("BChoice(\\d)\\..+","\\1",row.names(plot.data)))
# #plot.data$situation <- as.numeric(gsub(".+(\\d)$","\\1",row.names(plot.data)))
# 
# 
# 
# ## recicpient plot
# plot.data <- out[which(out$role==2),c(1,6,134,17:40)]
# plot.data <- plot.data[which(!is.na(plot.data$proposer)),]
# plot.data <- melt(plot.data,id.vars=c("Session","Group","proposer"))
# plot.data <- data.frame(tapply(plot.data$value,list(plot.data$variable,plot.data$proposer),mean))
# plot.data$choice <- row.names(plot.data)
# plot.data <- melt(plot.data,id.vars="choice")
# plot.data$A <- as.numeric(gsub("BChoice(\\d)\\_.+","\\1",plot.data$choice))
# plot.data$situation <- as.numeric(gsub(".+(\\d)$","\\1",plot.data$choice))
# levels(plot.data$variable) <- paste("Proposer =",1:3) 
# 
# plot.data$A <- factor(plot.data$A,labels=c('A1','A2','A3'))
# plot.data$situation <- factor(plot.data$situation,labels=c('(1,1,1)','(1,1,2)','(1,2,1)','(2,1,1)','(1,2,2)','(2,1,2)','(2,2,1)','(2,2,2)'))
# #names(plot.data)[1] <- 'value'
# g <- ggplot(plot.data, aes(x=A,y=value))
# g + geom_histogram(stat='identity')+facet_grid(variable~situation,scales='free_y')+ylab("Average Deduction Points")+xlab("Player")
# #ggsave("average_deduction_points_with_proposer.pdf",width=10,height=6)
# 
# ##
# plot.data <- out[which(out$role==2),c(1,6,134,17:40,140)]
# plot.data <- plot.data[which(!is.na(plot.data$proposer)),]
# plot.data <- melt(plot.data,id.vars=c("Session","Group","proposer",'proposer_choice'))
# plot.data$session_group <- paste(plot.data$Session,plot.data$Group,sep=".")
# plot.data$proposer_choice <- factor(plot.data$proposer_choice,labels=c("9 & 7","9 & 5", "7 & 5"))
# plot.data$y_situation <- factor(sprintf("Prop: %d, Choice: (%s)",plot.data$proposer,plot.data$proposer_choice))
# plot.data <- data.frame(tapply(plot.data$value,list(plot.data$variable,plot.data$y_situation),mean))
# plot.data$choice <- row.names(plot.data)
# plot.data <- melt(plot.data,id.vars="choice")
# plot.data$A <- as.numeric(gsub("BChoice(\\d)\\_.+","\\1",plot.data$choice))
# plot.data$situation <- as.numeric(gsub(".+(\\d)$","\\1",plot.data$choice))
# levels(plot.data$variable)
# plot.data$variable.2 <- plot.data$variable
# levels(plot.data$variable) <- c("Prop:1, Choice:(7 & 5)","Prop:1, Choice:(9 & 7)",
#                                 "Prop:2, Choice:(9 & 5)","Prop:2, Choice:(9 & 7)",
#                                 "Prop:3, Choice:(9 & 5)","Prop:3, Choice:(9 & 7)")
# plot.data$variable.short <- plot.data$variable
# levels(plot.data$variable.short) <- c("Prop:1, (7 & 5)","Prop:1, (9 & 7)",
#                                 "Prop:2, (9 & 5)","Prop:2, (9 & 7)",
#                                 "Prop:3, (9 & 5)","Prop:3, (9 & 7)")
# 
# 
# # temp <- data.frame(cbind(plot.data$session_group, plot.data$proposer_choice, plot.data$proposer))
# # temp <- temp[!duplicated(temp),]
# # temp$X1 <- gsub(' ','.',temp$X1)
# # names(temp)[1] <- 'variable'
# # plot.data <- data.frame(tapply(plot.data$value,list(plot.data$variable,plot.data$session_group),mean))
# # plot.data$choice <- row.names(plot.data)
# # plot.data <- melt(plot.data,id.vars="choice")
# # plot.data$A <- as.numeric(gsub("BChoice(\\d)\\..+","\\1",plot.data$choice))
# # plot.data$situation <- as.numeric(gsub(".+(\\d)$","\\1",plot.data$choice))
# # plot.data <- merge(plot.data,temp)
# # plot.data$proposer_choice <- factor(plot.data$X2,labels=c("9 & 7","9 & 5", "7 & 5"))
# # plot.data$y_situation <- factor(sprintf("Prop: %d, Choice: (%s)",plot.data$X3,plot.data$proposer_choice))
# #levels(plot.data$variable) <- 
# #levels(plot.data$variable) <- paste("Proposer =",1:3) 
# 
# plot.data$A <- factor(plot.data$A,labels=c('A1','A2','A3'))
# plot.data$situation <- factor(plot.data$situation,labels=c('(1,1,1)','(1,1,2)','(1,2,1)','(2,1,1)','(1,2,2)','(2,1,2)','(2,2,1)','(2,2,2)'))
# #names(plot.data)[1] <- 'value'
# g <- ggplot(plot.data, aes(x=A,y=value))
# g + geom_histogram(stat='identity')+facet_grid(variable~situation,scales="free_y")+ylab("Average Deduction Points")+xlab("Player")
# #ggsave("average_deduction_points_with_proposer2.pdf",width=10,height=10)
# 
# g <- ggplot(plot.data[which(as.numeric(plot.data$situation)<5),], aes(x=A,y=value))
# g + geom_histogram(stat='identity')+facet_grid(situation~variable.short)+ylab("Average Deduction Points")+xlab("Player")
# #ggsave("average_deduction_points_with_proposer_unfair.pdf",width=7,height=5)
# 
# g <- ggplot(plot.data[which(as.numeric(plot.data$situation)>=5),], aes(x=A,y=value))
# g + geom_histogram(stat='identity')+facet_grid(situation~variable.short)+ylab("Average Deduction Points")+xlab("Player")
# #ggsave("average_deduction_points_with_proposer_fair.pdf",width=7,height=5)

## total deductions
data <- read.dta('long_data.dta')
head(data)
data$proposer <- ifelse(is.na(data$proposer),4,data$proposer)
data$proposer.fac <- factor(data$proposer,labels=c(1,2,3,"None"))
data$proposer_choice <- ifelse(is.na(data$proposer_choice),4,data$proposer_choice)
data$proposer_choice3 <- factor(data$proposer_choice,labels=c(levels(data$proposer_choice2),"None"))
data$outcome <- factor(data$equitable_outcome,labels=c("Unfair","Fair"))

plot.data <- data.frame(tapply(data$value,list(data$proposer_choice3,data$outcome),mean))
plot.data$choice <- row.names(plot.data)
plot.data <- melt(plot.data,id.vars='choice')
#plot.data$choice <- factor(plot.data$choice)
plot.data$choice.lab <- factor(rep(1:4,2),labels=c("(9,1) & (7,3)","(9,1) & (5,5)", "(7,3) & (5,5)","None"))
plot.data$choice.lab2 <- factor(plot.data$choice.lab,levels=c("None","(9,1) & (7,3)","(9,1) & (5,5)", "(7,3) & (5,5)"))
g <- ggplot(plot.data,aes(y=value,x=variable))
g + geom_histogram(stat='identity')+facet_wrap(~choice.lab2)+ylab("Average Deduction Points")+xlab("Outcome")+theme_bw()
ggsave("average_deduction_points_by_proposal.pdf",width=5,height=5,
       path="/Users/akitaka/Dropbox/Ray_Projects/shared_folders/CESS_Aki/Responsibility/Pivot Experiment/Text2/graphics")
