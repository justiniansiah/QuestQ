
nonpeak <- read.csv("C:/Users/bhara/Google Drive/SUTD/ESD T6/Simulation/project/QuestQ/09) Data Collection/Data/SimulationData/simulated_nonpeak.csv")
peak <- read.csv("C:/Users/bhara/Google Drive/SUTD/ESD T6/Simulation/project/QuestQ/09) Data Collection/Data/SimulationData/simulated_peak.csv")
leadpeak<- read.csv("C:/Users/bhara/Google Drive/SUTD/ESD T6/Simulation/project/QuestQ/09) Data Collection/Data/SimulationData/simulated_peak_lead.csv")
leadnpeak<-read.csv("C:/Users/bhara/Google Drive/SUTD/ESD T6/Simulation/project/QuestQ/09) Data Collection/Data/SimulationData/simulated_nonpeak_lead.csv")
modelcompare<-read.csv("C:/Users/bhara/Google Drive/SUTD/ESD T6/Simulation/project/QuestQ/09) Data Collection/Data/SimulationData/tandem.csv")
require(reshape)
nonpeak<-subset(nonpeak, select=c('simulated','observed'))
peak<-subset(peak, select=c('simulated','observed'))
leadnpeak<-subset(leadnpeak, select=c('simulated','observed'))
leadpeak<-subset(leadpeak, select=c('simulated','observed'))
modelcompare<-subset(modelcompare, select=c('tandem','fcfs'))

nonpeak<-melt(nonpeak)
peak<-melt(peak)
leadnpeak<-melt(leadnpeak)
leadpeak<-melt(leadpeak)
modelcompare<-melt(modelcompare)


nonpeak<-nonpeak[!is.na(nonpeak$value), ]
peak<-peak[!is.na(peak$value), ]
leadnpeak<-leadnpeak[!is.na(leadnpeak$value), ]
leadpeak<-leadpeak[!is.na(leadpeak$value), ]
modelcompare<-modelcompare[!is.na(modelcompare$value), ]


require(ggplot2)
require(ggthemes)
ggplot(nonpeak,aes(x=value, color=variable))+geom_density()+
  theme_minimal()+labs(title="Densityplot of simulated and observed Non-peak interarrival times",color="Legend")

ggplot(peak,aes(x=value, color=variable))+geom_density()+
  theme_minimal()+labs(title="Densityplot of simulated and observed Peak interarrival times",color="Legend")

ggplot(leadnpeak,aes(x=value, color=variable))+geom_density()+
  theme_minimal()+labs(title="Densityplot of simulated and observed Non-peak lead times",color="Legend")

ggplot(leadpeak,aes(x=value, color=variable))+geom_density()+
  theme_minimal()+labs(title="Densityplot of simulated and observed Peak lead times",color="Legend")

ggplot(modelcompare,aes(x=value, color=variable))+geom_density()+
  theme_minimal()+labs(title="Tandem vs FCFS Lead Times",color="Legend")
