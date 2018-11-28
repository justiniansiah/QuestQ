nonpeak <- read.csv("C:/Users/bhara/Google Drive/SUTD/ESD T6/Simulation/project/QuestQ/09) Data Collection/Data/SimulationData/simulated_nonpeak.csv")
peak <- read.csv("C:/Users/bhara/Google Drive/SUTD/ESD T6/Simulation/project/QuestQ/09) Data Collection/Data/SimulationData/simulated_peak.csv")

require(reshape)
nonpeak<-subset(nonpeak, select=c('simulated','observed'))
peak<-subset(peak, select=c('simulated','observed'))

nonpeak<-melt(nonpeak)
nonpeak<-nonpeak[!is.na(nonpeak$value), ]

peak<-melt(peak)
peak<-peak[!is.na(peak$value), ]

require(ggplot2)
require(ggthemes)
