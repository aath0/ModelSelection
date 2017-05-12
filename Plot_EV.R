rm(list=ls())

library(R.matlab)
library(nlme) #open nlme library
library(ggplot2)
library(RColorBrewer)

#General path
setwd('D:/Data/SCR/writing/Results/')
outfol <-"D:/Data/SCR/writing/version12/"
#------------------------------------------------------------------------------------
#Dataframe-formated data: 

plot_Bayes = 0
plot_RL = 0
plot_all = 0
plot_families = 1

if (plot_Bayes == 1){
  model_lab = c('BM', 'KL', 'BS', 'BH1', 'BH2')
  lab_save = "Bayesian"
  
}

if (plot_RL == 1){
  model_lab = c('RW1','RW2','PH', 'HM1', 'HM2','HM3','TD')
  lab_save = "RL"
  
}

if (plot_all == 1){
  model_lab = c('RW1','RW2','PH', 'HM1', 'HM2','HM3','TD', 'BM', 'KL', 'BS', 'BH1', 'BH2', 'VO', 'NL')
  lab_save = "All"  
}

if (plot_families == 1){
  family_lab = c('Value','Surprise','Hybrid')
  lab_save = "Family"
  
}
d2t = 'Res4RFamilies.mat'


toplo = 3 # 1<- pxp, 2 <- Explained Variance, 3 <- Family-based comparison

if (toplo == 1){
#load day 2 file
matdata <- readMat(d2t) #read matlab data in R
matdata = matdata$res
matdata = matdata[, ,1]

pxp = matdata$pxp

Model = (matdata$models)
Experiment = (matdata$experiment)

for (m in 1:length(model_lab)){
  Model[Model == m] <- model_lab[m]
}

Model = factor(Model, levels = model_lab)

Experiment[Experiment == 1] <- "Exp. 1: SCR"
Experiment[Experiment == 2] <- "Exp. 2: SCR"
Experiment[Experiment == 3] <- "Exp. 3: SCR"
Experiment[Experiment == 4] <- "Exp. 3: PSR"
Experiment = factor(Experiment, levels = c("Exp. 1: SCR","Exp. 2: SCR","Exp. 3: SCR","Exp. 3: PSR"))

DF <- data.frame(Experiment, Model, pxp) #create data frame with results

# 1. Plot p.x.p. for all models:
p <- ggplot(DF, aes(x = Model, y = pxp, fill=Experiment))
p <- p + geom_bar(stat = "identity", position="dodge")+scale_fill_brewer(palette="RdYlBu")+theme_minimal()+ggtitle("Bayesian models")
p <- p + theme(axis.text.y= element_text(size=14), axis.title.y = element_text(size = 14, face = "bold"))+ ylim(c(0, 1.05))+scale_y_discrete(breaks=c(0, 0.5, 1), limits=c(0, 0.5, 1))#+coord_flip()
p <- p + theme(axis.text.x= element_text(size=14), axis.title.x = element_text(size = 14, face = "bold"))+scale_x_discrete(breaks=levels(Model))
p <- p + theme(plot.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=14), legend.title=element_text(size=14))+coord_flip()
p

print(p)
ggsave(paste("D:/Data/SCR/writing/version12/PXP_", lab_save, ".png", sep=""), width = 6, height = 8, dpi = 350)
} 
if (toplo == 2){

  matdata <- readMat(d2t) #read matlab data in R
  matdata = matdata$res2
  matdata = matdata[, ,1]
  
  EV = matdata$EV
  
  Model = (matdata$models)
  Experiment = (matdata$experiment)
  

  for (m in 1:length(model_lab)){
    Model[Model == m] <- model_lab[m]
  }
  
  Model = factor(Model, levels = model_lab)
  
  Experiment[Experiment == 1] <- "Exp. 1: SCR"
  Experiment[Experiment == 2] <- "Exp. 2: SCR"
  Experiment[Experiment == 3] <- "Exp. 3: SCR"
  Experiment[Experiment == 4] <- "Exp. 3: PSR"
  
  Experiment = factor(Experiment, levels = c("Exp. 1: SCR","Exp. 2: SCR","Exp. 3: SCR","Exp. 3: PSR"))
  
  DF <- data.frame(Experiment, Model, EV) #create data frame with results
  #DFsum <- data.frame(mean = tapply(DF$EV, DF$Model, mean))
  
  #means.sem <- ddply(melted, c("group", "variable"), summarise,
    #                 mean=mean(value), sem=sd(value)/sqrt(length(value)))
  
  # 1. Plot p.x.p. for all models:
  p <- ggplot(DF, aes(x = Model, y = EV))+ggtitle("All models")
  #p <- p + geom_jitter(aes(color=Experiment, fill=NULL), shape=21,width=0.05)
  p <- p + geom_bar(stat = "summary", fun.y = "mean", position="dodge")+ facet_wrap(~Experiment)+theme_minimal()
  #p <- p + geom_errorbar(aes(ymax = position="dodge")+ facet_wrap(~Experiment)+theme_minimal()
  p <- p + theme(axis.text.y= element_text(size=16), axis.title.y = element_text(size = 18, face = "bold"))#+scale_y_discrete(breaks=c(0, 0.5, 1), limits=c(0, 0.5, 1))#+coord_flip()
  p <- p + theme(axis.text.x= element_text(size=16, angle = 90, hjust = 1), axis.title.x = element_text(size = 18, face = "bold"))+scale_x_discrete(breaks=levels(Model))
  p <- p + theme(plot.title = element_text(size = 18, face = "bold"), strip.text = element_text(size=18))#+coord_flip()
  p
  
  print(p)
  ggsave(paste("D:/Data/SCR/writing/version12/EV_", lab_save, ".png", sep=""), width = 16, height = 10, dpi = 350)
}

# Plot families of models:
if (toplo == 3){
  #load day 2 file
  matdata <- readMat(d2t) #read matlab data in R
  matdata = matdata$resf
  matdata = matdata[, ,1]
  
  pxp = matdata$pxp
  
  Family = (matdata$families)
  Experiment = (matdata$experiment)
  
  for (m in 1:length(family_lab)){
    Family[Family == m] <- family_lab[m]
  }
  
  Family = factor(Family, levels = family_lab)
  
  Experiment[Experiment == 1] <- "Exp. 1: SCR"
  Experiment[Experiment == 2] <- "Exp. 2: SCR"
  Experiment[Experiment == 3] <- "Exp. 3: SCR"
  Experiment[Experiment == 4] <- "Exp. 3: PSR"
  Experiment = factor(Experiment, levels = c("Exp. 1: SCR","Exp. 2: SCR","Exp. 3: SCR","Exp. 3: PSR"))
  
  DF <- data.frame(Experiment, Family, pxp) #create data frame with results
  
  # 1. Plot p.x.p. for all families:
  p <- ggplot(DF, aes(x = Family, y = pxp, fill=Experiment))
  p <- p + geom_bar(stat = "identity", position="dodge")+scale_fill_brewer(palette="RdYlBu")+theme_minimal()+ggtitle("Model families")
  p <- p + theme(axis.text.y= element_text(size=14), axis.title.y = element_text(size = 14, face = "bold"))+ ylim(c(0, 1.05))+scale_y_discrete(breaks=c(0, 0.5, 1), limits=c(0, 0.5, 1))#+coord_flip()
  p <- p + theme(axis.text.x= element_text(size=14), axis.title.x = element_text(size = 14, face = "bold"))+scale_x_discrete(breaks=levels(Family))
  p <- p + theme(plot.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=14), legend.title=element_text(size=14))+coord_flip()
  p
  
  print(p)
  ggsave(paste("D:/Data/SCR/writing/version12/PXP_", lab_save, ".png", sep=""), width = 6, height = 8, dpi = 350)
  
}