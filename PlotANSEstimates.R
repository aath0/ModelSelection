rm(list=ls())

library(R.matlab)
library(nlme) #open nlme library
library(ggplot2)
#General path
setwd('D:/Data/SCR/writing/Results/')

exp_lab = c('Exp. 1: SCR', 'Exp. 2: SCR', 'Exp. 3: SCR', 'Exp. 3: PSR')
d2t = 'ANSEstimates.mat'
lab_save = "ANS"
order_cs = c('CS+', 'CS-')

#load day 2 file
matdata <- readMat(d2t) #read matlab data in R
matdata = matdata$res
matdata = matdata[, ,1]

Estimates = matdata$estim
CS = matdata$cond
Trial = matdata$trials
Experiment = matdata$experiment

CS[CS == 1] <- "CS+"
CS[CS == 2] <- "CS-"

#create data frame:
CS = factor(CS, levels = order_cs)

for (f in 1:length(exp_lab)){
  Experiment[Experiment == f] <- exp_lab[f]
}

Experiment = factor(Experiment, levels = exp_lab)

#group factors:

TF <- data.frame(CS, Estimates, Trial, Experiment) #create data frame with results

p <- ggplot(data = TF, aes(x=Trial, y = Estimates, linetype = CS), size = 2)
p <- p + stat_summary(fun.y = 'mean', geom = 'line', size = 1.25, color = "#636363")+xlim(1, 40)+theme_minimal()
p <- p + facet_wrap(~Experiment, scales = "free")+theme( axis.text.y=element_blank())
p <- p + theme(axis.text.x= element_text(size=14), axis.title.x = element_text(size = 12, face = "bold"))+ scale_x_continuous(breaks=c(0, 40))
p <- p + theme(axis.title.y = element_text(size = 14, face = "bold"))
p <- p + theme(strip.text = element_text(size=18), legend.text=element_text(size=14), legend.title=element_text(size=14))
p

print(p)
ggsave(paste("D:/Data/SCR/writing/version12/Simulations_", lab_save, ".png", sep=""), width = 10, height = 6, dpi = 350)
