rm(list=ls())

library(R.matlab)
library(nlme) #open nlme library
library(ggplot2)
#General path
setwd('D:/Data/SCR/writing/Results/')

plot_Bayes = 1

if (plot_Bayes == 1){
  model_lab = c('BM', 'KL', 'BS', 'BH1', 'BH2')
  d2t = 'Sim4R_Bayesian.mat'
  lab_save = "Bayesian"
  
}else{
  model_lab = c('RW1', 'RW2', 'PH', 'HM1', 'HM2', 'HM3', 'TD')
  d2t = 'Sim4R_RL.mat'
  lab_save = "RL"
  
}
  
order_cs = c('CS+', 'CS-')
family_lab = c('Outcome','Surprise','Hybrid')

#load day 2 file
matdata <- readMat(d2t) #read matlab data in R
matdata = matdata$res
matdata = matdata[, ,1]

Estimates = matdata$estim
CS = matdata$cond
Trial = matdata$trials
Model = matdata$model
Family = (matdata$families)

CS[CS == 1] <- "CS-"
CS[CS == 2] <- "CS+"

#create data frame:
CS = factor(CS, levels = order_cs)

for (f in 1:length(family_lab)){
  Family[Family == f] <- family_lab[f]
}

for (f in 1:length(model_lab)){
  Model[Model == f] <- model_lab[f]
}

Model = factor(Model, levels = model_lab)

Family <- factor(Family, levels = family_lab)
#group factors:

TF <- data.frame(CS, Estimates, Trial, Model) #create data frame with results

p <- ggplot(data = TF, aes(x=Trial, y = Estimates, colour = Family, linetype = CS), size = 2)
p <- p + stat_summary(fun.y = 'mean', geom = 'line', size = 1.25)+xlim(1, 40)+theme_minimal()
p <- p + facet_wrap(~Model, scales = "free")+theme( axis.text.y=element_blank())+scale_color_manual(values=c("#66c2a5","#fc8d62","#8da0cb"))
p <- p + theme(axis.text.x= element_text(size=14), axis.title.x = element_text(size = 12, face = "bold"))+ scale_x_continuous(breaks=c(0, 40))
p <- p + theme(axis.title.y = element_text(size = 14, face = "bold"))
p <- p + theme(strip.text = element_text(size=18), legend.text=element_text(size=14), legend.title=element_text(size=14))
p

print(p)
ggsave(paste("D:/Data/SCR/writing/version12/Simulations_", lab_save, ".png", sep=""), width = 10, height = 6, dpi = 350)

#light colors:"#66c2a5","#fc8d62","#8da0cb","#66c2a5"
# dark colors: "#1b9e77", "#d95f02", "#7570b3"

#+ scale_color_manual(values=c("#0571b0", "#92c5de", "#ca0020", "#f4a582"))+geom_line(aes(linetype = 4)) #"#1b9e77", "#d95f02", "#7570b3",



# 
# 
# 
# 
# Condition = factor(matdata$cond)#condition = factor for model
# subj = factor(matdata$subj)#subj = factor for model
# Block = matdata$time
# CS = factor(matdata$modcond)
# pow2 <- data.frame(subj, Condition, Power, Block, CS) #create data frame with results
# 
# 
# cond_levels = levels(pow2$Condition)
# subj_levels = levels(pow2$subj)
# 
# #for (lv in 1:length(cond_levels)) {
# #  for (sb in 1:length(subj_levels)) {
# #    n_trials = dim(pupil.data_reext[pupil.data_reext$cond == cond_levels[lv] & pupil.data_reext$subj == subj_levels[sb],])[1]
# #    pupil.data_reext[pupil.data_reext$cond == cond_levels[lv] & pupil.data_reext$subj == subj_levels[sb],"n_cond_trl_plus"] = 1:n_trials
# #    pupil.data_reext[pupil.data_reext$cond == cond_levels[lv] & pupil.data_reext$subj == subj_levels[sb],"n_cond_trl_minus"] = -n_trials:-1
# #  }
# #}
# 
# # model for CSa vs. CSb analaysis last 3 trials extinction vs. first 3 trials reextinction
# model.CSpCSm = lme(Power ~ 1+CS*Block, random = ~ 1|subj, data = pow2)
# 
# summary(model.CSpCSm )
# 
# an = anova(model.CSpCSm)
# 
# ggplot(pow2)+geom_jitter(aes(Condition, Power, color=Condition, label = Condition), width=0.1)+geom_boxplot(aes(Condition, Power, color=Condition, label = Condition), width=0.3)#+ ylim(-0.01, 0.21)
# #ggplot(pow2)+geom_boxplot(aes(Condition, Power, color=Condition, label = Condition), width=0.1)+geom_point(aes(Condition, Power, color=Condition, label = Condition), alpha=0.5)

