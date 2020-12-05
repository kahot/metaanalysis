library(metafor)
library(ggplot2)

master<-read.csv("Data/MASTER.csv")

dat<-escalc(measure="SMD", m1i=treat, sd1i=sd_treat, n1i=N_treat, m2i=ctrl, sd2i=sd_ctrl,
            n2i=N_ctrl, data=master, append =T)

re1<-rma(yi, vi, data=dat, method="REML" )
summary(re1)


#Response as a modulator?
#1. look separately
responses<-levels(master$Response) #3 levels
re1<-data.frame(Response=responses)

for (i in 1:length(responses)){
    re<-rma(yi=yi,vi=vi, dat= dat[dat$Response==responses[i],], method="REML")
    re1$estimate[i]<-re[[1]]
    re1$Pvalue  [i]<-re[[5]]
    re1$CI_lb   [i]<-re[[6]]
    re1$CI_ub   [i]<-re[[7]]
}
re1
#      Response   estimate      Pvalue     CI_lb       CI_ub
#1       Growth -0.7789289 0.017427521 -1.421046 -0.13681202
#2 Reproduction -1.1771252 0.005871094 -2.014588 -0.33966214
#3     Survival -1.2328278 0.062163512 -2.528341  0.06268571

#2. Response as a modulator
re2<-rma(yi,vi, mods=~Response, dat=dat, method="REML")
summary(re2)
#                      estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt                -0.7855  0.3365  -2.3345  0.0196  -1.4451  -0.1260  * 
#ResponseReproduction   -0.4990  0.5944  -0.8395  0.4012  -1.6641   0.6660    
#ResponseSurvival       -0.4007  0.6643  -0.6032  0.5464  -1.7028   0.9013 
contrasts(dat$Response)



