library(metafor)
library(ggplot2)
library(tidyverse)

master<-read.csv("Data/MASTER.csv")

dat<-escalc(measure="SMD", m1i=treat, sd1i=sd_treat, n1i=N_treat, m2i=ctrl, sd2i=sd_ctrl,
            n2i=N_ctrl, data=master, append =T)

re1<-rma(yi, vi, data=dat, method="REML" )
summary(re1)
#Test for Heterogeneity:
#Q(df = 98) = 10559.6105, p-val < .0001
#
#Model Results:
#    
#    estimate      se     zval    pval    ci.lb    ci.ub 
#-0.9864  0.2467  -3.9989  <.0001  -1.4699  -0.5030  *** 
    

#Response as a moderator?
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


#2. Response as a moderator
re2<-rma(yi,vi, mods=~Response, dat=dat, method="REML")
summary(re2)
#                      estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt                -0.7855  0.3365  -2.3345  0.0196  -1.4451  -0.1260  * 
#ResponseReproduction   -0.4990  0.5944  -0.8395  0.4012  -1.6641   0.6660    
#ResponseSurvival       -0.4007  0.6643  -0.6032  0.5464  -1.7028   0.9013 



# test bias

re2$ni<-dat$N_treat+dat$N_ctrl
funnel(re2)
funnel(re2, yaxis="ni")
funnel(re2, yaxis="ni")
dat$ni<-dat$N_ctrl+dat$N_treat
cor.test(dat$yi,dat$ni,method="kendall")
#z = 1.0892, p-value = 0.2761
#     tau  0.08133761 

#2. Response & calficying as a moderator

#            Calcifying Non-calcifying
#Growth               26             27
#Reproduction         27              0
#Survival             19              0

#Calcifying an be tested only for growth:

#separate the data
growth<-dat[dat$Response=="Growth",]
dat2<-dat[dat$Response!="Growth",]

#### Growth only #####

remv<-rma.mv(yi=yi,V=vi, dat= growth, mod=~pH*Calcifying, method="REML")


growth$Study_ID


g1<-rma(yi,vi, dat=growth, method="REML")
summary(g1)
#estimate      se     zval    pval    ci.lb    ci.ub 
#-0.7789  0.3276  -2.3776  0.0174  -1.4210  -0.1368  * 

funnel(g1)
ranktest(g1)
#Kendall's tau = -0.3019, p = 0.0013

g1$ni<-growth$ni

ggplot(data=growth, aes(x=yi, y=ni))+
    geom_point()+
    geom_vline(xintercept = g1$b)
ggsave("Output/yi_ni_plot.pdf", width = 4, height = 3)

funnel(trimfill(g1))
funnel(g1, addtau2=TRUE)
regtest(g1)    #p = 0.0056
ranktest(g1, pa) #p = 0.0013
cor.test(growth$yi,growth$ni,method="kendall")
#z = 0.39911, p-value = 0.6898


#Calcification as a moderator
g2<-rma(yi,vi, mods=~Calcifying, dat=growth, method="REML")
summary(g2)
#             estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt       -0.8135  0.3074  -2.6465  0.0081  -1.4160  -0.2110  ** 
#Calcifying1   -0.9122  0.3074  -2.9676  0.0030  -1.5146  -0.3097  **  non-calcifying

funnel(g2)
ranktest(g2)


#meta-regression with pH
g3<-rma(yi,vi, mods=pH, dat=growth, method="REML")
summary(g3)
#Test of Moderators (coefficient 2):
#QM(df = 1) = 0.8132, p-val = 0.3672

#Model Results:
#         estimate       se     zval    pval     ci.lb    ci.ub 
#intrcpt  -19.1446  19.3757  -0.9881  0.3231  -57.1203  18.8310    
#mods       2.2545   2.5002   0.9018  0.3672   -2.6457   7.1548  

g3<-rma(yi,vi, mods=~pH*Calcifying, dat=growth, method="REML")
summary(g3)

###### Reproduction and Survival

re3<-rma(yi,vi, mods=~Response, dat=dat2, method="REML")
summary(re3)
#Test of Moderators (coefficient 2):
#QM(df = 1) = 0.0212, p-val = 0.8844
#
#Model Results:
#    
#           estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt     -1.3106  0.5068  -2.5862  0.0097  -2.3038  -0.3174  ** 
#Response1   -0.1135  0.7803  -0.1454  0.8844  -1.6429   1.4159  

contrasts(dat2$Response)<-contr.sum
re4<-rma(yi,vi, mods=~Response, dat=dat2, method="REML")
summary(re4)
#           estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt     -1.3106  0.5068  -2.5862  0.0097  -2.3038  -0.3174  ** 
#Response1   -0.1135  0.7803  -0.1454  0.8844  -1.6429   1.4159   

#pH as a moderator -NO Don't use
re5<-rma(yi,vi, mods=pH, dat=dat2, method="REML")
summary(re5)
#Test of Moderators (coefficient 2):
#QM(df = 1) = 1.9369, p-val = 0.1640

#Model Results:
    
#         estimate      se     zval    pval     ci.lb   ci.ub 
#intrcpt  -10.2606  6.4853  -1.5821  0.1136  -22.9716  2.4505    
#mods       1.1870  0.8529   1.3917  0.1640   -0.4847  2.8587

#simple randome model
re6<-rma(yi, vi, dat=dat2, method="REML")
summary(re6)

#Test for Heterogeneity:
#Q(df = 45) = 5180.9189, p-val < .0001

#estimate      se     zval    pval    ci.lb    ci.ub 
#-1.2532  0.3790  -3.3065  0.0009  -1.9961  -0.5104  *** 

#bias test
funnel(re6)
funnel(re4)
ranktest(re6) #Kendall's tau = -0.3295, p = 0.0011
regtest(re6)  #p < .0001
cor.test(dat2$yi,dat2$ni,method="kendall")
#z = 0.92754, p-value = 0.3536
#tau  0.1042819 


#sample size
dat2$ni<-dat2$N_treat+dat2$N_ctrl
re6$ni<-dat2$ni

ggplot(data=dat2, aes(x=yi, y=ni))+
    geom_point()+
    geom_vline(xintercept = re6$b)
ggsave("Output/funnel_ni.vs.yi.pdf", width = 5, height = 4)


cor.test(dat2$yi,dat2$ni,method="kendall")
#z = 0.92754, p-value = 0.3536



#remove the one with effect size=-32

dat3<-dat2[-which.min(dat2$yi),]

#run rma again
re4.2<-rma(yi,vi, mods=~Response, dat=dat3, method="REML")
summary(re4.2)
#Test of Moderators (coefficient 2):
#QM(df = 1) = 0.0827, p-val = 0.7736

#           estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt     -1.3025  0.5015  -2.5974  0.0094  -2.2854  -0.3196  ** 
#Response1   -0.2223  0.7727  -0.2876  0.7736  -1.7368   1.2923 

#pH as a moderator 
re5.2<-rma(yi,vi, mods=pH, dat=dat3, method="REML")
summary(re5.2)
#Test of Moderators (coefficient 2):
#QM(df = 1) = 0.9369, p-val = 0.3331

#Model Results:
#         estimate      se     zval    pval     ci.lb   ci.ub 
#intrcpt   -7.4671  6.4873  -1.1510  0.2497  -20.1819  5.2477    
#mods       0.8256  0.8529   0.9679  0.3331   -0.8461  2.4972

#simple randome model
re6.2<-rma(yi, vi, dat=dat3, method="REML")
summary(re6.2)
#estimate      se     zval    pval    ci.lb    ci.ub 
# -1.2003  0.3749  -3.2020  0.0014  -1.9351  -0.4656  ** 

funnel(re6.2,addtau2=TRUE)
#
ggplot(data=dat3, aes(x=yi, y=ni))+
    geom_point()+
    geom_vline(xintercept = re6.2$b)
ggsave("Output/funnel_ni.vs.yi_noOutlier.pdf", width = 4, height = 3)

ranktest(re4)
#p = 0.0011








contrasts(dat$Response)
contrasts(dat$Calcifying)
#                          estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt                    -1.7517  0.4723  -3.7088  0.0002  -2.6774  -0.8260  *** 
#ResponseReproduction        0.4934  0.6690   0.7375  0.4608  -0.8178   1.8045      
#ResponseSurvival            0.5758  0.7271   0.7919  0.4284  -0.8493   2.0010      
#CalcifyingNon-calcifying    1.8485  0.6503   2.8424  0.0045   0.5739   3.1231   ** 
    

#Intercept="calcifying, growth ->significantly negative
#calcifying, reproduction
re3[[1]][1]+re3[[1]][2]
#-1.263876

#calcifying, survival
re3[[1]][1]+re3[[1]][3]
#-1.178012

#noncalcifying, growth
re3[[1]][1]+re3[[1]][2]+re3[[1]][3]+B_R4[[1]][4]
re3[[1]][1]+re3[[1]][2]+re3[[1]][3]+B_R4[[1]][4]
re3[[1]][1]+re3[[1]][2]+re3[[1]][3]+B_R4[[1]][4]
re3[[1]][1]+re3[[1]][2]+re3[[1]][3]+B_R4[[1]][4]
#


#2. Response & calficying as a moderator
re4<-rma(yi,vi, mods=~Calcifying, dat=dat, method="REML")
summary(re4)
#                          estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt                    -1.4135  0.2841  -4.9758  <.0001  -1.9702  -0.8567  *** 
#CalcifyingNon-calcifying    1.5105  0.5268   2.8675  0.0041   0.4781   2.5430   ** 


#contra.sum
contrasts(dat$Response)<-contr.sum
contrasts(dat$Calcifying)<-contr.sum
re5<-rma(yi,vi, mods=~Response*Calcifying, dat=dat, method="REML")
summary(re5)

table(dat$Response,dat$Calcifying)
