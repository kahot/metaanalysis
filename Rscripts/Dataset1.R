library(metafor)
library(ggplot2)

#Dataset 1
LK<-read.csv("Data/Leimu_Koricheva2006.csv")

#1.1.2 Perform  fixed effects meta-analysis of the data. Produce forest plot. 
# z is the r (correlation coefficeint)
LK_F<-rma(yi=LK$z,vi=LK$variance, method="FE")
summary(LK_F)
forest(LK_F)

#1.1.3	Is the fixed effects model appropriate?
#No.

#1.1.4 Perform random effects meta-analysis using REML. What are your conclusions? 
LK_R<-rma(yi=LK$z,vi=LK$variance, method="REML")
summary.rma(LK_R)

#1.1.5 Compare the width of confidence intervals for the combined effect for the fixed- and random-effects meta-analysis. Why does the width differ?

#Fixed
#estimate      se     zval    pval   ci.lb   ci.ub 
#  0.4628  0.0444  10.4192  <.0001  0.3758  0.5499  *** 

#Random
#estimate      se    zval    pval   ci.lb   ci.ub 
#  0.4731  0.0610  7.7507  <.0001  0.3535  0.5927  *** 

#Wider for randome because of added variance (tau, interstudy variability).  


#1.1.6  Try out different estimators of the τ2 such as DL, REML, PM and compare the results. 
#Also compare standard results to those using HKSJ method (use test=”knha”). 
#HKSJ t-based inference: another way to infer τ2

LK_R1<-rma(yi=LK$z,vi=LK$variance, method="REML")
LK_R2<-rma(yi=LK$z,vi=LK$variance, method="REML", test="knha")
LK_R3<-rma(yi=LK$z,vi=LK$variance, method="PM")
LK_R4<-rma(yi=LK$z,vi=LK$variance, method="DL")

results<-data.frame(method=c("REML","REML+HKSJ","PM","DJ"))
for (i in 1:4){
    re<-get(paste0("LK_R",i))
    results$estimate[i]<-re[[1]]
    results$Pvalue[i]<-re[[5]]
    results$CI_lb[i]<-re[[6]]
    results$CI_ub[i]<-re[[7]]
}

ggplot()+
    geom_point(data=results, aes(x=method, y=estimate, size=1.25))+
    geom_point(data=results, aes(x=method, y=CI_lb, shape='-'))+
    geom_point(data=results, aes(x=method, y=CI_ub))+
    ylab("CI")+theme_bw()+
    theme(legend.position = "none")
#HKSJ has wider CI


#3.1.5  Explore publication bias for the random and fixed effects models. 
#Produce funnel plots and conduct tests for publication bias. What are your conclusions?
    
#Try and explain the results for 

funnel(LK_F)
funnel(LK_R)
funnel(LK_R, yaxis="ni")
funnel(LK_R, addtau2=TRUE) 
funnel(trimfill(LK_F))
plot()
ranktest(LK_F) #nonparametric test (Use this!)
#Kendall's tau = -0.1950, p = 0.0910
regtest(LK_F)
#test for funnel plot asymmetry: z = -0.0932, p = 0.9258