library(metafor)
library(ggplot2)

#Dataset 3
#1.3.1 Enter the data and print the names of the variables. 
#Use escalc procedure in metafor to calculate lnOR and its variance. 
#Check the direction of the obtained lnOR.

hyatt<-read.csv("Data/Hyatt2003.csv")
colnames(hyatt)
dat <- escalc(measure="OR", bi= Near.total-Near.surv, di= Far.total-Far.surv, ai= Near.surv, ci= Far.surv,data=hyatt, append=TRUE )

#This is lnOR of survival, near to far, i.e. negative values mean lower probability of survival near the parents.

#1.3.2 Perform meta-analysis of the full data-set. Which model (random- or fixed-effects) should be used? What are your conclusions?
hyatt_r<-rma(yi,vi, dat=dat, method="REML")
summary(hyatt_r)
#Random since high heterogeneity of data

#1.3.3 Perform subset analysis by environment and by life stage separately. What are your conclusions?
hyatt_r1<-rma(yi,vi, dat=dat, subset=(Stage==2), method="REML")
summary(hyatt_r1)

unique(hyatt$Stage)
hyatt_r1<-rma(yi,vi, dat=dat, subset=(Stage==1), method="REML")
hyatt_r2<-rma(yi,vi, dat=dat, subset=(Stage==2), method="REML")

results3<-data.frame(Stage=1:2)
for (i in 1:2){
    hy<-rma(yi,vi, dat=dat, subset=(Stage==i), method="REML")
    results3$estimate[i]<-hy[[1]]
    results3$Pvalue[i]<-hy[[5]]
    results3$CI_lb [i]<-hy[[6]]
    results3$CI_ub [i]<-hy[[7]]
}

results3
#  Stage     estimate       Pvalue      CI_lb      CI_ub
#1     1  0.009385499 0.9553115883 -0.3188786  0.3376496
#2     2 -0.657591815 0.0008636024 -1.0444543 -0.2707293

#At Stage 2, the chance of survival is significantly small near parents



#2.3.4  Perform m-a using Dens as a continuous moderator for Stage=1. 
#What are your conclusions?
hy_r1<-rma(yi,vi, mods=~Dens, dat=dat[dat$Stage==1,], method="REML")
summary(hy_r1)

#Test of Moderators (coefficient 2):
#QM(df = 1) = 4.5432, p-val = 0.0331

#Model Results:
#         estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt    0.1017  0.1762   0.5769  0.5640  -0.2437   0.4471    
#Dens      -0.0042  0.0020  -2.1315  0.0331  -0.0080  -0.0003  * 

# Density appears as a significant moderator (affects negatively)



#2.3.5 Try plotting effects yi vs density for Stage=1 and density less than 200. 
#Does this change your conclusions?
dat2<-dat[dat$Stage==1&dat$Dens<200,]
ggplot(dat=dat2, aes(x=Dens, y=yi))+
    geom_point()
#when the density is <200, it seems no apparent effects of density. Outlier? at density=125

#include all density
ggplot(dat=dat, aes(x=Dens, y=yi, color=Stage))+
    geom_point()
ggplot(dat=dat[dat$Stage==1,], aes(x=Dens, y=yi))+
    geom_point()


#2.3.6  Perform m-a using Stage and then Env as a single moderator. What are your conclusions?
hy_r2<-rma(yi,vi, mods=~as.factor(Stage), dat=dat, method="REML")
summary(hy_r2)
#Test of Moderators (coefficient 2):
#QM(df = 1) = 3.7464, p-val = 0.0529
#
#Model Results:
#                   estimate      se     zval    pval    ci.lb   ci.ub 
#intrcpt              0.0011  0.1572   0.0071  0.9943  -0.3070  0.3092    
#as.factor(Stage)2   -0.7639  0.3947  -1.9356  0.0529  -1.5374  0.0096  . 

hy_r3<-rma(yi,vi, mods=~as.factor(Env), dat=dat, method="REML")
summary(hy_r3)
#Test of Moderators (coefficient 2):
#    QM(df = 1) = 2.7674, p-val = 0.0962
#
#Model Results:
#                 estimate      se     zval    pval    ci.lb   ci.ub 
#intrcpt           -0.2925  0.1776  -1.6475  0.0995  -0.6405  0.0555  . 
#as.factor(Env)2    0.5060  0.3042   1.6635  0.0962  -0.0902  1.1021  

#neither moderators are significant. Stage is marginally significant. 

#2.3.7 Perform 2-way analysis using Stage and Env as moderators.
#Tabulate the numbers of observations to explain the results.
hy_r4<-rma(yi,vi, mods=~(as.factor(Env))*as.factor(Stage), dat=dat, method="REML")
summary(hy_r4)
table(dat$Stage,dat$Env)
#   1  2
#1 83 46
#2 21  2

#There are only 2 Env2 data. Using Env does not make sense. Stage alone is marginally significant.

#2.3.8 Perform m-a using Stage and Dens as moderators. What are your conclusions?

hy_r5<-rma(yi,vi, mods=~Dens*as.factor(Stage), dat=dat, method="REML")
summary(hy_r5)
#Test of Moderators (coefficients 2:4):
#QM(df = 3) = 10.9267, p-val = 0.0121
#
#Model Results:
#                        estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt                   0.0923  0.1655   0.5578  0.5770  -0.2321   0.4167    
#Dens                     -0.0041  0.0018  -2.2662  0.0234  -0.0077  -0.0006  * 
#as.factor(Stage)2        -1.5127  0.6023  -2.5117  0.0120  -2.6931  -0.3323  * 
#Dens:as.factor(Stage)2    0.1294  0.0981   1.3196  0.1870  -0.0628   0.3216    

dat$Stage<-factor(dat$Stage)
contrasts(dat$Stage)<-contr.sum
hy_r6<-rma(yi,vi, mods=~Dens*as.factor(Stage), dat=dat, method="REML")
summary(hy_r6)
#Model Results:
#                        estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt                  -0.6640  0.3011  -2.2051  0.0274  -1.2542  -0.0738  * 
#Dens                      0.0606  0.0490   1.2356  0.2166  -0.0355   0.1567    
#as.factor(Stage)1         0.7564  0.3011   2.5117  0.0120   0.1661   1.3466  * 
#Dens:as.factor(Stage)1   -0.0647  0.0490  -1.3196  0.1870  -0.1608   0.0314   

#The mean combined effect is -0.6640.
#Stage1 has  -0.6640+0.7564=0.0924  slightly positive effects
#Density has  -0.6640+ 0.0606= -0.6032 negative effects
#Stage2 has -0.6640-0.7564=-1.4204 negative effects


#3.3.7 Explore publication bias for the original random effects model, and for the 2-way model. Produce funnel plots and conduct tests for publication bias. What are your conclusions? Note the drop in standard errors between the two models.

#Try and explain the results for 

funnel(hyatt_r)
funnel(hyatt_r,atrans=exp)

funnel(hyatt_r1)
regtest(hyatt_r)

