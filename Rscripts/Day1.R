library(metafor)
library(ggplot2)
valkama<-read.csv("Data/Valkama2008.csv")
names(valkama)
dat <- escalc(measure="ROM", m1i=Mean_treat, sd1i=SD_treat,
              n1i=N_treat, m2i=Mean_control, sd2i=SD_control, n2i=N_control,
              data=valkama, append=TRUE)
names(dat)

valkama_r1<-rma(yi,vi, data=dat,
                subset=(Variable.2=="stem height")
                &(Species.2=="reed"),method="FE")
forest(valkama_r1)
summary(valkama_r1)


valkama_r2<-rma(yi,vi, data=dat, subset=(Variable.2=="stem height")
                 &(Species.2=="reed"),method="REML")
summary(valkama_r2)

###


#Practical 1
#Dataset 1
LK<-read.csv("Data/Leimu_Koricheva2006.csv")
LK[1:5,]

#1.1.2 Perform  fixed effects meta-analysis of the data. Produce forest plot. 
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
#0.4628  0.0444  10.4192  <.0001  0.3758  0.5499  *** 
#Random
#estimate      se    zval    pval   ci.lb   ci.ub 
#0.4731  0.0610  7.7507  <.0001  0.3535  0.5927  *** 
    
#Wider for randome because of added variance (tau, interstudy variability).  
    
#1.1.6  Try out different estimators of the τ2 such as DL, REML, PM and compare the results. 
#Also compare standard results to those using HKSJ method (use test=”knha”).

LK_R1<-rma(yi=LK$z,vi=LK$variance, method="REML")
LK_R2<-rma(yi=LK$z,vi=LK$variance, method="REML", test="knha")
LK_R3<-rma(yi=LK$z,vi=LK$variance, method="PM")
LK_R4<-rma(yi=LK$z,vi=LK$variance, method="DL")

results<-data.frame(method=c("REML","REML+HKSJ","PM","DJ"))
for (i in 1:4){
    LK<-get(paste0("LK_R",i))
    results$estimate[i]<-LK[[1]]
    results$Pvalue[i]<-LK[[5]]
    results$CI_lb[i]<-LK[[6]]
    results$CI_ub[i]<-LK[[7]]
}

library(ggplot2)
ggplot()+
    geom_point(data=results, aes(x=method, y=estimate, size=1.25))+
    geom_point(data=results, aes(x=method, y=CI_lb, shape='-'))+
    geom_point(data=results, aes(x=method, y=CI_ub))+
    ylab("CI")+theme_bw()+
    theme(legend.position = "none")
    
#dataset 2
#1.2.1 Read the data. Print 5 first rows.
batary<-read.csv("Data/Batary1.csv")
batary[1:5,]
#1.2.2 Perform  fixed-effects meta-analysis of the data. Produce forest plot. 
B_F<-rma(yi=d,vi=Var_d,dat=batary, method="FE")
summary.rma(B_F)
forest(B_F)

#1.2.3	Is the fixed effects model appropriate?
 #no   
#1.2.4 Perform random-effects meta-analysis using REML. What are your conclusions? 
B_R<-rma(yi= d,vi=Var_d,dat=batary,  method="REML")
summary.rma(B_R)

#1.2.5	Repeat the MA separately for each habitat. What are your conclusions?
levels(batary$Habitat)

#1. Cropland
B_R1<-rma(yi=d,vi=Var_d, dat=batary, subset=(Habitat=="Cropland"),method="REML")
summary(B_R1)
#estimate      se    zval    pval   ci.lb   ci.ub 
#0.7901  0.1464  5.3953  <.0001  0.5031  1.0771  *** 
    
#2, Grassland 
B_R2<-rma(yi=d,vi=Var_d, dat=batary[batary$Habitat=="Grassland",],method="REML")
summary(B_R2)
#estimate      se    zval    pval   ci.lb   ci.ub 
#0.6755  0.0920  7.3413  <.0001  0.4951  0.8558  *** 
    
#1.2.6	Repeat the MA for each combination of habitat by landscape. What are your conclusions? Do they agree with the published results?
B_R21<-rma(yi=d,vi=Var_d, dat= batary, subset=(landscape_n==1)&(habitat_n==2), method="REML")
summary(B_R21)

#Repeat for each combination of landscape and habitat. Write down the answers.
habitats<-levels(batary$Habitat)
lands<-levels(batary$Landscape)
results2<-data.frame(expand.grid(habitats,lands))
colnames(results2)<-c("Habitat","Landscape")
for (i in 1:2){
    for (k in 1:2){
        BR<-rma(yi=d,vi=Var_d, dat= batary, subset=(Landscape==lands[i])&(Habitat==habitats[k]), method="REML")
        results2$estimate[results2$Habitat==habitats[k]&results2$Landscape==lands[i]]<-BR[[1]]
        results2$Pvalue[results2$Habitat==habitats[k]&results2$Landscape==lands[i]]<-BR[[5]]
        results2$CI_lb [results2$Habitat==habitats[k]&results2$Landscape==lands[i]]<-BR[[6]]
        results2$CI_ub [results2$Habitat==habitats[k]&results2$Landscape==lands[i]]<-BR[[7]]
    }
}
        
results2
#    Habitat Landscape  estimate       Pvalue      CI_lb     CI_ub
#1  Cropland   Complex 0.2337893 4.291912e-01 -0.3458112 0.8133897
#2 Grassland   Complex 0.7603099 8.563479e-09  0.5014622 1.0191577
#3  Cropland    Simple 0.9498882 9.606226e-09  0.6254030 1.2743734
#4 Grassland    Simple 0.5283512 2.363769e-06  0.3089338 0.7477686

#1.2.7  Compare previous  results to those using HKSJ method (use test=”knha”).







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

#Dataset4
#1.4.1 Enter the data and print the names of the variables. 
valkama<-read.csv("Data/Valkama2008.csv")
names(valkama)
#Use procedure escalc in metaphor to calculate the log Response Ratio and its variance.
dat<-escalc(measure="ROM", m1i=Mean_treat, sd1i=SD_treat, n1i=N_treat, m2i=Mean_control, sd2i=SD_control, n2i=N_control, data=valkama, append =T)
    
#1.4.2 Perform meta-analysis of the stem height of re-growing reed. 
#Use variables Species.2 and Variable.2 to select reed as the species and stem height as the response. 
#Which model (random- or fixed-effects) should be used?  Produce the forest plot. What are your conclusions?
 
vl<-rma(yi, vi, data=dat[dat$Species.2=="reed"&dat$Variable.2=="stem height",], method="REML" )
#valkama_r1<-rma(yi,vi, data=dat, subset=(Variable.2=="stem height") &(Species.2=="reed"),method="REML")
forest(vl)
summary(vl)

#1.4.3 Try out different estimators of the τ2 such as DL, REML, PM and compare the results. Also compare standard results to those using HKSJ method (use test=”knha”).

valkama_r2<-rma(yi,vi, data=dat, subset=(Variable.2=="stem height") &(Species.2=="reed"),method="PM",test="knha")
summary(valkama_r2)


vl_R1<-rma(yi,vi,data=dat[dat$Species.2=="reed"&dat$Variable.2=="stem height",], method="REML")
vl_R2<-rma(yi,vi,data=dat[dat$Species.2=="reed"&dat$Variable.2=="stem height",], method="REML", test="knha")
vl_R3<-rma(yi,vi,data=dat[dat$Species.2=="reed"&dat$Variable.2=="stem height",], method="PM")
vl_R4<-rma(yi,vi,data=dat[dat$Species.2=="reed"&dat$Variable.2=="stem height",], method="DL")

results4<-data.frame(method=c("REML","REML+HKSJ","PM","DJ"))
for (i in 1:4){
    re<-get(paste0("vl_R",i))
    results4$estimate[i]<-re[[1]]
    results4$Pvalue[i]<-re[[5]]
    results4$CI_lb[i]<-re[[6]]
    results4$CI_ub[i]<-re[[7]]
}

ggplot()+
    geom_point(data=results4, aes(x=method, y=estimate, size=0.95))+
    geom_point(data=results4, aes(x=method, y=CI_lb, size=0.9))+
    geom_point(data=results4, aes(x=method, y=CI_ub, size=0.9))+
    ylab("CI")+theme_bw()+
    theme(legend.position = "none")


#1.4.4 Repeat the analysis for all non-reed species.

valkama_r2<-rma(yi,vi, data=dat, subset=(Variable.2=="stem height") &(Species.2!="reed"),method="REML")
summary(valkama_r2)

#1.4.5 Back-transform log Response Ratio r for stem height of reed by using (Exp(r)-1)*100% to express the results as the percentage change from unmanaged sites.

c((exp(vl_R1$b)-1)*100,(exp(vl_R1$ci.lb)-1)*100, (exp(vl_R1$ci.ub)-1)*100)
#[1] -19.315961 -37.822489   4.698853
#-19% reduction in stem height


#1.4.6  Perform subset analyses by the types of reed management (select first burning and then harvesting in Management.1). 
#What are your conclusions?

levels(valkama$Management.1)
valkama_m1<-rma(yi,vi, data=dat, subset=(Variable.2=="stem height")&(Species.2=="reed")&(Management.1=="harvesting"),method="REML", test="knha")
summary(valkama_m1)
valkama_m2<-rma(yi,vi, data=dat, subset=(Variable.2=="stem height")&(Species.2=="reed")&(Management.1=="burning"),method="REML", test="knha")
summary(valkama_m2)

#neither are signifincat at alpha=0.05, but burning is better managmenet as marginally significant. 