library(metafor)
library(ggplot2)

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


#2.2.7 Perform full 2-way analysis of the data. Obtain estimate for each combination of the factors. 
#Do your results agree with those from separate analyses? 
#What are your conclusions?

B_re1<-rma(yi= d,vi=Var_d,mod=~Landscape*Habitat, dat=batary,  method="REML")
summary(B_re1)

B_re1[1]
#Compare 2 results
Compare<-results2[,1:4]

Compare$two.way_estimate[1]<-B_re1[[1]][1]
Compare$two.way_estimate[3]<-B_re1[[1]][1]+B_re1[[1]][2]
Compare$two.way_estimate[2]<-B_re1[[1]][1]+B_re1[[1]][3]
Compare$two.way_estimate[4]<-B_re1[[1]][1]+B_re1[[1]][2]+B_re1[[1]][3]

Compare

contrasts(batary$Landscape)<-contr.sum
contrasts(batary$Habitat)<-contr.sum
B_R4<-rma(yi=d,vi=Var_d, mods=~Landscape*Habitat,dat=batary,method="REML")
summary(B_R4)

#cropland&complex
B_R4[[1]][1]+B_R4[[1]][2]+B_R4[[1]][3]+B_R4[[1]][4]

#cropland&simple
B_R4[[1]][1]-B_R4[[1]][2]+B_R4[[1]][3]-B_R4[[1]][4]

#grassland&complex
B_R4[[1]][1]+B_R4[[1]][2]-B_R4[[1]][3]-B_R4[[1]][4]

#grassland&simple
B_R4[[1]][1]-B_R4[[1]][2]-B_R4[[1]][3]+B_R4[[1]][4]


#3.2.8  Explore publication bias for the original  random  effects model, and for the 2-way model. Produce funnel plots and conduct tests for publication bias. What are your conclusions?
    
#Try and explain the results for 

funnel(B_R)
funnel(B_R, addtau2=TRUE) 
funnel(trimfill(B_R))

funnel(B_R4)
regtest(B_R4)
ranktest(B_R4)
