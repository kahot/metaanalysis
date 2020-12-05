library(metafor)
library(ggplot2)


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



df<-valkama[(valkama$Variable.2=="stem height")
   &(valkama$Species.2=="reed"),]

