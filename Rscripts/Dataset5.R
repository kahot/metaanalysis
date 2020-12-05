library(metafor)
library(ggplot2)

#which patch size effects are important determinants of local population density for animals living in patchy landscapes. 

#1.5.1 Enter the data and print the names of the variables. 
#How are you going to calculate the variances?
bender<-read.csv("Data/Bender.csv") 
names(bender)

#effect size is reported as r (correlation coefficient):
#variance is calculated as v=1/(N-3)

bender$vi<-1/(bender$No.patches-3)

#1.5.2 Perform meta-analysis of the full data-set. Which model (random- or fixed-effects) should be used? What are your conclusions?
    
bender_r<-rma(yi=Effect.size.r, vi=vi, dat=bender, method="REML")
summary(bender_r)
#should be random effects model
#Model Results:

#   estimate      se    zval    pval   ci.lb   ci.ub 
#0.1136  0.0388  2.9307  0.0034  0.0376  0.1896  ** 
    

#1.5.3 Perform separate analyses for each Habitat. What are your conclusions?
b_habitats<-levels(bender$Habitat) #3 levels
b_re1<-data.frame(Habitat=b_habitats)

for (i in 1:length(b_habitats)){
        re<-rma(yi=Effect.size.r,vi=vi, dat= bender[bender$Habitat==b_habitats[i],], method="REML")
        b_re1$estimate[i]<-re[[1]]
        b_re1$Pvalue  [i]<-re[[5]]
        b_re1$CI_lb   [i]<-re[[6]]
        b_re1$CI_ub   [i]<-re[[7]]
}

b_re1
#     Habitat    estimate       Pvalue       CI_lb       CI_ub
#1       edge -0.19752411 3.099938e-03 -0.32841862 -0.06662960
#2 generalist  0.01489144 6.263832e-01 -0.04506183  0.07484471
#3   interior  0.34967088 8.279682e-14  0.25787327  0.44146849


#1.5.4 Perform separate analyses for each Taxon. What are your conclusions?
b_taxa<-levels(bender$Taxon) #3 levels
b_re2<-data.frame(Taxon=b_taxa)

for (i in 1:length(b_taxa)){
    re<-rma(yi=Effect.size.r,vi=vi, dat= bender[bender$Taxon==b_taxa[i],], method="REML")
    b_re2$estimate[i]<-re[[1]]
    b_re2$Pvalue  [i]<-re[[5]]
    b_re2$CI_lb   [i]<-re[[6]]
    b_re2$CI_ub   [i]<-re[[7]]
}

b_re2
#   Taxon  estimate     Pvalue       CI_lb     CI_ub
#1   bird 0.1013907 0.01475322  0.01989313 0.1828883
#2 insect 0.2035267 0.19997070 -0.10772027 0.5147738
#3 mammal 0.1842857 0.18033098 -0.08531454 0.45388


#1.5. 5 Tabulate number of studies in Habitat by Taxon

table(bender$Habitat,bender$Taxon)
#           bird insect mammal
#edge         48      0      4
#generalist   32      4      0
#interior     52      8      5


#1.5. 6 For birds, perform separate analyses for each Habitat. What are your conclusions?
bird1<-data.frame(Habitat=b_habitats)

for (i in 1:3){
    re<-rma(yi=Effect.size.r,vi=vi, dat= bender[bender$Habitat==b_habitats[i]& bender$Taxon=="bird",], method="REML")
    bird1$estimate[i]<-re[[1]]
    bird1$Pvalue  [i]<-re[[5]]
    bird1$CI_lb   [i]<-re[[6]]
    bird1$CI_ub   [i]<-re[[7]]
}
bird1
#     Habitat     estimate       Pvalue       CI_lb       CI_ub
#1       edge -0.194469161 6.541350e-03 -0.33463309 -0.05430523
#2 generalist  0.003715188 9.059770e-01 -0.05793379  0.06536416
#3   interior  0.370110744 2.708224e-16  0.28149187  0.45872962

#Patch size is significant on birds in the edge and interior habitats (not for generalist)

#2.5. 7 Perform meta-analysis using Habitat and Taxon as moderators, first separately, and then in a joint model. What are your conclusions?

# Habitat as a moderator
bender_rh<-rma(yi=Effect.size.r, vi=vi, dat=bender, mod=~Habitat,method="REML")
summary(bender_rh)
#Test of Moderators (coefficients 2:3):
#QM(df = 2) = 53.1388, p-val < .0001
#
#Model Results:
#                   estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt             -0.2025  0.0584  -3.4697  0.0005  -0.3169  -0.0881  *** 
#Habitatgeneralist    0.2784  0.0925   3.0096  0.0026   0.0971   0.4596   ** 
#Habitatinterior      0.5523  0.0760   7.2661  <.0001   0.4034   0.7013  ***    
#bender_r<-rma(yi=Effect.size.r,vi=1/(No.patches-3), dat=bender, mods=~Habitat, method="REML")

#Taxon as a moderator
bender_rt<-rma(yi=Effect.size.r, vi=vi, dat=bender, mod=~Taxon,method="REML")
summary(bender_rt)
#Test of Moderators (coefficients 2:3):
#QM(df = 2) = 0.6178, p-val = 0.7342
#
#Model Results:
#             estimate      se    zval    pval    ci.lb   ci.ub 
#intrcpt        0.1014  0.0420  2.4155  0.0157   0.0191  0.1837  * 
#Taxoninsect    0.0961  0.1402  0.6857  0.4929  -0.1787  0.3710    
#Taxonmammal    0.0746  0.1723  0.4333  0.6648  -0.2630  0.4123   

# 2-way analysis
bender_rht<-rma(yi=Effect.size.r, vi=vi, dat=bender, mod=~Taxon*Habitat,method="REML")
summary(bender_rht)
#Test of Moderators (coefficients 2:7):
#QM(df = 6) = 55.7934, p-val < .0001

#Model Results:
#                               estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt                         -0.2013  0.0607  -3.3185  0.0009  -0.3202  -0.0824  *** 
#Taxoninsect                     -0.2010  0.1452  -1.3840  0.1664  -0.4856   0.0836      
#Taxonmammal                     -0.0164  0.2323  -0.0705  0.9438  -0.4717   0.4389      
#Habitatgeneralist                0.2556  0.0975   2.6211  0.0088   0.0645   0.4467   ** 
#Habitatinterior                  0.5715  0.0816   7.0008  <.0001   0.4115   0.7315  *** 
#Taxoninsect:Habitatgeneralist    0.3949  0.2707   1.4589  0.1446  -0.1356   0.9254      
#Taxonmammal:Habitatinterior      0.1063  0.3032   0.3507  0.7258  -0.4879   0.7005  

contrasts(bender$Taxon)
contrasts(bender$Habitat)
#intercetp is bird at edge 


#2.5. 8  For birds, perform meta-analysis using Habitat as a moderator. What are your conclusions?
bender_bird<-rma(yi=Effect.size.r, vi=vi, dat=bender[bender$Taxon=="bird",], mod=~Habitat, method="REML")
summary(bender_bird)
#                   estimate      se     zval    pval    ci.lb    ci.ub 
#intrcpt             -0.2028  0.0587  -3.4518  0.0006  -0.3179  -0.0876  *** 
#Habitatgeneralist    0.2560  0.0945   2.7078  0.0068   0.0707   0.4413   ** 
#Habitatinterior      0.5730  0.0790   7.2553  <.0001   0.4182   0.7278  *** 


#2.5.9 Which models would you include in your paper?
    

#3.5.9 Explore the existence of publication bias.