## Group project

## Load packages
library(tidyverse)
library(metafor)

## Load data
data<-read_csv("Data/MASTER.csv")

data1<-data%>%
     select(Study_ID, Study_ID2, Study_ID3, Study, pH, Species, Taxon, Calcifying, Response, ctrl, sd_ctrl, N_ctrl, treat, sd_treat, N_treat)%>%
     filter(Response=="Growth")%>%
     mutate(N_total=N_ctrl+N_treat)

data1$StudyID<-as.factor(data1$Study_ID)
data1$Calcifying<-as.factor(data1$Calcifying)

## Calculate effect sizes
# 1s should be treatment and 2s should be control
data2<-escalc("SMD", n1i=N_treat, n2i=N_ctrl, m1i=treat, m2i=ctrl, sd1i=sd_treat, sd2i=sd_ctrl, append=TRUE, data=data1)

## Random effects meta-analysis
## First run the mdoel without moderators to get at the overall effect
data_1<-rma(yi, vi, data=data2, method = "REML")
summary(data_1)
# Significant heterogeneity
# Acidification had a significantly negative relationship with growth

forest(data_1)
# Study #40 is potentially an outlier so we may wish to conduct a sensitivity analysis later excluding this data point
funnel(data_1)
# Doesn't look very good

# Run trimfill to identifying and correct for missing studies
funnel(trimfill(data_1), addtau2=TRUE)
trimfill(data_1)
# There aree no identified missing studies

# Run regression test to test for asymmetry
# On standard error
regtest(data_1)

# On sample size
regtest(data_1, predictor="ni")

#non-parametric test
ranktest(data_1)
# on sampel size
cor.test(data2$yi, data2$N_total, method = "kendall")


# visualize with sample size
funnel(data_1, yaxis = "ni",addtau2=TRUE)
# or...
plot(data2$yi, data2$N_total)
abline(v=data_1$b)
# with sample size there seems to be not bias.. larger sample sizes have great precision

# Calculate Rostenthal's fail safe
fsn(yi, vi, type = "Rosenthal", data=data2)
# Passes that test

## Conduct a sensitivity analysis where we remove the outlier
data3<-data2[-which.min(data2$yi),]

## Random effects meta-analysis
## First run the mdoel without modeerators to get at the overall effect
data_outlier<-rma(yi, vi, data=data3, method = "REML")
summary(data_outlier)
# Removing the outlier did not significantly change the results


## Now run the model with moderators for pH and calcifying organism
data_mod<-rma(yi, vi, data=data2, mods=~pH*Calcifying, method = "REML")
summary(data_mod)
# Removed about half of the studies that didn't have information for both moderators
# The Test of moderators suggests that the moderators do not signifcnatly reduce heterogeneity


## Multivariate meta-analysis
## First run the mdoel without moderators to get at the overall effect
data_r1<-rma.mv(yi, vi, random = list(~1 | Study_ID), data=data2, method = "REML")
summary(data_r1)
# Significant heterogeneity
# Acidification had a significantly negative relationship with growth

forest(data_r1)
# Study #40 is potentially an outlier so we may wish to conduct a sensitivity analysis later excluding this data point
funnel(data_r1)

## Now run the multivariate test with moderators
data_full<-rma.mv(yi, vi, random = list(~1 | Study_ID), mods=~pH*Calcifying, data=data2, method = "REML")
summary(data_full)

