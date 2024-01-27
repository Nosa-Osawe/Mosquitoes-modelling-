library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)


omo_Data <- read.csv("C:\\Users\\HP\\Desktop\\Dr Omoregir\\Phd_Omo\\Updated_Omo_Phd.csv",
                     stringsAsFactors = TRUE)
view(omo_Data)

str(omo_Data)
####  Data cleaning
length(colnames(omo_Data))  

omo<- omo_Data[,1:24]    
view(omo)

sum(is.na(omo))  ###  No missing value
length(omo)

attach(omo)
### check data distribution 

hist(omo$Anopheles)
hist(log((omo$Anopheles)+1))
hist(log((omo$Aedes)+1))
hist(log((omo$Cules)+1))  ### Even a log (x + 1) transformation did not normalize the data

omo$Anopheles_T <- log((omo$Anopheles)+1)

mean(omo$Anopheles)
var(omo$Anopheles)

###  

Anopheles_pred <- glmer(Anopheles ~ scale(Turbidity)* DO + Chloride* Depth+  (1|Ecozones),
                      data = omo,
                      family = poisson(link = "log"))
summary(Anopheles_pred)

 
Anopheles_pred2 <- glmer(Anopheles ~ scale(Turbidity)* scale(DO) + scale(Cules)+scale(Depth)+
                           scale(Magnesium) + 
                           (1|Ecozones),
                        data = omo,
                        family = poisson(link = "log"))
summary(Anopheles_pred2)


anova(Anopheles_pred,Anopheles_pred2 )
  