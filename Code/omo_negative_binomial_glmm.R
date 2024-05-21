library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)
library(corrplot)
library(multcomp)
library(emmeans)
library(car)

omo_Data <- read.csv("C:\\Users\\HP\\Documents\\Mosquitoes-modelling-\\Data\\Updated_Omo_Phd.csv",
                     stringsAsFactors = TRUE)
view(omo_Data)

str(omo_Data)
####  Data cleaning
length(colnames(omo_Data))  

omo<- omo_Data[,1:24]    
view(omo)
attach(omo)

attach(omo)
sum(is.na(omo))  ###  No missing value
length(omo)
### check data distribution 


mean(omo$Anopheles)
var(omo$Anopheles)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

Anopheles_pred_new <- glmer.nb(Anopheles ~ scale(Turbidity)+
                              scale(Magnesium) + 
                              scale(pH)+
                             (1|Habitat),
                            data = omo)
summary(Anopheles_pred_new)

# Summarize the model
summary(Anopheles_pred_new_nb)

Aedes_nb<- glmer.nb(Aedes ~ 
                        scale(Anopheles) +
                        scale(Suspended.Solid)+
                         scale(TDS)+
                         scale(Chloride)+
                         scale(Colour)+
                         (1|Ecozones)+ (1|Habitat),
                       data = omo)
summary(Aedes_nb)



culex_pred_new_nb <- glmer.nb(Cules ~ normalize(Turbidity) +
                                normalize(pH) +
                                normalize(Nitrate) +
                                normalize(TDS) +
                                normalize(Anopheles) +
                                (1|Ecozones) + 
                                (1|Habitat),
                              data = omo)

# Summarize the model
summary(culex_pred_new_nb)
