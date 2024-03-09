library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)
library(corrplot)

omo_Data <- read.csv("C:\\Users\\HP\\Documents\\Mosquitoes-modelling-\\Data\\Updated_Omo_Phd.csv",
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

### WE are done with the anopheles model
Anopheles_pred1 <- glmer(Anopheles ~ scale(Turbidity)* scale(DO) +
                          # scale(Cules)+
                           scale(Aedes)+scale(Depth)+
                           scale(Magnesium) + 
                           (1|Ecozones)+ (1|Habitat),
                        data = omo,
                        family = poisson(link = "log"))
summary(Anopheles_pred1)### this is the best model, thus far!



Anopheles_pred2 <- glmer(Anopheles ~ scale(Turbidity)* scale(DO) +
                            scale(Cules)+
                           scale(Aedes)+scale(Depth)+
                           scale(Magnesium) + 
                           (1|Ecozones)+ (1|Habitat),
                         data = omo,
                         family = poisson(link = "log"))
summary(Anopheles_pred2)   
confint(Anopheles_pred2)



anova(Anopheles_pred1,Anopheles_pred2)

################################################################################

Aedes_pred<- glmer(Aedes ~ 
                      scale(Anopheles) +
                      scale(Suspended.Solid)+
                     scale(TDS)+
                    scale(Chloride)+
                     scale(Colour)+
                     scale(BOD)+
                       (1|Ecozones)+ (1|Habitat),
                     data = omo,
                     family = poisson(link = "log"))
summary(Aedes_pred)
confint(Aedes_pred)
####################################################################################

culex_pred <- glmer(Cules ~ scale(Turbidity) +
                      scale(pH)+
                      scale(Nitrate)+
                      scale(BOD)+
                      scale(DO)*
                      scale(TDS)+
                      (1|Ecozones)+ (1|Habitat),
                    data = omo,  
                    family = poisson(link = "log"))
summary(culex_pred)



# Calcium, Colour, 


          ### Correlation of parameters

cor_matrix <- cor(omo[, c("Turbidity", "pH", "Depth", "Colour", "TDS", "Suspended.Solid",
                          "Total.Solid", "Conductivity","Chloride", "Alkalinity", 
                          "Hardness.as.CaCO3", "Phosphate", "Sulphate", "Nitrate",
                          "DO", "BOD", "Calcium",  "Magnesium", "Anopheles" )], 
                  method= "spearman")
corrplot(cor_matrix, 
         method = "color", 
         addCoef.col = 'black', 
         tl.cex = 0.7,      # Variable font size
         tl.col = 'black',
         number.cex = 0.5) # font size

########################################################################################

anopheles_compare <- glm.nb(Anopheles ~ Habitat,
                    data = omo,
                    link = log)
summary(anopheles_compare)        ### No sig. difference


aedes_compare <- glm.nb(Aedes ~ Habitat,
                            data = omo,
                            link = log)
summary(aedes_compare)          ### No sig. difference




culex_compare <- glm.nb(Cules ~ Habitat,
                        data = omo,
                        link = log)
summary(culex_compare)


omo$Habitat <- factor(omo$Habitat, levels = c("Containers",
                                              "Gutters",
                                              "Puddles",
                                              "Tyre track",
                                              "Used Tyres"))


omo$Habitat <- factor(omo$Habitat, levels = c(
                                              "Gutters","Containers",
                                              "Puddles",
                                              "Tyre track",
                                              "Used Tyres"))


omo$Habitat <- factor(omo$Habitat, levels = c("Puddles",
  "Gutters","Containers",
  "Tyre track",
  "Used Tyres"))  

omo$Habitat <- factor(omo$Habitat, levels = c("Tyre track","Puddles",
                                              "Gutters","Containers",
                                              "Used Tyres")) 

omo$Habitat <- factor(omo$Habitat, levels = c("Used Tyres","Tyre track","Puddles",
                                              "Gutters","Containers"
                                              )) 
