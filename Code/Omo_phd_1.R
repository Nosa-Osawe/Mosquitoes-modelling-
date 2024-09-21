library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)
library(corrplot)
library(multcomp)
library(emmeans)
library(car)

omo_Data <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Mosquitoes-modelling-\\Data\\Updated_Omo_Phd.csv",
                     stringsAsFactors = TRUE)
 
####  Data cleaning
length(colnames(omo_Data))  

omo<- omo_Data[,1:24]    
view(omo)
attach(omo)

library(dplyr)


omo <- omo %>%
  mutate(ecozone = if_else(Ecozones == "Lowland Rainforest", 
                           "Lowland Forest", 
                           Ecozones)) %>% 
  select(-Ecozones) %>% 
  rename("Ecozones" = ecozone) %>% 
  select(Ecozones, everything()) %>%
  as.data.frame()

attach(omo)
sum(is.na(omo))  ###  No missing value
length(omo)
### check data distribution 

hist(omo$Anopheles)
hist(log((omo$Anopheles)+1))
hist(log((omo$Aedes)+1))
hist(log((omo$Cules)+1))  ### Even a log (x + 1) transformation did not normalize the data

omo$Anopheles_T <- log((omo$Anopheles)+1)

mean(omo$Anopheles)
var(omo$Anopheles)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalize(omo$Turbidity)
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

Anopheles_pred_new <- glmer(Anopheles ~ scale(Turbidity)+
                              scale(Magnesium) + 
                              scale(pH)+
                           (1|Ecozones)+ (1|Habitat),
                         data = omo,
                         family = poisson(link = "log"))
summary(Anopheles_pred_new)
vif(Anopheles_pred_new)


confint(Anopheles_pred_new)



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



Aedes_pred_new<- glmer(Aedes ~ 
                     scale(Anopheles) +
                       scale(Suspended.Solid)+
                     scale(TDS)+
                     scale(Chloride)+
                       scale(Colour)+
                     (1|Ecozones)+ (1|Habitat),
                   data = omo,
                   family = poisson(link = "log"))
summary(Aedes_pred_new)


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



culex_pred_new <- glmer(Cules ~ scale(Turbidity) +
                      scale(pH)+
                        scale(Nitrate)+
                        scale(TDS)+
                        scale(Anopheles)+
                      (1|Habitat) + (1|Ecozones),
                    data = omo,  
                    family = poisson(link = "log"))
summary(culex_pred_new)

#
culex_play <- glmer(Cules ~ scale(Turbidity) +
                       scale(Nitrate)+
                      scale(TDS)+
                      scale(pH)+
                     # scale(Anopheles)+
                          (1|Habitat) + (1|Ecozones),
                        data = omo,  
                        family = poisson(link = "log"))
summary(culex_play)



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
nb_glm_omo <- omo
nb_glm_omo$Aedes <- nb_glm_omo$Aedes + 1
nb_glm_omo$Anopheles <- nb_glm_omo$Anopheles + 1
nb_glm_omo$Cules <- nb_glm_omo$Cules + 1


anopheles_compare <- glm.nb(Anopheles ~ Habitat,
                    data = nb_glm_omo,
                    link = log)
summary(anopheles_compare)        ### No sig. difference
multi_comp_anopheles <- glht(anopheles_compare,
                             linfct = mcp(Habitat = "Tukey"))
summary(multi_comp_anopheles)

emm_anopheles <- emmeans(anopheles_compare, ~ Habitat)
summary(emm_anopheles)
#-----------------------------------------------------------------

aedes_compare <- glm.nb(Aedes ~ Habitat,
                            data = nb_glm_omo,
                            link = log)
summary(aedes_compare)          ### No sig. difference
multi_comp_aedes <- glht(aedes_compare, linfct = mcp(Habitat = "Tukey"))
summary(multi_comp_aedes)

# -----------------------------------------------------------------

culex_compare <- glm.nb(Cules ~ Habitat, 
                        data = nb_glm_omo,
                        link = log)
summary(culex_compare)
multi_comp_culex <- glht(culex_compare, linfct = mcp(Habitat = "Tukey"))
summary(multi_comp_culex)

# ------------------------------------------------------------------

# -       Descriptive statistics

omo_mosquito_summary <- as.data.frame(omo %>% group_by(Habitat) %>% 
  summarise(mean_anopheles = mean(Anopheles),
                  sd_anopheles = sd(Anopheles),
                  mean_culex = mean(Cules),
                  sd_culex= sd(Cules),
            mean_aedes = mean(Aedes),
            sd_aedes = sd(Aedes)))
write.csv(omo_mosquito_summary, "C:\\Users\\HP\\Documents\\Mosquitoes-modelling-\\Data\\omo_mosquito_summary.csv")
