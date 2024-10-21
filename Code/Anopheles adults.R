require(tidyverse)
library(lmerTest)
 
# library(MASS)
library(ggfortify)
library(corrplot)
library(multcomp)
library(emmeans)
library(car)

Ano_adult <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Mosquitoes-modelling-\\Data\\Anopheles_adult_abundance.csv", 
                      stringsAsFactors = TRUE)
view(Ano_adult)
colnames(Ano_adult)

Ano_adult$Abundance<- Ano_adult$Abundance + 1

ano_1<- glm.nb(Abundance ~ Anopheles,
                            data = Ano_adult,
                            link = log)
summary(ano_1)        ### No sig. difference
multi_comp_ano_1 <- glht(ano_1,
                             linfct = mcp(Anopheles = "Tukey"))
summary(multi_comp_ano_1)
#_________________________________________________________________

A.gambie <- Ano_adult %>% 
  filter(Anopheles == "An. gambiae s.s.") 

ano_2 <- glm.nb(Abundance ~ Ecovegetation.Zones,
                data = A.gambie,
                link = log)
summary(ano_2)        ### No sig. difference
multi_comp_ano_2 <- glht(ano_2,
                             linfct = mcp(Ecovegetation.Zones = "Tukey"))
summary(multi_comp_ano_2)

# ____________________________________________________________________________________
A.leesoni <- Ano_adult %>% 
  filter(Anopheles == "An. leesoni") 

var(Ano_adult$Abundance)

ano_3 <- glm.nb(Abundance ~ Ecovegetation.Zones,
                data = A.leesoni,
                link = log)
summary(ano_3)        ### No sig. difference
multi_comp_ano_3 <- glht(ano_3,
                         linfct = mcp(Ecovegetation.Zones = "Tukey"))
summary(multi_comp_ano_3)
# ___________________________________________________________________________________________

dersav <- Ano_adult %>% 
  filter(Ecovegetation.Zones == "Derived Savanna") 

var(Ano_adult$Abundance)

mod1 <- glm.nb(Abundance~ Anopheles,
                data = dersav,
                link = log)
summary(mod1)        ### No sig. difference

#_____________________________________________________________________________________

dersav <- Ano_adult %>% 
  filter(Ecovegetation.Zones == "Derived Savanna") 

var(Ano_adult$Abundance)

mod1 <- glm.nb(Abundance~ Anopheles,
               data = dersav,
               link = log)
summary(mod1)        ### No sig. difference

# ___________________________________________________________________________________

fresh <- Ano_adult %>% 
  filter(Ecovegetation.Zones == "Freshwater Swamp") 


mod2 <- glm.nb(Abundance~ Anopheles,
               data = fresh,
               link = log)
summary(mod2)        ### No sig. difference
#__________________________________________________________________________________


fresh <- Ano_adult %>% 
  filter(Ecovegetation.Zones == "Freshwater Swamp") 


mod2 <- glm.nb(Abundance~ Anopheles,
               data = fresh,
               link = log)
summary(mod2)        ### No sig. difference
# _________________________________________________________________________________


lowlnd <- Ano_adult %>% 
  filter(Ecovegetation.Zones == "Lowland Forest") 

mod3 <- glm.nb(Abundance~ Anopheles,
               data = lowlnd,
               link = log)
summary(mod3)        ### No sig. difference













