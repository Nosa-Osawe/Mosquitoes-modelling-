library(vegan)
library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)
library(corrplot)

CCA_mosquito <- cca(omo)
# Install and load the Vegan package
install.packages("vegan")
library(vegan)

# Load the example dataset
data(varespec)
data(varechem)


# Perform Canonical Correspondence Analysis (CCA)
cca_result <- cca(varespec, varechem)

# Summary of the CCA
summary(cca_result)

# Scree plot
screeplot(cca_result)

# Biplot 
plot(cca_result)


omo_physicochemical <- omo[,4:20]
head(omo_physicochemical)

Omo_mosquito <- omo[,21:23]
head(Omo_mosquito)




length(rownames(varespec))
view(varespec)
