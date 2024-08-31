
library(tidyverse)
library(devEMF)  # create vector files (Enhanced Metafile (EMF) format)


omo_Data <- read.csv("C:\\Users\\HP\\Desktop\\Dr Omoregir\\Phd_Omo\\Updated_Omo_Phd.csv",
                     stringsAsFactors = TRUE)
omo_Data <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Mosquitoes-modelling-\\Data\\Updated_Omo_Phd.csv",
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

length(unique(omo$Habitat)) ## see the unique types of habitat
attach(omo)

# Define lighter colors for the manual fill scale
very_light_orange <- "#FFEBCC"    # Very light shade of orange
very_light_red <- "#FFCCCC"       # Very light shade of red
very_light_black <- "#E0E0E0"     # Very light shade of black
very_light_brown <- "#D2B48C"     # Very light shade of brown
very_light_darkgreen <- "#C1FFC1"  # Very light shade of dark green

# Create a new palette with very light colors
very_light_fill_manual <- c(very_light_orange, very_light_red, very_light_black, very_light_brown, very_light_darkgreen)


fill_manual <- c("orange", "red", "black", "brown", "darkgreen")
omo$Habitat <- factor(omo$Habitat, levels = c("Containers",
                                              "Gutters",
                                              "Puddles",
                                              "Tyre track",
                                              "Used Tyres"))

  ## Culex sp. abundance
culex<- ggplot(aes(x = Habitat, y = Cules, fill = Habitat), data = omo) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.9) +
  geom_point(aes(x = Habitat, y = Cules, colour = Habitat),
             position = position_jitterdodge(jitter.width = 2, 
                                             dodge.width = 0.8),
             size = 5, alpha = 0.6) + 
  scale_fill_manual(values = very_light_fill_manual) +
  scale_color_manual(values = fill_manual) + 
  labs(x = "Habitat", 
       y = expression(italic("Culex")~ "spp.")) + 
  theme_bw()




  ## Aedes sp. abundance
Aedes<- ggplot(aes(x=Habitat, y= Aedes, fill = Habitat), data = omo)+
    stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
                 width = 0.2, colour = "black", size = 0.9)+
    geom_point(aes(x= Habitat, y = Aedes, colour = Habitat),
               position = position_jitterdodge(jitter.width = 2, 
                                               dodge.width = 0.8),
               size = 5, alpha = 0.6)+ 
    scale_fill_manual(values = very_light_fill_manual)+
    scale_color_manual(values = fill_manual)+
    labs(x = "Habitat", y = expression(italic("Aedes")~"spp."))+
    theme_bw()
  
  ## Anopheles sp. abundance
 Anopheles<- ggplot(aes(x=Habitat, y= Anopheles, fill = Habitat), data = omo)+
    stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.5) +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.5),
                 width = 0.2, colour = "black", size = 0.9)+
    geom_point(aes(x= Habitat, y = Anopheles, colour = Habitat),
               position = position_jitterdodge(jitter.width = 2, 
                                               dodge.width = 0.8),
               size = 5, alpha = 0.6)+ 
    scale_fill_manual(values = very_light_fill_manual)+
    scale_color_manual(values = fill_manual)+
    labs(x = "Habitat",
         y = expression(italic("Anopheles")~"spp."))+
    theme_bw()
  
  
  
  # Save Plots
  
  ggsave(plot= culex, 
         "C:\\Users\\DELL\\Documents\\Git in R\\Mosquitoes-modelling-\\Figures\\Culex.jpg", 
         width = 7, height = 5)
  
  ggsave(plot= Aedes, 
         "C:\\Users\\DELL\\Documents\\Git in R\\Mosquitoes-modelling-\\Figures\\Aedes.jpg", 
         width = 7, height = 5)
  
  ggsave(plot= Anopheles, 
         "C:\\Users\\DELL\\Documents\\Git in R\\Mosquitoes-modelling-\\Figures\\Anopheles.jpg", 
         width = 7, height = 5)
  
  
  
  
  
  
  # EMF file
  
  ggsave(plot= culex, 
         "C:\\Users\\DELL\\Documents\\Git in R\\Mosquitoes-modelling-\\Figures\\Culex.emf", 
         width = 8, height = 6)
  
  

  
  
  