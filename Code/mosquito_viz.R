


unique(omo$Habitat)
attach(omo)

Viz_anopheles <- omo %>% 
  select(Habitat, Anopheles) %>% 
  mutate(Habitat = as.factor(Habitat)) %>% 
  ggplot()+
  geom_boxplot(aes(x= Habitat, y = Anopheles, fill = Anopheles))+
  geom_point(aes(x= Habitat, y = Anopheles, fill = Habitat))+  
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 

PT_simpson <- PT_diversity %>%
  select(Location, Simpson_1.D, Site) %>%
  mutate(Site = as.factor(Site)) %>%  # Convert Site to a factor
  ggplot() +
  geom_boxplot(aes(x = Location, y = Simpson_1.D, fill = Site),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,) +
  geom_point(aes(x = Location, y = Simpson_1.D, colour = Site),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Simpson's diversity index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(PT_simpson)
