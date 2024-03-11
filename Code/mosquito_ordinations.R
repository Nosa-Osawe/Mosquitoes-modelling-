library(vegan)
library(lme4)
library(lmerTest)
library(tidyverse)
library(MASS)
library(ggfortify)
library(corrplot)
library(factoextra)
library(FactoMineR)

###########################################################################################
##   CCA 

omo_physicochemical <- omo[,4:20]
head(omo_physicochemical)

Omo_mosquito <- omo[,21:23]
head(Omo_mosquito)

CCA_mosq <- cca(Omo_mosquito, omo_physicochemical)
summary(CCA_mosq)
screeplot(CCA_mosq)

plot(CCA_mosq)


# Assuming CCA_mosq is your CCA object


plot(CCA_mosq, display = c("species", "sites"), type = "p", pch = "", col = "blue")
text(CCA_mosq, display = "species", col = "red", cex = 1, pos = 3)
text(CCA_mosq, display = "cn", col = "black", cex = 0.7, pos = 3)
##############################################################################################

# CA

mosquito_tab <- omo %>% 
  group_by(Habitat) %>% 
  summarise(
    Anopheles = sum(Anopheles),
    Culex = sum(Cules),
    Aedes = sum(Aedes)
  )
mosquito_tab <- as.data.frame(mosquito_tab)

row.names(mosquito_tab) <- mosquito_tab$Habitat

# Remove the first column (if needed)
mosquito_tab <- mosquito_tab[, -1]  ### Careful here!!!

view(mosquito_tab)


chisq_mosq <- chisq.test(mosquito_tab)

res.ca.mosq <- CA(mosquito_tab, graph = FALSE)
summary(res.ca.mosq)

eig_mosq <- get_eigenvalue(res.ca.mosq)

fviz_screeplot(res.ca.mosq, addlabels = TRUE, ylim = c(0, 50)) ## 100


biplot_mosq <-fviz_ca_biplot(res.ca.mosq, alpha.col = 0.5,
                             map ="colgreen", arrow = c(FALSE, TRUE),
                             repel = TRUE,
                             col.col = "black", 
                             col.row = "red",
                             pointsize = 2.5, size.text = 2)+
  theme_classic()

mosq.desc <- dimdesc(res.ca.mosq, axes = 1:2)
mosq.desc

###########################################################################

#   PCA

omo_physicochemical <- omo[,4:20]
Location <- omo[,"Location"]

omo_physic_PCA <- cbind(Location, omo_physicochemical)
head(omo_physic_PCA) # well done

rownames(omo_physic_PCA) <- omo_physic_PCA$Location

omo_physic_PCA_active <- omo_physic_PCA[,-1]
omo.pca <- PCA(omo_physic_PCA_active, graph = FALSE)

eig.val_omo <- get_eigenvalue(omo.pca)
eig.val_omo

fviz_eig(omo.pca, addlabels = TRUE, ylim = c(0, 50)) # scree plot

fviz_pca_var(omo.pca, col.var = "black", repel = TRUE)

fviz_pca_var(omo.pca, col.var = "cos2",
             gradient.cols = c("red", "#E7B800", "darkgreen"),
             repel = TRUE # Avoid text overlapping
)

fviz_pca_ind(omo.pca, col.ind = "cos2", pointsize = "cos2",
             pointshape = 21, fill = "#E7B800", 
             repel = TRUE) # Avoid text overlapping (slow if many points)

fviz_pca_var(omo.pca, col.var = "contrib",
             gradient.cols = c("red", "#E7B800", "darkgreen"),
             repel = TRUE # Avoid text overlapping
)
fviz_contrib(omo.pca, choice = "var", axes = 1)
fviz_contrib(omo.pca, choice = "var", axes = 2)


omo.pca$ind$cos2
omo.pca$ind$cos2

coordinate_ind <- as.data.frame(omo.pca$ind$coord)
coordinate_ind12<- coordinate_ind[,1:2]   

coordinate_ind123 <- as.data.frame(cbind((omo[,1:3]),
                                         coordinate_ind12))
head(coordinate_ind123)

quality_of_rep_ind <- as.data.frame(omo.pca$ind$cos2)
quality_of_rep_ind12<- quality_of_rep_ind[,1:2]   
quality_of_rep_ind12<- quality_of_rep_ind12 %>% 
  rename(CO2.Dim.1 = Dim.1, 
         CO2.Dim.2 = Dim.2)
ind_viz_data <- cbind(coordinate_ind123, quality_of_rep_ind12)
head(ind_viz_data)


co2_var_dataframe12 <-  as.data.frame(omo.pca$var$coord[,1:2])

co2_var<- cbind(row.names(co2_var_dataframe12),co2_var_dataframe12) 
 row.names(co2_var)  <- NULL     
head(co2_var) 

co2_var <- co2_var %>% 
  rename(parameter = "row.names(co2_var_dataframe12)",
         PCA1 = Dim.1, 
         PCA2 = Dim.2)
head(co2_var) 
head(co2_ind_viz_data)
co2_var<- as.data.frame(co2_var)
omo.pca$var$cos2
omo.pca$var$coord
omo.pca$ind$contrib

length(omo$Location)


final_PCA <- ggplot() +
  geom_point(data = co2_ind_viz_data, 
             aes(x = Dim.1, y = Dim.2, 
                 color = Ecozones, fill = Ecozones, 
             shape  = factor(Habitat)),
             size = 3) + 
  stat_ellipse(data = co2_ind_viz_data,  
               geom = "polygon", 
               aes(x = Dim.1, y = Dim.2, group = Ecozones, fill = Ecozones),
               level = 0.95, 
               linewidth = 0.1,
               alpha = 0.1,
               show.legend = NA)+ 
  scale_fill_manual(values = c("Containers" = "orange",
                               "Gutters" = "blue",
                               "Puddles" = "darkgreen",
                               "Tyre track"= "red",
                               "Used Tyres" = "purple",
                               "Derived Savanna"= "orange",
                               "Freshwater Swamp" = "blue",
                               "Lowland Rainforest"= "darkgreen")) +
  scale_color_manual(values = c("orange", "blue", "darkgreen", "red","purple"))+
  geom_text(data = co2_var, aes(x = PCA1, y = PCA2, label = parameter),
            position = position_jitter(width = 0.1, height = 0.1))+ 
  theme(
    text = element_text(family = "Times New Roman", size = 20),
    axis.line = element_line(color = "black", size = 0.5) 
  ) + labs(x = "PCA 1", y = "PCA 2")+
  theme_minimal()


#############################################################################################

final_PCA <- ggplot() +
  geom_point(data = ind_viz_data, 
             aes(x = Dim.1, y = Dim.2, 
                 color = Habitat, fill = Habitat, 
                 shape  = Ecozones),
             size = 3) + 
  stat_ellipse(data = ind_viz_data,  
               geom = "polygon", 
               aes(x = Dim.1, y = Dim.2, group = Habitat, fill = Habitat),
               level = 0.90, 
               alpha = 0.1,
               show.legend = NA)+ 
  scale_fill_manual(values = c("Containers" = "orange",
                               "Gutters" = "red",
                               "Puddles" = "black",
                               "Tyre track"= "brown",
                               "Used Tyres" = "darkgreen",
                               "Derived Savanna"= "orange",
                               "Freshwater Swamp" = "blue",
                               "Lowland Rainforest"= "darkgreen")) +
  scale_color_manual(values = c("orange", "red", "black", "brown","darkgreen"))+
  theme(
    text = element_text(family = "Times New Roman", size = 20),
  ) + labs(x = "Dim1 (41.2%)", y = "Dim2 (14.3%)")+
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
