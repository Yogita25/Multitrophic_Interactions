#libraries ####

library(ggplot2)

library(ggdist)

library(dplyr)

library(colorspace)

library(patchwork)

# source codes ####

source("Pollinators.R")

source("Herbivores.R")

p1 <- ggplot(Pol.Ind.Dis, mapping = aes(x = Treatment_Code,
                                  y = Vis_Rate_Pl, 
                                  colour = Treatment_Code,
                                  fill = Treatment_Code)) +
  geom_swarm(side = "both",linewidth = 0, 
            alpha = 0.4)+
  geom_point(Mod.Tem, size = 2,
             mapping = aes(x = Treatment_Code, y = emmean, 
                           fill = Treatment_Code,
                           colour = Treatment_Code),
             position = position_dodge(width = 0.2))+
  guides(fill = "none", color = "none", shape = "none")+
  geom_errorbar(Mod.Tem, linewidth = 1.2,
                mapping = aes(x = Treatment_Code, y = emmean, 
                              fill = Treatment_Code,
                              colour = Treatment_Code, 
                              ymin = lower.CL,
                              ymax = upper.CL),
                position = position_dodge(width = 0.2), 
                width = 0.1)+
  scale_color_manual(values=c( "deeppink4",
                               "dodgerblue4",
                               "green4"))+
  scale_fill_manual(values=c( "deeppink4",
                              "dodgerblue4",
                              "green4"))+
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 10))+
  geom_text(data.frame("Treatment_Code" = c("Control", 
                            "Predator Exclusion",
                            "Predator Model"), "label"=c("a","a","a")),
                       mapping = aes(x = Treatment_Code, 
                                     y = 35,label = label), 
            color = "black", size = 5)+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12))+
  xlab("Treatment")+
  ylab("Visitation rate")

# Florivore abundance

p2 <- ggplot(seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                        !is.na(seeds.Eat.herb$Site_Code),], 
       aes(x = Treatment_Code, y = Insects, 
           colour = Treatment_Code,
           fill = Treatment_Code)) +
  scale_y_log10(limits = c(-1, 100),
                breaks = c(0,1,10,100))+
  geom_swarm(side = "both",linewidth = 0, 
             alpha = 0.4)+
  geom_point(HVis.STem, size = 2,
             mapping = aes(x = Treatment_Code,
                           y = response,
                           fill = Treatment_Code,
                           colour = Treatment_Code),
             position = position_dodge(width = 0.2))+
  guides(fill = "none", color = "none", shape = "none")+
  geom_errorbar(HVis.STem, linewidth = 1.2,
                mapping = aes(x = Treatment_Code, y = response, 
                              fill = Treatment_Code,
                              colour = Treatment_Code, 
                              ymin = asymp.LCL,
                              ymax = asymp.UCL),
                position = position_dodge(width = 0.2), width = 0.1)+
  geom_text(data.frame("Treatment_Code" = c("Control","Pollinator Exclusion", 
                                            "Predator Exclusion",
                                            "Predator Model"), 
                       "label"=c("a", "b","a","a")),
            mapping = aes(x = Treatment_Code, 
                          y = 95,label = label), 
            color = "black", size = 5)+
  
  scale_color_manual(values=c( "deeppink4",
                               "darkgoldenrod4",
                               "dodgerblue4",
                               "green4"))+
  scale_fill_manual(values=c( "deeppink4",
                              "darkgoldenrod4",
                              "dodgerblue4",
                              "green4"))+
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 10))+
  
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12))+
  xlab("Treatment")+
  ylab("Florivore abundance")


#Viable seeds

p3 <- ggplot(seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) &
                              !is.na(seeds.herb.Pol$Site_Code),], 
             aes(x = Treatment_Code, y = Viable_Seeds/Flowers,
                     color = Treatment_Code,
                     fill = Treatment_Code))+
  geom_swarm(side = "both", linewidth = 3.5, 
             alpha = 0.7)+
  
  geom_point(Viable.Temm, size = 2,
             mapping = aes(x = Treatment_Code,
             y = prob,
             fill = Treatment_Code,
             colour = Treatment_Code),
             position = position_dodge(width = 0.2))+
  guides(fill = "none", color = "none", shape = "none")+
  geom_errorbar(Viable.Temm, linewidth = 1.2,
                mapping = aes(x = Treatment_Code, y = prob, 
                              fill = Treatment_Code,
                              colour = Treatment_Code, 
                              ymin = asymp.LCL,
                              ymax = asymp.UCL),
                position = position_dodge(width = 0.2), width = 0.1)+
  geom_text(data.frame("Treatment_Code" = c("Control", "Pollinator Exclusion",
                                            "Predator Exclusion",
                                            "Predator Model"), 
                       "label"=c("a","b", "a","a")),
            mapping = aes(x = Treatment_Code, 
                          y = 1.1,label = label), 
            color = "black", size = 5)+
  scale_color_manual(values=c( "deeppink4",
                               "darkgoldenrod4",
                               "dodgerblue4",
                               "green4"))+
  scale_fill_manual(values=c( "deeppink4",
                              "darkgoldenrod4",
                              "dodgerblue4",
                              "green4"))+
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 10))+
  
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12))+
  xlab("Treatment")+
  ylab(stringr::str_wrap("Proportion of fertilised seeds",
                         width = 18))

#predated seeds

eaten.plot <- seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) & 
                               !is.na(seeds.herb.Pol$Site_Code),]

p4 <- ggplot(eaten.plot, aes(x = Treatment_Code, 
                             y = Eaten_Seeds/Flowers,
                            color = Treatment_Code,
                            fill = Treatment_Code))+
  geom_swarm(side = "both",linewidth = 0, 
             alpha = 0.4)+
  geom_point(Eat.Temm,  size = 2,
             mapping = aes(x = Treatment_Code,
             y = prob,
             fill = Treatment_Code,
             colour = Treatment_Code),
             position = position_dodge(width = 0.2))+
 guides(fill = "none")+
 geom_errorbar(Eat.Temm, linewidth = 1.2,
               mapping = aes(x = Treatment_Code, y = prob, 
                             fill = Treatment_Code,
                             colour = Treatment_Code, 
                             ymin = asymp.LCL,
                             ymax = asymp.UCL),
               position = position_dodge(width = 0.2), width = 0.2)+
  scale_color_manual(values=c( "deeppink4",
                               "darkgoldenrod4",
                               "dodgerblue4",
                               "green4"))+
  scale_fill_manual(values=c( "deeppink4",
                              "darkgoldenrod4",
                              "dodgerblue4",
                              "green4"))+
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 10))+
  
  geom_text(data.frame("Treatment_Code" = c("Control", "Pollinator Exclusion",
                                            "Predator Exclusion",
                                            "Predator Model"), 
                       "label"=c("a","b", "a","a")),
            mapping = aes(x = Treatment_Code, 
                          y = 1.1,label = label), 
            color = "black", size = 5)+
  
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12))+
  xlab("Treatment")+
  ylab(stringr::str_wrap("Proportion of predated seeds",
                         width = 18))

 p <- p1 + p2 + p3 + p4 +
   plot_annotation(tag_levels = 'A') +
   plot_layout(guides = "collect") &
   theme(legend.position= "bottom", 
         legend.text = element_text(colour = "black", size = 12),
         legend.title = element_blank())
 
 ggsave("Fig_Field.svg", plot = p, 
        height = 7, width = 9.4, dpi = 800, units = "in")

############################################################################# 

 #plots for supplementary material
 
 
 p1s <-  ggplot(Pol.Ind.Dis, mapping = aes(x = Treatment_Code,
                                         y = Vis_Rate_Pl, 
                                         colour = Treatment_Code,
                                         fill = Treatment_Code)) +
   geom_swarm(side = "both",linewidth = 0, 
              alpha = 0.7)+
   guides(fill = FALSE)+
   facet_wrap(~ Site_Code, drop = TRUE)+
   geom_point(Mod.Tem, size = 2,
              mapping = aes(x = Treatment_Code, y = emmean, 
                            fill = Treatment_Code,
                            colour = Treatment_Code),
              position = position_dodge(width = 0.2))+
   geom_errorbar(Mod.Tem, linewidth = 1.2,
                 mapping = aes(x = Treatment_Code, y = emmean, 
                               fill = Treatment_Code,
                               colour = Treatment_Code, 
                               ymin = lower.CL,
                               ymax = upper.CL),
                 position = position_dodge(width = 0.2), 
                 width = 0.1)+
   scale_color_manual(values=c( "deeppink4",
                                "dodgerblue4",
                                "green4"))+
   scale_fill_manual(values=c( "deeppink4",
                               "dodgerblue4",
                               "green4"))+
   scale_x_discrete(labels = function(x) 
     stringr::str_wrap(x, width = 12))+
   
   theme(legend.position = "none",
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         panel.border = element_rect(color = "black", fill = NA),
         axis.text.x = element_text(color = "black", size = 20),
         axis.text.y = element_text(color = "black", size = 20),
         axis.title.x = element_text(colour = "black", size = 21),
         axis.title.y = element_text(colour = "black", size = 21),
         strip.background = element_rect(fill = F,linetype = "solid", 
                                         colour = "black"),
         strip.text = element_text(colour = "black", size = 20))+
   xlab("Treatment")+
   ylab("Visitation rate")
 
 ggsave("Vis_Site.png", plot = p1s,
        height = 8, width = 10, dpi = 300)


 p2s <- ggplot(seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                         !is.na(seeds.Eat.herb$Site_Code),], 
        aes(x = Treatment_Code, y = Insects, 
            colour = Treatment_Code,
            fill = Treatment_Code)) +
   scale_y_log10(limits = c(-1, 100),
                 breaks = c(0,1,10,100))+
   
   geom_swarm(side = "both",linewidth = 0, 
              alpha = 0.7)+
   facet_wrap(~ Site_Code, drop = TRUE)+
   
   geom_point(HVis.STem, size = 2,
              mapping = aes(x = Treatment_Code,
                            y = response,
                            fill = Treatment_Code,
                            colour = Treatment_Code),
              position = position_dodge(width = 0.2))+
   
   geom_errorbar(HVis.STem, linewidth = 1.2,
                 mapping = aes(x = Treatment_Code, y = response, 
                               fill = Treatment_Code,
                               colour = Treatment_Code, 
                               ymin = asymp.LCL,
                               ymax = asymp.UCL),
                 position = position_dodge(width = 0.2), width = 0.1)+
   
   scale_color_manual(values=c( "deeppink4",
                                "darkgoldenrod4",
                                "dodgerblue4",
                                "green4"))+
   scale_fill_manual(values=c( "deeppink4",
                               "darkgoldenrod4",
                               "dodgerblue4",
                               "green4"))+
   scale_x_discrete(labels = function(x) 
     stringr::str_wrap(x, width = 10))+
   
   theme(legend.position = "none",
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         panel.border = element_rect(color = "black", fill = NA),
         axis.text.x = element_text(color = "black", size = 15),
         axis.text.y = element_text(color = "black", size = 15),
         axis.title.x = element_text(colour = "black", size = 16),
         axis.title.y = element_text(colour = "black", size = 16),
         strip.background = element_rect(fill = F,linetype = "solid", 
                                         colour = "black"),
         strip.text = element_text(colour = "black", size = 20))+
   xlab("Treatment")+
   ylab("Florivore abundance")
 
 ggsave("Flor_Site.png", plot = p2s,
        height = 8, width = 10, dpi = 300)
  
 
 
 p3s <- ggplot(Viable.data, aes(x = Treatment_Code, 
                               y = Viable_Seeds/Flowers,
                               color = Treatment_Code,
                               fill = Treatment_Code))+
   geom_swarm(side = "both",linewidth = 0, 
              alpha = 0.7)+
   facet_wrap(~ Site_Code, drop = TRUE)+
   
   geom_point(Viable.Temm, size = 2,
              mapping = aes(x = Treatment_Code,
                            y = prob,
                            fill = Treatment_Code,
                            colour = Treatment_Code),
              position = position_dodge(width = 0.2))+
   geom_errorbar(Viable.Temm, linewidth = 1.2,
                 mapping = aes(x = Treatment_Code, y = prob, 
                               fill = Treatment_Code,
                               colour = Treatment_Code, 
                               ymin = asymp.LCL,
                               ymax = asymp.UCL),
                 position = position_dodge(width = 0.2), width = 0.1)+
   scale_color_manual(values=c( "deeppink4",
                                "dodgerblue4",
                                "green4"))+
   scale_fill_manual(values=c( "deeppink4",
                               "dodgerblue4",
                               "green4"))+
   scale_x_discrete(labels = function(x) 
     stringr::str_wrap(x, width = 15))+
   
   theme(legend.position = "none",
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(color = "black", fill = NA),
         axis.text.x = element_text(color = "black", size = 15),
         axis.text.y = element_text(color = "black", size = 15),
         axis.title.x = element_text(colour = "black", size = 16),
         axis.title.y = element_text(colour = "black", size = 16),
         strip.background = element_rect(fill = F,linetype = "solid", 
                                         colour = "black"),
         strip.text = element_text(colour = "black", size = 16))+
   xlab("Treatment")+
   ylab("Proportion of fertilised seeds")
 
 ggsave("Viable_Site.png", plot = p3s,
        height = 8, width = 10, dpi = 300)
 
 p4s<- ggplot(eaten.plot, aes(x = Treatment_Code, 
                              y = Eaten_Seeds/Flowers,
                              color = Treatment_Code,
                              fill = Treatment_Code))+
   geom_swarm(side = "both",linewidth = 0, 
              alpha = 0.7)+
   facet_wrap( ~ Site_Code, drop = TRUE)+
   geom_point(Eat.Temm,  size = 2,
              mapping = aes(x = Treatment_Code,
                            y = prob,
                            fill = Treatment_Code,
                            colour = Treatment_Code),
              position = position_dodge(width = 0.2))+
   geom_errorbar(Eat.Temm, linewidth = 1.2,
                 mapping = aes(x = Treatment_Code, y = prob, 
                               fill = Treatment_Code,
                               colour = Treatment_Code, 
                               ymin = asymp.LCL,
                               ymax = asymp.UCL),
                 position = position_dodge(width = 0.2), width = 0.2)+
   
   scale_color_manual(values=c( "deeppink4",
                                "darkgoldenrod4",
                                "dodgerblue4",
                                "green4"))+
   scale_fill_manual(values=c( "deeppink4",
                               "darkgoldenrod4",
                               "dodgerblue4",
                               "green4"))+
   scale_x_discrete(labels = function(x) 
     stringr::str_wrap(x, width = 10))+
   
   theme(legend.position = "none",
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(color = "black", fill = NA),
         axis.text.x = element_text(color = "black", size = 15),
         axis.text.y = element_text(color = "black", size = 15),
         axis.title.x = element_text(colour = "black", size = 16),
         axis.title.y = element_text(colour = "black", size = 16),
         strip.background = element_rect(fill = F,linetype = "solid", 
                                         colour = "black"),
         strip.text = element_text(colour = "black", size = 16))+
   xlab("Treatment")+
   ylab("Proportion of predated seeds")
 
 ggsave("Pred_Site.png", plot = p4s,
        height = 8, width = 10, dpi = 300)
 
 # number of visits by pollinator families
 
 #data manipulation for plots (Pollinators data) ####
 
 Pol.comb <- cbind(pollinator_id[,c(7,10,2)], pollinators[
   match(pollinator_id$Key, pollinators$Key), c(3,4,5,6,10)]) 
 
 levels(Pol.comb$Site_Code) <- c("Site A", "Site B", "Site C", 
                                 "Site D") 
 #replace site names with codes
 
 colnames(Pol.comb)[colnames(Pol.comb) == "Visited_Flowers"] <-
   "Visited" 
 
 #change column name
 
 colnames(Pol.comb)[colnames(Pol.comb) == "Number_openFlowerhead"] <- 
   "Open" 
 
 #change column name
 
 levels(Pol.comb$Treatment_Code) <- c("Control", "Predator Exclusion", 
                                      "Predator Model") 
 #renaming the treatment codes
 
 Pol.comb$Open[is.na(Pol.comb$Open)] <- 0 #replace NA with 0 in the column with openflowerhead
 
 Pol.comb$Visited[is.na(Pol.comb$Visited)] <- 0 #replace na with 0 in the column with visited flowerheads
 
 Pol.Fam <- aggregate(Visited ~ Site_Code + Treatment_Code + 
                        Block_Code + Family + Open,
                      data = Pol.comb, FUN = sum) #each row is one plant
 
 Pol.Fam[Pol.Fam$Family == "",] <- NA
 
 Pol.Fam$Visited_SD <- sd(Pol.Fam$Visited)
 
 #visitation rate per hour by each family of pollinator
 
 Pol.Fam$VisR_plant_hour <- (Pol.Fam$Visited/Pol.Fam$Open)*4 
 
 Pol.Fam.Dis <- cbind(Pol.Fam, site.1[match(Pol.Fam$Block_Code, 
                                            site.1$Block_Code), c(4)])
 
 names(Pol.Fam.Dis)[9] <- "Distance"
 
 # plots related to pollinators ####
 
 Pol.Fam.bar<- Pol.FLW %>%
   mutate(Family_bar = recode(Species_Code, "Aethina Sp1" = "Others" , "Syrphidae Sp1" = "Syrphidae", 
                              "apis melifera" = "Apidae", "Apis melifera" = "Apidae", "Sarcophaga Sp1" = "Sacrophagidae", 
                              "Sphaerophoria scripta" = "Syrphidae", "Helophilus pendulus" = "Syrphidae", 
                              "Sarcophagidae Sp1" = "Sacrophagidae", "Aglais urticae" = "Others", 
                              "Andrena Sp1" = "Others", "Anthidium Sp1"= "Others", "Asillidae Sp1" = "Others","Bombus Sp1" = "Apidae",
                              "Ceratina Sp1" = "Apidae", "Curculionidae Sp1" = "Others", "Diptera Sp1" = "Others", "Empididae Sp1" = "Others",
                              "Lucilia  caesar" = "Others", "Nitidulidae Sp1" = "Others",  "Nomanidae Sp1" = "Others", "Oxythyrea funesta" = "Others",
                              "Pieris rapae" = "Others", "Rhagonycha fulva" = "Others", "Rhagonycha Sp1" = "Others", 
                              "Sciaridae Sp1" = "Others", "Others" = "Others"))
 
 Pol.Fam.con <- Pol.Fam.bar[!is.na(Pol.Fam.bar$Site_Code), 
                            c("Site_Code", "Treatment_Code",
                              "Family_bar", "Visited")]
 
 
 Pol.plant <- aggregate(Visited ~ Site_Code + Treatment_Code + 
                          Family_bar, 
                        data = Pol.Fam.con, FUN = sum) #each row is one plant
 
 Pol.conOrd <- Pol.plant%>% 
   mutate(Family_bar = factor(Family_bar, levels = c("Apidae", 
                                                     "Syrphidae",
                                                     "Others", 
                                                     "Sacrophagidae")))
 
 S11 <- ggplot(Pol.conOrd, aes(x = Family_bar, y = log(Visited)))+ 
   geom_bar(stat = "identity", width = 0.5)+
   theme(panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(colour = "black", fill = NA),
         strip.background = element_rect(fill = F, linetype = "solid", colour = "black"),
         plot.margin=unit(c(6,6,6,6), 'mm'),
         axis.text.x = element_text(size = 20, colour = "black"),
         axis.text.y = element_text(size = 20, colour = "black"),
         axis.title.x = element_text(size = 21),
         axis.title.y = element_text(size = 21),
         strip.text = element_text(size = 17),
         legend.position = "none")+
   xlab("Pollinator family")+
   ylab("Number of visits")
 
 
 S12 <- ggplot(Pol.conOrd,aes(x = Family_bar, y = log(Visited)))+ 
   geom_bar(stat = "identity", width = 0.5)+
   facet_wrap(~Site_Code, drop = TRUE, ncol = 1,
              strip.position = "right")+
   theme(panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(colour = "black", fill = NA),
         panel.spacing = unit(4, "mm", data = NULL),
         strip.background = element_rect(fill = F, linetype = "solid",
                                         colour = "black"),
         axis.text.x = element_text(size = 20, color = "black"),
         axis.text.y = element_text(size = 20, color = "black"),
         axis.title.x = element_text(size = 21, color = "black"),
         axis.title.y = element_text(size = 21, color = "black"),
         strip.text = element_text(size = 17, color = "black"),
         legend.position = "none")+
   xlab("Pollinator family")+
   ylab("Number of visits")
 
 S13 <- ggplot(Pol.conOrd,aes(x = Family_bar, y = log(Visited)))+ 
   geom_bar(stat = "identity", width = 0.5)+
   facet_wrap(~Treatment_Code, drop = TRUE, ncol = 1, 
              strip.position = "right")+
   theme(panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(colour = "black", fill = NA),
         panel.spacing = unit(4, "mm", data = NULL),
         strip.background = element_rect(fill = F, linetype = "solid", 
                                         colour = "black"),
         axis.text.x = element_text(size = 20, color = "black"),
         axis.text.y = element_text(size = 20, color = "black"),
         axis.title.x = element_text(size = 21, colour = "black"),
         axis.title.y = element_text(size = 21, color = "black"),
         strip.text = element_text(size = 17, color = "black"),
         legend.position = "none")+
   xlab("Pollinator family")+
   ylab("Number of visits")

 S1 <- S11 + S12 + S13 + 
   plot_annotation(tag_levels = 'A') +
   plot_layout(axis_titles = "collect") &
   theme(legend.position='bottom', 
         legend.text = element_text(colour = "black", size = 16),
         legend.title = element_blank())
 
 ggsave("Pol.Fam.png", plot = S1, 
        height = 10, width = 22, dpi = 300)

 ############################################################################
 
 
 #florivore families
 
 Herb.Fam.bar<- herbivores %>%
   mutate(SpCode_bar = recode(Sp_Code, "Coleoptera NA Curculionidae" = "Others" , 
                              "Coleoptera NA Latridiidae" = "Others", 
                              "Coleoptera NA NA" = "Others", 
                              "Diptera NA Cecidomyiidae" = "Cecidomyiidae", 
                              "Diptera NA Syrphidae" = "Syrphidae", 
                              "Diptera NA Tephritidae" = "Tephritidae", 
                              "Diptera Nematocera NA" = "Others", 
                              "Hemiptera NA Auchenorhyncha" = "Others", 
                              "Hemiptera NA NA" = "Others", 
                              "Hymenoptera NA NA" = "Others", 
                              "Hymenoptera NA Platygastridae"= "Platygastridae",
                              "Thysanoptera NA NA" = "Others",
                              "Syrphidae" = "Other"))
 
 Herb.Fam.con <- Herb.Fam.bar[!is.na(Herb.Fam.bar$Site_Code),
                              c("Site_Code", "Treatment_Code",
                                "SpCode_bar", "Number")] #each row is one flowerhead
 Herb.plant <- aggregate(Number ~ Site_Code + Treatment_Code 
                         + SpCode_bar, data = Herb.Fam.con,
                         FUN = sum) #each row is one plant
 
 Herb.conOrd <- Herb.Fam.con %>% 
   mutate(SpCode_bar = factor(SpCode_bar, levels = c("Cecidomyiidae", 
                                                     "Platygastridae",
                                                     "Tephritidae",
                                                     "Others")))
 
 hp1 <-ggplot(Herb.conOrd[!is.na(Herb.conOrd$SpCode_bar),], 
              aes(x = SpCode_bar, y = log(Number))) +
   geom_bar(stat = "identity", width = 0.5)+
   theme(panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(colour = "black", fill = NA),
         panel.spacing = unit(4, "mm", data = NULL),
         strip.background = element_rect(fill = F, linetype = "solid", colour = "black"),
         axis.text.x = element_text(size = 20, color = "black"),
         axis.text.y = element_text(size = 20, color = "black"),
         axis.title.x = element_text(size = 21, color = "black"),
         axis.title.y = element_text(size = 21, color = "black"),
         strip.text = element_text(size = 20, color = "black"),
         legend.position = "none")+
   xlab("Florivore Family")+
   ylab("Florivore abundance") 

 hp2 <- ggplot(Herb.conOrd[!is.na(Herb.conOrd$SpCode_bar),], 
               aes(x = SpCode_bar, y = log(Number))) +
   geom_bar(stat = "identity", width = 0.5)+
   facet_wrap(~Site_Code, ncol = 1, 
              strip.position = "right", drop = TRUE)+
   scale_x_discrete(label = c("Cecidomyiidae", "Platygastridae", 
                              "Others", "Tephritidae"))+
   theme(panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(colour = "black", fill = NA),
         panel.spacing = unit(4, "mm", data = NULL),
         strip.background = element_rect(fill = F, linetype = "solid",
                                         colour = "black"),
         axis.text.x = element_text(size = 20, color = "black"),
         axis.text.y = element_text(size = 20, color = "black"),
         axis.title.x = element_text(size = 21, color = "black"),
         axis.title.y = element_text(size = 21, color = "black"),
         strip.text = element_text(size = 20,color = "black"),
         legend.position = "none")+
   xlab("Florivore Family")+
   ylab("Florivore abundance") 
   
 hp3 <- ggplot(Herb.conOrd[!is.na(Herb.conOrd$SpCode_bar),], 
               aes(x = SpCode_bar, y = log(Number))) +
   geom_bar(stat = "identity", width = 0.5)+
   facet_wrap(~Treatment_Code, ncol = 1, drop = TRUE, 
              strip.position = "right")+
   scale_x_discrete(label = c("Cecidomyiidae", "Platygastridae", 
                              "Tephritidae", "Others"))+
   theme(panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(colour = "black", fill = NA),
         panel.spacing = unit(4, "mm", data = NULL),
         strip.background = element_rect(fill = F, linetype = "solid", 
                                         colour = "black"),
         axis.text.x = element_text(size = 20, color = "black"),
         axis.text.y = element_text(size = 20, color = "black"),
         axis.title.x = element_text(size = 21, color = "black"),
         axis.title.y = element_text(size = 21, color = "black"),
         strip.text = element_text(size = 18,color = "black"),
         legend.position = "none")+
   xlab("Florivore Family")+
   ylab("Florivore abundance")

  hp <- hp1 + hp2 + hp3 +
   plot_annotation(tag_levels = 'A') +
   plot_layout(axis_titles = "collect") &
   theme(legend.position='bottom', 
         legend.text = element_text(colour = "black", size = 17),
         legend.title = element_blank())

  ggsave("Flor.Fam.png", plot = hp, 
         height = 10, width = 25, dpi = 300)

###############################################################################    