
#libraries ####

library(tidyverse)
library(lme4) #statistical model
library(emmeans)#post-hoc test
library(bbmle)
library(vegan)#species richness index


#read raw data ####

pollinators <- read.csv("Pollinators.csv", header = T, stringsAsFactors = T) #data with site characteristics

pollinator_id <- read.csv("Pollinator_Id.csv", header = T, stringsAsFactors =  T) #pollinator images identified

#read raw site data 

site <- read.csv("site.dist_V01.csv", 
                 header = T, stringsAsFactors = T)#data for min distance

colnames(site) <- c("Site_Code", "Block_Code", "Treatment_Code", 
                    "Distance", 
                    "Geometry", "Geometry.1")
site.1 <- site[,c(1:4)]

#read  raw seeds data

seeds <- read.csv("seeds.csv", header = T, stringsAsFactors = T) #each row is one flowerhead

seeds <- seeds[,-c(1)] #remove first column

levels(seeds$Site_Code) <- c("Site A", "Site B", "Site C", "Site D") #replace site names with codes

# data cleaning and manipulation ####

# 1. Calculations for Pollinator visitation rate per plant per hour

Pol.FLW <- cbind(pollinator_id[,c(10,2)], pollinators[
  match(pollinator_id$Key, pollinators$Key), c(3,4,5,6,10)]) 
                            #each row is one pollinator

Pol.FLW <- Pol.FLW[,c(3,4,5,1,6,2,7)]

levels(Pol.FLW$Site_Code) <- c("Site A", "Site B", "Site C", "Site D") 
                                    #replace site names with codes

colnames(Pol.FLW)[colnames(Pol.FLW) == "Visited_Flowers"] <- "Visited" 
                                      #change column name

colnames(Pol.FLW)[colnames(Pol.FLW) == "Number_openFlowerhead"] <- "Open" 
                                      #change column name

levels(Pol.FLW$Treatment_Code) <- c("Control", "Predator Exclusion",
                                    "Predator Model") 
            #renaming the treatment codes

Pol.FLW$Open[is.na(Pol.FLW$Open)] <- 0 #replace NA with 0 in the column with openflowerhead

Pol.FLW$Visited[is.na(Pol.FLW$Visited)] <- 0 #replace na with 0 in the column with visited flowerheads

### Here aggregate by site and treatment, add richness visitation rate

Pol.Ind <- aggregate(Visited ~ Site_Code + Treatment_Code +
                       Block_Code + Open, data = Pol.FLW, FUN = sum) 
                                    #each row is one plant

Pol.Ind <- Pol.Ind[Pol.Ind$Open>0,]#filter out the plants without open flowerheads

Pol.Ind$Vis_Rate_Pl <- (Pol.Ind$Visited/Pol.Ind$Open)*4 #visitation rate per plant per hour

Pol.Site <- aggregate(cbind(Visited, Open) ~ Site_Code + Treatment_Code, 
                      data = Pol.Ind, FUN = sum)#each row is one site + treatment

Pol.Site$Vis_Rate_Treat <- (Pol.Site$Visited/Pol.Site$Open)*4 #visitation rate per 
                                                        #treatment per hour in each site

# 2. calculations for pollinator species richness per site 

Pol.comm <- table(Pol.FLW$Site_Code, Pol.FLW$Species_Code)

spr <- as.data.frame(t(apply(Pol.comm, 1, estimateR)))#species diversity for each site

Pol.Site[,6:7] <- spr[rep(1:4,3),1:2] #add species richness data to pollinator visitation dataset

names(Pol.Site)[names(Pol.Site)=="S.obs"] <- "Sp_Rich"

names(Pol.Site)[names(Pol.Site)=="S.chao1"] <- "Sp_RichIndex"

Pol.Ind <- cbind(Pol.Ind, Pol.Site[match(Pol.Ind$Site_Code, 
                                         Pol.Site$Site_Code),
                                   c(6,7)]) #add species richness indes and species richness to each block

Pol.Ind$Individual <- paste(Pol.Ind$Block_Code, 
                            Pol.Ind$Treatment_Code, sep = "-") #ID for each plant

Pol.Ind.Dis <- cbind(Pol.Ind, site.1[match(Pol.Ind$Block_Code, 
                                           site.1$Block_Code), c(4)]) #add the distance from male plant data to every plant

names(Pol.Ind.Dis)[10] <- "Distance" # rename the distance column



# statistical modelling ####

# 1. models for pollinator visitation rate

Mod.0 <- lmer(Vis_Rate_Pl ~ 1 + (1|Block_Code), 
              data = Pol.Ind.Dis, REML = F)  #null model

Mod.Site <- lmer(Vis_Rate_Pl ~ Site_Code + 
                   (1|Block_Code), data = Pol.Ind.Dis, REML = F)  

Mod.Treat <- lmer(Vis_Rate_Pl ~ Treatment_Code + (1|Block_Code), 
                  data = Pol.Ind.Dis, REML = F)  #adding species richness as a factor

Mod.Rich <- lmer(Vis_Rate_Pl ~ scale(Sp_RichIndex) + (1|Block_Code), 
                    data = Pol.Ind.Dis, REML = F)

Mod.Dist <- lmer(Vis_Rate_Pl ~ scale(sqrt(Distance)) + (1|Block_Code), 
                    data = Pol.Ind.Dis, REML = F)

Mod.Rich_Treat <- lmer(Vis_Rate_Pl ~ scale(Sp_RichIndex) + Treatment_Code + (1|Block_Code), 
                       data = Pol.Ind.Dis, REML = F)

Mod.Rich_Dist <- lmer(Vis_Rate_Pl ~ scale(Sp_RichIndex) + Distance + (1|Block_Code), 
                        data = Pol.Ind.Dis, REML = F)

Mod.Site_Treat <- lmer(Vis_Rate_Pl ~ Site_Code + Treatment_Code + (1|Block_Code), 
                       data = Pol.Ind.Dis, REML = F)

Mod.Site_Dist <- lmer(Vis_Rate_Pl ~ Site_Code + Distance + (1|Block_Code), 
                       data = Pol.Ind.Dis, REML = F)

Mod.Site_Rich <- lmer(Vis_Rate_Pl ~ Site_Code + scale(Sp_RichIndex) + (1|Block_Code), 
                      data = Pol.Ind.Dis, REML = F)

Mod.Treat_Dist <- lmer(Vis_Rate_Pl ~ Treatment_Code + Distance + (1|Block_Code), 
                       data = Pol.Ind.Dis, REML = F)

Mod.STrD <- lmer(Vis_Rate_Pl ~ Site_Code + Treatment_Code + Distance+
                              (1|Block_Code), data = Pol.Ind.Dis, REML = F)


Mod.STrDR <- lmer(Vis_Rate_Pl ~ Site_Code + Treatment_Code + Distance +
                    scale(Sp_RichIndex) + (1|Block_Code), 
                  data = Pol.Ind.Dis, REML = F)

#rank deficiency probably because the species richness and site code give same information or
#they are in one sense duplicate #or they are linear combination of each other.

AICctab(Mod.0, Mod.Site, Mod.Treat, Mod.Rich,Mod.Dist,
        Mod.Rich_Treat,  Mod.Rich_Dist, Mod.Site_Treat, Mod.Site_Dist, 
        Mod.Site_Rich,
        Mod.Treat_Dist,Mod.STrD, Mod.STrDR) #AIC test

#post-hoc analysis

confint(Mod.Dist)

Mod.Treat.emm <- emmeans::emmeans(Mod.Treat, pairwise ~ Treatment_Code, 
                                     type = "response")
Mod.Tem <- as.data.frame(Mod.Treat.emm$emmeans)

# Pollinator data + seeds data ####

seeds$Individual <- paste(seeds$Block_Code, seeds$Treatment_Code, sep = "-")

seeds$Flowerhead <- as.factor(1:nrow(seeds))

seeds[,13:15] <- Pol.Site[match(paste(seeds$Site_Code,seeds$Treatment_Code), 
                                paste(Pol.Site$Site_Code,Pol.Site$Treatment_Code)),5:7]

seeds$Eaten_Seeds <- seeds$Damaged_Seeds + seeds$Missing_Seeds

seeds.Pol <- seeds[seeds$Eaten_Seeds>0,]#take only the rows with positive number eaten seeds 

seeds.Pol$Vis_Rate_Treat[is.na(seeds.Pol$Vis_Rate_Treat)] <- 0

seeds.Pol$Sp_Rich[is.na(seeds.Pol$Sp_Rich)] <- 0

seeds.Pol[,17] <- site.1[match(paste(seeds.Pol$Site_Code, 
                                     seeds.Pol$Block_Code),
                               paste(site.1$Site_Code, site.1$Block_Code)), 
                         c(4)] #add distance data


colnames(seeds.Pol)[colnames(seeds.Pol) == "V17"] <- "Distance"


# Statistical model for fertilised seeds ####

# (i. excluding the rows for pollinator exclusion treatment from the dataset
#  ii. excluding the rows with missing values in site and treatment columns from the dataset)

Viable.M0 <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                     1 + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                  data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                     !is.na(seeds.Pol$Site_Code) & 
                                     seeds.Pol$Treatment_Code != "Exclude Pollinator",],
                  family = binomial)


Viable.Site <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ Site_Code + 
                       (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                     data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                        !is.na(seeds.Pol$Site_Code) & 
                                        seeds.Pol$Treatment_Code != "Exclude Pollinator",],
                     family = binomial, glmerControl(optimizer = "bobyqa")) 


Viable.Treat <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
              Treatment_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                        data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                           !is.na(seeds.Pol$Site_Code) & 
                                           seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
              family = binomial, glmerControl(optimizer = "bobyqa")) 


Viable.Dist <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                        scale(sqrt(Distance)) + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                      data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                         !is.na(seeds.Pol$Site_Code) & 
                                         seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                      family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Vis <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                       Vis_Rate_Treat + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                     data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                        !is.na(seeds.Pol$Site_Code) & 
                                        seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                     family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.SPR <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ scale(Sp_RichIndex) + 
                       (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                     data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                        !is.na(seeds.Pol$Site_Code) & 
                                        seeds.Pol$Treatment_Code != "Exclude Pollinator",],
                     family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Treat.Site <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ Site_Code + 
                        Treatment_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                        data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                           !is.na(seeds.Pol$Site_Code) & 
                                           seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                        family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Treat.SPR<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ scale(Sp_RichIndex) + 
                             Treatment_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                           data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                              !is.na(seeds.Pol$Site_Code) & 
                                              seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                           family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Treat.Vis <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ Vis_Rate_Treat + 
                             Treatment_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                           data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                              !is.na(seeds.Pol$Site_Code) & 
                                              seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                           family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Treat.Dist <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ scale(sqrt(Distance)) + 
                             Treatment_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                           data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                              !is.na(seeds.Pol$Site_Code) & 
                                              seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                           family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Site.Vis <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ Vis_Rate_Treat + 
                            Site_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                          data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                             !is.na(seeds.Pol$Site_Code) & 
                                             seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                          family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Site.Dist <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ scale(sqrt(Distance)) + 
                           Site_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                         data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                            !is.na(seeds.Pol$Site_Code) & 
                                            seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                         family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Site.SPR <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ scale(Sp_RichIndex) + 
                            Site_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                          data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                             !is.na(seeds.Pol$Site_Code) & 
                                             seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                          family = binomial, glmerControl(optimizer = "bobyqa")) 


Viable.Dis.SPR <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ scale(Sp_RichIndex) + 
                              scale(sqrt(Distance)) + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                            data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                               !is.na(seeds.Pol$Site_Code) & 
                                               seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                            family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Dis.Vis <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~  + Vis_Rate_Treat +
                          scale(sqrt(Distance)) + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                        data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                           !is.na(seeds.Pol$Site_Code) & 
                                           seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                        family = binomial, glmerControl(optimizer = "bobyqa")) 

Viable.Vis.SPR <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ scale(Sp_RichIndex) + 
                            Vis_Rate_Treat + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                          data = seeds.Pol[!is.na(seeds.Pol$Treatment_Code) & 
                                             !is.na(seeds.Pol$Site_Code) & 
                                             seeds.Pol$Treatment_Code != "Exclude Pollinator",], 
                          family = binomial, glmerControl(optimizer = "bobyqa")) 


AICctab(Viable.M0,Viable.Site,Viable.Treat, Viable.Dist, Viable.Vis, Viable.SPR,
       Viable.Treat.Site, Viable.Treat.SPR,Viable.Treat.Dist,Viable.Treat.Vis,
       Viable.Site.Dist, Viable.Site.Vis, Viable.Site.SPR,
        Viable.Vis.SPR, Viable.Dis.Vis,
       Viable.Dis.SPR) 
       

