#libraries
library(tidyverse)
library(lme4) #statistical model
library(emmeans)#post-hoc test
library(vegan)#species richness index
library(stringr)
library(data.table)
library(bbmle)#aic values
library(nlme)


#import data

herbivores <- read.csv("Herbivore.csv", header = T, stringsAsFactors = T) 
#each row is also one herbivore flowerhead

levels(herbivores$Site_Code) <- c("Site A", "Site B", "Site C", "Site D")

seeds <- read.csv("seeds.csv", header = T, stringsAsFactors = T) #each row is one flowerhead

seeds <- seeds[,-c(1)] #remove first column

levels(seeds$Site_Code) <- c("Site A", "Site B", "Site C", "Site D")

seeds$Eaten_Seeds <- seeds$Damaged_Seeds + seeds$Missing_Seeds

seeds.eaten <- seeds[seeds$Eaten_Seeds>0,]#take only the rows with positive Insects eaten seeds 

seeds.Eat <- seeds.eaten[!is.na(seeds.eaten$Site_Code) & !is.na(seeds.eaten$Treatment_Code) &
                           !is.na(seeds.eaten$Vial_Code) & !is.na(seeds.eaten$Block_Code),] #remove rows with NA

#read pollinator data

pollinator.sheet1 <- read.csv("Pollinators.csv", header = T, stringsAsFactors = T) #data with site characteristics

pollinator.sheet3 <- read.csv("Pollinator_Id.csv", header = T, stringsAsFactors =  T) #pollinator images identified

#read site data

site <- read.csv("Sites.csv", header = T, stringsAsFactors = T)#data for min distance

colnames(site) <- c("Site_Code", "Block_Code", "Treatment_Code", "Distance", 
                   "Geometry", "Geometry.1")
site.1 <- site[,c(1:4)]

#calculate number of insects of each morphospecies

herb.Ind <- aggregate(Number ~ Site_Code + Block_Code + 
                        Treatment_Code + Sp_Code,
                      data = herbivores,
                      FUN = sum) #each row is one herbivore 

#Method 1: herbivore species richness per site
herbS.tab <- table(herbivores$Site_Code, herbivores$Sp_Code)# morphospeices as input value

herbS.spr <- as.data.frame(t(apply(herbS.tab, 1, estimateR)))

herbS.spr$Site_Code <- rownames(herbS.spr) #add site codes as a column

herb_rich <- cbind(herb.Ind, herbS.spr[match(herb.Ind$Site_Code, herbS.spr$Site_Code), c(1:2)])

names(herb_rich)[names(herb_rich)=="S.obs"] <- "HerbS_Rich"
names(herb_rich)[names(herb_rich)=="S.chao1"] <- "HerbS_RichIndex"

#Pollinator visitation rate

Pol.FLW <- cbind(pollinator.sheet3[,c(10,2)], pollinator.sheet1[
  match(pollinator.sheet3$Key, pollinator.sheet1$Key), c(3,4,5,6,10)]) 
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

Pol.FLW <- Pol.FLW[-348,] # exclude the row with site_code = NA and Treatment_Code = NA

### Insects of visted flowerheads per treatment in each site

Pol.Ind <- aggregate(Visited ~ Site_Code + Treatment_Code +
                       Block_Code + Open, data = Pol.FLW, FUN = sum) #each row is one plant

Pol.Ind <- Pol.Ind[Pol.Ind$Open>0,]#filter out the plants without flowerheads

Pol.Ind$Vis_Rate_Pl <- (Pol.Ind$Visited/Pol.Ind$Open)*4 #visitation rate per plant per hour

Pol.Site <- aggregate(cbind(Visited, Open) ~ Site_Code + Treatment_Code, 
                      data = Pol.Ind, FUN = sum)#each row is one site and treatment combination

Pol.Site$Vis_Rate_Treat <- (Pol.Site$Visited/Pol.Site$Open)*4 #visitation rate per 
#treatment per hour in each site

#calculations for pollinator species richness per site
Pol.comm <- table(Pol.FLW$Site_Code, Pol.FLW$Species_Code)

spr <- as.data.frame(t(apply(Pol.comm, 1, estimateR)))#species diversity for each site

Pol.Site[,6:7] <- spr[rep(1:4,3),1:2] #add species richness data to pollinator visitation dataset

names(Pol.Site)[names(Pol.Site)=="S.obs"] <- "Pol_Rich"
names(Pol.Site)[names(Pol.Site)=="S.chao1"] <- "Pol_RichIndex"

Pol.Ind <- cbind(Pol.Ind, Pol.Site[match(Pol.Ind$Site_Code, 
                                         Pol.Site$Site_Code), c(5:7)])

#ID for each plant
Pol.Ind$Individual <- paste(Pol.Ind$Block_Code, Pol.Ind$Treatment_Code, sep = "-")

#add the distance data

Pol.Ind.Dis <- cbind(Pol.Ind, site.1[match(Pol.Ind$Block_Code, site.1$Block_Code), c(4)])

names(Pol.Ind.Dis)[11] <- "Distance"


#############################################################################################################
#add herbivore richness data with seeds data data

seeds.Eat.herb <- cbind(seeds.Eat, herbS.spr[match(seeds.Eat$Site_Code, herbS.spr$Site_Code), c(1:2)])

names(seeds.Eat.herb)[names(seeds.Eat.herb)=="S.obs"] <- "HerbS_Rich"
names(seeds.Eat.herb)[names(seeds.Eat.herb)=="S.chao1"] <- "HerbS_RichIndex"

#combine seeds herbivore data to pollinator data

seeds.Eat.herb[,14:16] <- Pol.Site[match(seeds.Eat.herb$Site_Code,Pol.Site$Site_Code),
                                   c(5:7)]

seeds.Eat.herb$Individual <- paste(seeds.Eat.herb$Block_Code,
                                   seeds.Eat.herb$Treatment_Code, sep = "-")

seeds.Eat.herb$Flowerhead <- as.factor(1:nrow(seeds.Eat.herb))

#add distance data to  seeds data

seeds.herb.Pol<- cbind(seeds.Eat.herb, site.1[match(seeds.Eat.herb$Block_Code,
                                                    site.1$Block_Code),
                                                    c(4)])

names(seeds.herb.Pol)[ncol(seeds.herb.Pol)] <- "Distance"

############################################################################################################

#models for herbivore abundance

HVis.0 <- glmer.nb(Insects ~ 1 + (1|Block_Code) + (1|Individual),
                   data = seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                                      !is.na(seeds.Eat.herb$Site_Code),], 
                   na.action = na.omit)

HVis.Site <- glmer.nb(Insects ~ Site_Code +(1|Block_Code) + (1|Individual),
                      data = seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                                         !is.na(seeds.Eat.herb$Site_Code),], 
                      na.action = na.omit)

HVis.Treat <- glmer.nb(Insects ~ Treatment_Code + (1|Block_Code) + (1|Individual),
                       data = seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                                          !is.na(seeds.Eat.herb$Site_Code),], 
                       na.action = na.omit, glmerControl(optimizer = "bobyqa"))


HVis.Hrich <- glmer.nb(Insects ~ scale(HerbS_RichIndex) + (1|Block_Code) + (1|Individual),
                       data = seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                                          !is.na(seeds.Eat.herb$Site_Code),], 
                       na.action = na.omit, glmerControl(optimizer = "bobyqa"))

HVis.Site.Treat <- glmer.nb(Insects ~ Site_Code +Treatment_Code + (1|Block_Code) + (1|Individual),
                            data = seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                                               !is.na(seeds.Eat.herb$Site_Code),], 
                            na.action = na.omit, glmerControl(optimizer = "bobyqa"))

HVis.ST.int <- glmer.nb(Insects ~ Site_Code * Treatment_Code + (1|Block_Code) + (1|Individual),
                        data = seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                                           !is.na(seeds.Eat.herb$Site_Code),], 
                        na.action = na.omit)

HVis.Site.Hrich <- glmer.nb(Insects ~ Site_Code + scale(HerbS_RichIndex) + (1|Block_Code) 
                            + (1|Individual),
                            data = seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                                               !is.na(seeds.Eat.herb$Site_Code),], 
                            na.action = na.omit, glmerControl(optimizer = "bobyqa"))

HVis.Treat.Hrich <- glmer.nb(Insects ~ Treatment_Code + scale(HerbS_RichIndex) + (1|Block_Code) 
                             + (1|Individual),
                             data = seeds.Eat.herb[!is.na(seeds.Eat.herb$Treatment_Code) &
                                                !is.na(seeds.Eat.herb$Site_Code),], 
                             na.action = na.omit, glmerControl(optimizer = "bobyqa"))

AICctab(HVis.0, HVis.Site, HVis.Treat, HVis.Hrich, HVis.Site.Hrich, 
        HVis.Site.Treat, HVis.Treat.Hrich)

HVis.ST.emm <- emmeans(HVis.Site.Treat, pairwise ~ Treatment_Code, 
                  type = "response")

HVis.STem <- as.data.frame(HVis.ST.emm$emmeans)

###############################################################################################################

#models for predated seeds

Eat.M0 <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ 
                  1 +  (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) &
                                     !is.na(seeds.herb.Pol$Site_Code),],
                na.action = na.omit, family = binomial)
#variance of random effect close to 0 hence warning for boundary singular fit

Eat.Insect <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ scale(Insects) + 
                       (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                    data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) & 
                                         !is.na(seeds.herb.Pol$Site_Code),], 
                    family = binomial, na.action = na.omit)  # here insects represent the abundance per plant.
#variance of random effect close to 0 hence warning for boundary singular fit

Eat.Site <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ Site_Code + 
                    (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                  data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) & 
                                       !is.na(seeds.herb.Pol$Site_Code),], 
                  family = binomial, na.action = na.omit) 

Eat.Treat <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ 
                     Treatment_Code + (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                   data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) & 
                                        !is.na(seeds.herb.Pol$Site_Code),],
                   family = binomial, na.action = na.omit) 

Eat.R <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ scale(HerbS_RichIndex) + 
                   (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                 data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) & 
                                      !is.na(seeds.herb.Pol$Site_Code),], 
                 family = binomial, glmerControl(optimizer = "bobyqa"), 
               na.action = na.omit) 

Eat.TS <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ Site_Code + 
                          Treatment_Code + (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                        data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) 
                                                & !is.na(seeds.herb.Pol$Site_Code),],
                        family = binomial, glmerControl(optimizer = "bobyqa"), 
                na.action = na.omit)

Eat.TR <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ Treatment_Code +
                  scale(HerbS_RichIndex) + (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) 
                                        & !is.na(seeds.herb.Pol$Site_Code),],
                family = binomial, glmerControl(optimizer = "bobyqa"), 
                na.action = na.omit)

Eat.TI <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ Treatment_Code +
                  scale(Insects) + (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) 
                                        & !is.na(seeds.herb.Pol$Site_Code),],
                family = binomial, glmerControl(optimizer = "bobyqa"), 
                na.action = na.omit)

Eat.SR <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ Site_Code +
                  scale(HerbS_RichIndex) + (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) 
                                        & !is.na(seeds.herb.Pol$Site_Code),],
                family = binomial, glmerControl(optimizer = "bobyqa"),
                na.action = na.omit)

Eat.SI <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ Site_Code +
                  scale(Insects) + (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) 
                                        & !is.na(seeds.herb.Pol$Site_Code),],
                family = binomial, glmerControl(optimizer = "bobyqa"), 
                na.action = na.omit)

Eat.TSI <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ Site_Code + 
                                 Treatment_Code + scale(Insects) + (1|Block_Code) + (1|Individual) + (1|Flowerhead), 
                               data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) & 
                                                       !is.na(seeds.herb.Pol$Site_Code),],
                               family = binomial, glmerControl(optimizer = "bobyqa"),
                 na.action = na.omit) 

Eat.TSIR <- glmer(cbind(Eaten_Seeds, Flowers-Eaten_Seeds) ~ Site_Code + 
                                 Treatment_Code + scale(Insects) + scale(HerbS_RichIndex)+(1|Block_Code)+
                    (1|Individual) + (1|Flowerhead), 
                               data = seeds.herb.Pol[!is.na(seeds.herb.Pol$Treatment_Code) & 
                                                    !is.na(seeds.herb.Pol$Site_Code),],
                               family = binomial, glmerControl(optimizer = "bobyqa"),
                  na.action = na.omit)

AICctab(Eat.M0, Eat.Insect, Eat.Site, Eat.Treat, Eat.R,Eat.TS,
        Eat.TR, Eat.TI, Eat.SR, Eat.SI, Eat.TSI, Eat.TSIR)
#variance of random effect close to 0 hence warning for boundary singular fit

#post-hoc test

Eat.Treat.emm <- emmeans(Eat.Treat, pairwise ~ Treatment_Code,
                         type = "response")

Eat.Temm <- as.data.frame(Eat.Treat.emm$emmeans)
############################################################################################

#model for viable seeds

#model for fertilized flowers

Viable.M0 <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                     1 + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                   data = seeds.herb.Pol,
                   family = binomial, na.action = na.omit)


Viable.Site <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ Site_Code + 
                       (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                     data = seeds.herb.Pol,
                     family = binomial, glmerControl(optimizer = "bobyqa"), 
                     na.action = na.omit) 


Viable.Treat <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                        Treatment_Code + (1|Flowerhead) + (1|Individual) + 
                        (1|Block_Code), 
                      data = seeds.herb.Pol,
                      family = binomial, glmerControl(optimizer = "bobyqa"),
                      na.action = na.omit) 


Viable.Dist <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                       scale(Distance) + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                     data = seeds.herb.Pol, 
                     family = binomial, glmerControl(optimizer = "bobyqa"),
                     na.action = na.omit) 

Viable.Vis <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                      scale(Vis_Rate_Treat) + 
                      (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                    data = seeds.herb.Pol, 
                    family = binomial, glmerControl(optimizer = "bobyqa"),
                    na.action = na.omit) 

Viable.Pol_Rich <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                           scale(Pol_RichIndex) + 
                          (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                        data = seeds.herb.Pol,
                        family = binomial, glmerControl(optimizer = "bobyqa"),
                        na.action = na.omit) 

Viable.Herb_Rich <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                            scale(HerbS_RichIndex) + 
                           (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                         data = seeds.herb.Pol,
                         family = binomial, glmerControl(optimizer = "bobyqa"),
                         na.action = na.omit) 

Viable.Treat.Site <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                             Site_Code + Treatment_Code + 
                             (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                           data = seeds.herb.Pol, 
                           family = binomial, glmerControl(optimizer = "bobyqa"),
                           na.action = na.omit) 

Viable.Treat.PolRich<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                               scale(Pol_RichIndex) + 
                              Treatment_Code + 
                               (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                            data = seeds.herb.Pol, 
                            family = binomial, glmerControl(optimizer = "bobyqa"),
                            na.action = na.omit) 

Viable.Treat.Vis <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                            scale(Vis_Rate_Treat) + 
                            Treatment_Code + 
                            (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                          data = seeds.herb.Pol, 
                          family = binomial, glmerControl(optimizer = "bobyqa"),
                          na.action = na.omit) 

Viable.Treat.HerbRich<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                                scale(HerbS_RichIndex) + 
                               Treatment_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                             data = seeds.herb.Pol, 
                             family = binomial, glmerControl(optimizer = "bobyqa"),
                             na.action = na.omit) 

Viable.Site.Vis <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                           scale(Vis_Rate_Treat) + 
                           Site_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                         data = seeds.herb.Pol, 
                         family = binomial, glmerControl(optimizer = "bobyqa"),
                         na.action = na.omit) 

Viable.Site.Dist <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                            scale(Distance) + 
                            Site_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                          data = seeds.herb.Pol, 
                          family = binomial, glmerControl(optimizer = "bobyqa"),
                          na.action = na.omit) 

Viable.Treat.Dist <- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                             scale(Distance) + 
                             Treatment_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                           data = seeds.herb.Pol, 
                           family = binomial, glmerControl(optimizer = "bobyqa"),
                           na.action = na.omit) 

Viable.Dis.PolRich<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                             scale(Pol_RichIndex) + 
                            scale(Distance) + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                          data = seeds.herb.Pol, 
                          family = binomial, glmerControl(optimizer = "bobyqa"),
                          na.action = na.omit) 

Viable.Dis.HerbRich<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                              scale(HerbS_RichIndex) + 
                             scale(Distance) + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                           data = seeds.herb.Pol, 
                           family = binomial, glmerControl(optimizer = "bobyqa"),
                           na.action = na.omit) 

Viable.Vis.PolRich<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                             scale(Pol_RichIndex) + 
                            scale(Vis_Rate_Treat) + 
                             (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                          data = seeds.herb.Pol, 
                          family = binomial, glmerControl(optimizer = "bobyqa"),
                          na.action = na.omit) 

Viable.Vis.HerbRich<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                              scale(HerbS_RichIndex) + 
                             Vis_Rate_Treat + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                           data = seeds.herb.Pol, 
                           family = binomial, glmerControl(optimizer = "bobyqa"),
                           na.action = na.omit) 

Viable.Site.PolRich<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                              scale(Pol_RichIndex) + 
                             Site_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                           data = seeds.herb.Pol, 
                           family = binomial, glmerControl(optimizer = "bobyqa"),
                           na.action = na.omit) 


Viable.Site.HerbRich<- glmer(cbind(Viable_Seeds, Flowers-Viable_Seeds) ~ 
                               scale(HerbS_RichIndex) + 
                              Site_Code + (1|Flowerhead) + (1|Individual) + (1|Block_Code), 
                            data = seeds.herb.Pol, 
                            family = binomial, glmerControl(optimizer = "bobyqa"),
                            na.action = na.omit) 

AICctab(Viable.M0,Viable.Site,Viable.Treat, Viable.Dist, Viable.Vis, Viable.Pol_Rich,
        Viable.Herb_Rich,Viable.Treat.Site, Viable.Treat.PolRich, Viable.Treat.HerbRich,
        Viable.Site.Dist, Viable.Site.Vis, Viable.Dis.HerbRich, Viable.Vis.HerbRich,
        Viable.Treat.Dist,Viable.Treat.Vis, Viable.Vis.PolRich, Viable.Dis.PolRich, 
        Viable.Site.PolRich, Viable.Site.HerbRich) 


#post-hoc test
emm1 <- emmeans(Viable.Treat, pairwise~ Treatment_Code,
                type = "response") #the results are given in the response scale

Viable.Temm <- as.data.frame(emm1$emmeans)
##########################################################################################################
#