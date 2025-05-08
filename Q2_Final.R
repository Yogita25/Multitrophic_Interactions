
set.seed(123)
#libraries ####
library(colorspace)
library(lme4)
library(tidyverse)
library(ggdist)
library(emmeans)

#Script for the functions ####

source("Model_FL.R")

# Grid for treatment effects ####

Treatment_Grid <- data.frame(Control = rep(0, 11),
                             Predator_Exclusion = c(seq(0,2,by = 0.2)))

# Grid for site pollinator effect ####

Sites_Pol_Grid <- data.frame(A = rep(0,11),
                             B = c(seq(0,1, by = 0.1)),
                             C = c(seq(0,-1, length = 11)))
# Grid for site seed-predator effects #### 

Sites_Herb_Grid <- as.data.frame(matrix(nrow = 11, ncol = 3)) #empty matrix for four sites

for(i in 1:nrow(Sites_Herb_Grid)){
  Sites_Herb_Grid[i,] <- (Sites_Pol_Grid[10,]*(i-1)*0.1)
}

colnames(Sites_Pol_Grid) <- c("A", "B", "C") #column names = site_codes

# Parameters grid for collected flowerheads ####

FL_Grid <- seq(1,10,by = 1)

# stack of datasets ####

Dataset_stack_Q2 <- vector(mode = "list", length = nrow(Sites_Pol_Grid)) #empty list to store the datasets

#generate test dataset

for(i in 1:length(Dataset_stack_Q2)){
  Model.df <- Model(No_Sites = 3, No_Block = 48, 
                    Treatments = c("Control","Predator_Exclusion"),
                    Treat_effect = Treatment_Grid[11,],
                    Site_Pol = Sites_Pol_Grid [i,],
                    Site_Herb = Sites_Herb_Grid[1,],
                    Dist_Male = rnorm(48,0,0),
                    open_flHeads = FL_Grid[3],
                    m = 0,
                    s = 0.5,
                    a = 1,
                    b = 0.2)
  
  Dataset_stack_Q2[[i]] <- Model.df
  
}

mod <- glmer(cbind(Fert_seeds, Number_Flowers-Fert_seeds) ~ 
               Treatment_Code + Distance_Male + Site_Code +  
               (1|Block_Code) + (1|Flowerhead_Id) + (1|Individual),
             Dataset_stack_Q2[[11]], family = "binomial") 
summary(mod)

#Analysis ####

#post-hoc analysis

Analysis_Treatment <- list()
Analysis_Site<- list()
Analysis_LRT <- list()
Analysis_SumMod <- list()
Analysis_emmeans <- list()

for(k in 1:length(Dataset_stack_Q2)){
  #predicted values for case:1 --only pollinated seeds are eaten
  
  Estimates <- Model.Analysis(Pollinated = Dataset_stack_Q2[[k]]$Fert_seeds,
                                       Flowers = Dataset_stack_Q2[[k]]$Number_Flowers,
                                       Treatment = Dataset_stack_Q2[[k]]$Treatment_Code,
                                       Distance_Male = Dataset_stack_Q2[[k]]$Distance_Male,
                                       Site = Dataset_stack_Q2[[k]]$Site_Code,
                                       Block = Dataset_stack_Q2[[k]]$Block_Code, 
                                       Flowerhead = Dataset_stack_Q2[[k]]$Flowerhead_Id,
                                       Individual = Dataset_stack_Q2[[k]]$Individual,
                                       Df = Dataset_stack_Q2[[k]])
  
  Analysis_Treatment[[k]] <- Estimates[[1]]
  
  Analysis_Site[[k]] <- Estimates[[2]]
  
  Analysis_LRT[[k]] <- Estimates[[3]]
  
  Analysis_SumMod[[k]] <- Estimates[[4]]
  
  Analysis_emmeans[[k]] <- Estimates[[5]]
  
}

# combine results from analysis of all datasets

Estimates_Sites <- do.call(rbind, lapply(Analysis_Site, as.data.frame)) 

Estimates_LRT <-   do.call("rbind", Analysis_LRT)

S <- c("Site","Site1","Site2","Site3","Site4","Site5",
       "Site6","Site7","Site8","Site9", "Site10")

LRT_Sites <-  Estimates_LRT[rownames(Estimates_LRT) %in% S, ] 

Estimates_emT_Q2 <- do.call(rbind, lapply(Analysis_emmeans, as.data.frame))

Estimates_emT_Q2$id = rep(apply(Sites_Pol_Grid, 1, sd), each = 2)


