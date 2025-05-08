set.seed(123)
#libraries ####
library(colorspace)
library(lme4)
library(tidyverse)
library(ggdist)
library(emmeans)

#source the generative and statistical model ####
source("Model_FL.R")

# Grid for the treatment effect ####

Treatment_Grid <- data.frame(Control = rep(0, 11),
                             Predator_Exclusion = c(seq(0,2,by = 0.2)))

FL_Grid <- seq(1,10,by = 1)

#stack of datasets ####

Dataset_stack_Q1 <- vector(mode = "list", 
                           length = nrow(Treatment_Grid)) #empty datasets

#generate test dataset

for(i in 1:length(Dataset_stack_Q1)){
  Model.df <- Model(No_Sites = 4, No_Block = 48, 
                    Treatments = c("Control","Predator_Exclusion"),
                    Treat_effect = Treatment_Grid[i,],
                    Site_Pol = c(0,0,0,0),
                    Site_Herb = c(0,0,0,0), 
                    Dist_Male = rnorm(48,0,0),
                    open_flHeads = FL_Grid[3],
                    m = 0,
                    s = 0.5,
                    a = 1,
                    b = 0.2) # repeat 3 for both the treatments in all 48 blocks
  
  Dataset_stack_Q1[[i]] <- Model.df
  
}

boxplot(Fert_seeds/Number_Flowers ~ Treatment_Code + 
          Site_Code, data = Dataset_stack_Q1[[1]],
        xlab = "Site",
        ylab = "Proportion of fertilized flowers/Flowerhead")

mod <- glmer(cbind(Fert_seeds, Number_Flowers-Fert_seeds) ~ 
               Treatment_Code + Distance_Male + Site_Code +  
               (1|Block_Code) + (1|Flowerhead_Id) + (1|Individual),
             Dataset_stack_Q1[[11]], family = "binomial") 
summary(mod)

#emmeans(mod, pairwise ~ Treatment_Code, type = "response")
######################################################################################################################

#Analysis ####

#post-hoc analysis

Analysis_Treatment<- list()
Analysis_Site <- list()
Analysis_LRT <- list()
Analysis_SumMod<- list()
Analysis_emmeans <- list()


for(k in 1:length(Dataset_stack_Q1)){
  #predicted values for case:1 --only pollinated seeds are eaten
  
  Estimates <- Model.Analysis(Pollinated = Dataset_stack_Q1[[k]]$Fert_seeds,
                                       Flowers = Dataset_stack_Q1[[k]]$Number_Flowers,
                                       Treatment = Dataset_stack_Q1[[k]]$Treatment_Code,
                                       Distance_Male = Dataset_stack_Q1[[k]]$Distance_Male,
                                       Site = Dataset_stack_Q1[[k]]$Site_Code,
                                       Block = Dataset_stack_Q1[[k]]$Block_Code,
                                       Flowerhead = Dataset_stack_Q1[[k]]$Flowerhead_Id,
                                       Individual = Dataset_stack_Q1[[k]]$Individual,
                                       Df = Dataset_stack_Q1[[k]])
  
  Analysis_Treatment[[k]] <- Estimates[[1]] #contrasts between treatments
  
  Analysis_Site[[k]] <- Estimates[[2]] #contrasts between sites
  
  Analysis_LRT[[k]] <-  Estimates[[3]] # partial test on fixed effects
  
  Analysis_SumMod[[k]] <- Estimates[[4]]
  
  Analysis_emmeans[[k]] <- Estimates[[5]]
  
  
}

#combined results from all datasets

Estimates_Treatments <- do.call(rbind, lapply(Analysis_Treatment, 
                                              as.data.frame)) #contrasts between treatments

Estimates_Sites <- do.call(rbind, lapply(Analysis_Site, as.data.frame))

Estimates_SumMod <- do.call(rbind, lapply(Analysis_SumMod, summary))

Estimates_Sum1<- do.call(rbind, lapply(Estimates_SumMod[1,10], as.data.frame))

Estimates_Sum6<- do.call(rbind, lapply(Estimates_SumMod[6,10], as.data.frame))

Estimates_Sum11<- do.call(rbind, lapply(Estimates_SumMod[11,10], as.data.frame))

Estimates_emT_Q1 <- do.call(rbind, lapply(Analysis_emmeans, as.data.frame))

Estimates_emT_Q1$id = rep(seq(0,2,0.2), each = 2)


