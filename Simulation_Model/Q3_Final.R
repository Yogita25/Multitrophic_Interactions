
set.seed(123)
#libraries ####
library(colorspace) 

#Script for the Model ####

source("Model_FL.R")

#Grid for treatment effects  ####
Treatment_Grid <- data.frame(Control = c(rep(0,11)), 
                             Predator_Exclusion = c(rep(1,11)),
                             Predator_Model = c(seq(-1,1, by = 0.2)))

# Grid for site pollinator effect ####
Sites_Pol_Grid <- as.data.frame(matrix(nrow = 11, ncol = 4))

for(i in 1:nrow(Sites_Pol_Grid)){
  Sites_Pol_Grid[i,] <- rnorm(4, 0.75, (i-1)/30)
}

colnames(Sites_Pol_Grid) <- c("A", "B", "C", "D") #column names = site_codes

# Grid for site seed-predator effects #### 

Sites_Herb_Grid <- as.data.frame(matrix(nrow = 11, ncol = 4)) #empty matrix for four sites

for(i in 1:nrow(Sites_Herb_Grid)){
  Sites_Herb_Grid[i,] <- (Sites_Pol_Grid[10,]*(i-1)*0.1)
}

colnames(Sites_Pol_Grid) <- c("A", "B", "C", "D") #column names = site_codes

# parameter grid for collected samples ####

FL_Grid <- seq(1,10,by = 1)

#stack of datasets

Dataset_stack_Q3 <- vector(mode = "list", length = nrow(Treatment_Grid)) #empty list to store the datasets

#generate test dataset

for(i in 1:length(Dataset_stack_Q3)){
  Model.df <- Model(No_Sites = 4, No_Block = 48, 
                    Treatments = c("Control","Predator_Exclusion",
                                   "Predator_Model"),
                    Treat_effect = Treatment_Grid[i,],
                    Site_Pol = c(0,0,0,0),
                    Site_Herb = c(0,0,0,0),
                    Dist_Male = rnorm(48,0,0), ## Distance - better to rename
                    open_flHeads = FL_Grid[3],
                    m = 0,
                    s = 0.5,
                    a = 1,
                    b = 0.2)
  
  Dataset_stack_Q3[[i]] <- Model.df
  
}

boxplot(Fert_seeds/Number_Flowers ~ Treatment_Code, 
        data = Dataset_stack_Q3[[1]],
        xlab = "Site",
        ylab = "Proportion of fertilized flowers/Flowerhead")

mod <- glmer(cbind(Fert_seeds, Number_Flowers-Fert_seeds) ~ 
               Treatment_Code + Distance_Male + Site_Code +  
               (1|Block_Code) + (1|Flowerhead_Id) + (1|Individual),
             Dataset_stack_Q3[[1]], family = "binomial") 
summary(mod)

#Analysis ####

#post-hoc analysis

Analysis_Treatment <- list()
Analysis_Site <- list()
Analysis_LRT <- list()
Analysis_SumMod <- list()
Analysis_emmeans <- list()

for(k in 1:length(Dataset_stack_Q3)){
  #predicted values for case:1 --only pollinated seeds are eaten
  
  Estimates <- Model.Analysis(Pollinated = Dataset_stack_Q3[[k]]$Fert_seeds,
                                       Flowers = Dataset_stack_Q3[[k]]$Number_Flowers,
                                       Treatment = Dataset_stack_Q3[[k]]$Treatment_Code,
                                       Distance_Male = Dataset_stack_Q3[[k]]$Distance_Male,
                                       Site = Dataset_stack_Q3[[k]]$Site_Code,
                                       Block = Dataset_stack_Q3[[k]]$Block_Code,
                                       Flowerhead = Dataset_stack_Q3[[k]]$Flowerhead_Id,
                                       Individual = Dataset_stack_Q3[[k]]$Individual,
                                       Df = Dataset_stack_Q3[[k]])
  
  Analysis_Treatment[[k]] <- Estimates[[1]]
  
  Analysis_Site[[k]] <- Estimates[[2]]
  
  Analysis_LRT[[k]] <-  Estimates[[3]]
  
  Analysis_SumMod[[k]] <- Estimates[[4]]
  
  Analysis_emmeans[[k]] <- Estimates[[5]]
  
}


Estimates_Treatment <- do.call("rbind", Analysis_Treatment)

Estimates_PredPred <- Estimates_Treatment[Estimates_Treatment$contrast == "Exclude_Predator / Predator_Model", 
                                         c("odds.ratio")]

Estimates_PredCon <- Estimates_Treatment[Estimates_Treatment$contrast == "Control / Predator_Model", 
                                          c("odds.ratio")]

Estimates_Site <- do.call("rbind", Analysis_Site)

Estimates_LRT <- do.call("rbind", Analysis_LRT)

Estimates_sumMod <- do.call("rbind", Analysis_SumMod)


T <- c("Treatment1", "Treatment2","Treatment3","Treatment4","Treatment5","Treatment6",
       "Treatment7","Treatment8","Treatment9", "Treatment10", "Treatment11" )

LRT_Treatment <- Estimates_LRT[rownames(Estimates_LRT) %in% T, ] 

Estimates_emT_Q3 <- do.call(rbind, lapply(Analysis_emmeans, as.data.frame))

Estimates_emT_Q3$id = rep(seq(0,2,0.2), each = 3)


