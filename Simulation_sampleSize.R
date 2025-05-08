set.seed(123)

#libraries ####
library(colorspace)

#Script for model ####
source("Model_FL.R")

#Treatment effect grid ####
Treatment_Grid <- data.frame(Control = rep(0, 11),
                             Predator_Exclusion = c(seq(0,2,by = 0.2)))

# Parameter grid for pollinators ####
Sites_Pol_Grid <- as.data.frame(matrix(nrow = 11, ncol = 4))

for(i in 1:nrow(Sites_Pol_Grid)){
  Sites_Pol_Grid[i,] <- rnorm(4, 0.75, (i-1)/30)
}

colnames(Sites_Pol_Grid) <- c("A", "B", "C", "D") #column names = site_codes

# Parameter grid for herbivores #### 
Sites_Herb_Grid <- as.data.frame(matrix(nrow = 11, ncol = 4))

for(i in 1:nrow(Sites_Herb_Grid)){
  Sites_Herb_Grid[i,] <- (Sites_Pol_Grid[10,]/(i*10))
}

colnames(Sites_Herb_Grid) <- c("A", "B", "C", "D") #column names = site_codes

#number of flowerheads collected ####

FL_Grid <- seq(1,10,by = 1)

#stack of datasets

Dataset_stack_Q4 <- vector(mode = "list", length = length(FL_Grid)) #empty list to store the datasets

#generate test dataset

for(i in 1:length(Dataset_stack_Q4)){
  Model.df <- Model(No_Sites = 4, No_Block = 48, 
                    Treatments = c("Control","Predator_Exclusion"),
                    Treat_effect = Treatment_Grid[3,],
                    Site_Pol = c(0,0,0,0),
                    Site_Herb = c(0,0,0,0),
                    Dist_Male = rnorm(48,0,0), 
                    open_flHeads = FL_Grid[i],
                    m = 0,
                    s = 0.5,
                    a = 1,
                    b = 0.2)
  
  Dataset_stack_Q4[[i]] <- Model.df
  
}


#Analysis ####

#post-hoc analysis

Analysis_Treatment <- list()
Analysis_Site <- list()
Analysis_LRT <- list()
Analysis_SumMod <- list()
Analysis_emmeans <- list()

for(k in 1:length(Dataset_stack_Q4)){
  #predicted values for case:1 --only pollinated seeds are eaten
  
  Estimates <- Model.Analysis(Pollinated = Dataset_stack_Q4[[k]]$Fert_seeds,
                                       Flowers = Dataset_stack_Q4[[k]]$Number_Flowers,
                                       Treatment = Dataset_stack_Q4[[k]]$Treatment_Code,
                                       Distance_Male = Dataset_stack_Q4[[k]]$Distance_Male,
                                       Site = Dataset_stack_Q4[[k]]$Site_Code,
                                       Block = Dataset_stack_Q4[[k]]$Block_Code, 
                                       Df = Dataset_stack_Q4[[k]])
  
  Analysis_Treatment[[k]] <- Estimates[[1]]
  
  Analysis_Site[[k]] <- Estimates[[2]]
  
  Analysis_LRT[[k]] <- Estimates[[3]]
  
  Analysis_SumMod[[k]] <- Estimates[[4]]
  
  Analysis_emmeans[[k]] <- Estimates[[5]]
  
}

# combine results from analysis of all datasets ####

Estimates_Treatment <- do.call("rbind", Analysis_Treatment)

Estimates_Site <- do.call("rbind", Analysis_Site)

Estimates_LRT <- do.call("rbind", Analysis_LRT)

Estimates_Sum <- do.call(rbind, lapply(Analysis_SumMod, summary))

Estimates_emT_Q4 <- do.call(rbind, lapply(Analysis_emmeans, as.data.frame))

Estimates_emT_Q4$id = rep(seq(1,10,1), each = 2)

