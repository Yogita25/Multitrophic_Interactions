#library ####

library(tidyr)
library(lme4)
library(emmeans)

#Model dataset ####

Design <- function(No_Sites, No_Block, Treatments, Treat_effect,
                   Site_Pol,Site_Herb, Dist_Male, open_flHeads,m,s){
  
  df.short <- data.frame( Block_Code = rep(1:No_Block, each = length(Treatments)),
                          Site_Code = rep(LETTERS[1:No_Sites],
                                           each = (No_Block/No_Sites)*length(Treatments)),
                          Treatment_Code = rep(Treatments, times = No_Block),
                          Treatment_Effect = rep(as.numeric(Treat_effect), times = No_Block),
                          Site_Pol_Eff = rep(as.numeric(Site_Pol), 
                                             each = (No_Block/No_Sites)*length(Treatments)),
                          Site_Herb_Eff = rep(as.numeric(Site_Herb), 
                                              each = (No_Block/No_Sites)*length(Treatments)),
                          Distance_Male = round(rep(Dist_Male, each = length(Treatments)),2),
                          Flowerhead = as.factor(rep(open_flHeads, 
                                                     times = No_Block * length(Treatments))),
                          Plant_Eff = rnorm(No_Block*length(Treatments),m,s)) # vearitaion between plant individuals
  
  
  df <- df.short[rep(row.names(df.short), each = open_flHeads),] #for equal number of flhead
  df$Number_Flowers <- rep(round(runif(nrow(df), min = 65, max = 80), 0))
  df$Individual = paste(df$Block_Code, df$Treatment_Code, sep = "-")
  df$Flowerhead_Id = as.factor(1:nrow(df))
  df$FlHead_Eff = rnorm(nrow(df), m,s) # variations between individual flowerheads
  
  return(df)
  
}

#probability of fertilization ####

Prob_Fert <- function(Site_Pol,Treat_Eff,Plant_Eff, FlHead_Eff, 
                      Dist_Male, Site_Herb, A, B){
  logOdds <- Site_Pol + A * Treat_Eff + Dist_Male - B * Treat_Eff - Site_Herb + 
    Plant_Eff + FlHead_Eff
  # here logOdds represents the likelihood of a flower getting pollinated
  # likelihood of a seed getting fertilized is a function of site pollinator richness,
  #treatement effect and distance from male plants
  #coefficients instead of probabilities
  prob = exp(logOdds)/(1 + exp(logOdds))
  #prob is the probability of a seed getting fertilized
  prob <- prob
  #prob[prob > 1] <- 1
  return(prob)
} 

#integrating all functions ####

#calculating actual probability based for pollination and herbivory


Model<- function(No_Sites, No_Block, Treatments,Treat_effect,
                 Site_Pol,Site_Herb, Dist_Male, open_flHeads,a, b,m,s){
  
  Data <- Design(No_Sites, No_Block, Treatments,Treat_effect, 
                 Site_Pol,Site_Herb, Dist_Male, open_flHeads,m,s) #call the dataframe
  
  
  Data$Fert_seeds <- rbinom(n = nrow(Data), size = Data$Number_Flowers, 
                           prob = Prob_Fert(Site_Pol = Data$Site_Pol_Eff,
                                           Treat_Eff = Data$Treatment_Effect,
                                           Dist_Male = Data$Distance_Male,
                                           Site_Herb = Data$Site_Herb_Eff,
                                           Plant_Eff = Data$Plant_Eff,
                                           FlHead_Eff = Data$FlHead_Eff,
                                           A = a,
                                           B = b))
  return(Data)
  
}

#statistical model ####

Model.Analysis <- function(Pollinated, Flowers, Treatment, Distance_Male, Site,
                           Block, Flowerhead, Individual, Df){
  
  Mod.Simulated<- glmer(cbind(Pollinated, Flowers-Pollinated) ~ Treatment + 
                          Distance_Male + Site +
                          (1|Block) + (1|Flowerhead_Id) + (1|Individual), Df,
                        family = "binomial") 
  
  Sum.Mod<- summary(Mod.Simulated)
 
  Mod.p <- drop1(Mod.Simulated, test = "Chisq")  #anova for fixed effects
  
  #post-hoc analysis
  
  #pairwise comparision for treatments
  
  Mod.T <- emmeans(Mod.Simulated, pairwise ~ Treatment, type = "response") 
  #type = response to predict probabilities on response scale
  
  Mod.T_contrasts <- Mod.T$contrasts %>%
    summary(infer = TRUE)%>%
    as.data.frame()
  
  #emmeans value for treatments
  
  Mod.emT <- Mod.T$emmean %>%
    summary(infer = TRUE)%>%
    as.data.frame()
  
  #pairwise comparision for sites
  
  Mod.S <- emmeans(Mod.Simulated, pairwise ~ Site, type = "response")
  
  Mod.S_contrasts <- Mod.S$contrasts %>%
    summary(infer = TRUE)%>%
    as.data.frame()
  
  #emmeans value for sites
  
  Mod.emS <- Mod.S$emmean %>%
    summary(infer = TRUE)%>%
    as.data.frame()
  
  return(list(Mod.T_contrasts,  Mod.S_contrasts, Mod.p, 
              Sum.Mod, Mod.emT, Mod.emS))
  
}







