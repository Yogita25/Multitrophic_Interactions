
source("Q1_Final.R")

source("Q2_Final.R")

source("Q3_Final.R")

source("Simulation_sampleSize_Final.R")

source("Model_FL.R")

#libraries

library(ggdist)

library(ggplot2)

library(patchwork)

#plot for Q1

Dataset_tidy_Q1 <- bind_rows(Dataset_stack_Q1, .id = "id") %>%
  mutate(id = (as.numeric(id)-1)/5) %>%
  mutate(fecundity = Fert_seeds/Number_Flowers)

Q1 <- ggplot(Dataset_tidy_Q1, mapping = aes(x = id, y = fecundity, 
                                      fill = Treatment_Code)) +
  geom_dots(side = "both", position  = position_dodge(), 
            linewidth = 0, alpha = 0.7) +
  guides(fill = "none", colour = "none") + 
  scale_y_continuous(limits = c(0,1)) +
  geom_point(data = Estimates_emT_Q1, size = 2.5,
             mapping = aes(x = id, y = prob, fill = Treatment, 
                           colour = Treatment),
             position = position_dodge(width = 0.2)) +
  geom_errorbar(data = Estimates_emT_Q1, linewidth = 1.2,
                mapping = aes(x = id, y = prob, fill = Treatment, 
                              colour = Treatment, ymin = asymp.LCL,
                              ymax = asymp.UCL), 
                position = position_dodge(width = 0.2)) +
  scale_color_manual(values=c("deeppink4","dodgerblue4"))+
  scale_fill_manual(values=c("deeppink4","dodgerblue4"))+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(colour = "black", size = 14),
        axis.text.y = element_text(colour = "black", size = 14),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14))+
  ylab("Proportion of fertilised seeds")+
  xlab("Treatment effect")

#plots for Q2
Dataset_tidy_Q2 <- bind_rows(Dataset_stack_Q2, .id = "id")%>%
  #produces numerical id for each row
  mutate(id = (as.numeric(id)-1)/10)%>% 
  mutate(fecundity = Fert_seeds/Number_Flowers)

Q2 <- ggplot(Dataset_tidy_Q2, mapping = aes(x = id, y = fecundity,
                                      color = Treatment_Code))+
  geom_point(aes(shape = as.factor(Site_Code)),
             position = position_dodge(0.1), alpha = 0.5, size = 0.5)+
  guides(fill = "none", shape = "none", colour = "none") + 
  scale_y_continuous(limits = c(0,1))+
  geom_point(data = Estimates_emT_Q2, size = 2.5,
             mapping = aes(x = id, y = prob, fill = Treatment, 
                           colour = Treatment),
             position = position_dodge(width = 0.1)) +
  
  geom_errorbar(data = Estimates_emT_Q2, linewidth = 1.2,
                mapping = aes(x = id, y = prob, fill = Treatment, 
                              colour = Treatment, ymin = asymp.LCL,
                              ymax = asymp.UCL), 
                position = position_dodge(width = 0.1)) +
  scale_color_manual(values=c("deeppink4","dodgerblue4"))+
  scale_fill_manual(values=c("deeppink4","dodgerblue4"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(colour = "black", size =14),
        axis.text.y = element_text(colour = "black", size = 14),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14))+
  ylab("Proportion of fertilised seeds")+
  xlab("Site Pollinator effect")

#plots for Q3
Dataset_tidy_Q3 <- bind_rows(Dataset_stack_Q3, .id = "id") %>%
  mutate(id = (as.numeric(id)-1)/5) %>%
  mutate(fecundity = Fert_seeds/Number_Flowers)

Q3 <- ggplot(Dataset_tidy_Q3, mapping = aes(x = id, y = fecundity, 
                                   fill = Treatment_Code)) +
  geom_dots(side = "both", position  = position_dodge(), 
            linewidth = 0, alpha = 0.7) +
  guides(fill = "none") + 
  scale_y_continuous(limits = c(0,1)) +
  geom_point(data = Estimates_emT_Q3, size = 2.5,
             mapping = aes(x = id, y = prob, fill = 
                             Treatment, colour = Treatment),
             position = position_dodge(width = 0.2)) +
  
  geom_errorbar(data = Estimates_emT_Q3, linewidth = 1.2,
                mapping = aes(x = id, y = prob, fill = Treatment, 
                              colour = Treatment, ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.2)) +
  scale_color_manual(values=c("deeppink4","dodgerblue4","green4"))+
  scale_fill_manual(values=c("deeppink4","dodgerblue4","green4"))+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(colour = "black", size = 14),
        axis.text.y = element_text(colour = "black", size = 14),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14))+
  ylab("Proportion of fertilised seeds")+
  xlab("Treatment effect")

#plots for sample size scripts
Dataset_tidy_Q4 <- bind_rows(Dataset_stack_Q4, .id = "id") %>%
  mutate(id = (as.numeric(id))) %>%
  mutate(fecundity = Fert_seeds/Number_Flowers)

Q4 <- ggplot(Dataset_tidy_Q4, mapping = aes(x = Flowerhead, y = fecundity, 
                                   fill = Treatment_Code)) +
  geom_dots(side = "both", position  = position_dodge(width = 0.7), 
            linewidth = 0, alpha = 0.7) +
  guides(fill = "none", colour = "none") + 
  scale_y_continuous(limits = c(0,1)) +
  geom_point(data = Estimates_emT_Q4, size = 2.5,
             mapping = aes(x = id, y = prob, fill = Treatment, 
                           colour = Treatment),
             position = position_dodge(width = 0.5)) +
  
  geom_errorbar(data = Estimates_emT_Q4, linewidth = 1.2,
                mapping = aes(x = id, y = prob, fill = Treatment, 
                              colour = Treatment, ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values=c("deeppink4","dodgerblue4"))+
  scale_fill_manual(values=c("deeppink4","dodgerblue4"))+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(colour = "black", size = 14),
        axis.text.y = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        axis.title.x = element_text(colour = "black", size = 14))+
  ylab("Proportion of fertilised seeds")+
  xlab("Sample Size")

Fig2 <- Q1 + Q2 + Q3 + Q4 + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = "collect") & 
  theme(legend.position='bottom', 
        legend.text = element_text(colour = "black", size = 14),
        legend.title = element_blank())

ggsave("Fig2.svg", plot = Fig2, height = 7, width = 9.4, 
      dpi = 800 , units = "in")

