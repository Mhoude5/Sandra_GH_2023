getwd()
setwd(C:/Users/Sandra/Documents/Experiments_R/Sandra_GH_2023)

list.files()
load("DF_Germination-Fresh-25-Only.RData")

library(ggplot2)
vec2 <- c( "khaki", "darkslategray3")
Germ1$Salinity <- factor(Germ1$Salinity, levels = c("25", "Fresh"))
ggplot(Germ1, aes(fill=Salinity, y=Total_Emergence, x=Species)) + 
  geom_bar(position="stack", stat="identity") +
  
  labs(title = "Salinity & Species Germination",
       y= "Total Germination Counts")+
  theme(axis.title = element_text(size = 18),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +
  scale_fill_manual(values=vec2)



setwd("~/Experiments_R/Sandra_GH_2023/Output")
ggsave(filename = "Stacked_graph2.png", 
       device = "png", # tiff or pdf for saving publication-quality figures
       width = 7, # define the width of the plot in your units of choice
       height = 7, # define the height of the plot in your units of choice
       units = "in", # define your units of choice ("in" stands for inches)
       dpi = 200)
