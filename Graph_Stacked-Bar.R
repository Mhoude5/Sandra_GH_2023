

#stacked

getwd()
setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
list.files()
load("Germ_mean.Rdata")
head(Germ_mean)



library(ggplot2)
vec2 <- c('firebrick', "khaki", "darkslategray3")
Germ_mean$Salinity <- factor(Germ_mean$Salinity, levels = c("40", "25", "Fresh"))
ggplot(Germ_mean, aes(fill=Salinity, y=Xbar_Germ, x=Species)) + 
  geom_bar(position="stack", stat="identity") +
  
  labs(title = "Salinity & Species Germination",
       y= "Average Germination Counts")+
  theme(axis.title = element_text(size = 18),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +
  scale_fill_manual(values=vec2)








setwd("~/Experiments_R/Sandra_GH_2023/Output")
ggsave(filename = "Stacked_graph.png", 
       device = "png", # tiff or pdf for saving publication-quality figures
       width = 14, # define the width of the plot in your units of choice
       height = 8, # define the height of the plot in your units of choice
       units = "in", # define your units of choice ("in" stands for inches)
       dpi = 200)
  



