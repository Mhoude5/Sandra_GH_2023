library(magrittr) #for piping
library(ggplot2)
library(dplyr)
getwd()

setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
list.files()
load("Germ_mean.Rdata")


vec2 <- c("darkslategray3", "khaki", 'firebrick')

Germ_mean$Salinity <- factor(Germ_mean$Salinity, levels = c("Fresh", "25", "40"))

ggplot(Germ_mean, aes(x=Day,
                 y=Xbar_Germ,
                 color = Salinity)) +
  theme_bw() +
  labs(title = "Species Germinations",
       x = "Monitoring Day",
       y = "Average Germination Counts") +
  theme(axis.title = element_text(size = 18),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +
  geom_point(size = 4
  ) +
  geom_line() +
  facet_wrap(~factor(Species, level = c("ALOC", "PHAU", "SARU")))+
  scale_color_manual(values=vec2)


setwd("~/Experiments_R/Sandra_GH_2023/Output")

ggsave(filename = "Species_Germinations.png",
       device = "png", # tiff or pdf for saving publication-quality figures
       width = 14, # define the width of the plot in your units of choice
       height = 8, # define the height of the plot in your units of choice
       units = "in", # define your units of choice ("in" stands for inches)
       dpi = 200)




