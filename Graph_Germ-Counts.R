library(magrittr) #for piping
library(ggplot2)
getwd()
setwd("~/Experiments_R/Sandra_GH_2023")

# SETUP ----
# First, we need to load our previously process .RData object (our dataframe)
setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
list.files()
load("DF_Germination.Rdata")
head(Germ, 10)


  
# Should we use mutate or summarize?
example_mutate <- Germ %>%
  dplyr::group_by(Species, Salinity, Date) %>%
  dplyr::mutate(Xbar_Germ = mean(Emergence))

# how many rows do we want?
# 4 species, how many monitoring dates?, 3 salinity levels = how many rows we want
unique(Germ$Date)
# We have 8 unique monitoring dates
4* 10* 3
# =96 total rows 

# how many rows in mutate?
# check DF manually
# 384

# mutate wasnt right.... lets use summarize
head(Germ, 6)
example_summarize <- Germ %>% 
 dplyr::group_by(Species, Salinity, Date) %>%
  dplyr::summarize(Xbar_Germ = mean(Emergence)) %>%
  dplyr::ungroup()
nrow(example_summarize)#this is 96, which is what we expected!

Germ <- Germ %>% 
  dplyr::group_by(Species, Salinity, Day) %>%
  dplyr::summarize(Xbar_Germ = mean(Emergence)) %>%
  dplyr::ungroup()
nrow(Germ)

setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
Germ_mean <- Germ

save(Germ_mean, file="Germ_mean.Rdata")
  
# GRAPH ----
# We want to create 3 graphs, one for each salinity level, that will have 
# All 4 species on it. 
# X axis = Monitoring day
# Y axis = Germination count

unique(Germ$Day)
vec2 <- c("dodgerblue", "lightgoldenrod", 'indianred')

ggplot(Germ, aes(x=Day,
                 y=Xbar_Germ,
                 color = Species)) +
  theme_bw() +
  labs(title = "Germination Counts",
       x = "Monitoring Day",
       y = "Average Germination Counts") +
  theme(axis.title = element_text(size = 18),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +
  geom_point(size = 4
  ) +
  geom_line() +
  facet_wrap(~factor(Salinity, level = c("Fresh", "25", "40")))+
  scale_color_manual(values=vec2)

# You can rearrange factor ordering at the data frame level, 
# HERE, we rearranged it (temporarily) inside the ggplot call
# This did not change it at the data frame level
levels(Germ$Salinity)

setwd("~/Experiments_R/Sandra_GH_2023/Output")
ggsave(filename = "Final_maingraph.png",
       device = "png", # tiff or pdf for saving publication-quality figures
       width = 14, # define the width of the plot in your units of choice
       height = 8, # define the height of the plot in your units of choice
       units = "in", # define your units of choice ("in" stands for inches)
       dpi = 200)


# Cumulative Germination ----

# We want to add a column for cumulative germination
# That adds the prior count to the cell after it.

setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
list.files()
load("DF_Germination.Rdata")
head(Germ, 10)
                  
# Take the raw data here, we can average at the end.
# Create function for standard error
std.error <- function(x) sd(x)/sqrt(length(x))

Cumulative_Germination <- Germ %>% 
  dplyr::group_by(Species, Salinity, Species_Tmt_Pos) %>% 
  dplyr::mutate(Total_Germination = cumsum(Emergence),
                Day = dplyr::case_when(
                  Date == "2023-01-30" ~ 3,
                  Date == "2023-02-02" ~ 6,
                  Date == "2023-02-05" ~ 9,
                  Date == "2023-02-08" ~ 12,
                  Date == "2023-02-11" ~ 15,
                  Date == "2023-02-14" ~ 18,
                  Date == "2023-02-17" ~ 21,
                  Date == "2023-02-20" ~ 24,
                  Date == "2023-02-23" ~ 27,
                  Date == "2023-02-27" ~ 31)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Species, Salinity, Day) %>% 
  dplyr::summarize(SE = std.error(Total_Germination),
                   Total_Germination = mean(Total_Germination))

# Cumulative_Germination Graph ----

vec2 <- c("dodgerblue", "lightgoldenrod", 'indianred')

ggplot(Cumulative_Germination, 
       aes(x=Day,
           y=Total_Germination,
           color = Species)) +
  theme_bw() +
  labs(title = "Germination Counts",
       x = "Monitoring Day",
       y = "Cumulative Germination Counts: Mean ± SE") +
  theme(axis.title = element_text(size = 18),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +
  ylim(0, 100) + # Set 100 upper limit since that is what we seeded
  geom_linerange(ymin = Cumulative_Germination$Total_Germination - 
                  Cumulative_Germination$SE,
                ymax = Cumulative_Germination$Total_Germination + 
                  Cumulative_Germination$SE) +
  geom_point(size = 4) +
  geom_line() +
  facet_wrap(~factor(Salinity, level = c("Fresh", "25", "40")))+
  scale_color_manual(values=vec2)


setwd("~/Experiments_R/Sandra_GH_2023/Output")
ggsave(filename = "Cumulative_Germ-by-Salinity.png", 
       device = "png", # tiff or pdf for saving publication-quality figures
       width = 14, # define the width of the plot in your units of choice
       height = 8, # define the height of the plot in your units of choice
       units = "in", # define your units of choice ("in" stands for inches)
       dpi = 200)







