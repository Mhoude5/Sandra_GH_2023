#Model for Sandras germination experiment

library(tidyverse) #gives ggplot2 and piping
library(glmmTMB)
library(car)
library(DHARMa)
library(emmeans)
library(magrittr) #piping

setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
list.files()
load("DF_Germination.RData")
Germ %>% dplyr::filter(Salinity == "40" & Species == "SARU")

# Create a new df that has total emergence counts
Germ$Whole_Plot <- as.factor(paste(Germ$Column, Germ$Row))
Germ <- Germ %>%
  dplyr::group_by(Species, Salinity, Whole_Plot) %>%
  dplyr::summarise(Total_Emergence = sum(Emergence)) %>%
  dplyr::ungroup()

which(is.na(Germ$Total_Emergence))

#Column combined with Row equates to the whole plot location (i.e. a replicate)
#emergence are integer counts of seedlings, we removed seedlings as we counted them

#The goal is to create a model in the Poisson family of germination counts
#as a function of species, accounting for the whole plot (salinity) level
#Our whole plots included all 4 species at a single salinity level
#So, our analysis will focus on species comparisons at a single salinity level

class(Germ$Species)
sapply(Germ, class)

sapply(Germ, levels)

hist(Germ$Total_Emergence)

# Model
# specify whole plot variance and pot subplot variance, temporal variance
G_Mod <- glmmTMB(Total_Emergence ~ Species * Salinity
            +  (1 | Whole_Plot/Species),
            # + ar1(0 + Day|Whole_Plot:Species),
            family = poisson, #count data
            data = Germ)
summary(G_Mod)

G_Mod.resid <- simulateResiduals(G_Mod, plot=T)

plotResiduals(G_Mod.resid, form = Germ$Salinity)

# Maddie add other model evaluation items here

car::Anova(G_Mod)

# IF you have a significant interaction, you continue to a
# Tukey post-hoc comparison

# Whole plot is on salintiy, ask Susan our statistical power to do this
emmeans(G_Mod, pairwise ~ Salinity | Species, type = "response")
# type = "response" back transforms from log scale to get this back into real
# (Communicable) numbers

Fresh <- Germ %>% 
  dplyr::filter(Salinity == "Fresh")
Twenty5 <- Germ %>% 
  dplyr::filter(Salinity == "25") 
Forty <- Germ %>% 
  dplyr::filter(Salinity == "40") 


# If we take out day, we can do cumulative emergence and then
G_Mod <- glmmTMB(Emergence ~ Species + Salinity + Species:Salinity
               +  (1 | Whole_Plot),
               family = poisson, #count data
               data = Germ)