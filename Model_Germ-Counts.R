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

# Create DF with total emergence counts ----
Germ$Whole_Plot <- as.factor(paste(Germ$Column, Germ$Row))
Germ <- Germ %>%
  dplyr::group_by(Species, Salinity, Whole_Plot) %>%
  dplyr::summarise(Total_Emergence = sum(Emergence)) %>%
  dplyr::ungroup()

which(is.na(Germ$Total_Emergence))

# Look at data ----
Summary <- Germ %>%
  dplyr::group_by(Species, Salinity) %>%
  dplyr::summarise(Total_Emergence = sum(Total_Emergence)) %>%
  dplyr::ungroup()
# Note 
# ALOC has counts at Fresh and 25, but not 40 PPT
# PHAU has counts at Fresh and 25, but not 40 PPT
# SARU has counts at Fresh, but not 25. There are 2 seedlings at 40 PPT, however
# We assume this is a miscount, because we became stricter in our 
# assessment of seedling emergence as the experiment went on.
# Water caused the seed to expand and the cotyledon to unfold,
# But a shoot did not emerge.

# Remove 2 counts of SARU at 40 PPT
# Find row number
# Change value at row number
Germ$Total_Emergence[Germ$Species == "SARU" & Germ$Salinity == "40" & 
                       Germ$Total_Emergence == "2"] <- 0

# Column combined with Row equates to the whole plot location (i.e. a replicate)
# emergence are integer counts of seedlings, we removed seedlings as we counted them

# The goal is to create a model in the Poisson family of germination counts
# as a function of species, accounting for the whole plot (salinity) level
# Our whole plots included all 4 species at a single salinity level
# So, our analysis will focus on species comparisons at a single salinity level

# Double check class
sapply(Germ, class)
# Look at levels of factors
sapply(Germ, levels)
# Reorder salinity factors
Germ$Salinity <- factor(Germ$Salinity, levels=c("Fresh", "25", "40"))
# Look at distribution of total_emergence counts (y variable)
hist(Germ$Total_Emergence)

# Model with all 3 salinities ----
# specify whole plot variance and pot subplot variance, temporal variance
G_Mod <- glmmTMB(Total_Emergence ~ Species * Salinity
            +  (1 | Whole_Plot/Species),
            # + ar1(0 + Day|Whole_Plot:Species), #No longer necessary
            family = poisson, #count data
            data = Germ)
# Model convergence problem

# Remove 40 since we have 0 counts across the board?
Germ1 <- Germ %>%
  dplyr::filter(Salinity %in% c("Fresh", "25")) %>%
  dplyr::mutate(Salinity = factor(Salinity)) #re-factor salinity since we removed 40

# Model w/ 2 salinities (Fresh & 25 only) ----
G_Mod1 <- glmmTMB(Total_Emergence ~ Species * Salinity
                 +  (1 | Whole_Plot),
                 family = poisson, #count data
                 data = Germ1)
G_Mod1 <- glmmTMB(Total_Emergence ~ Species * Salinity
                  +  (1 | Whole_Plot),
                  family = nbinom1(), # quasi poisson
                  data = Germ1)


summary(G_Mod1)

## Check assumptions G_Mod1 ----

G_Mod1.resid <- simulateResiduals(G_Mod1, plot=T)
# Model performance seems poor because we have a small number of samples/points
# Maybe the initial model (with day included) performs better after all

## Error with below code ----
plotResiduals(G_Mod1.resid, form = Germ1$Salinity)

emmip(G_Mod1, Species ~ Salinity, CIs = F) #salinity effect
emmip(G_Mod1, Salinity ~ Species, CIs = T, type = "response") #Species effect
emmip(G_Mod1, Salinity ~ Species, CIs = F)

car::Anova(G_Mod1) #significant interaction
emmeans(G_Mod1,  ~ Salinity | Species, type = "response")

emmeans(G_Mod1, pairwise ~ Salinity | Species)
# customized contrast would match these vals
# huge standard error for SARU


