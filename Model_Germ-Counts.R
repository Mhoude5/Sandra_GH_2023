# Germination Counts Model for 2023 Greenhouse Undergraduate Research Experiment

# Load packages ----
library(tidyverse) #gives ggplot2 and piping
library(glmmTMB)
library(car)
library(DHARMa)
library(emmeans)
library(blme)

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

# See a short summary table of total counts, added across reps ----
Summary <- Germ %>%
  dplyr::group_by(Species, Salinity) %>%
  dplyr::summarise(Total_Emergence = sum(Total_Emergence)) %>%
  dplyr::ungroup()
# Note 
# ALOC has counts at Fresh and 25, but not 40 PPT
# PHAU has counts at Fresh and 25, but not 40 PPT
# SARU has counts at Fresh, but not 25 or 40.

# Column combined with Row equates to the whole plot location (i.e. a replicate)
# emergence are integer counts of seedlings, we removed seedlings as we counted them

# The goal is to create a model in the Poisson family of germination counts
# as a function of species, accounting for the whole plot (salinity) level
# Our whole plots included all 4 species at a single salinity level

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
G_Mod <- glmmTMB(Total_Emergence ~ Species + Salinity + Species:Salinity
            +  (1 | Whole_Plot),
            # + ar1(0 + Day|Whole_Plot:Species), # Not necessary, we removed day
            family = poisson, # count data
            data = Germ)
# Model convergence problem




# Remove 40PPT values, as we have 0's across the board
# and we recognize this as too high of a salinity
# and not worth analyzing.

Germ1 <- Germ %>%
  dplyr::filter(Salinity %in% c("Fresh", "25"))
Germ1$Salinity <- factor(Germ1$Salinity)
levels(Germ1$Salinity) # re-factor salinity since we removed 40

# Another way to refactor (less intuitive for Maddie's brain)
Germ1 <- Germ %>%
  dplyr::filter(Salinity %in% c("Fresh", "25")) %>%
  dplyr::mutate(Salinity = factor(Salinity)) # re-factor salinity since we removed 40


# Poisson Model Fresh & 25 only ----
G_Mod1 <- glmmTMB(Total_Emergence ~ Species + Salinity + Species:Salinity
                 +  (1 | Whole_Plot),
                 family = poisson, 
                 data = Germ1)
summary(G_Mod1)

G_Mod1.resid <- simulateResiduals(G_Mod1, plot=T)
plotResiduals(G_Mod1.resid, form = Germ1$Salinity)
# Diagnostic plots could be improved


# Quasi Poisson Fresh and 25 only ----
G_Mod1 <- glmmTMB(Total_Emergence ~ Species + Salinity + Species:Salinity
                  +  (1 | Whole_Plot),
                  family = nbinom1(), # quasi poisson
                  data = Germ1)
summary(G_Mod1)
G_Mod1.resid <- simulateResiduals(G_Mod1, plot=T)

# Spread of residuals is improved using this model 


## Explore model ----
plotResiduals(G_Mod1.resid, form = Germ1$Salinity)


emmip(G_Mod1, Species ~ Salinity, CIs = F) # salinity effect
# issue with SARU

emmip(G_Mod1, Salinity ~ Species, CIs = T, type = "response") # Species effect
# issue with SARU

emmip(G_Mod1, Salinity ~ Species, CIs = F)

car::Anova(G_Mod1) #significant interaction Species:Salinity
emmeans(G_Mod1,  ~ Salinity | Species, type = "response") # back transformed

emmeans(G_Mod1, pairwise ~ Salinity | Species) # on log scale
emmeans(G_Mod1, ~ Salinity | Species) 
# customized contrasts would match these vals (Fresh - 25)
# huge standard error for SARU that must be fixed

# Customized contrasts ----
# (I have to fix model first, but practicing contrasts here)

emm1 <- emmeans(G_Mod1, ~ Salinity | Species) 
emm1
con1 <- contrast(emm1, interaction = "pairwise")
con1
pairs(con1, by = NULL)


# Original method I tried to test contrasts below
alocF <- c(1, rep(0, 5))
aloc25 <- c(0, 1, rep(0, 4))
PHAUF <- c(0, 0, 1, rep(0, 3))
PHAU25 <- c(rep(0, 3), 1, 0, 0)
SARUF <- c(rep(0, 4), 1, 0)
SARU25 <- c(rep(0, 5), 1)


alocRED <- alocF - aloc25
phauRED <- PHAUF - PHAU25
saruRED <- SARUF - SARU25

contrast(emm1, method = list("ALOC to PHAU" = alocRED - phauRED,
                             "PHAU to SARU" = phauRED - saruRED,
                             "ALOC to SARU" = alocRED - saruRED) ) 
# non conforming, I am pretty sure this has to do with the EMMgrid object
# being separated by species rather than a single column,
# it may be acting more like an array so my c(0,0,0,0,0,0) objects wont work
# but unsure how to fix it.


# Model with fixed effects priors ----
## Poisson fixed effects priors ----
G_Mod1 <- bglmer(Total_Emergence ~ Species * Salinity
                  +  (1 | Whole_Plot),
                  family = poisson(),
                  fixef.prior = normal(sd = c(3, 3, 3)), 
                  data = Germ1)
                
summary(G_Mod1)
G_Mod1.resid <- simulateResiduals(G_Mod1, plot=T)
# Diagnostic plots could be improved, go to nbinom

## Neg binom fixed effects prior ----
G_Mod1 <- bglmer(Total_Emergence ~ Species * Salinity
                 +  (1 | Whole_Plot),
                 family = nbinom2(),
                 # family = nbinom1(), 
                 # nbinom1() error: "Downdated VtV is not positive definite"
                 fixef.prior = normal(sd = c(3, 3, 3)), 
                 data = Germ1)
summary(G_Mod1) # vcov issue
# Diagnostic plots won't run for nbinom2(). How to evaluate ?
G_Mod1.resid <- simulateResiduals(G_Mod1, plot=T)

## Neg binom, fixed effects on Species but not salinity ----
G_Mod1 <- bglmer(Total_Emergence ~ Species * Salinity
                 +  (1 | Whole_Plot),
                 family = nbinom2(),
                 fixef.prior = normal(sd = c(3, Inf, 3)), 
                 data = Germ1)

summary(G_Mod1) #vcov issue ?


