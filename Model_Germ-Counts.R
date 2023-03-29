# Germination Counts Model Creation Script
# for 2023 Greenhouse Undergraduate Research Experiment

# Load packages ----
# Sandra- you will probably have to install these, you can do so in the bottom 
# right (click on packages tab, and then install)
# or you can install directly in the console (copy and paste, ...
# dont run it in the script, as you don't want to re-install every time..)
# install.packages("package name")

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
# (We wont analyze this table, it's just for me to quickly see the data..)
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

# Another way to re-factor (less intuitive for Maddie's brain)
Germ1 <- Germ %>%
  dplyr::filter(Salinity %in% c("Fresh", "25")) %>%
  dplyr::mutate(Salinity = factor(Salinity)) # re-factor salinity since we removed 40




# Poisson Model Fresh & 25 only ----
G_Mod1 <- glmmTMB(Total_Emergence ~ Species + Salinity + Species:Salinity
                 +  (1 | Whole_Plot),
                 family = poisson, 
                 data = Germ1)
OG_summ <- print(summary(G_Mod1)) # Saving this summary for future comparison

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
    # note issue with SARU
  
    emmip(G_Mod1, Salinity ~ Species, CIs = T, type = "response") # Species effect
    # note issue with SARU
  
    emmip(G_Mod1, Salinity ~ Species, CIs = F) # You can toggle CI's on and off
  
    # Check for significance of fixed effects
    car::Anova(G_Mod1) #significant interaction Species:Salinity
    
    emmeans(G_Mod1,  ~ Salinity | Species, type = "response") # back transformed
    emmeans(G_Mod1, pairwise ~ Salinity | Species) # on log scale & pairwise
    emmeans(G_Mod1, ~ Salinity | Species) # Just by species (not pairwise)
    # customized contrasts would match these vals (Fresh - 25)
    # huge standard error for SARU that must be fixed


# Model with fixed effects priors ----
    
# We have 6 fixed effects to add priors to (including intercept), you can see this in
    
## Model with reasonable priors (3) ----
G_Mod1 <- bglmer(Total_Emergence ~ Species * Salinity
                  +  (1 | Whole_Plot),
                  family = poisson(), # back to Poisson for now
                  fixef.prior = normal(sd = c(3, 3, 3, 3, 3, 3)), 
                  data = Germ1)
# if you don't specify enough parameters in c(# # #), the intercept is taken separate
# and the tail is repeated. 
# for example c(10, 2.5) would have a SD of 10 on the intercept, and 2.5 for remaining
# parameters
                
# Notice the change in SE's from OG_summ to line 139.
OG_summ
summary(G_Mod1)


## Model with default normal priors ----
G_Mod1 <- bglmer(Total_Emergence ~ Species * Salinity
                 +  (1 | Whole_Plot),
                 family = poisson(),
                 fixef.prior = normal(), # going with defaults
                 data = Germ1)

summary(G_Mod1)
# You can see in the summary that default is c(10, 2.5)
# The SE is slightly smaller for defaults than for our original vals c(3, 3)



## Model with weak penalization (1e4) for all but the interaction (2.5) ----
G_Mod2 <- bglmer(Total_Emergence ~ Species * Salinity
                 +  (1 | Whole_Plot),
                 family = poisson(),
                 fixef.prior = normal(c(1e4, 1e4, 1e4, 1e4, 2.5, 2.5)), 
                 data = Germ1)

# Note that our standard error estimates seem pretty reasonable, again compare to OG
# Also notice only the interaction terms estimate really changed 
# (SpeciesPHAU:Salinity25, SpeciesSARU:Salinity25)
OG_summ
summary(G_Mod2)
# I like this fixed effects prior the best, because we only penalize the interaction
# I don't think we have to penalize our main effects since they are so strong.
# But I will mention I am no expert at this, and Susan might have a more nuanced
# view of this particular model.

    ## Explore model ----
    G_Mod2.resid <- simulateResiduals(G_Mod2, plot=T) # warning message DHARMa

    plotResiduals(G_Mod2.resid, form = Germ1$Salinity)
    # Note difference in variance.. Remember we first fixed this by moving
    # to a nbinom1() distribution. However, that is not an option with fixef.priors,
    # so we have to accept that our poisson model is perhaps not the best fit,
    # but the best we can do in this scenario.
    
    emmip(G_Mod2, Species ~ Salinity, CIs = T) # salinity effect
    # much more reasonable compared to first attempt
    # Which interactions would you expect to be different here?
    # Remember interactions take parallel lines and make them cross at some point.
    # because it shifts the slope of the line
    # MY guess would be ALOC and PHAU are different
    # but not SARU and PHAU, and not SARU and ALOC.
    
    emmip(G_Mod2, Salinity ~ Species, CIs = T, type = "response") # Species effect
    # note difference in CI bars between Fresh and 25
    emmip(G_Mod2, Salinity ~ Species, CIs = F, type = "response")
    
    # Check for significance of fixed effects
    car::Anova(G_Mod2) #significant interaction Species:Salinity
    # Note that the interaction is a LOT more significant than our first model now that
    # we have fixed the SARU estimate
    
    emmeans(G_Mod2,  ~ Salinity | Species, type = "response") # back transformed
    # i.e., our actual germination rate estimates (~66 seeds at Fresh for PHAU)
    emmeans(G_Mod2, pairwise ~ Salinity | Species) # on log scale & pairwise
    # customized contrasts would match these vals (Fresh - 25)
    emmeans(G_Mod2, ~ Salinity | Species) # Just by species (not pairwise)

# Check diagnostics
G_Mod2.resid <- simulateResiduals(G_Mod2, plot=T)
# Diagnostic plots could be improved
# Per Susan: Lots of problems with these data for other distributions -
# e.g., negative binomal nbinom1() or nbinom2()
# We can try a different package (brms), BUT the story of our data is pretty clear
# I.E.- we dont have to go crazy in our analysis given our clear story through graphs
# and my limited understanding of fixed effects priors. 
# (It is hard to justify a complex analysis that I don't fully understand)


# Let's call it a day, save the model object and move on.
getwd()
setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
save(G_Mod2, file = "Model_object.RData")
save(Germ1, file = "DF_Germination-Fresh-25-Only.RData")
list.files()

# From here, we will further investigate our selected model ... 
# Move to the Model_Custom-Contrasts R Script



