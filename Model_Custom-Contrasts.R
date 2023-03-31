# Germination Counts Model Analysis Script
# for 2023 Greenhouse Undergraduate Research Experiment
# Using a model we selected in 'Model_Germ-Counts.R'

# Load packages
library(emmeans)
library(glmmTMB)
library(car)
library(DHARMa)
library(blme)


getwd()
setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
list.files()
load("Model_object.RData") # Load model object here
load("DF_Germination-Fresh-25-Only.RData") # Load dataframe here


# Here we will specify a comparison between model estimates of the reduction
# in germination between Fresh and 25, across Species
# Which interaction is significant?

# Customized contrasts (the lengthy way)----

summary(G_Mod2)
emmeans(G_Mod2, pairwise ~ Salinity | Species)$contrasts 
#these contrasts are what we will create

emm <- emmeans(G_Mod2, ~ Species*Salinity)
emm

# Estimate means for specific treatment combinations
#   Should match emmeans output
# You create a vector where 1 = the emmean estimate for the species you want
# and 0's for the rest (it will only save the desired mean)
# for ALOC_Fresh, you would do c(1, 0, 0, 0, 0, 0)
# This is equivalent to c(4.22, 0, 0, 0, 0, 0)
# Because it is first in our summary, and we have 6 parameters total.

custom1 <- list(`ALOC_Fresh` = c(1, 0, 0, 0, 0, 0), 
                `ALOC_25`     = c(0, 0, 0, 1, 0, 0),
                `PHAU_Fresh`  = c(0, 1, 0, 0, 0, 0),
                `PHAU_25`     = c(0, 0, 0, 0, 1, 0),
                `SARU_Fresh`  = c(0, 0, 1, 0, 0, 0),
                `SARU_25`     = c(0, 0, 0, 0, 0, 1)
                )
contrast(emm, custom1)
emmeans(G_Mod2, ~ Species*Salinity)
# Note these match, but with order changed to our specification.


# Now estimate pairwise comparisons of Salinity effects for each Species
#   Note that for Fresh - 25, we subtract 25 coefficients from Fresh coefficients
#   Should match emmeans pairwise output
custom2 <- list(`ALOC_Fresh - ALOC_25` = c(1, 0, 0, -1,  0, 0), 
                `PHAU_Fresh - PHAU_25`  = c(0, 1, 0,  0, -1, 0),
                `SARU_Fresh - SARU_25`  = c(0, 0, 1,  0, 0, -1))
contrast(emm, custom2)
emmeans(G_Mod2, pairwise ~ Salinity | Species)$contrasts

# Now estimate interaction of Species (ALOC & PHAU & SARU) and Salinity (Fresh & 25)
#   Again, subtracting coefficients
custom3 <- list(`ALOC_Salinity - PHAU_Salinity` = c(1, -1, 0, -1, 1, 0),
                `ALOC_Salinity - SARU_Salinity` = c(1, 0, -1, -1, 0, 1),
                `PHAU_Salinity - SARU_Salinity` = c(0, 1, -1, 0, -1, 1)
                )
contrast(emm, custom3)
# You can check this by hand:
#   ALOC_Salinity = 1.90
#   PHAU_Salinity = 5.44
#   SARU_Salinity = 4.25
#   ALOC - PHAU = -3.54
#   ALOC - SARU = -2.35
#   PHAU - SARU = 1.19

# Note p values. As expected, PHAU experienced significantly more reduction in 
# germination than ALOC from the freshwater to 25 ppt level.
# No other interaction is significant.


# Customized contrasts (the short way)----

emm1 <- emmeans(G_Mod2, ~ Salinity | Species) 
emm1
con1 <- contrast(emm1, interaction = "pairwise")
con1
pairs(con1, by = NULL)
# Note that the pairs() is a tukey pairwise estimate
# Some notes from Susan --

    # Many useful contrast methods are available other than "pairwise"
    #   See documentation for Contrast families
    #   contrast-methods {emmeans}
    
    # If custom2 or custom3 had multiple contrasts with re-use of means,
    #   you can add  adjust = "tukey"  to contrast()

# However, the numbers here match what we did earlier, and the p values are more
# conservative, but the story remains the same
# the significant diff here is between ALOC and PHAU. 
# YAY!


