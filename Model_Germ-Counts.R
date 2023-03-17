#Model for Sandras germination experiment

library(tidyverse) #gives ggplot2 and piping
library(glmmTMB)
library(car)
library(DHARMa)
library(emmeans)

setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
list.files()
load("DF_Germination.RData")

#Column combined with Row equates to the whole plot location (i.e. a replicate)
#emergence are integer counts of seedlings, we removed seedlings as we counted them

#The goal is to create a model in the Poisson family of germination counts
#as a function of species, accounting for the whole plot (salinity) level
#Our whole plots included all 4 species at a single salinity level
#So, our analysis will focus on species comparisons at a single salinity level

Germ$Whole_Plot <- as.factor(paste(Germ$Column, Germ$Row))
levels(Germ$Whole_Plot)
with(Germ, table(Whole_Plot, Salinity, Species))
Germ$Day <- as.factor(Germ$Day)
levels(Germ$Day)

#Model
#specify whole plot variance and pot subplot variance, temporal variance
G_Mod<-glmmTMB(Emergence ~ Species * Salinity * Day
            +  (1 | Whole_Plot/Species),
            # + ar1(0 + Day|Whole_Plot:Species),
            family = poisson, #count data
            data = Germ)
summary(G_Mod)


#If we take out day, we can do cumulative emergence and then
G_Mod<-glmmTMB(Emergence ~ Species * Salinity
               +  (1 | Whole_Plot),
               family = poisson, #count data
               data = Germ)