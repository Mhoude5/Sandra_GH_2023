# In this script I will create a graph showing
# the salinity measurements (mean +/- SE) that Sandra took
# of our 3 salinity treatments for the top and bottom of the soil
# and then add a secondary line with the salinity measurements
# that USUAL took for comparison.

library(tidyverse) #for graphing etc
getwd()
setwd("./Raw_Data")
list.files()
EC <- read.csv("EC_Final.csv", header = T)
EC_usual <- read.csv("USUAL_EC.csv", header = T, skip = 13)

# EC datasheet wrangling ----
# We have strings within 'Sample' we can separate to facilitate grouping
# DT = DISP TOP; DB = DISP BOTTOM
# AT = ALOC TOP; AB = ALOC BOTTOM
# PT = PHAU TOP; PB = PHAU BOTTOM
# ST = SARU TOP; SB = SARU BOTTOM
# Function for standard error
std.error <- function(x) sd(x)/sqrt(length(x))
EC <- EC %>% 
  dplyr::mutate(Species = 
                  dplyr::case_when(
                    endsWith(Sample, "AT") | endsWith(Sample, "AB") ~ "ALOC",
                    endsWith(Sample, "PT") | endsWith(Sample, "PB") ~ "PHAU",
                    endsWith(Sample, "DT") | endsWith(Sample, "DB") ~ "DISP",
                    endsWith(Sample, "ST") | endsWith(Sample, "SB") ~ "SARU"),
                Position = 
                  dplyr::case_when(
                    endsWith(Sample, "T") ~ "Top",
                    endsWith(Sample, "B") ~ "Bottom"
                  )) %>% 
  dplyr::select(Species_Tmt_Pos, Species, Target_Salinity, Position,
                EC_microS_cm, ECe_dS_m, PSU_e) %>% 
  na.omit() %>% 
  dplyr::group_by(Species, Position, Target_Salinity) %>% 
  dplyr::mutate(sd_EC = std.error(EC_microS_cm),
                EC_microS_cm = mean(EC_microS_cm),
                sd_ECe = std.error(ECe_dS_m),
                ECe_dS_m = mean(ECe_dS_m))
  

# The EC_USUAL wrangling ----
EC_usual <- EC_usual %>% 
  dplyr::mutate(Species = stringr::word(Identification, 2),
                Target_Salinity = dplyr::case_when(
                  startsWith(Identification, "0") ~ "0",
                  startsWith(Identification, "25") ~ "25",
                  startsWith(Identification, "40") ~ "40"),
                Position = dplyr::case_when(
                  endsWith(Identification, "Top") ~ "Top",
                  endsWith(Identification, "Bottom") ~ "Bottom"),
                EC_microS_cm = dS.m * 1000,
                ECe_dS_m_USUAL = EC_microS_cm * (Water_Volume / 35) / 1000) %>% 
  dplyr::select(Species_Tmt_Pos, Species, Target_Salinity, Position, 
                EC_microS_cm, ECe_dS_m_USUAL) 
  
# Combine dataframes ----

comparison <- EC_usual %>% 
  left_join(EC, by = c("Species_Tmt_Pos", "Target_Salinity", "Species",
                       "Position")) %>% 
  pivot_longer(cols = c("ECe_dS_m", "ECe_dS_m_USUAL"),
               names_to = "Lab",
               values_to = "ECe") %>% 
  select(Species, Target_Salinity, Position, ECe, Lab) %>% 
  na.omit() %>% 
  as.data.frame() %>% 
  mutate(Lab = case_when(Lab == "ECe_dS_m" ~ "In-House",
                         Lab == "ECe_dS_m_USUAL" ~ "USU Analytical Lab")) %>% 
  unite("Temp", Species:Position, remove= F) %>% 
  distinct()

## Drop any that dont have a value for In-House AND USUAL

# remove if sum of spp occurrences is less than 2
# tabulate the id variable
tab <- table(comparison$Temp) # Should only have vals of 1 or 2 here

# Get the names of the ids that we care about.
# In this case the ids that occur >1 (2) times
idx <- names(tab)[tab > 1]
idx
# Save only the data that we care about
comparison <- comparison[comparison$Temp %in% idx,] #30 to 24

comparison$Temp <- as.factor(comparison$Temp)
levels(comparison$Temp)

# Graph ECe comparisons ----

## SARU ----
S <-  comparison %>% 
  filter(Species == "SARU") %>% 
  ggplot(aes(x = Position,
             y = ECe,
             color = Lab)) +
  facet_wrap( ~Target_Salinity) +
  geom_point(size = 3) +
  labs(x = "Species",
       title = "SARU \nSalinity analysis by Soil Depth",
       color = "Location of Analysis") +
  theme_bw()
S

##PHAU ----
P <-  comparison %>% 
  filter(Species == "PHAU") %>% 
  ggplot(aes(x = Position,
             y = ECe,
             color = Lab)) +
  facet_wrap( ~Target_Salinity) +
  geom_point(size = 3) +
  labs(x = "Species",
       title = "PHAU \nSalinity analysis by Soil Depth",
       color = "Location of Analysis") +
  theme_bw()
P

##ALOC
A <-  comparison %>% 
  filter(Species == "ALOC") %>% 
  ggplot(aes(x = Position,
             y = ECe,
             color = Lab)) +
  facet_wrap( ~Target_Salinity) +
  geom_point(size = 3) +
  labs(x = "Species",
       title = "ALOC \nSalinity analysis by Soil Depth",
       color = "Location of Analysis") +
  theme_bw()
A
## View together ----
A
P
S





# Graph EC first ----
EC <- EC %>%
  dplyr::filter(Species != "DISP") 

EC_g <- ggplot(EC, 
               aes(x = Position, 
                   y = EC_microS_cm,
                   color = Species)) +
  ylim(0, 70000) +
  geom_point(size = 3)  +
  geom_linerange(ymin =  EC$EC_microS_cm - EC$sd_EC,
                 ymax = EC$EC_microS_cm + EC$sd_EC) +
  labs(title = "In-house Salinity",
       y = "EC microS/cm") +
  facet_wrap( ~Target_Salinity)
EC_g

# Graph EC_usual second ----
EC_usg <- ggplot(EC_usual,
       aes(x = Position,
           y = EC_microS_cm,
           color = Species)) +
  ylim(0, 70000) +
  labs(title = "USUAL Salinity",
       y = "EC microS/cm") +
  geom_point(size = 3)  +
  facet_wrap( ~Target_Salinity)

# Show together
gridExtra::grid.arrange(EC_g, EC_usg)


# I think I want EC_usual appended onto EC, species name should be modified first
# to say SPP_Usual
# Then I can plot singular species, faceted by salinity, with both measurements 
# on it

EC_usual <- EC_usual %>% 
  dplyr::mutate(Species = dplyr::case_when(
    Species == "ALOC" ~ "ALOC_USUAL",
    Species == "PHAU" ~ "PHAU_USUAL",
    Species == "SARU" ~ "SARU_USUAL" 
  ))

EC_comb <- EC %>% 
  rbind(EC_usual)

# Graph EC ----
## ALOC graph USUAL & in-house EC ----
ALOC <- EC_comb %>% 
  dplyr::filter(Species %in% c("ALOC", "ALOC_USUAL"))

a <- ggplot(ALOC,
       aes(x = Position,
           y = EC_microS_cm,
           color = Species)) +
  ylim(0, 70000) +
  labs(title = "ALOC Salinity",
       y = "EC microS/cm") +
  geom_linerange(ymin =  ALOC$EC_microS_cm - ALOC$sd_EC,
                 ymax = ALOC$EC_microS_cm + ALOC$sd_EC) +
  geom_point(size = 3)  +
  facet_wrap( ~Target_Salinity)

## SARU graph USUAL & in-house ----
SARU <- EC_comb %>% 
  dplyr::filter(Species %in% c("SARU", "SARU_USUAL"))

s <- ggplot(SARU,
       aes(x = Position,
           y = EC_microS_cm,
           color = Species)) +
  ylim(0, 70000) +
  labs(title = "SARU Salinity",
       y = "EC microS/cm") +
  geom_linerange(ymin =  SARU$EC_microS_cm - SARU$sd_EC,
                 ymax = SARU$EC_microS_cm + SARU$sd_EC) +
  geom_point(size = 3)  +
  facet_wrap( ~Target_Salinity)

## PHAU graph USUAL & in-house ----
PHAU <- EC_comb %>% 
  dplyr::filter(Species %in% c("PHAU", "PHAU_USUAL"))

p <- ggplot(PHAU,
       aes(x = Position,
           y = EC_microS_cm,
           color = Species)) +
  ylim(0, 70000) +
  labs(title = "PHAU Salinity",
       y = "EC microS/cm") +
  geom_linerange(ymin =  PHAU$EC_microS_cm - PHAU$sd_EC,
                 ymax = PHAU$EC_microS_cm + PHAU$sd_EC) +
  geom_point(size = 3)  +
  facet_wrap( ~Target_Salinity)

# Combine ALOC, PHAU, SARU plots ----

gridExtra::grid.arrange(a, p, s)

# a little hard to see (squashed)
# view alone
a
p
s


# Graph ECe vals ----
## ALOC graph USUAL & in-house ECe ----
  ggplot(ALOC,
            aes(x = Position,
                y = ECe_dS_m,
                color = Species)) +
  labs(title = "ALOC Salinity",
       y = "EC microS/cm") +
  ylim(0, 80) +
  geom_linerange(ymin =  ALOC$ECe_dS_m - ALOC$sd_ECe,
                 ymax = ALOC$ECe_dS_m + ALOC$sd_ECe) +
  geom_point(size = 3)  +
  facet_wrap( ~Target_Salinity)

# SARU graph USUAL & in-house ----
  ggplot(SARU,
            aes(x = Position,
                y = ECe_dS_m,
                color = Species)) +
  labs(title = "SARU Salinity",
       y = "EC microS/cm") +
  ylim(0, 80) +
  geom_linerange(ymin =  SARU$ECe_dS_m - SARU$sd_ECe,
                 ymax = SARU$ECe_dS_m + SARU$sd_ECe) +
  geom_point(size = 3)  +
  facet_wrap( ~Target_Salinity)

# PHAU graph USUAL & in-house ----
  ggplot(PHAU,
            aes(x = Position,
                y = ECe_dS_m,
                color = Species)) +
  labs(title = "PHAU Salinity",
       y = "EC microS/cm") +
  ylim(0, 80) +
  geom_linerange(ymin =  PHAU$ECe_dS_m - PHAU$sd_ECe,
                 ymax = PHAU$ECe_dS_m + PHAU$sd_ECe) +
  geom_point(size = 3)  +
  facet_wrap( ~Target_Salinity)

