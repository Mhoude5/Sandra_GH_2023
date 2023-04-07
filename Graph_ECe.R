# In this script I will create a graph showing
# the salinity measurements (mean +/- SE) that Sandra took
# of our 3 salinity treatments for the top and bottom of the soil
# and then add a secondary line with the salinity measurements
# that USUAL took for comparison.

library(magrittr) # for piping
library(ggplot2)

setwd("./Raw_Data")
list.files()
EC <- read.csv("EC_Final.csv", header = T)
EC_usual <- read.csv("USUAL_EC.csv", header = T, skip = 13)

# EC datasheet
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
  dplyr::select(Species, Target_Salinity, Position,
                EC_microS_cm, ECe_dS_m, PSU_e) %>% 
  na.omit() %>% 
  dplyr::group_by(Species, Target_Salinity, Position) %>% 
  dplyr::summarize(SE = std.error(EC_microS_cm),
                   EC_microS_cm = mean(EC_microS_cm))
  

# The EC Usual tables have not had the conversion values applied
# So we can only compare EC values for now (not ECe)
EC_usual <- EC_usual %>% 
  dplyr::mutate(Species = stringr::word(X.1, 2),
                Target_Salinity = dplyr::case_when(
                  startsWith(X.1, "0") ~ "0",
                  startsWith(X.1, "25") ~ "25",
                  startsWith(X.1, "40") ~ "40"),
                Position = dplyr::case_when(
                  endsWith(X.1, "Top") ~ "Top",
                  endsWith(X.1, "Bottom") ~ "Bottom"),
                EC_microS_cm = dS.m * 1000) %>% 
  dplyr::select(Species, Target_Salinity, Position, EC_microS_cm) %>% 
  dplyr::group_by(Species, Target_Salinity, Position) %>% 
  dplyr::summarize(SE = std.error(EC_microS_cm),
                   EC_microS_cm = mean(EC_microS_cm))


# Graph EC first ----
EC_g <- ggplot(EC,
       aes(x = Position,
           y = EC_microS_cm,
           color = Position)) +
  ylim(0, 70000) +
  geom_point(size = 3)  +
  geom_linerange(ymin = EC$EC_microS_cm - EC$SE,
                 ymax = EC$EC_microS_cm + EC$SE) +
  labs(title = "In-house Salinity",
       y = "EC microS/cm") +
  facet_wrap( ~Target_Salinity)

# Graph EC_usual second ----
EC_usg <- ggplot(EC_usual,
       aes(x = Position,
           y = EC_microS_cm,
           color = Position)) +
  ylim(0, 70000) +
  labs(title = "USUAL Salinity",
       y = "EC microS/cm") +
  geom_point(size = 3)  +
  facet_wrap( ~Target_Salinity)

# Show together
gridExtra::grid.arrange(EC_g, EC_usg)


# We need to wait to get samples from USUAL and for Sandra to finish measuring
# to do a 1:1 comparison of their data to ours.
# Here, our in-house salinity is averaged so we would expect some differences.
