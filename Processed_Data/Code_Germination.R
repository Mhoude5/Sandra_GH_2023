library(magrittr)
getwd()
setwd("~/Experiments_R/Sandra_GH_2023/Raw_Data")
getwd()
list.files()
Germ<-read.csv("Germination.csv", header=T)
head(Germ, 20)
sapply(Germ, class)

#Drop columns we don't need
#row, column
Germ <- Germ[ , -c(7, 8, 9)]
head(Germ, 2)

#Change class
#Old classes 
sapply(Germ, class)
#Factor = (species, column, row, salinity)
#Date = date

#Slow way to change factor
Germ$Species <- as.factor(Germ$Species)
class(Germ$Species)

#Fast way to change factor (multiple columns at once)
factor_cols <- c("Species", "Column", "Row", "Salinity")
factor_cols

Germ[factor_cols]<-lapply(Germ[factor_cols], as.factor)
sapply(Germ, class)

#library(lubridate) #one way to call a package is to load it
#im unsure what to do with lubridate
#put '?lubridate' directly into console and the help file will come up
Germ$Date <- lubridate::mdy(Germ$Date)
class(Germ$Date)

which(is.na(Germ$Emergence))
#no NAs 

tail(Germ) #check how many rows we have #here it is 480

#If we had NAs that we wished to remove we could
Germ <- na.omit(Germ)
tail(Germ) #this works but is annoying because it prints your entire DF
which(is.na(Germ)) #This specifies which row NA's (or other functions) occur on



#Lets check values in all of our columns to make sure we have what we expec
min(Germ$Emergence)
max(Germ$Emergence)


levels(Germ$Species) #one way to check factor levels (check for typos)
#levels(Germ[ , 4)]) #Different way to check factor levels

#This is the better way, because its easier to read what you're doing
levels(Germ$Column)
levels(Germ$Row)
levels(Germ$Salinity)

#Let's create a new column
day <- seq(from = 3, to = 30, by = 3)
day
unique(Germ$Date)

#Adding a column for monitoring day(s)
Germ <- Germ %>%
  dplyr::mutate(Day = dplyr::case_when(
                          Date == "2023-01-30" ~ 3,
                          Date == "2023-02-02" ~ 6,
                          Date == "2023-02-05" ~ 9,
                          Date == "2023-02-08" ~ 12,
                          Date == "2023-02-11" ~ 15,
                          Date == "2023-02-14" ~ 18,
                          Date == "2023-02-17" ~ 21,
                          Date == "2023-02-20" ~ 24,
                          Date == "2023-02-23" ~ 27,
                          Date == "2023-02-27" ~ 31
  ))

#We will remove DISP from our analyses and graphs, because it had such low germination
temp <- Germ %>% dplyr::filter(Species == "DISP") 
head(temp)
sum(temp$Emergence) #3 germination total


#480 in Germ minus 120 temp = 360 after removing DISP
#vector of species to keep 
vec <- c("PHAU", "ALOC", "SARU")
Germ <- Germ %>%
  dplyr::filter(Species %in% vec)


#Lets save your dataframe, its still "raw", but its cleaned up 
#And we dont want to do all this cleaning in the future

#Save data object
getwd() #where are we again?
setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")

save(Germ, file="DF_Germination.Rdata")
list.files()









