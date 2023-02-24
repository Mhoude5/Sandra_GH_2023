library(magrittr) #for piping
library(ggplot2)
getwd()
setwd("~/Experiments_R/Sandra_GH_2023")

#SETUP ----
#First, we need to load our previously process .RData object (our dataframe)
setwd("~/Experiments_R/Sandra_GH_2023/Processed_Data")
list.files()
load("DF_Germination.Rdata")
head(Germ, 10)

  
#Should we use mutate or summarize?
example_mutate <- Germ %>%
  dplyr::group_by(Species, Salinity, Date) %>%
  dplyr::mutate(Xbar_Germ = mean(Emergence))

#how many rows do we want?
#4 species, how many monitoring dates?, 3 salinity levels = how many rows we want
unique(Germ$Date)
#We have 8 unique monitoring dates
4* 8* 3
#=96 total rows 

#how many rows in mutate?
#check DF manually
#384



#mutate wasnt right.... lets use summarize
head(Germ, 6)
example_summarize <- Germ %>% 
 dplyr::group_by(Species, Salinity, Date) %>%
  dplyr::summarize(Xbar_Germ = mean(Emergence)) %>%
  dplyr::ungroup()
nrow(example_summarize)#this is 96, which is what we expected!


Germ <- Germ %>% 
  dplyr::group_by(Species, Salinity, Date) %>%
  dplyr::summarize(Xbar_Germ = mean(Emergence)) %>%
  dplyr::ungroup()
nrow(Germ)
  
  
#GRAPH ----
#We want to create 3 graphs, one for each salinity level, that will have 
#All 4 species on it. 
#X axis = Monitoring date
#Y axis = Germination count


ggplot(aes(x = Germ$Date, y = Germ$Xbar_Germ, color = Germ$Species) +
       geom_point() +
         facet_wrap(Germ$Salinity)#This will create a different graph for each salinity level
       #You could facet wrap by SPECIES and that might be interesting
)






#Example
ggplot(aes(x=Date, y=Percent_Cover, color=SPP),
       show.legend=T) + 
  geom_line()+
  geom_point(size=2) + 
  facet_wrap(~factor(Plot, level=c('C', 'G', 'F', 'F&G', 'FtG', 'FtGp', 'NtGp'))) +
  scale_color_manual(values = c("Alkali Bulrush" = "burlywood4",
                                "Saltgrass" ="dodgerblue3",
                                "Nuttall's Alkaligrass"="darkorchid3",
                                "Phragmites" = "red")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  theme(plot.margin=unit(c(0.5,1,0.5,1), 'cm')) +
  theme(axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 20)) +  
  labs(title = "Driving Species through the Growing Season", 
       x = "Month", 
       y = "Percent Cover")





