#-------------------------------------------------------------------#
#              Vegetation Abundance ~ Insect Abundance              #
#-------------------------------------------------------------------#

#Research Question: How does the abundance of vegetation within the study area influence insect abundance?

#Objectives:
#Create model(s) to explore relationship between vegetation abundance and insect abundance
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Semester 5/Restoration Ecology/Paper/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(ggplot2)

#Read in data
Insects <- read.csv("Insects.csv")
#Date = Date sample was collected (mdy)
#Site = Location of sample collection (corn, grazed prairie, ungrazed prairie)
#Family name = Number of individual specimens collected from each family
Vegetation <- read.csv("Vegetation.csv")
#Date = Date sample was collected (mdy)
#Site = Location of sample collection (corn, grazed prairie, ungrazed prairie)
#Quadrat = Number of quadrat sampled out of five total
#Scientific name = Specific epithet of vegetation detected in quadrat
#Common name = Common name of vegetation detected in quadrat
#Cover = Percent coverage of vegetation in quadrat
#BG = Percent coverage of bare ground in quadrat

#Use lubridate to allow R to recognize the dates
Insects$Date <- mdy(Insects$Date)
Vegetation$Date <- mdy(Vegetation$Date)

#Determine total insect abundance for each site on each date
Insects$Abundance <- rowSums(Insects[3:19])

#Sum number of specimens collected for each date at each site
InsectAbundance <- Insects %>%
  group_by(Site, Date) %>%
  summarise(Abundance = sum(Abundance)) %>%
  arrange(Date)

#Calculate average vegetation cover at each site on each date
AverageVA <- Vegetation %>%
  select(Site, Date, Cover) %>%
  group_by(Date, Site) %>%
  summarise(AverageVegCover = sum(Cover)/10)

#Join the two datasets together
VAonIA <- full_join(InsectAbundance, AverageVA, by = c("Date", "Site"))

#Model for insect abundance predicted by vegetation cover
VAonIAmodel <- glm(Abundance ~ AverageVegCover + Site + Date,
                   family = poisson,
                   data = VAonIA)
summary(VAonIAmodel)

#Find intercept and slope to plot best fit line on graph
coef(VAonIAmodel)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
VAonIAplot <- ggplot(VAonIA, aes(x = AverageVegCover,
                                 y = Abundance)) +
  geom_point(aes(shape = Site,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Vegetation Cover (%)",
       y = "Arthropod Abundance") +
  ggtitle("Influence of Vegetation Cover on Arthropod Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
VAonIAplot
