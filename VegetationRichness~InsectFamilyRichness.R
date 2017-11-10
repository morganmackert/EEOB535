#-------------------------------------------------------------------#
#           Vegetation Richness ~ Insect Family Richness            #
#-------------------------------------------------------------------#

#Research Question: How does the number of plant species within the study area influence arthropod family richness?

#Objectives:
#Create model(s) to explore relationship between vegetation richness and arthropod family richness
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Semester 5/Restoration Ecology/Paper/Data")

#Load libraries
library(lubridate)
library(tidyr)
library(vegan)
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

#Convert from wide to long format
InsectsLong <- gather(Insects, Family, Abundance, Apoidea:Chrysopidae, factor_key = TRUE)

#In order to find family richness for each treatment, need to determine presence/absence of each family within each treatment.
InsectsPresAbs <- decostand(InsectsLong[4], method = "pa")

#Paste presence/absence column into long dataset
InsectsLong$PresAbs <- InsectsPresAbs$Abundance

#Group by Site and Date and sum PresAbs to determine family richness
InsectFamRich <- InsectsLong %>%
  group_by(Site, Date) %>%
  summarise(FamRich = sum(PresAbs)) %>%
  arrange(Date)

#Determine vegetation species richness for each site on each date
VegRich <- Vegetation %>%
  group_by(Date, Site) %>%
  summarise(VegRich = n_distinct(Scientific.Name))

#Join the two datasets together
VRonIR <- full_join(InsectFamRich, VegRich, by = c("Date", "Site"))

#Model for insect family richness predicted by vegetation richness
VRonIRmodel <- glm(FamRich ~ VegRich + Site + Date,
                   family = poisson,
                   data = VRonIR)
summary(VRonIRmodel)

#Find intercept and slope to plot best fit line on graph
coef(VRonIRmodel)

#Plot model to view relationship graphically
VRonIRplot <- ggplot(VRonIR, aes(x = VegRich,
                                 y = FamRich)) +
  geom_point(aes(shape = Site,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Plant Species",
       y = "Number of Arthropod Families") +
  ggtitle("Influence of Vegetation Richness on \nArthropod Family Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
VRonIRplot
