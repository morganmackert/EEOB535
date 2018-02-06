#-------------------------------------------------------------------#
#                         ANOVA of Vegetation                       #
#-------------------------------------------------------------------#

#Research Question: How does vegetation cover differ between three landuse categories: corn, grazed reconstructed prairie, and ungrazed reconstructed prairie?

#Objectives:
#Perform an ANOVA on the three landuse categories to determine significant differences between vegetation cover

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Semester 5/Restoration Ecology/Paper/Data")

#Load libraries
library(tidyr)
library(vegan)
library(dplyr)
library(lubridate)
library(ggResidpanel)

#Read in data
Vegetation <- read.csv("Vegetation.csv")
#Date = Date sample was collected (mdy)
#Site = Location of sample collection (corn, grazed prairie, ungrazed prairie)
#Family name = Number of individual specimens collected from each family

#Use lubridate to allow R to recognize the dates
Vegetation$Date <- mdy(Vegetation$Date)

#Determine number of plant species present at each site
VegetationRichness <- Vegetation %>%
  group_by(Site, Date) %>%
  summarise(Veg.Species.Richness = length(unique(Scientific.Name)))

#Calculate average vegetation cover for each quadrat at each site on each date
AverageVA <- Vegetation %>%
  group_by(Date, Site) %>%
  filter(Scientific.Name != "Dead litter") %>%
  summarise(AverageVegCover = sum(Cover)/5)

#Do ANOVA on vegetation richness
VegRichAOV <- aov(Veg.Species.Richness ~ Site, VegetationRichness)
summary(VegRichAOV)

#Check residuals for constant variance
resid_panel(resid(VegRichAOV), fitted(VegRichAOV), bins = 20)
#The residual plot and histogram of residuals look kinda goofy, but we don't have enough data points to tell for sure

#Do ANOVA on vegetation abundance
VegAbundanceAOV <- aov(AverageVegCover ~ Site, AverageVA)
summary(VegAbundanceAOV)

#Check residuals for constant variance
resid_panel(resid(VegAbundanceAOV), fitted(VegAbundanceAOV), bins = 20)
#Again, plots are a little off but hard to tell with so little data.
