#-------------------------------------------------------------------#
#                     ANOVA of Insect Composition                   #
#-------------------------------------------------------------------#

#Research Question: How does insect composition differ between three landuse categories: corn, grazed reconstructed prairie, and ungrazed reconstructed prairie?

#Objectives:
#Perform an ANOVA on the three landuse categories to determine significant differences between insect compositions

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
Insects <- read.csv("Insects.csv")
#Date = Date sample was collected (mdy)
#Site = Location of sample collection (corn, grazed prairie, ungrazed prairie)
#Family name = Number of individual specimens collected from each family

#Use lubridate to allow R to recognize the dates
Insects$Date <- mdy(Insects$Date)

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

#Determine total insect abundance for each site on each date
Insects$Abundance <- rowSums(Insects[3:19])

#Sum number of specimens collected for each date at each site
InsectAbundance <- Insects %>%
  group_by(Site, Date) %>%
  summarise(Abundance = sum(Abundance))

#Do ANOVA on family richness
FamRichAOV <- aov(FamRich ~ Site, InsectFamRich)
summary(FamRichAOV)

#Check residuals for constant variance
resid_panel(resid(FamRichAOV), fitted(FamRichAOV), bins = 20)
#The residual plot and histogram of residuals look kinda goofy, but we don't have enough data points to tell for sure

#Do ANOVA on overall abundance
AbundanceAOV <- aov(Abundance ~ Site, InsectAbundance)
summary(AbundanceAOV)

#Check residuals for constant variance
resid_panel(resid(AbundanceAOV), fitted(AbundanceAOV), bins = 20)
#Again, plots are a little off but hard to tell with so little data.
