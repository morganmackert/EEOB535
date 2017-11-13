#-------------------------------------------------------------------#
#                   PRINCIPAL COMPONENT ANALYSIS                    #
#-------------------------------------------------------------------#

#Research Question: How much variation can be attributed to each variable within the study?

#Objectives:
#Perform PCA on full data set to determine which variables should be included in the model.

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Semester 5/Restoration Ecology/Paper/Data")

#Load libraries
library(lubridate)
library(tidyr)
library(vegan)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(cluster)

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

#-------------------------------------------------------------------#
#                         Data Manipulation                         #
#-------------------------------------------------------------------#
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
  summarise(InsectFamRich = sum(PresAbs)) %>%
  arrange(Date)

#Group by Site to determine family richness at each location
InsectFamRichbySite <- InsectsLong %>%
  group_by(Site) %>%
  summarise(FamRich = sum(PresAbs))

#Determine total insect abundance for each site on each date
Insects$Abundance <- rowSums(Insects[3:19])

#Sum number of specimens collected for each date at each site
InsectAbundance <- Insects %>%
  group_by(Site, Date) %>%
  summarise(Abundance = sum(Abundance))

#Calculate average vegetation cover at each site on each date
AverageVA <- Vegetation %>%
  select(Site, Date, Cover) %>%
  group_by(Date, Site) %>%
  summarise(AverageVegCover = sum(Cover)/10)

#Determine vegetation species richness for each site on each date
VegRich <- Vegetation %>%
  group_by(Date, Site) %>%
  summarise(VegRich = n_distinct(Scientific.Name))

#Calculate average bare ground coverage at each site on each date.
AverageBG <- Vegetation %>%
  select(Date, Site, Quadrat, BG) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(BG = BG[1]) %>%
  group_by(Date, Site) %>%
  summarise(AverageBG = mean(BG))

#Join all of the data sets together into new data set
Fulldata <- full_join(InsectAbundance, InsectFamRich, by = c("Date", "Site"))
Fulldata <- full_join(Fulldata, AverageVA, by = c("Date", "Site"))
Fulldata <- full_join(Fulldata, VegRich, by = c("Date", "Site"))
Fulldata <- full_join(Fulldata, AverageBG, by = c("Date", "Site"))

#-------------------------------------------------------------------#
#                             Analysis                              #
#-------------------------------------------------------------------#
#Log transform data because the internet said to. Venables, W. N., Brian D. R. Modern applied statistics with S-PLUS. Springer-verlag. (Section 11.1)
log.Fulldata <- log(Fulldata[, 5:7])

#Subset Fulldata to include "Site" in another dataframe
Site <- as.data.frame(Fulldata[(1)])

#Apply PCA (scale. = TRUE is advised)
Fulldata.PCA <- prcomp(log.Fulldata,
                       center = TRUE,
                       scale. = TRUE)

#print() function returns standard deviation of all the PCs and their loadings (rotation)
print(Fulldata.PCA)

#plot() returns a graph of variances associated with PCs
plot(Fulldata.PCA, type = "lines")

#summary() describes importance of PCs
summary(Fulldata.PCA)

#-------------------------------------------------------------------#
#                               Graphs                              #
#-------------------------------------------------------------------#
#B&W
autoplot(Fulldata.PCA)

#Color points by Site
autoplot(Fulldata.PCA,
         data = Site,
         colour = "Site",
         size = 3) +
  theme_bw()

#Color points by Site and include eigenvectors with labels
autoplot(Fulldata.PCA,
         data = Site,
         colour = "Site",
         size = 3,
         loadings = TRUE,
         loadings.label = TRUE) +
  theme_bw()

#Cluster and frame groups by Site
autoplot(Fulldata.PCA,
         data = Site,
         colour = "Site",
         size = 3,
         frame = TRUE) +
  theme_bw()

#Include probability ellipse
autoplot(Fulldata.PCA,
         data = Site,
         colour = "Site",
         size = 3,
         frame = TRUE,
         frame.type = "norm") +
  theme_bw()

