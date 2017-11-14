#-------------------------------------------------------------------#
#                            FULL MODEL                             #
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
library(factoextra)

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

#Remove unnecessary columns
Subdata <- Fulldata[-c(1:4)]

#Perform Spearman Rank Correlation test to test variables for correlation
corSubdata <- cor(Subdata, method = "spearman")
#No correlation values are greater than 0.7, thus no correlation between variables and they can all be included in the model

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

#fviz_eig() returns a scree plot of variances associated with PCs
fviz_eig(Fulldata.PCA)

#summary() describes importance of PCs
summary(Fulldata.PCA)
#All variables explain a good amount of variation, include them all in the model and see what happens

#Visualize PCA graphically
autoplot(Fulldata.PCA,
         data = Site,
         colour = "Site",
         size = 3,
         frame = TRUE,
         frame.type = "norm") +
  theme_bw()

fviz_pca_ind(Fulldata.PCA,
             geom.ind = "point",
             pointsize = 3,
             col.ind = Fulldata$Site,
             palette = "aaas",
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             xlab = "PC1 (51.8%)",
             ylab = "PC2 (30.8%)",
             mean.point = FALSE)

#-------------------------------------------------------------------#
#                              Models                               #
#-------------------------------------------------------------------#
#Full abundance model
AbundanceFullModel <- glm(Abundance ~ AverageVegCover + VegRich + AverageBG,
                      data = Fulldata)
summary(AbundanceModel)
#AIC = 137.36

#Reduced models; do they fit the data better?
#Remove AverageBG
AbundanceRedModel <- glm(Abundance ~ AverageVegCover + VegRich,
                         data = Fulldata)
summary(AbundanceRedModel)
#AIC = 137.91

#Remove VegRich
AbundanceRedModel2 <- glm(Abundance ~ AverageVegCover,
                          data = Fulldata)
summary(AbundanceRedModel2)
#AIC = 137.51
#Full model has lowest AIC, so we'll use that one!

#Full richness model
RichnessFullModel <- glm(InsectFamRich ~ AverageVegCover + VegRich + AverageBG,
                     data = Fulldata)
summary(RichnessFullModel)
#AIC = 36.151

#Reduced models; do they fit the data better?
#Remove AverageBG
RichnessRedModel <- glm(InsectFamRich ~ AverageVegCover + VegRich,
                        data= Fulldata)
summary(RichnessRedModel)
#AIC = 35.181

#Remove VegRich
RichnessRedModel2 <- glm(InsectFamRich ~ AverageVegCover,
                         data = Fulldata)
summary(RichnessRedModel2)
#AIC = 46.306
#RichnessRedModel (without AverageBG) has lowest AIC, use that one!