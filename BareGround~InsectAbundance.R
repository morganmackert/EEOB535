#-------------------------------------------------------------------#
#                    Bare Ground ~ Insect Abundance                 #
#-------------------------------------------------------------------#

#Research Question: How does the presence/absence of bare ground within the study area influence insect abundance?

#Objectives:
#Create model(s) to explore relationship between bare ground abundance and insect abundance
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Semester 5/Restoration Ecology/Paper/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(vegan)
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

#Calculate average bare ground coverage at each site on each date.
AverageBG <- Vegetation %>%
  select(Date, Site, Quadrat, BG) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(BG = BG[1]) %>%
  group_by(Date, Site) %>%
  summarise(AverageBG = mean(BG),
            NumberQuadrats = length(BG))

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

#Join datasets together
BGonIA <- full_join(InsectAbundance, AverageBG, by = c("Date", "Site"))
BGonIR <- full_join(InsectFamRich, AverageBG, by = c("Date", "Site"))

#Model for insect abundance predicted by bare ground
BGonIAmodel <- lm(Abundance ~ AverageBG, + Date,
                   data = BGonIA)
summary(BGonIAmodel)

#Find intercept and slope to plot best fit line on graph
coef(BGonIAmodel)

#Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonIAplot <- ggplot(BGonIA, aes(x = AverageBG,
                                 y = Abundance)) +
  geom_point(aes(shape = Site,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Arthropod Abundance") +
  ggtitle("Influence of Bare Ground on Arthropod Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonIAplot

#Model for insect family richness predicted by bare ground
BGonIRmodel <- lm(FamRich ~ AverageBG,
                  data = BGonIR)
summary(BGonIRmodel)

#Percent Bare Ground vs. Insect Family Richness plot using ggplot2
BGonIRplot <- ggplot(BGonIR, aes(x = AverageBG,
                                 y = FamRich)) +
  geom_point(aes(shape = Site,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Number of Arthropod Families") +
  ggtitle("Influence of Bare Ground on Arthropod Family Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonIRplot
