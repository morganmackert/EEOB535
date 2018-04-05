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
  summarise(VegRich = length(unique(Scientific.Name)))

#Determine which species are present in each site
VegSpp <- Vegetation %>%
  group_by(Date, Site) %>%
  filter(Common.Name != "Dead litter") %>%
  count(Common.Name)

#Classify each vegetation species as native or introduced
VegSpp <- VegSpp %>%
  mutate(Classification = case_when(
    Common.Name == "Giant foxtail" | Common.Name == "Yellow foxtail" | Common.Name == "Creeping Charlie" | Common.Name == "Dandelion" | Common.Name == "Corn" | Common.Name == "Green foxtail" | Common.Name == "Brome grass" | Common.Name == "Canada thistle" | Common.Name == "Velvet leaf" | Common.Name == "Musk thistle" | Common.Name == "Queen Anne's lace" ~ "Introduced",
    Common.Name == "Big bluestem" | Common.Name == "Black eyed Susan" | Common.Name == "Culver's root" | Common.Name == "Elm" | Common.Name == "Gray-headed coneflower" | Common.Name == "Indian grass" | Common.Name == "Little bluestem" | Common.Name == "Marestail" | Common.Name == "Pale gentian" | Common.Name == "Partridge pea" | Common.Name == "Pennsylvania smart weed" | Common.Name == "Rattlesnake master" | Common.Name == "Red mulberry" | Common.Name == "Round headed bush clover" | Common.Name == "Showy tick trefoil" | Common.Name == "Side oats grama" | Common.Name == "Sugar maple" | Common.Name == "Switchgrass" | Common.Name == "Waterhemp" | Common.Name == "Wild grape" | Common.Name == "Wild white indigo" ~ "Native"
  ))

#Determine number of native/introduced species for each site
NatIntro <- VegSpp %>%
  group_by(Date, Site) %>%
  count(Classification)

#Determine number of native/introduced species observations for each site
NatIntroObs <- Vegetation %>%
  group_by(Site, Date) %>%
  filter(Native.Introduced != "") %>%
  count(Native.Introduced)

#Join InsectFamRich and VegRich datasets together
VRonIR <- full_join(InsectFamRich, VegRich, by = c("Date", "Site"))

#Join NatIntro and InsectFamRich together
NIonIR <- full_join(NatIntroObs, InsectFamRich, by = c("Date", "Site"))

#Model for insect family richness predicted by vegetation richness
VRonIRmodel <- glm(FamRich ~ VegRich + Date,
                   data = VRonIR)
summary(VRonIRmodel)
#AIC is 45.627

#Add site to the model
VRonIRmodelwithSite <- glm(FamRich ~ VegRich + Site + VegRich*Site,
                  data = VRonIR)
summary(VRonIRmodelwithSite)
#AIC is 50.375
#Previous model fits better than when including Site

#Plot model to view relationship graphically
VRonIRplot <- ggplot(VRonIR, aes(x = VegRich,
                                 y = FamRich,
                                 colour = Site)) +
  geom_point(aes(shape = Site),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
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

#Graph native/introduced species observations by site
NatIntroObsplot <- ggplot(NatIntroObs, aes(x = Site,
                                           y = n,
                                           fill = Native.Introduced)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_smooth(method = "glm",
              se = FALSE,
              size = 0.5) +
  theme_bw() +
  labs(x = "Site",
       y = "Number of Observations") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  labs(fill = "Classification")
NatIntroObsplot

#Graph number of native/introduced species by site
NatIntroplot <- ggplot(NatIntro, aes(x = Site,
                                     y = nn,
                                     fill = Classification)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_smooth(method = "glm",
              se = FALSE,
              size = 0.5) +
  theme_bw() +
  labs(x = "Site",
       y = "Number of Species") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
NatIntroplot
