#######################################################################
#####  EcoFOCI Shelikof Strait Ichthyoplankton Cleaning Script    #####
#####        Script by Colette Ward (ward at nceas.ucsb.edu)      #####
#####             and                                             #####
#######################################################################

## load packages
library(dplyr)
library(httr)

#Load data (use the following lines starting with # once the dataset is posted on AOOS)
#URL_Ich <- "url here"
#IchGet <- GET(URL_Ich)
#Ich1 <- content(IchGet, as='text')
#Ich <- read.csv(file=textConnection(Ich1), stringsAsFactors=F, na.strings = c("NA", " ", ""))

# for now, file is in PFx shared folder -> Ichthyoplankton on Google Drive
setwd("~/Google Drive/GoA project/Data/Datasets/Ichthyoplankton/Original files") # set to whatever working directory you're using
Ich <- read.csv('IchTime Series_for binning.csv', header=T, stringsAsFactors=F, na.strings = c("NA", " ", ""))
head(Ich)
str(Ich)

# basic cleaning & processing:
Ich1 = Ich %>%
  filter(GeographicArea == "GOA") %>%
  mutate(gmtDate=as.Date(GMTDate, "%d-%B-%y")) %>%   # split GMTDate into day, month, year
  mutate(Year=strsplit(as.character(gmtDate),split="-") %>% #
           sapply(function(x) x[1])) %>%
  mutate(year=as.numeric(Year)) %>%
  mutate(Month=strsplit(as.character(gmtDate),split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(month=as.numeric(Month)) %>%
  mutate(Day=strsplit(as.character(gmtDate),split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(day=as.numeric(Day)) %>%
  select(-DispVol, -CatchPer1000m3, -IchStageName, -IchCode, -IchStageCode, -NumberCaught, -NumberMeasuredOrStaged, 
         -GMTDate, -gmtDate, -Year, -Month, -Day, -LatitudeHemisphere, -LongitudeHemisphere)
head(Ich1)


unique(sort(Ich1$IchName))

# insert binning script here once categories are finalized

# then use spread () to make each species' CatchPer10m2 into a column