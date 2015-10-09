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
#IchA <- content(IchGet, as='text')
#Ich <- read.csv(file=textConnection(IchA), stringsAsFactors=F, na.strings = c("NA", " ", ""))

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



# visualize sample timing
IchMonths=Ich1 %>%
  mutate(myp=paste(month,year,Purpose)) %>%
  filter(!duplicated(myp)) %>%
  select(month,year,Purpose)
plot(IchMonths$month ~ IchMonths$year, pch=16) # consider removing March & April samples in 1985


# Binning:
unique(sort(Ich1$IchName)) 
# Janet advises that some taxa should be binned prior to analyses.
# Her advice for binning is in "Final IchName list for binning_Sept 28 2015.xlsx" in the shared Ichthyoplankton folder on Google Drive

# I'm not sure how you want the spreadsheet to be organized for analyses so it's probably best you take over the scripting from here
# I'd been thinking about doing something like this:
# create new empty BinnedTaxa column in Ich1
# then create binning instructions; example for Anoplarchus spp.:
Ano_sp = c("Anoplarchus insignis", "Anoplarchus purpurescens", "Anoplarchus spp.")
# then some kind of if statement, eg: if (IchName %in% Ano_sp)  { then assign "Anoplarchus spp." in the BinnedTaxa column }
# etc for all taxa
# then use dplyr package to: group_by(column i, ... ,  column n, BinnedTaxa) %>% summarise(density=sum(CatchPer10m2)) %>% ungroup


# then use spread () to make each species' CatchPer10m2 into a column