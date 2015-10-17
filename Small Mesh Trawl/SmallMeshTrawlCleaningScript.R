######################################################################
#####            Small Mesh Trawl Cleaning Script                #####
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           #####
#####                       October 2015                         #####
######################################################################

# Load packages
library(httr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


# Load metadata ("adfgSmallmeshHaul.csv")
URL_SMTh <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ud09TSDdMeWV3TGs"
SMThGet <- GET(URL_SMTh)
SMTh1 <- content(SMThGet, as='text')
SMTh <- read.csv(file=textConnection(SMTh1),stringsAsFactors=F)
head(SMTh)
str(SMTh)


# Split date information into month, day, year; clean up column names
SMTmetadata=SMTh %>%
  mutate(Date=as.Date(fish_date, "%m/%d/%y")) %>%   # output is reordered as yyyy-mm-dd
  mutate(year1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[1])) %>%
  mutate(year=as.numeric(year1)) %>%
  mutate(month1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(month=as.numeric(month1)) %>%
  mutate(day1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(day=as.numeric(day1)) %>%
  rename(lat = lat_start) %>%
  rename(lon = lon_start) %>%
  rename(startHr = start_hour) %>%
  rename(duration = duration.hr.) %>%
  rename(distance = distance.km.) %>%
  rename(bottomDepth = bottom_depth.m.) %>%
  rename(gearTemp = gear_temp.c.) %>%
  select(-fish_date, -Date, -year1, -month1, -day1, -lat_end, -lon_end)
View(SMTmetadata)
str(SMTmetadata)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Load biological data ("adfg_smallmesh_catch.csv")
URL_SMTc <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uekVENlBqaFJLaWM"
SMTcGet <- GET(URL_SMTc)
SMTc1 <- content(SMTcGet, as='text')
SMTc <- read.csv(file=textConnection(SMTc1),stringsAsFactors=F)
head(SMTc)
str(SMTc)

# Clean up column names
SMTcatch = SMTc %>%
  rename(raceCode = race_code) %>%
  rename(catchnum = num_caught)
head(SMTcatch)
str(SMTcatch)

# Some hauls have multiple catchkg and catchnum entries for the same species
# Sum catchkg and catchNum over rows for which cruise, haul, and race_code are identical 
# (as per email conversation with Kally Spalinger (ADFG), July 2015)

SMTcatchAgg = SMTcatch %>%
  group_by(cruise, haul, raceCode) %>%
  summarise(catchKg=sum(catchkg), catchNum=sum(catchnum)) %>%
  ungroup
View(SMTcatchAgg)
str(SMTcatchAgg)
# Test:
# For cruise=7759, haul=128, race_code=10130: catchkg should be 23.587+2.268 = 25.8550.  Yes.


# Work with catchKg from here onwards, because catchNum is not always recorded.
# For catchKg, make each species into a column; now data are organized by Haul:
SMTcatchSpread = SMTcatchAgg %>%
  select(-catchNum) %>%
  spread(raceCode,catchKg,fill=0)
View(SMTcatchSpread)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Join metadata & biological dataframes

# create Sample info columns for metadata & biological dataframes (to use when joining dataframes)
SMTmetadata1 = SMTmetadata %>%
  unite(Sample, cruise:haul, sep=" ", remove=FALSE)
str(SMTmetadata1)

SMTcatchSpread1 = SMTcatchSpread %>%
  unite(Sample,cruise:haul,sep=" ",remove=FALSE) %>%
  select(-cruise, -haul) # remove these for ease of joining dataframes below
str(SMTcatchSpread1) 

# join the dataframes
SMT = full_join(SMTmetadata1, SMTcatchSpread1, by = "Sample")
View(SMT)
str(SMT)
