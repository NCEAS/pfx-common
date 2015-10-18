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
#View(SMTmetadata)
str(SMTmetadata)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Load taxonomic codes
URL_SMTt <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uTXpuUWFBbDBrN0E"
SMTtGet <- GET(URL_SMTt)
SMTt1 <- content(SMTtGet, as='text')
SMTt <- read.csv(file=textConnection(SMTt1),stringsAsFactors=F)
head(SMTt)
str(SMTt)

# Clean up column names
SMTtaxa = SMTt %>%
  rename(raceCode = rcode) %>%
  rename(comName = common_name) %>%
  rename(sciName = scientific_name)
head(SMTtaxa)
str(SMTtaxa)


# Clean up sciName
SMTtaxa1 = SMTtaxa %>%
  mutate(sciName = gsub("Class ", "", sciName)) %>%
  mutate(sciName = gsub("Family ", "", sciName)) %>%
  mutate(sciName = gsub("Order ", "", sciName)) %>%
  mutate(sciName = gsub("Phylum ", "", sciName)) %>%
  mutate(sciName = gsub(" unident.", "", sciName)) %>%
  mutate(sciName = gsub("\\(|\\)", "", sciName)) %>% # remove brackets
  mutate(sciName = gsub("Platichthys stellatus X Pleuronectes qua", "HybridPstellatusPqua", sciName)) %>%
  mutate(sciName = gsub("Podothecus (or Agonus) acipenserinus", "Podothecus acipenserinus", sciName)) %>% 
  mutate(sciName = gsub("Delolepis gigantea (or Cryptacanthodes giganteus)", "Delolepis gigantea", sciName)) %>%
  mutate(sciName = gsub("\\=", "", sciName)) %>% # remove "="
  mutate(sciName = gsub("Sebastes (Sebastomus) sp.", "Sebastes", sciName)) %>%
  mutate(sciName = gsub("Cryptonatica Natica aleutica", "Cryptonatica aleutica", sciName)) %>%
  mutate(sciName = gsub("Basketstarfish use 83020", "Gorgonocephalus eucnemis", sciName)) %>%
  mutate(sciName = gsub(" sp.$", "", sciName))

for(j in 1:nrow(SMTtaxa1)) {
  if(SMTtaxa1$raceCode[j] == 29999) {SMTtaxa1$sciName[j] <- "Roundfish"
  }
  if(SMTtaxa1$raceCode[j] == 30150) {SMTtaxa1$sciName[j] <- "Sebastes" # assign 30150 (dusky rockfishes unid.) to Sebastes
  }
  if(SMTtaxa1$raceCode[j] == 30590) {SMTtaxa1$sciName[j] <- "Sebastes" # assign 30590 (red rockfish unident.) to Sebastes
  }
}
SMTtaxa1
str(SMTtaxa1)


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

# remove unidentifiable tissue & debris:
SMTcatch1 = SMTcatch %>%
  filter(raceCode != 2) %>% # remove "fish larvae unident."
  filter(raceCode != 3) %>% # remove "fish unident."
  filter(raceCode != 401) %>% # remove "skate egg case unident."
  filter(raceCode != 71001) %>% # remove "gastropod eggs"
  filter(raceCode != 99000) %>% # remove "Unknown - from shrimp surveys"
  filter(raceCode != 99990) %>% # remove "invertebrate unident."
  filter(raceCode != 99993) %>% # remove "empty bivalve shells"
  filter(raceCode != 99999) # remove "natural debris"

# Replace raceCode with sciName
SMTtaxa2 = SMTtaxa1 %>% 
  select(-comName)
SMTcatch2 = left_join(SMTcatch1, SMTtaxa2, by="raceCode")
SMTcatch3 = SMTcatch2 %>% select(-raceCode)
head(SMTcatch3)


# Some hauls have multiple catchkg and catchnum entries for the same species
# Sum catchkg and catchNum over rows for which cruise, haul, and sciName are identical 
# (as per email conversation with Kally Spalinger (ADFG), July 2015)

SMTcatchAgg = SMTcatch3 %>%
  group_by(cruise, haul, sciName) %>%
  summarise(catchKg=sum(catchkg), catchNum=sum(catchnum)) %>%
  ungroup
#View(SMTcatchAgg)
str(SMTcatchAgg)
# Test:
# For cruise=7759, haul=128, sciName == Hippoglossoides elassodon (raceCode == 10130): catchkg should be 23.587+2.268 = 25.8550.  Yes.



# Work with catchKg from here onwards, because catchNum is not always recorded.
# For catchKg, make each species into a column; now data are organized by Haul:
SMTcatchSpread = SMTcatchAgg %>%
  select(-catchNum) %>%
  spread(sciName,catchKg,fill=0)

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


# join the dataframes (merge metadata onto catch because some hauls are excluded from catch db because of poor gear performance)
SMT = right_join(SMTmetadata1, SMTcatchSpread1, by = "Sample")
#View(SMT)
str(SMT)