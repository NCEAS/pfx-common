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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load ADFG datasets

# Load ADFG metadata ("adfgSmallmeshHaul.csv")
URL_ASMTh <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ud09TSDdMeWV3TGs"
ASMThGet <- GET(URL_ASMTh)
ASMTh1 <- content(ASMThGet, as='text')
ASMTh <- read.csv(file=textConnection(ASMTh1),stringsAsFactors=F)
head(ASMTh)
str(ASMTh)


# Split date information into month, day, year; clean up column names
ASMTmetadata=ASMTh %>%
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
#View(ASMTmetadata)
str(ASMTmetadata)

# add a vector with agency name
for(i in 1:nrow(ASMTmetadata)) {
  ASMTmetadata$agency[[i]] <- "ADFG"
}
head(ASMTmetadata)
str(ASMTmetadata)


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
  mutate(sciName = gsub("Atheresthes stomias", "Reinhardtius stomias", sciName)) %>% # update to accepted name
  mutate(sciName = gsub("Platichthys stellatus X Pleuronectes qua", "Platichthys stellatus", sciName)) %>%
  mutate(sciName = gsub("Podothecus (or Agonus) acipenserinus", "Podothecus accipenserinus", sciName)) %>% # update to accepted name & spelling
  mutate(sciName = gsub("Delolepis gigantea (or Cryptacanthodes giganteus)", "Cryptacanthodes giganteus", sciName)) %>% # update to accepted name
  mutate(sciName = gsub("\\=", "", sciName)) %>% # remove "="
  mutate(sciName = gsub("Sebastes (Sebastomus) sp.", "Sebastes", sciName)) %>%
  mutate(sciName = gsub("Decapoda", "Caridea", sciName)) %>%
  mutate(sciName = gsub("Eualus macrophthalmus", "Eualus macropthalmus", sciName)) %>% # correct a spelling error
  mutate(sciName = gsub("Crangon communis", "Neocrangon communis", sciName)) %>% # update to accepted name
  mutate(sciName = gsub("crab", "Brachyura", sciName)) %>%
  mutate(sciName = gsub("Cryptonatica Natica aleutica", "Cryptonatica aleutica", sciName)) %>% # update to accepted name
  mutate(sciName = gsub("Basketstarfish use 83020", "Gorgonocephalus eucnemis", sciName)) %>%
  mutate(sciName = gsub(" sp.$", "", sciName))

for(j in 1:nrow(SMTtaxa1)) {
  if(SMTtaxa1$raceCode[j] == 29999) {SMTtaxa1$sciName[j] <- "Acanthopterygii"}
  if(SMTtaxa1$raceCode[j] == 21741) {SMTtaxa1$sciName[j] <- "Gadus chalcogrammus"} # update to accepted name
  if(SMTtaxa1$raceCode[j] == 30150) {SMTtaxa1$sciName[j] <- "Sebastes"} # assign 30150 (dusky rockfishes unid.) to Sebastes
  if(SMTtaxa1$raceCode[j] == 30590) {SMTtaxa1$sciName[j] <- "Sebastes"} # assign 30590 (red rockfish unident.) to Sebastes
}
#SMTtaxa1
str(SMTtaxa1)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Load ADFG biological data ("adfg_smallmesh_catch.csv")
URL_ASMTc <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uekVENlBqaFJLaWM"
ASMTcGet <- GET(URL_ASMTc)
ASMTc1 <- content(ASMTcGet, as='text')
ASMTc <- read.csv(file=textConnection(ASMTc1),stringsAsFactors=F)
head(ASMTc)
str(ASMTc)

# Clean up column names
ASMTcatch = ASMTc %>%
  rename(raceCode = race_code) %>%
  rename(catchnum = num_caught)
head(ASMTcatch)
str(ASMTcatch)

# remove unidentifiable tissue & debris:
ASMTcatch1 = ASMTcatch %>%
  filter(raceCode != 2) %>% # remove "fish larvae unident."
  filter(raceCode != 3) %>% # remove "fish unident."
  filter(raceCode != 401) %>% # remove "skate egg case unident."
  filter(raceCode != 71001) %>% # remove "gastropod eggs"
  filter(raceCode != 99000) %>% # remove "Unknown - from shrimp surveys"
  filter(raceCode != 99990) %>% # remove "invertebrate unident."
  filter(raceCode != 99993) %>% # remove "empty bivalve shells"
  filter(raceCode != 99999) # remove "natural debris"

# Add sciName & comName to biological dataset
ASMTcatch2 = left_join(ASMTcatch1, SMTtaxa1, by="raceCode")
head(ASMTcatch2)


# Some hauls have multiple catchkg and catchnum entries for the same species
# Sum catchkg and catchNum over rows for which cruise, haul, and sciName are identical 
# (as per email conversation with Kally Spalinger (ADFG), July 2015)

ASMTcatchAgg = ASMTcatch2 %>%
  group_by(cruise, haul, raceCode, comName, sciName) %>%
  summarise(catchKg=sum(catchkg), catchNum=sum(catchnum)) %>%
  ungroup
#View(ASMTcatchAgg)
str(ASMTcatchAgg)
# Test:
# For cruise=7759, haul=128, sciName == Hippoglossoides elassodon (raceCode == 10130): catchkg should be 23.587+2.268 = 25.8550.  Yes.


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Add higher order taxonomic information to biological dataset

# Query ITIS for higher order taxonomic information:
#SMTsp <- unique(ASMTcatchAgg$sciName)
#tax.info = tax_name(query = ASMTsp, 
#                    get = c("phylum", "subphylum", "class", "subclass", "infraclass", 
#                            "order", "suborder", "infraorder", "suborder", "infraorder", "family", 
#                            "genus", "species"), 
#                    db = "itis")
#View(tax.info)
#ASMTtaxinfo = tax.info %>%
#  mutate(sciName = query) %>%
#  select(-db, -query)
#View(ASMTtaxinfo)
#setwd()
#write.csv(SMTtaxinfo, file = "SMT_Taxonomic_Info.csv", row.names=F)

# The above code to create tax.info takes 25min to run with sporadic inputs needed
# upload the resulting file from here instead:
URL_SMTtaxinfo1 <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uSERzVENUR0o1emM"
SMTtaxinfo1Get <- GET(URL_SMTtaxinfo1)
SMTtaxinfo1 <- content(SMTtaxinfo1Get, as='text')
SMTtaxinfo <- read.csv(file=textConnection(SMTtaxinfo1),stringsAsFactors=F)
#View(SMTtaxinfo)

# Join taxonomic info to biological data
ASMTbiol = right_join(SMTtaxinfo, ASMTcatchAgg, by = "sciName")
#View(ASMTbiol)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Join metadata & biological dataframes

# create Sample info columns for metadata & biological dataframes (to use when joining dataframes)
ASMTmetadata1 = ASMTmetadata %>%
  unite(Sample, cruise:haul, sep=" ", remove=FALSE)
str(ASMTmetadata1)

ASMTbiol1 = ASMTbiol %>%
  unite(Sample,cruise:haul,sep=" ",remove=FALSE) %>%
  select(-cruise, -haul) # remove these for ease of joining dataframes below
str(ASMTbiol1) 


# join the dataframes (merge metadata onto catch because some hauls are excluded from catch db because of poor gear performance)
ASMT = right_join(ASMTmetadata1, ASMTbiol1, by = "Sample")
#View(ASMT)
str(ASMT)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load NOAA datasets

# Load NOAA metadata ("NOAA_SmallMeshTrawl_haul.csv")
URL_NSMTh <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uX0JhV04xbUdWc1E"
NSMThGet <- GET(URL_NSMTh)
NSMTh1 <- content(NSMThGet, as='text')
NSMTh <- read.csv(file=textConnection(NSMTh1),stringsAsFactors=F)
head(NSMTh)
str(NSMTh)


# Split date information into month, day, year; clean up column names to match ADFG datasets
NSMTmetadata=NSMTh %>%
  mutate(Date=as.Date(START_TIME, "%d-%b-%y")) %>%   # output is reordered as yyyy-mm-dd, but note all years before 1970 become 20xx (eg 2053)
  mutate(year1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[1])) %>%
  mutate(year=as.numeric(year1)) %>% # insert function here to deal with comment in line 221
  mutate(month1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(month=as.numeric(month1)) %>%
  mutate(day1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(day=as.numeric(day1)) %>%
  rename(basin = REGION) %>%
  rename(cruise = CRUISE) %>%
  rename(haul = HAUL) %>%
  rename(haulType = HAUL_TYPE) %>%
  rename(performance = PERFORMANCE) %>%
  rename(duration = DURATION) %>%
  rename(distance = DISTANCE_FISHED) %>%
  rename(netWidth = NET_WIDTH) %>%
  rename(netMeasured = NET_MEASURED) %>%
  rename(netHeight = NET_HEIGHT) %>%
  rename(stratum = STRATUM) %>%
  rename(lat = START_LATITUDE) %>%
  rename(lon = START_LONGITUDE) %>%
  rename(stationID = STATIONID) %>%
  rename(gearDepth = GEAR_DEPTH) %>%
  rename(bottomDepth = BOTTOM_DEPTH) %>%
  rename(surfaceTemp = SURFACE_TEMPERATURE) %>%
  rename(gearTemp = GEAR_TEMPERATURE) %>%
  rename(wireLength = WIRE_LENGTH) %>%
  rename(gear = GEAR) %>%
  rename(surfaceSalinity = SURFACE_SALINITY) %>%
  rename(bottomSalinity = BOTTOM_SALINITY) %>%
  rename(bay = BAYCODE) %>%
  select(-VESSEL, -START_TIME, -Date, -year1, -month1, -day1, -CRUISEJOIN, -HAULJOIN, 
         -END_LATITUDE, -END_LONGITUDE, -BOTTOM_TYPE, -ACCESSORIES, -SUBSAMPLE, 
         -AUDITJOIN, -RECORDCREATEDATE)
#View(SMTmetadata)
head(NSMTmetadata)
str(NSMTmetadata)

# add a vector with agency name
for(i in 1:nrow(NSMTmetadata)) {
  NSMTmetadata$agency[[i]] <- "NOAA"
}
head(NSMTmetadata)
str(NSMTmetadata)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load NOAA biological data ("NOAA_SmallMeshTrawl_catch.csv")
URL_NSMTc <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ucDhjQzZQMEx5WGc"
NSMTcGet <- GET(URL_NSMTc)
NSMTc1 <- content(NSMTcGet, as='text')
NSMTc <- read.csv(file=textConnection(NSMTc1),stringsAsFactors=F)
head(NSMTc)
str(NSMTc)

# Clean up column names
NSMTcatch = NSMTc %>%
  rename(basin = REGION) %>%
  rename(cruise = CRUISE) %>%
  rename(haul = HAUL) %>%
  rename(raceCode = SPECIES_CODE) %>%
  rename(catchkg = WEIGHT) %>%
  rename(catchnum = NUMBER_FISH) %>%
  select(-CRUISEJOIN, -HAULJOIN, -CATCHJOIN, -VESSEL, -SUBSAMPLE_CODE, -VOUCHER, -AUDITJOIN, -GEAR, -RECORDCREATEDATE)
head(NSMTcatch)
str(NSMTcatch)

# select GOA sites; remove unidentifiable tissue & debris:
NSMTcatch1 = NSMTcatch %>%
  filter(basin == "GOA") %>% # remove samples from outside GoA
  filter(raceCode != 2) %>% # remove "fish larvae unident."
  filter(raceCode != 3) %>% # remove "fish unident."
  filter(raceCode != 401) %>% # remove "skate egg case unident."
  filter(raceCode != 71001) %>% # remove "gastropod eggs"
  filter(raceCode != 99000) %>% # remove "Unknown - from shrimp surveys"
  filter(raceCode != 99900) %>%
  filter(raceCode != 99990) %>% # remove "invertebrate unident."
  filter(raceCode != 99993) %>% # remove "empty bivalve shells"
  filter(raceCode != 99994) %>%
  filter(raceCode != 99999) # remove "natural debris"
head(NSMTcatch1)
unique(sort(NSMTcatch1$raceCode))

# Add sciName & comName to biological dataset
NSMTcatch2 = left_join(NSMTcatch1, SMTtaxa1, by="raceCode")
head(NSMTcatch2)


# Some hauls have multiple catchkg and catchnum entries for the same species
# Sum catchkg and catchNum over rows for which cruise, haul, and sciName are identical 
# (as per email conversation with Kally Spalinger (ADFG), July 2015)

NSMTcatchAgg = NSMTcatch2 %>%
  group_by(basin, cruise, haul, raceCode, comName, sciName) %>%
  summarise(catchKg=sum(catchkg), catchNum=sum(catchnum)) %>%
  ungroup
#View(NSMTcatchAgg)
str(NSMTcatchAgg)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Add higher order taxonomic information to biological dataset

NSMTbiol = right_join(SMTtaxinfo, NSMTcatchAgg, by = "sciName")
#View(NSMTbiol)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Join metadata & biological dataframes

# create Sample info columns for metadata & biological dataframes (to use when joining dataframes)
NSMTmetadata1 = NSMTmetadata %>%
  unite(Sample, basin:haul, sep=" ", remove=FALSE)
str(NSMTmetadata1)

NSMTbiol1 = NSMTbiol %>%
  unite(Sample,basin:haul,sep=" ",remove=FALSE) %>%
  select(-basin, -cruise, -haul) # remove these for ease of joining dataframes below
str(NSMTbiol1) 


# join the dataframes
NSMT = right_join(NSMTmetadata1, NSMTbiol1, by = "Sample")
#View(NSMT)
str(NSMT)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Join ADFG & NOAA datasets

SMT = bind_rows(ASMT, NSMT)
View(SMT)
