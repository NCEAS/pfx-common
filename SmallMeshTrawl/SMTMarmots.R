######################################################################
#####          Small Mesh Trawl: Marmot Bay & Gully              #####
#####   Script by Colette Ward (ward at nceas.ucsb.edu)          #####
#####                       October 2015                         #####
######################################################################

# first run SmallMeshTrawlProcessingScript.R
#View(SMT2)

Marmots <- SMT2 %>%
  filter(bay %in% c(1002, 1003))


#Groupings:
# Forage fish Group (Eulachon, Capelin, Sandlance, Sandfish, Herring, Tomcod)
# Shrimp (Pink vs all others)
# Tanner crab
# (Adult) pollock
# Arrowtooth flounder
# Demersal predators Group (Flounder, Sole, Halibut, Gadids; excluding Pollock & Arrowtooth)
# Dogfish (and Dogfish + Sleeper)
# Jellies


# see unique taxa info
#uniTaxa <- ASMTbiol %>%
#  filter(!duplicated(raceCode)) %>%
#  select(-cruise, -haul, -catchKg, -catchNum)
#View(uniTaxa)


# Construct annual CUE (means of non-zero catches)
# Forage Fish (Eulachon, Capelin, Sandlance, Sandfish, Herring, Tomcod) add smelts? (family == "Osmeridae")
ForageFish <- Marmots %>%
  filter(sciName %in% c("Thaleichthys pacificus", "Mallotus villosus", "Ammodytes hexapterus", 
                        "Trichodon trichodon", "Clupea pallasii", "Microgadus proximus")) %>%
  group_by(year, Sample) %>% # group by year & haul
  summarise(FFByHaul=sum(cue)) %>% # sum cue for all group members collected in each haul
  ungroup %>%
  group_by(year) %>% # group by year
  summarise(ForageFish=mean(FFByHaul, na.rm=T)) %>% # calculate mean annual CUEs across hauls
  ungroup
#View(ForageFish)


# Pink Shrimp
PinkShrimp <- Marmots %>%
  filter(sciName == "Pandalus borealis") %>%
  group_by(year) %>%
  summarise(PinkShrimp=mean(cue)) %>%
  ungroup
#View(PinkShrimp)


# Shrimp excluding Pink shrimp
ShrimpNoPink <- Marmots %>%
  filter(infraorder == "Caridea") %>%
  filter(sciName != "Pandalus borealis") %>% #exclude Pink shrimp
  group_by(year, Sample) %>%
  summarise(ShrByHaul=sum(cue)) %>% 
  ungroup %>%
  group_by(year) %>%
  summarise(ShrimpNoPink=mean(ShrByHaul, na.rm=T)) %>%
  ungroup
#View(ShrimpNoPink)


# Tanner crab
Tanner <- Marmots %>%
  filter(sciName == "Chionoecetes bairdi") %>%
  group_by(year) %>%
  summarise(Tanner=mean(cue)) %>%
  ungroup
#View(Tanner)


# Adult pollock
PollockAd <- Marmots %>%
  filter(raceCode == 21740) %>%
  group_by(year) %>%
  summarise(PollockAd=mean(cue)) %>%
  ungroup
#View(PollockAd)


# Juvenile pollock
PollockJuv <- Marmots %>%
  filter(raceCode == 21741) %>%
  group_by(year) %>%
  summarise(PollockJuv=mean(cue)) %>%
  ungroup
#View(PollockJuv)


# Arrowtooth Flounder
Arrowtooth <- Marmots %>%
  filter(sciName == "Reinhardtius stomias") %>%
  group_by(year) %>%
  summarise(Arrowtooth=mean(cue)) %>%
  ungroup
#View(Arrowtooth)


# Demersal predators (Flounder, Sole, Halibut, Gadids, excluding Pollock, Arrowtooth, Tomcod)
DemPred <- Marmots %>%
  filter(order %in% c("Pleuronectiformes", "Gadiformes")) %>%
  filter(sciName != "Gadus chalcogrammus") %>% # exclude Pollock
  filter(sciName != "Reinhardtius stomias") %>% # exclude Arrowtooth
  filter(sciName != "Microgadus proximus") %>% # exclude Tomcod
  group_by(year, Sample) %>%
  summarise(DPByHaul=sum(cue)) %>%
  ungroup %>%
  group_by(year) %>% # group by year
  summarise(DemPred=mean(DPByHaul, na.rm=T)) %>% # calculate mean annual CUEs across hauls
  ungroup
#View(DemPred)


# Dogfish
Dogfish <- Marmots %>%
  filter(sciName == "Squalus acanthias") %>%
  group_by(year) %>%
  summarise(Dogfish=mean(cue)) %>%
  ungroup
#View(Dogfish)


# Jellies
Jellies <- Marmots %>%
  filter(subphylum == "Medusozoa" | sciName %in% c("Ctenophora", "Beroe")) %>%
  group_by(year) %>%
  summarise(Jellies=sum(cue)) %>%
  ungroup
#View(Jellies)


# create dataframe with years
MarmotsAnnCue <- data.frame('year'=c(1976:2013))

# Merge in the taxon-specific biomass data
MarmotsAnnCue <- merge(MarmotsAnnCue,ForageFish,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,PinkShrimp,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,ShrimpNoPink,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,Tanner,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,PollockAd,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,PollockJuv,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,Arrowtooth,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,DemPred,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,Dogfish,all.x=T)
MarmotsAnnCue <- merge(MarmotsAnnCue,Jellies,all.x=T)

View(MarmotsAnnCue)

