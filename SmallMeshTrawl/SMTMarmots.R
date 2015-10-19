######################################################################
#####          Small Mesh Trawl: Marmot Bay & Gully              #####
#####   Script by Colette Ward (ward at nceas.ucsb.edu)          #####
#####                       October 2015                         #####
######################################################################

# call output from Small Mesh Trawl cleaning script
sourceDir <- function(path, trace=TRUE) {
  for (nm in list.files(path, pattern = "[.][Rr]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm))
    if(trace) cat("\n")
  }
}
sourceDir("SmallMeshTrawl")
#View(SMT2)

Marmots = SMT2 %>%
  filter(bay %in% c(1002, 1003))




#Groupings:
# Forage fish Group (Eulachon, Capelin, Sandlance, Sandfish, Herring, Tomcod)
# Shrimp (Pink vs all others)
# Tanner crab
# (Adult) pollock
# Arrowtooth flounder
# Demersal predators Group (Flounder, Sole, Halibut, Gadids)
# Dogfish (and Dogfish + Sleeper)
# Jellies


# see unique taxa info
uniTaxa=SMTbiol %>%
  filter(!duplicated(raceCode)) %>%
  select(-cruise, -haul, -catchKg, -catchNum)
View(uniTaxa)


# Construct annual CUE. 
# Use sum instead of mean because incidences of zero catch are not present when sp catch is organized by rows instead of columns. 

# Pink Shrimp
PinkShrimp = Marmots %>%
  filter(sciName == "Pandalus borealis") %>%
  group_by(year) %>%
  summarise(PinkShrimp=sum(cue)) %>%
  ungroup
View(PinkShrimp)


# Shrimp excluding Pink shrimp
ShrimpNoPink = Marmots %>%
  filter(infraorder == "Caridea") %>%
  filter(sciName != "Pandalus borealis") %>% #exclude Pink shrimp
  group_by(year) %>%
  summarise(ShrimpNoPink=sum(cue)) %>%
  ungroup
View(ShrimpNoPink)


# Tanner crab
Tanner = Marmots %>%
  filter(sciName == "Chionoecetes bairdi") %>%
  group_by(year) %>%
  summarise(Tanner=sum(cue)) %>%
  ungroup
View(Tanner)


# Adult pollock
PollockAd = Marmots %>%
  filter(raceCode == 21740) %>%
  group_by(year) %>%
  summarise(PollockAd=sum(cue)) %>%
  ungroup
View(PollockAd)


# Juvenile pollock
PollockJuv = Marmots %>%
  filter(raceCode == 21741) %>%
  group_by(year) %>%
  summarise(PollockJuv=sum(cue)) %>%
  ungroup
View(PollockJuv)


# Arrowtooth Flounder
Arrowtooth = Marmots %>%
  filter(sciName == "Reinhardtius stomias") %>%
  group_by(year) %>%
  summarise(Arrowtooth=sum(cue)) %>%
  ungroup
View(Arrowtooth)


# Demersal predators (Flounder, Sole, Halibut, Gadids, excluding Pollock)
DemPred = Marmots %>%
  filter(order %in% c("Pleuronectiformes", "Gadiformes")) %>%
  filter(sciName != "Gadus chalcogrammus") %>% # exclude Pollock
  group_by(year) %>%
  summarise(DemPred=sum(cue)) %>%
  ungroup
View(DemPred)


# Dogfish
Dogfish = Marmots %>%
  filter(sciName == "Squalus acanthias") %>%
  group_by(year) %>%
  summarise(Dogfish=sum(cue)) %>%
  ungroup
View(Dogfish)


# Jellies
Jellies = Marmots %>%
  filter(subphylum == "Medusozoa" | sciName %in% c("Ctenophora", "Beroe")) %>%
  group_by(year) %>%
  summarise(Jellies=sum(cue)) %>%
  ungroup
View(Jellies)


# create dataframe with years
MarmotsAnnCue=data.frame('year'=c(1972:2013))

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

