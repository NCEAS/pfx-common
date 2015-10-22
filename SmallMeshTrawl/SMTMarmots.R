######################################################################
#####          Small Mesh Trawl: Marmot Bay & Gully              #####
#####   Script by Colette Ward (ward at nceas.ucsb.edu)          #####
#####                       October 2015                         #####
######################################################################

# first run SmallMeshTrawlProcessingScript.R
#View(SMT2)

Marmots <- SMT4 %>%
  filter(bay %in% c(1002, 1003))



# Which taxa comprise the most biomass?
MarImp <- as.data.frame(colSums(Marmots[,18:335], na.rm=T)) # sum each taxa's CUE across all years
View(MarImp)
colnames(MarImp)[which(colnames(MarImp)=="colSums(Marmots[, 18:335], na.rm = T)")] = "TotCUE" #rename the column
MarImp[["colSums(Marmots[, 18:335], na.rm = T)"]]
MarImp1 <- data.frame(Taxon=colnames(Marmots[, 18:335]), MarImp) %>%
  mutate(percent=(TotCUE/sum(TotCUE))*100) %>%
  arrange(desc(percent))
View(MarImp1)


# Annual means:
Marmots1 <- aggregate(Marmots[18:335],
                     by = list(year = Marmots$year),
                     FUN = "mean",
                     na.rm=T)
View(Marmots1)
dim(Marmots1) # 30 319



# StErr for annual means (needs fixing, problem = removing NAs)
#sterr <- sd(x)/sqrt(length(x))

#sterr <- function(x) {
#  for(i in nrow:(Marmots)) {
#    if(Marmots[18:335][i] != NA) {sd(x)/sqrt(length(x))}
#    }
#}
#Marmots1Err <- aggregate(Marmots[18:335], 
#                        by = list(year = Marmots$year),
#                        FUN = "sterr")
#View(Marmots1Err) # error, all NAs


# Demersal predators (Flounder, Sole, Halibut, Gadids, excluding Pollock & Arrowtooth)
DemPred <- Marmots1 %>%
  select("Hippoglossoides elassodon", "Glyptocephalus zachirus", "Limanda aspera", "Platichthys stellatus", 
         "Lepidopsetta polyxystra", "Microstomus pacificus", "Hippoglossus stenolepis", "Parophrys vetulus",
         "Pleuronectes quadrituberculatus", "Isopsetta isolepis", "Psettichthys melanostictus", "Lepidopsetta bilineata",
         "Lyopsetta exilis", "Pleuronectiformes", "Lepidopsetta", "Gadus macrocephalus adult", "Microgadus proximus",
         "Eleginus gracilis", )
head(DemPred)
