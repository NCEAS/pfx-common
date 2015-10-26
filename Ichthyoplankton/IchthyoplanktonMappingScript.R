#######################################################################
##### EcoFOCI Shelikof Strait Ichthyoplankton Site Mapping Script #####
#####        Script by Colette Ward (ward at nceas.ucsb.edu)      #####
#####             and                                             #####
#######################################################################


# Call output from cleaning script
sourceDir <- function(path, trace=TRUE) {
  for (nm in list.files(path, pattern = "[.][Rr]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm))
    if(trace) cat("\n")
  }
}
sourceDir("Ichthyoplankton") # returns error message but it's not a problem
head(Ich1)

#i like fish, but not fishbots

## load additional packages
library(rworldmap)
library(rworldxtra)
library(rgdal)
library(ggplot2)
library(grid)


# extract unique sampling events
IchSites=Ich1 %>%
  mutate(uni=paste(Latitude,Longitude,year)) %>%
  filter(!duplicated(uni)) %>%
  select(Latitude,Longitude,year,Purpose)
View(IchSites)


# Inititate a blank map
world=getMap('low',projection=NA)
worldB=world[!is.na(world$continent),]
world2=worldB[worldB$continent=='North America' & worldB$LON<0,]
fWorld=fortify(world2)
colMap=c('dimgrey','black')


# Map all stations, all years
ggplot(data=fWorld) +
  geom_map(map=fWorld,aes(x=long,y=lat,map_id=id)) +
  coord_map(xlim = c(-165.5, -147),ylim = c(53.5, 60.5)) + 
  scale_fill_manual(values=colMap) +
  geom_point(data=IchSites,mapping=aes(x=Longitude, y=Latitude,colour=Purpose),size=1,alpha=0.5, shape=20) +
  ggtitle('All Sites, All Years') +
  theme(axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position='right',
        axis.text=element_text(size=8),
        title=element_text(size=12,face="bold"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(colour = guide_legend(override.aes = list(size=6)))


# Sample script for mapping sites in individual years:
Ich81 <- filter(IchSites, year == 1981)

ggplot(data=fWorld) +
  geom_map(map=fWorld,aes(x=long,y=lat,map_id=id)) +
  coord_map(xlim = c(-165.5, -147),ylim = c(53.5, 60.5)) + 
  scale_fill_manual(values=colMap) +
  geom_point(data=Ich81,mapping=aes(x=Longitude, y=Latitude,colour=Purpose),size=2,alpha=0.5, shape=20) +
  ggtitle('All Sites, All Years') +
  theme(axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position='right',
        axis.text=element_text(size=8),
        title=element_text(size=12,face="bold"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(colour = guide_legend(override.aes = list(size=6)))
