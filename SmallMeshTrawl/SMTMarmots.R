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

Marmots = SMT4 %>%
  filter(bay %in% c(1002, 1003))



# Which taxa comprise the most biomass?
MarImp = as.data.frame(colSums(Marmots[,18:335], na.rm=T)) # sum each taxa's CUE across all years
View(MarImp)
colnames(MarImp)[which(colnames(MarImp)=="colSums(Marmots[, 18:335], na.rm = T)")] = "TotCUE" #rename the column
MarImp[["colSums(Marmots[, 18:335], na.rm = T)"]]
MarImp1 = data.frame(Taxon=colnames(Marmots[, 18:335]), MarImp) %>%
  mutate(percent=(TotCUE/sum(TotCUE))*100) %>%
  arrange(desc(percent))
View(MarImp1)


# Annual means:
Marmots1 = aggregate(Marmots[18:335],
                     by = list(year = Marmots$year),
                     FUN = "mean")
View(Marmots1)
dim(Marmots1) # 28 319



# StErr for annual means:
sterr = function(x) sd(x)/sqrt(length(x))
Marmots1Err = aggregate(Marmots[18:335], 
                        by = list(year = Marmots$year),
                        FUN = "sterr")
View(Marmots1Err)