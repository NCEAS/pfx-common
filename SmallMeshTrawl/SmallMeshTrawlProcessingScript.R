######################################################################
#####          Small Mesh Trawl Processing Script                #####
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
#View(SMT)


# Select hauls to use:
SMT1 = SMT %>%
  filter(month %in% c(9,10)) #%>% # select Sept & Oct samples (most consistent across years)
  #filter(bottom_depth.m.>= ) %>% # select depth range (see notes below)
  #filter(bottom_depth.m.<= ) # select depth range
#View(SMT1)

### Which depths to use?  
# Anderson & Piatt 1996 used depths >55m
# Litzow 2006 used all depths
# See plots of percent catch by depth for each year in CUEbyDepth script
# From this, use samples collected at ...



# Calculate CUE (kg/km2):
SMT2=SMT1 %>%
  mutate(effort = distance*0.0098) # create effort column (area swept (km2); see notes below)
SMTcue = as.data.frame(sapply(SMT2[,17:334], function(x) (x/SMT2$effort))) # create table of CUE (kg/km2) for each species by haul
#View(SMTcue)

SMT3=SMT2 %>%
  select(-c(17:334)) # remove original absolute catchKg data
SMT4=bind_cols(SMT3, SMTcue) # merge CUE with metadata
#View(SMT4)

# Effort
# Standardized to area towed (vs temporal duration) because sampling protocol aims to trawl over a standard distance, and temporal duration can reflect things that affect tow speed
# Sampling protocol is to tow at 3.7 km / hr for a distance of 1.85 km (Jackson 2003, ADFG Small mesh trawl protocol), ie 30 min
# Net opening is 9.8m wide, 4.0m high, 3.1cm (1.2 inch) stretched mesh (Jackson 2003, ADFG Small mesh trawl protocol)
# Therefore area trawled = 1.85km (or whatever distance is recorded in distance column) x 0.0098km 
# 1.85km x 0.0098km = 0.01813 km2