#####################################################
####  GoA Hydrocarbon Data Cleaning              ####
####   March 2015 ; Script by Rachael Blake      ####
#####################################################

# Set your working directory (this should be changed to download from the repository later)
setwd("C:/Users/rblake/Documents/NCEAS/GoA Portfolio Effects WG/Hydrocarbon Data")

#############################
# Calculate Total Aromatics  
PAH <- read.csv("PAH.csv")  # read in the PAH data file
head(PAH) ; str(PAH)

# Taking means of all chemical compound concentrations to get Total PAHs
PAH$TtlAromatic <- rowSums(PAH[,24:71], na.rm=T)   ; PAH[c(25:50),] # sum the chemicals across rows
library(plyr) 
PAH1 <- arrange(PAH, Sin)  ; head(PAH1)  # arrange by the Sample ID (Sin)
TotalAromat <- PAH1[PAH1$Sin > 0, -c(3,8,15:71)]   # remove rows with Sin < zero and individual compound columns


##########################
# Extract Total Alkanes
Alk <- read.csv("Alkane.csv")  # read in the Alkanes data file
#library(plyr)  # only need to run this if you haven't loaded plyr previously
Alk <- rename(Alk, c("QCBatch"="QCbatch")) # rename QCBatch column to match QCbatch column from PAH table
head(Alk) ; str(Alk)

# extracting Total Alkanes from the spreadsheet
TtlAlk <- Alk[,c(1,2,4:7,9:14,51)]   ; TtlAlk[c(50:70),]
#library(plyr)  # only need to run this if you haven't loaded plyr previously
TtlAlk1 <- arrange(TtlAlk, Sin)
TtlAlkane <- TtlAlk1[TtlAlk1$Sin > 0, ]   # remove rows with Sin < zero 


#########################
# Adding the data columns together
names(TotalAromat) ; names(TtlAlkane)  # make sure the columns are all named the same
#library(plyr)  # only run this if you haven't loaded plyr previously
AromAlk <-  join(TotalAromat, TtlAlkane, by="Sin", type="full")


#############################
# Adding in the Sample information 
SamIDs <- read.csv("sample.csv")
#library(plyr)
Samples1 <- arrange(SamIDs, Sin)  ; head(Samples1) # arranges the rows by Sample ID 
  

######################################
# Joining the two data frames together
TotalAromAlk1 <- join(AromAlk, Samples1, by="Sin", type="full")  # join data frames
TotalAromAlk <- arrange(TotalAromAlk1, Sin)  # sort by Sample ID (Sin)
head(TotalAromAlk) ; tail(TotalAromAlk)


###################################
# Cleaning and filtering the data
# remove the rows where QCERROR is "BIAS" and check the result using unique()
TotalAromAlk2 <- TotalAromAlk[!TotalAromAlk$QCERROR %in% "BIAS",]  ; unique(TotalAromAlk2$QCERROR)
# remove the rows where SampleType is "blank" and check the result using unique()
TotalAromAlk3 <- TotalAromAlk2[!TotalAromAlk2$SampleType %in% c("blank","BLANK","SPIKE"),] 
unique(TotalAromAlk3$SampleType)
# Remove rows with NAs in _BOTH_ the Aromatics and Alkanes columns
TotalAromAlk3a <- TotalAromAlk3[which(!is.na(TotalAromAlk3$TOTALKANES) | 
                                      !is.na(TotalAromAlk3$TtlAromatic)),]
# Remove rows with "BLANK", "QCSED" or "FBLANK" in the matrix column and check the result using unique()
TotalAromAlk3b <- TotalAromAlk3a[!TotalAromAlk3a$matrix %in% c("FBLANK","BLANK","QCSED"),]
unique(TotalAromAlk3b$matrix)
# Replace NAs in the column "Funding" with "EVOSTC" when the value in column "FundingSource" is "EVOSTC"
TotalAromAlk3b$Funding[is.na(TotalAromAlk3b$Funding) & TotalAromAlk3b$FundingSource=="EVOSTC"] <- "EVOSTC"
# Remove "FundingSource" column because it is redundant now
TotalAromAlk4 <- TotalAromAlk3b[,-c(18)]

### Remove NON-EVOSTC Samples (list confirmed by Mark Carls at NOAA Auk Bay Lab)
Non_EVOS <- read.csv("Non-EVOS SINs.csv") # read in the list of non_EVOS Sample ID numbers
head(Non_EVOS) ; nrow(Non_EVOS)

TotalAromAlk5 <- TotalAromAlk4[!TotalAromAlk4$Sin %in% Non_EVOS$Sin,]
head(TotalAromAlk5) ; str(TotalAromAlk5) ; nrow(TotalAromAlk5)
nrow(TotalAromAlk4) - nrow(TotalAromAlk5)  # should equal 440 even though there are 390 rows in
                                           # Non-EVOS because there are duplicates!!!

# Copy entries from "AnalysisType" column to "matrix" column only for rows with NA in matrix column
TotalAromAlk5$matrix <- as.character(TotalAromAlk5$matrix) # have to first make these columns character strings
TotalAromAlk5$AnalysisType <- as.character(TotalAromAlk5$AnalysisType)

TotalAromAlk5$matrix[is.na(TotalAromAlk5$matrix)] <- TotalAromAlk5$AnalysisType[is.na(TotalAromAlk5$matrix)] 


#########################
# create new CSV data file  (this should be changed to upload to repository later)
write.csv(TotalAromAlk5, "C:/Users/rblake/Documents/NCEAS/GoA Portfolio Effects WG/Hydrocarbon Data/Total_Aromatic_Alkanes_PWS.csv", row.names=F)





















