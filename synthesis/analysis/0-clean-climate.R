library(ggplot2)
library(dplyr)

npgo <- read.table("synthesis/data/raw/npgo.txt", comment.char = "#", header = T) %>%
  dplyr::as_data_frame() %>%
  rename(year = YEAR, month = MONTH, npgo = NPGO_index) %>%
  mutate(time = lubridate::ymd(paste(year, month, "01"))) %>%
  rename(value = npgo) %>%
  mutate(index = "NPGO")

pdo <- rpdo::pdo %>%
  mutate(time = lubridate::ymd(paste(Year, Month, "01"))) %>%
  rename(value = PDO) %>%
  mutate(index = "PDO")

d <- bind_rows(pdo, npgo)

enso <- read.table("synthesis/data/raw/enso.txt", comment.char = "#", header = T) %>%
  dplyr::as_data_frame()
names(enso) <- c("year", seq_len(12))
enso <- reshape2::melt(enso, id.vars = "year") %>%
  mutate(time = lubridate::ymd(paste(year, variable, "01"))) %>%
  mutate(index = "ENSO")

d <- bind_rows(d, enso)

d <- group_by(d, index) %>%
  arrange(time) %>%
  mutate(value_roll = zoo::rollmean(value, k = 12, fill = NA))

ggplot(d, aes(time, value)) + geom_point(alpha = 0.1) +
  geom_line(aes(y = value_roll), col = "red") +
  facet_wrap(~index, nrow=3)

d %>% select(-Year, -Month, -year, -variable) %>%
  mutate(year = lubridate::year(time)) %>%
  saveRDS("synthesis/data/generated/climate.rds")

# sst <- readr::read_csv("synthesis/data/raw/salmon_sst.csv") %>%
  # reshape2::melt(id.vars = "X1")

# ggplot(sst, aes(X1, value, colour = variable)) + geom_line()


#######################
# from https://github.com/NCEAS/pfx-covariation-pws/blob/master/data/environmental_data/ERSST_data_for_PWS_salmon_survival.R
library(ncdf4)
library(maps)
library(mapdata)
library(chron)
library(fields)
library(zoo)
library(RCurl)

# Open netCDF file from NOAA website via ftp
# URL_SST <- "ftp://ftp.cdc.noaa.gov/Datasets/noaa.ersst/sst.mnmean.nc"
URL_SST <- "ftp://ftp.cdc.noaa.gov/Datasets/noaa.ersst/sst.mnmean.v4.nc"
SSTGet <- getBinaryURL(URL_SST, ftp.use.epsv = FALSE)
tmpSST <- tempfile(pattern="SSTwB", fileext=".nc")
writeBin(object=SSTGet, con=tmpSST)
nc <- nc_open(tmpSST)

# view dates
d <- dates(ncvar_get(nc, "time"), origin=c(1,15,1800))

# Extract SST data for desired period and locations:
# Pick start and end dates (January 1950-February 2015):
d[c(1153,1934)]
d <- d[1153:1934]  # for later use!

# Extract GOA SST, 54-61 deg. N, 200-226 deg. E:
x <- ncvar_get(nc, "lon", start=101, count=14)
y <- ncvar_get(nc, "lat", start=14, count=5)
x ; y
SST <- ncvar_get(nc, "sst", start=c(101,14,1153), count=c(14,5,length(d)), verbose = T)
dim(SST) # 14 longitudes, 5 latitudes, 782 months


# Change data from a 3-D array to a matrix of monthly data by grid point:
SST <- aperm(SST, 3:1)  # reverse order of dimensions ("transpose" array)

SST <- SST[,5:1,]  # Reverse order of latitudes to be increasing for convenience (in later plotting)
y <- rev(y)  # Also reverse corresponding vector of lattidues
SST <- matrix(SST, nrow=dim(SST)[1], ncol=prod(dim(SST)[2:3]))  # Change to matrix
dim(SST)  # Matrix with column for each grid point, rows for monthly means
# Keep track of corresponding latitudes and longitudes of each column:
lat <- rep(y, length(x))   # Vector of latitudes
lon <- rep(x, each = length(y))   # Vector of longitudes
plot(lon, lat)  # Show grid of longitude x latitude
map('world2Hires',fill=F,xlim=c(100,255), ylim=c(20,66),add=T, lwd=2)
dimnames(SST) <- list(as.character(d), paste("N", lat, "W", lon, sep=""))
head(SST)

# Overall mean temperature at each location:
SST.mean <- colMeans(SST)
z <- t(matrix(SST.mean,length(y)))  # Re-shape to a matrix with latitudes in columns, longitudes in rows
image(x,y,z, col=tim.colors(64))
contour(x, y, z, add=T)  # Mean temperature pattern
map('world2Hires',fill=F,xlim=c(130,250), ylim=c(20,66),add=T, lwd=2)

#restrict to the central GOA region from Mueter et al. 2005
use <- c("N60W210", "N60W212", "N60W214")
SST <- SST[,use]

sst.mean <- rowMeans(SST) #mean SST across the three grid cells

m <- months(d)  # Extracts months from the date vector
y <- as.integer((years(d)))   # Ditto for year

temp <- data.frame(cbind(y,m,sst.mean)) #combine with mean temp
rownames(temp) <- 1:nrow(temp) #clean up
temp$y <- y+1949 #and correct year...

sst <- subset(temp, y < 2015 & y > 1960)
# sst <- tapply(sst$sst.mean, sst$y, mean) #avg SST for each year

sst <- sst %>% rename(year = y, month = m) %>%
  mutate(time = lubridate::ymd(paste(year, month, "01")))

# ----------------

sst <- sst %>% rename(value = sst.mean)
sst <- sst %>%
  arrange(time) %>%
  mutate(value_roll = zoo::rollmean(value, k = 12, fill = NA)) %>%
  # group_by(year) %>%
  # mutate(value_roll = mean(value)) %>%
  mutate(index = "SST")

cl <- readRDS("synthesis/data/generated/climate.rds")
cl <- bind_rows(cl, sst) %>% ungroup() %>%
  select(-month)

saveRDS(cl, "synthesis/data/generated/climate-sst.rds")

# -------
library(lubridate)
up <- readr::read_csv("synthesis/data/raw/PFEL_Upwelling.csv")
up <- up[!grepl("Year", up$Year), ]
up <- up[!is.na(up$Year), ]
up <- up[!is.na(up$Mar), ]
up <- as.data.frame(up)
for (i in 3:ncol(up)) {
  up[,i] <- as.numeric(up[,i])
}
names(up)[4:ncol(up)] <- 1:12
up <- reshape2::melt(up, id.vars = c("Lat", "Lon", "Year"))
up <- mutate(up, time = lubridate::ymd(paste(Year, variable, "01")))
up2 <- group_by(up, time) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  arrange(time) %>%
  mutate(value_roll = zoo::rollmean(value, k = 12, fill = NA)) %>%
  mutate(index = "Upwelling") %>%
  mutate(year = lubridate::year(time))

cl2 <- bind_rows(cl, up2)

fr <- read.csv("synthesis/data/raw/freshwaterDischarge.csv", header = T, comment.char = "#") %>%
  mutate(year = floor(DecimalYear), time = as.Date(lubridate::date_decimal(DecimalYear))) %>%
  rename(value = Total_discharge_Seward) %>%
  select(year, time, value) %>%
  arrange(time) %>%
  mutate(value_roll = zoo::rollmean(value, k = 12, fill = NA)) %>%
  mutate(index = "Freshwater discharge")

cl3 <- bind_rows(cl2, fr)

ggplot(cl3, aes(time, value)) + #geom_point(alpha = 0.1) +
  geom_line(aes(y = value_roll), col = "black") +
  facet_wrap(~index, ncol=1, scales = "free_y") +
  xlim(ymd("1970-01-01"), ymd("2016-01-01")) +
  theme_light()

saveRDS(cl3, "synthesis/data/generated/climate-all.rds")
