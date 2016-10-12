d <- read.csv("synthesis/data/raw/herring.csv", stringsAsFactors = F)
library(tidyverse)
look_up <- data.frame(region = c("togiak", "kamishak", "pws", "sitka"),
  region_combined = c("Bristol Bay", NA, "PWS/Cook Inlet", "Southeast"), 
  stringsAsFactors = F)
d <- inner_join(d, look_up)

dat <- d$recruitment_millions[d$region == "togiak"]
lag <- d$lag[d$region == "togiak"][[1]]
d$recruitment_millions[d$region == "togiak"] <- c(rep(NA, lag), 
  dat[seq(1, (length(dat) - lag))])

dat <- d$recruitment_millions[d$region == "sitka"]
lag <- d$lag[d$region == "sitka"][[1]]
d$recruitment_millions[d$region == "sitka"] <- c(rep(NA, lag), 
  dat[seq(1, (length(dat) - lag))])

write_csv(d, "synthesis/data/generated/herring.csv")
