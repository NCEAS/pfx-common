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

d <- bind_rows(d, enso) %>%

d <- group_by(d, index) %>%
  arrange(time) %>%
  mutate(value_roll = zoo::rollmean(value, k = 12, fill = NA))

ggplot(d, aes(time, value)) + geom_point(alpha = 0.1) +
  geom_line(aes(y = value_roll), col = "red") +
  facet_wrap(~index, nrow=3)

d %>% select(-Year, -Month, -year, -variable) %>%
  mutate(year = lubridate::year(time)) %>%
  saveRDS("synthesis/data/generated/climate.rds")
