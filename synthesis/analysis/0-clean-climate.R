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

ggplot(d, aes(time, value)) + geom_point(alpha = 0.5) +
  geom_smooth(span = 0.1, se = FALSE, color = "red", method = "loess") +
  facet_wrap(~index, nrow=2)
