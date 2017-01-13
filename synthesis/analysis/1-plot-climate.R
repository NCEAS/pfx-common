library(tidyverse)
library(lubridate)
d <- readRDS("synthesis/data/generated/climate-all.rds")

lu <- tibble(index = c("PDO", "NPGO", "ENSO", "SST", "Upwelling", "Freshwater discharge"
), group = c("Climate", "Climate", "Climate", "SST", "Upwelling", "Freshwater discharge"))

d <- inner_join(d, lu)

cols <- c(RColorBrewer::brewer.pal(3, "Set2")[1:3], rep("grey40", 3))
cols <- cols[c(1,4,3,2,5,6)]

theme_gg <- function(base_size = 11, base_family = "") {
  theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey10"),
      axis.text = element_text(colour = "grey30"),
      axis.title = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30"),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 1),
      legend.key.size = unit(0.8, "lines"),
      legend.text = element_text(colour = "grey30"),
      legend.key = element_rect(colour = NA)
    )
}
d %>% #filter(index != "ENSO") %>%
ggplot(aes(time, value, colour = index)) +
  geom_line(aes(y = value_roll)) +
  facet_wrap(~group, ncol=1, scales = "free_y") +
  xlim(ymd("1978-01-01"), ymd("2016-01-01")) +
  theme_light() +
  scale_colour_manual(values = cols)

d2 <- read.csv("synthesis/data/raw/prices_by_speciesYear.csv")
d2 <- group_by(d2, spec) %>% mutate(price_rel = priceDollars / priceDollars[year==1985])

filter(d2, spec %in% c(""))
ggplot(d2, aes(year, price_rel, colour = spec)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1")
