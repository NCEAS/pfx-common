library(tidyverse)
commercial <- read_csv("synthesis/data/raw/commercial-region-revenue.csv") %>% 
  filter(!is.na(region_combined)) %>%
  mutate(taxa = factor(taxa, 
    levels = c("Salmon", "Groundfish", "Herring", "Invertebrate")))

diversity <- read_csv("synthesis/data/raw/commercial-region-diversity.csv") %>% 
  filter(!is.na(region_combined))

her <- read_csv("synthesis/data/generated/herring.csv")

# par(mfrow = c(2, 5))
# par(cex = 0.8)

theme_gg <- function(base_size = 11, base_family = "") {
  theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey10"),
      axis.text = element_text(colour = "grey30"),
      axis.title = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30", size = rel(0.6)),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 1),
      legend.key.size = unit(0.8, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA)
    )
}

commercial <- commercial %>% group_by(taxa, region_combined) %>% 
  mutate(revenue_scaled = revenue/max(revenue)) %>% 
  ungroup()

p1 <- ggplot(commercial, aes(year, revenue, fill = taxa)) +
  geom_bar(stat='identity') +
  facet_wrap(~region_combined, nrow = 1) +
  theme_light()

p99 <- ggplot(commercial, aes(year, revenue, fill = taxa)) +
  geom_area(position = 'stack') +
  facet_wrap(~region_combined, nrow = 1) +
  theme_light() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Blues")[2:5]) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
p99

p2 <- ggplot(commercial, aes(year, revenue, colour = taxa)) +
  geom_line() +
  facet_wrap(~region_combined, nrow = 1) +
  theme_light()

s1 <- 1
s2 <- 0.5

c1 <- "#000000"
cs <- rep(c1, 5)
cs <- paste0(cs, "60")
cs[4] <- "grey30"
cs[5] <- paste0(RColorBrewer::brewer.pal(3, "Blues")[3], "60")
cs[1] <- paste0(RColorBrewer::brewer.pal(3, "Reds")[3], "60")

p3 <- ggplot(diversity, aes(year, mean_species_diversity, 
  size = jackknife, color = jackknife)) +
  geom_line() +
  facet_wrap(~region_combined, scales = "fixed", nrow = 1) +
  theme_light() +
  scale_colour_manual(values = cs) +
  scale_size_manual(values = c(s2, s2, s2, s1, s2)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
  
p3

# ggplot(her, aes(year, recruitment_millions)) +
#   geom_line() +
#   facet_wrap(~region_combined, scales = "free_y", nrow = 1) +
#   theme_light()
# 
# ggplot(her, aes(year, biomass_short_tons)) +
#   geom_line() +
#   facet_wrap(~region_combined, scales = "free_y", nrow = 1) +
#   theme_light()

p4 <- filter(her, !is.na(region_combined)) %>%
ggplot(aes(year, recruitment_millions/biomass_short_tons)) +
  geom_line() +
  facet_wrap(~region_combined, scales = "free_y", nrow = 1) +
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
p4

gridExtra::grid.arrange(p99, p3, nrow = 3)
