library(tidyverse)
commercial <- read_csv("synthesis/data/raw/commercial-region-revenue.csv") %>% 
  filter(!is.na(region_combined)) %>%
  mutate(taxa = factor(taxa, 
    levels = c("Salmon", "Groundfish", "Herring", "Invertebrate")))
commercial <- commercial %>% group_by(taxa, region_combined) %>% 
  mutate(revenue_scaled = revenue/max(revenue)) %>% 
  ungroup()

diversity <- read_csv("synthesis/data/raw/commercial-region-diversity.csv") %>% 
  filter(!is.na(region_combined))

her <- read_csv("synthesis/data/generated/herring.csv") %>%
  rename(region_orig = region, region = region_combined) %>%
  mutate(bio = recruitment_millions / biomass_short_tons) %>%
  group_by(region) %>%
  # mutate(bio = bio / max(bio, na.rm = T)) %>%
  mutate(bio = biomass_short_tons / max(biomass_short_tons, na.rm = T)) %>%
  mutate(taxa = "Herring") %>%
  filter(!is.na(region)) %>% 
  ungroup()

lu <- tibble(AreaSyn = c("AKP", "KOD", "PWSCI"),
  region = c("Alaska Peninsula", "Kodiak", "PWS/Cook Inlet"))

gf <- read_csv("synthesis/data/raw/AggGroundfishCPUEbyRegion.csv") %>%
  rename(year = Year, bio = Totalbio) %>%
  inner_join(lu) %>%
  group_by(region) %>%
  mutate(bio = bio / max(bio, na.rm = T)) %>% 
  ungroup()

biol <- bind_rows(select(her, region, year, bio, taxa), gf)

# --------------------------------
# plot

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

p99 <- ggplot(commercial, aes(year, revenue, fill = taxa)) +
  geom_area(position = 'stack') +
  facet_wrap(~region_combined, nrow = 1) +
  theme_gg() +
  ylab("Revenue ($ millions)") +
  xlim(1985, 2014) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Blues")[2:5]) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
# p99

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
  theme_gg() +
  ylab("Mean species diversity") +
  xlim(1985, 2014) +
  scale_colour_manual(values = cs) +
  scale_size_manual(values = c(s2, s2, s2, s1, s2))
# p3

p5 <- ggplot(biol, aes(year, bio, colour = taxa)) +
  geom_line() +
  facet_wrap(~region, nrow = 1) +
  theme_gg() +
  ylab("Biomass") +
  xlim(1985, 2014) +
  scale_color_brewer(palette = "Set2")
# p5


pdf("synthesis/figs/biology-and-fisheries.pdf", width = 8, height = 6)
gridExtra::grid.arrange(p99, p3, p5, ncol = 1)
dev.off()
