rm(list=ls())

library(patchwork)
library(ggplot2)
library(cowplot)
library(tidyr)

# make dataframe for species
df <- data.frame(
  Taxon = c("Bat", "Bird", "Mammal", "Moth", "Plants"),
  North_species_richness = c(2, 11, 4, 23, 25),
  South_species_richness = c(5, 9, 3, 19, 18)
)

df_long <- pivot_longer(
  df,
  cols = c(North_species_richness, South_species_richness),
  names_to = "Site",
  values_to = "Richness"
)

df_long$Site <- ifelse(df_long$Site == "North_species_richness", "North", "South")

# make species richness graphs
p1 <- ggplot(df_long, aes(x = Taxon, y = Richness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Species Richness by Taxon and Site",
    x = "Taxonomic group",
    y = "Species Richness",
    fill = "Site"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
p1

ggsave("Speciesrichness.png", plot = p1, width = 12, height = 8, dpi = 300)

# make dataframe for orders
df <- data.frame(
  Taxon = c("Aquatic invertebrate", "Terrestrial invertebrate", "Moth"),
  North_species_richness = c(6, 8, 6),
  South_species_richness = c(6, 10, 6)
)

df_long <- pivot_longer(
  df,
  cols = c(North_species_richness, South_species_richness),
  names_to = "Site",
  values_to = "Richness"
)

df_long$Site <- ifelse(df_long$Site == "North_species_richness", "North", "South")

# make order richness graph
p2 <- ggplot(df_long, aes(x = Taxon, y = Richness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 14) +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  labs(
    title = "Order Richness by Invertebrate type and Site",
    x = "Invertebrate type",
    y = "Order Richness",
    fill = "Site"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
p2

ggsave("Orderrichness.png", plot = p2, width = 12, height = 8, dpi = 300)
