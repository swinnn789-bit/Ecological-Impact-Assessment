rm(list=ls())

install.packages("patchwork")
library(patchwork)
library(ggplot2)
install.packages("cowplot")
library(cowplot)

# Import data
AllPlants <- read.csv("C:/Users/Swinn/Downloads/Lochranza Data Plants (Associated Occurrences).csv")
NorthPlant <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthPlants.csv")
SouthPlant <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthPlants.csv")
NorthLowPlant <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthLowPlants.csv")
NorthMidPlant <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthMidPlants.csv")
NorthHighPlant <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthHighPlants.csv")
SouthLowPlant <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthLowPlants.csv")
SouthMidPlant <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthMidPlants.csv")
SouthHighPlant <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthHighPlants.csv")

# Colour palette
species_colours <- c(
  "Bell heather" = "#e57a7a",
  "Big shaggy-moss" = "#e68276",
  "Bog myrtle" = "#e78b72",
  "Bracken" = "#e7956e",
  "Broom fork-moss" = "#e89f6b",
  "Common bent" = "#e8aa6c",
  "Common foxglove" = "#e8b76d",
  "Common haircap" = "#e8c46f",
  "Common heather" = "#e0ca6f",
  "Common nettle" = "#d6cd6f",
  "Creeping buttercup" = "#cad170",
  "European blackberry" = "#bed470",
  "European honeysuckle" = "#b0d178",
  "Gorse" = "#a5ce7f",
  "Hard rush" = "#97cb87",
  "Heath bedstraw" = "#8dc991",
  "Hedge bedstraw" = "#84c89d",
  "Little mouse-ear chickweed" = "#80c8a7",
  "Marsh violet" = "#80c8ba",
  "Mat grass" = "#7fc2c4",
  "Primrose" = "#7fb8cb",
  "Sedge" = "#7fb0d1",
  "Soft rush" = "#7fa7d8",
  "Sphagnum moss" = "#859dd6",
  "St. johns wort" = "#8a92d5",
  "Sweet vernal grass" = "#8f89d3",
  "Tormentil" = "#9682d2",
  "Tufted hair-grass" = "#9e83d2",
  "Wavy hair-grass" = "#a683d2",
  "White clover" = "#ad84d2",
  "Wood sage" = "#b684ca",
  "Wood sorrel" = "#c084bf",
  "Yellow pimpernel" = "#ca84b3",
  "Yorkshire fog" = "#d284a9"
)

# Making plots for each plant transect

#North Low Plant plot
p1 <- ggplot(NorthLowPlant, aes(x = Plant, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "North low plants",
       x = "Plant Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#North Mid Plant plot
p2 <- ggplot(NorthMidPlant, aes(x = Plant, y = Frequency, fill = Plant)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
    labs(title = "North mid plants",
         x = "Plant Species",
         y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none") 

#North High Plant plot
p3 <- ggplot(NorthHighPlant, aes(x = Plant, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "North high plants",
       x = "Plant Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South Low Plant plot
p4 <- ggplot(SouthLowPlant, aes(x = Plant, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "South low plants",
       x = "Plant Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South Mid Plant plot
p5 <- ggplot(SouthMidPlant, aes(x = Plant, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "South mid plants",
       x = "Plant Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South High Plant plot
p6 <- ggplot(SouthHighPlant, aes(x = Plant, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "South high plants",
       x = "Plant Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# Combining plots together
NorthSouthPlantsTransects <- (p1 | p2 | p3) /
  (p4 | p5 | p6)
NorthSouthPlantsTransects

ggsave("NorthSouthPlantsTransects2.png", plot = NorthSouthPlantsTransects, width = 12, height = 8, dpi = 300)

# Making plots for overall North and South

# North plot
p7 <- ggplot(NorthPlant, aes(x = Plant, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,20, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "North plants",
       x = "Plant Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# South plot
p8 <- ggplot(SouthPlant, aes(x = Plant, y = Frequency, fill = Plant)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,26, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "South plants",
       x = "Plant Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# Combining plots together
NorthSouthPlants <- (p7 | p8)
NorthSouthPlants

ggsave("NorthSouthPlants3.png", plot = NorthSouthPlants, width = 12, height = 8, dpi = 300)

# legend

# Dummy data to create the legend
df <- data.frame(
  x = 1,
  y = 1,
  species = names(species_colours)
)

# Create ggplot for the legend
p <- ggplot(df, aes(x, y, color = species)) +
  geom_point() + 
  scale_color_manual(values = species_colours,
  name = "Common names of plant species") +
  guides(color = guide_legend(
    nrow = 7,
    override.aes = list(
      shape = 15,  # solid square
      size = 6     # adjust size
    )
  )) +
  theme(
    legend.position = "right",
    legend.title = element_text()
  )

# Extract and plot only the legend
legend_vertical <- cowplot::get_legend(p)
legend_plot <- cowplot::ggdraw(legend_vertical)

ggsave("legend_plot.png", plot = legend_plot, width = 12, height = 8, dpi = 300)
