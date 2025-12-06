rm(list=ls())

library(patchwork)
library(ggplot2)
library(cowplot)

# Import data
NorthMammal <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/northmammal.csv")
SouthMammal <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/southmammal.csv")


# colour palette
mammal_colours <- c(
  "Badger" = "#e57a7a",
  "Domestic sheep" = "#e8b76d",
  "Red deer" = "#b0d178",
  "Red Squirrel" = "#7fa7d8"
)

# species plot for overall data 
#North mammal species plot
p1 <- ggplot(NorthMammal, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=1)) +
  scale_fill_manual(values = mammal_colours) +
  coord_flip() +
  labs(title = "North mammal species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South mammal species plot
p2 <- ggplot(SouthMammal, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=1)) +
  scale_fill_manual(values = mammal_colours) +
  coord_flip() +
  labs(title = "South mammal species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combining plots
MammalSpecies <- (p1 | p2)
MammalSpecies

ggsave("MammalSpecies.png", plot = MammalSpecies, width = 12, height = 8, dpi = 300)

# mammal pie chart
p3 <- ggplot(NorthMammal, aes(x="", y=Common_Name, fill=Common_Name)) +
  geom_col() +
  scale_fill_manual(values = mammal_colours) +
  coord_polar(theta="y") +
  labs(title="North mammal species") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5))

p4 <- ggplot(SouthMammal, aes(x="", y=Common_Name, fill=Common_Name)) +
  geom_col() +
  scale_fill_manual(values = mammal_colours) +
  coord_polar(theta="y") +
  labs(title="South mammal species") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5))

# combining plots
MammalPie <- (p3 | p4)
MammalPie

ggsave("MammalPie.png", plot = MammalPie, width = 12, height = 8, dpi = 300)

# legend

# Dummy data to create the legend
df <- data.frame(
  x = 1,
  y = 1,
  species = names(mammal_colours)
)

# Create ggplot for the legend
p <- ggplot(df, aes(x, y, color = species)) +
  geom_point() + 
  scale_color_manual(values = mammal_colours,
                     name = "Species") +
  guides(color = guide_legend(
    nrow = 1,
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
legend_plot

ggsave("legend_plotmammal.png", plot = legend_plot, width = 12, height = 8, dpi = 300)