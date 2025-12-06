rm(list=ls())

library(patchwork)
library(ggplot2)
library(cowplot)

# Import data
NorthLowBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthLowBirds.csv")
NorthLowOOBBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthLowBirds+oob.csv")
NorthMidBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthMidBirds.csv")
NorthHighBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthHighBirds.csv")
NorthHighOOBBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthHighBirds+oob.csv")
SouthLowBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthLowBirds.csv")
SouthMidBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthMidBirds.csv")
SouthMidOOBBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthMidBirds+oob.csv")
SouthHighBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthHighBirds.csv")
SouthHighOOBBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthHighBirds+oob.csv")
NorthBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthBirds.csv")
SouthBirds <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthBirds.csv")
NorthBirdsnoOOB <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthBirdsnoOOB.csv")
SouthBirdsnoOOB <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthBirdsnoOOB.csv")

# colour palette
bird_colours <- c(
  "Blue tit" = "#e57a7a",
  "Carrion crow" = "#e78b72",
  "Coal tit" = "#e89f6b",
  "Common kestrel" = "#e8b76d",
  "Common raven" = "#e0ca6f",
  "Eurasion wren" = "#cad170",
  "European robin" = "#b0d178",
  "Golden eagle" = "#97cb87",
  "Long tailed tit" = "#84c89d",
  "Meadow pipit" = "#80c8ba",
  "Pied wagtail" = "#7fb8cb",
  "Skylark" = "#7fa7d8",
  "Song thrush" = "#8a92d5",
  "Treecreeper" = "#9682d2",
  "Western jackdaw" = "#a683d2",
  "Wood pigeon" = "#b684ca"
)

# species plot for overall data with OOB
#North bird species plot
p1 <- ggplot(NorthBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "North bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South bird species plot
p2 <- ggplot(SouthBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "South bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combining plots
BirdSpecies <- (p1 | p2)
BirdSpecies

ggsave("BirdSpecies.png", plot = BirdSpecies, width = 12, height = 8, dpi = 300)

# north species plots
#North low species plot
p3 <- ggplot(NorthLowBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "North low bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# North low OOB species plot
p4 <- ggplot(NorthLowOOBBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "North low OOB bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# North mid species plot
p5 <- ggplot(NorthMidBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=1)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "North mid bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# North high species plot
p6 <- ggplot(NorthHighBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "North high bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# North high OOB species plot
p7 <- ggplot(NorthHighOOBBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "North high OOB bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combining north plots
BirdNorthSpecies <- (p3 | p5 | p6)/(p4 | p7)
BirdNorthSpecies

ggsave("BirdNorthSpecies.png", plot = BirdNorthSpecies, width = 12, height = 8, dpi = 300)

# south species plots
# South low species plot
p8 <- ggplot(SouthLowBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "South low bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# South mid species plot
p9 <- ggplot(SouthMidBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=1)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "South mid bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# South mid OOB species plot
p10 <- ggplot(SouthMidOOBBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=1)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "South mid OOB bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# South high species plot
p11 <- ggplot(SouthHighBirds, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=1)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "South high bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# South high OOB species plot
p12<- ggplot(SouthHighOOBBirds,aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=1)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "South high OOB bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combing south plots
BirdSouthSpecies <- (p8 | p9 |  p11 )/ (p10 | p12)
BirdSouthSpecies

ggsave("BirdSouthSpecies.png", plot = BirdSouthSpecies, width = 12, height = 8, dpi = 300)

# species plot for overall data without OOB
#North bird species plot
p13 <- ggplot(NorthBirdsnoOOB, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,18, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "North bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South bird species plot
p14 <- ggplot(SouthBirdsnoOOB, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = bird_colours) +
  coord_flip() +
  labs(title = "South bird species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combining plots
BirdSpeciesnoOOB <- (p13 | p14)
BirdSpeciesnoOOB

ggsave("BirdSpeciesnoOOB2.png", plot = BirdSpeciesnoOOB, width = 12, height = 8, dpi = 300)

# legend

# Dummy data to create the legend
df <- data.frame(
  x = 1,
  y = 1,
  species = names(bird_colours)
)

# Create ggplot for the legend
p <- ggplot(df, aes(x, y, color = species)) +
  geom_point() + 
  scale_color_manual(values = bird_colours,
                     name = "Species") +
  guides(color = guide_legend(
    nrow = 4,
    byrow=TRUE,
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

ggsave("legend_plotbirdspecies.png", plot = legend_plot, width = 12, height = 8, dpi = 300)
