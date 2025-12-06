rm(list=ls())

library(patchwork)
library(ggplot2)
library(cowplot)
library(ggplot2)

# Import data
NorthAquatic <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/AquaticNorth.csv")
SouthAquatic <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/AquaticSouth.csv")
SouthAquaticData <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/AquaticSouthData.csv")
SouthSweepnet <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SweepnettingSouthBog1.csv")
SouthNetdip <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NetdippingSouthBog1.csv")
SouthKicksample <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/KicksampleSouthWest1.csv")
NorthNetdip <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NetdippingNorthEast1.csv")
NorthKicksample <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/KicksampleNorthWest1.csv")

# colour palette
order_colours <- c(
  "Coleoptera" = "#E57A7A", 
  "Diptera" = "#E89B6B", 
  "Ephemeroptera" = "#E8C86F", 
  "Hemiptera"= "#BED470", 
  "Odonata"= "#8EC98C", 
  "Oligochaeta"= "#7FC8BF", 
  "Plecoptera"= "#7FA7D8", 
  "Trichoptera"= "#9382D2" 
)

# order plot for overall data
#North aquatic order plot
p1 <- ggplot(NorthAquatic, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,12, by=2)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North aquatic species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South aquatic order plot
p2 <- ggplot(SouthAquaticData, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,12, by=2)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South aquatic species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

p2
# combining plots
AquaticOrders <- (p1 | p2)
AquaticOrders

ggsave("AquaticOrders2.png", plot = AquaticOrders, width = 12, height = 8, dpi = 300)


# legend

# Dummy data to create the legend
df <- data.frame(
  x = 1,
  y = 1,
  species = names(order_colours)
)

# Create ggplot for the legend
p <- ggplot(df, aes(x, y, color = species)) +
  geom_point() + 
  scale_color_manual(values = order_colours,
                     name = "Species orders") +
  guides(color = guide_legend(
    nrow = 2,
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

ggsave("legend_plotaquaticorder.png", plot = legend_plot, width = 12, height = 8, dpi = 300)

# order plot for each data collection 
#North kick order plot
p3 <- ggplot(NorthKicksample, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North East - kick sample species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#North net order plot
p4 <- ggplot(NorthNetdip, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North West - net dipping species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South kick order plot
p5 <- ggplot(SouthKicksample, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South West - kick sampling species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South net order plot
p6 <- ggplot(SouthNetdip, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South East - net dipping species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South sweep order plot
p7 <- ggplot(SouthSweepnet, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South East - sweep netting species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combining plots
AquaticSpotOrders <- (p4 | p3)/
  (p5 | p6 | p7)
AquaticSpotOrders

ggsave("AquaticSpotOrders2.png", plot = AquaticSpotOrders, width = 14, height = 8, dpi = 300)

