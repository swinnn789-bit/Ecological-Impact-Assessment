rm(list=ls())

library(patchwork)
library(ggplot2)
library(cowplot)

# Import data
SouthInsects <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthInsects.csv")
NorthInsects <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthInsects.csv")
SouthLowInsects <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthLowInsects.csv")
SouthMidInsects <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthMidInsects.csv")
SouthHighInsects <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthHighInsects.csv")
NorthMidInsects <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthMidInsects.csv")
NorthHighInsects <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthHighInsects.csv")
NorthPitfalls <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/Northpitfalls.csv")
SouthPitfalls <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/Southpitfalls.csv")
NorthSweepNets <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthSweepNets.csv")
SouthSweepNets <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthSweepNets.csv")
SouthInsects_notick <- subset(SouthInsects, Common_Name !="Tick")

# Colour palette
order_colours <- c(
  "Araneae" = "#E57A7A", 
  "Coleoptera" = "#E89B6B", 
  "Diptera" = "#E8C86F", 
  "Hemiptera"= "#BED470", 
  "Homoptera"= "#8EC98C",
  "Hymenoptera"= "#7FC8BF", 
  "Ixodida"= "#7FA7D8", 
  "Lepidoptera"= "#9382D2",
  "Poduromorpha"= "#B084D2", 
  "Thysanoptera" = "#D284A9"  
)

# Overall North and South Order
# South order
p10 <- ggplot(SouthInsects, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,180, by=25)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South insect orders",
       x = "Insect Order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p10

#South order no ixodida
p11 <- ggplot(SouthInsects_notick, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,35, by=5)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South insect orders (without Ixodida)",
       x = "Insect Order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p11

# North order
p12 <- ggplot(NorthInsects, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North insect orders",
       x = "Insect Order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p12

# combing plots
NorthSouthInsectOrders <- p12 / 
  (p10|p11)
NorthSouthInsectOrders


ggsave("NorthSouthInsectOrders2.png", plot = NorthSouthInsectOrders, width = 12, height = 8, dpi = 300)

# Overall North South Pitfalls
# North order
p14 <- ggplot(NorthPitfalls, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North insect orders in pitfalls",
       x = "Insect Order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p14

# South order
p15 <- ggplot(SouthPitfalls, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,3, by=1)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South insect orders in pitfalls",
       x = "Insect Order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p15

# combine
Pitfalls <- p14 | p15
Pitfalls

ggsave("NorthSouthPitfalls.png", plot = Pitfalls, width = 12, height = 8, dpi = 300)

# Overall North South Sweep Nets
# North order
p16 <- ggplot(NorthSweepNets, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10, by=2)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North insect orders in sweep nets",
       x = "Insect Order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p16

# South order
p17 <- ggplot(SouthSweepNets, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,180, by=25)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South insect orders in sweep nets",
       x = "Insect Order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p17

# combine
SweepNets <- p16 | p17
SweepNets

ggsave("NorthSouthSweepNets.png", plot = SweepNets, width = 12, height = 8, dpi = 300)

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
                     name = "Insect orders") +
  guides(color = guide_legend(
    nrow = 2,
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

ggsave("legend_plot.png", plot = legend_plot, width = 12, height = 8, dpi = 300)