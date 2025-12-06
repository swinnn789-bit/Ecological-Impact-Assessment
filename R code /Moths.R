rm(list=ls())

library(patchwork)
library(ggplot2)
library(cowplot)

# Import data
NorthMoth <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthLowMoth.csv")
SouthMoth <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthLowMoth.csv")
NorthAMoth <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthAMoth.csv")
NorthEMoth <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthEMoth.csv")
NorthJMoth <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/NorthJMoth.csv")
SouthAMoth <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthAMoth.csv")
SouthEMoth <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthEMoth.csv")
SouthJMoth <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthJMoth.csv")

# colour palette
order_colours <- c(
  "Araneae" = "#E57A7A",
  "Diptera" = "#E89B6B", 
  "Hemiptera" = "#E8C86F",
  "Hymenoptera"= "#a5ce7f", 
  "Ixodida" = "#7fc2c4",
  "Julida" = "#7fa6d6",
  "Lepidoptera" = "#9e83d2", 
  "Trichoptera"= "#795EB1" 
)

species_colours <- c(
  "Autumnal rustic" = "#e57a7a",
  "Black rustic" = "#e68276",
  "Caddis fly" = "#e78b72",
  "Chestnut moth" = "#e7956e",
  "Cinnamon sedge caddisfly" = "#e89f6b",
  "Common marbled carpet" = "#e8aa6c",
  "Convolvulus hawk moth" = "#e8b76d",
  "Crane fly" = "#e8c46f",
  "Dark winged fungus gnats" = "#e0ca6f",
  "European crane fly" = "#d6cd6f",
  "Fly" = "#cad170",
  "Garden dart" = "#bed470",
  "Green carpet moth" = "#b0d178",
  "Heath rustic" = "#a5ce7f",
  "Large pale masoner" = "#97cb87",
  "Large yellow underwing" = "#8dc991",
  "Larval geometrid" = "#84c89d",
  "Leaf curling sac spider" = "#80c8a7",
  "Marsh crane fly" = "#80c8ba",
  "Money spider" = "#7fc2c4",
  "Mosquito" = "#7fb8cb",
  "Mottled rustic" = "#7fb0d1",
  "Neglected rustic" = "#7fa7d8",
  "Northern rustic" = "#7fa6d6",
  "Parasitic wasps" = "#859dd6",
  "Pink barred sallow moth" = "#8a92d5",
  "Scentless plant bugs" = "#8f89d3",
  "Sheep tick" = "#9682d2",
  "Small wainscot" = "#9e83d2",
  "Square spot rustic" = "#a683d2",
  "The anomalous" = "#ad84d2",
  "White legged snake millipede" = "#b684ca"
)

# order plot for each trap
#North A order plot
p1 <- ggplot(NorthAMoth, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=2)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North moth trap A species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#North E order plot
p2 <- ggplot(NorthEMoth, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,14, by=2)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North moth trap E species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#North J order plot
p3 <- ggplot(NorthJMoth, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,14, by=2)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North moth trap J species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South A order plot
p4 <- ggplot(SouthAMoth, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,22, by=5)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South moth trap A species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South E order plot
p5 <- ggplot(SouthEMoth, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,5, by=1)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South moth trap E species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South J order plot
p6 <- ggplot(SouthJMoth, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,5, by=1)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South moth trap J species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combining plots
MothtrapsOrders <- (p1 | p2 | p3) /
  (p4 | p5 | p6)
MothtrapsOrders

ggsave("MothtrapOrders2.png", plot = MothtrapsOrders, width = 12, height = 8, dpi = 300)

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

ggsave("legend_plotmothorder.png", plot = legend_plot, width = 12, height = 8, dpi = 300)

# species plot for each trap
#North A species plot
p7 <- ggplot(NorthAMoth, aes(x = Common_Name, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "North moth trap A species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#North E species plot
p8 <- ggplot(NorthEMoth, aes(x = Common_Name, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "North moth trap E species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#North J species plot
p9 <- ggplot(NorthJMoth, aes(x = Common_Name, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "North moth trap J species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South A species plot
SouthAMothedit <-read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthAMothedit.csv")

p10 <- ggplot(SouthAMothedit, aes(x = Common_Name, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,16, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "South moth trap A species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South E species plot
p11 <- ggplot(SouthEMoth, aes(x = Common_Name, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "South moth trap E species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South J species plot
p12 <- ggplot(SouthJMoth, aes(x = Common_Name, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,8, by=1)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "South moth trap J species",
       x = "Species",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combining plots
MothtrapsSpecies <- (p7 | p8 | p9) /
  (p10 | p11 | p12)
MothtrapsSpecies

ggsave("MothtrapSpecies.png", plot = MothtrapsSpecies, width = 12, height = 8, dpi = 300)

# order plot for each hill
#North order plot
p13 <- ggplot(NorthMoth, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,28, by=5)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "North moth traps species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p13

#South order plot
p14 <- ggplot(SouthMoth, aes(x = Order, y = Frequency, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,28, by=5)) +
  scale_fill_manual(values = order_colours) +
  coord_flip() +
  labs(title = "South moth traps species order",
       x = "Species order",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 
p14

# combining plots
MothtrapsOrdersHill <- (p13 | p14) 
MothtrapsOrdersHill

ggsave("MothtrapOrdersHill2.png", plot = MothtrapsOrdersHill, width = 12, height = 8, dpi = 300)

# Taxon plot for each hill
#North taxon plot
p15 <- ggplot(NorthMoth, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,12, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "North moth traps",
       x = "Taxon",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

#South taxon plot
SouthMothEdit <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/SouthLowMothEdit.csv")
p16 <- ggplot(SouthMothEdit, aes(x = Common_Name, y = Frequency, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,28, by=2)) +
  scale_fill_manual(values = species_colours) +
  coord_flip() +
  labs(title = "South moth traps",
       x = "Taxon",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") 

# combining plots
MothtrapsSpeciesHill <- (p15 | p16) 
MothtrapsSpeciesHill

ggsave("MothtrapSpeciesHill3.png", plot = MothtrapsSpeciesHill, width = 12, height = 8, dpi = 300)

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
                     name = "Taxon") +
  guides(color = guide_legend(
    nrow = 7,
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

ggsave("legend_plotmothspecies.png", plot = legend_plot, width = 12, height = 8, dpi = 300)

# pie chart for moths order
summary(NorthMoth)
summary(SouthMoth)

#North Moth pie
p17 <- ggplot(NorthMoth, aes(x="", y=Order, fill=Order)) +
  geom_col() +
  scale_fill_manual(values = order_colours) +
  coord_polar(theta="y") +
  labs(title="North site moth order") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)) 

#South Moth pie
p18 <- ggplot(SouthMoth, aes(x="", y=Order, fill=Order)) +
  geom_col() +
  scale_fill_manual(values = order_colours) +
  coord_polar(theta="y") +
  labs(title="South site moth order") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5))

# combining plots
MothPieOrder <- (p17 | p18)
MothPieOrder

ggsave("mothpieorder.png", plot = MothPieOrder, width = 12, height = 8, dpi = 300)
