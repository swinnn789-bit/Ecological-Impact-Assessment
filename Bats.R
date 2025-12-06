rm(list=ls())

library(patchwork)
library(ggplot2)
library(cowplot)
library(ggplot2)
library(tidyverse)

# Import data
BatMatrix2 <- read.csv("C:/Users/Swinn/Documents/EDI MASTERS/Arran/BatMatrix2.csv")
head(BatMatrix2)

library(tidyr)
library(dplyr)

# edit names
rownames(BatMatrix2) <- c("1" = "North Low","2" ="North Mid","3" = "North High", "4" = "South Low", "5" = "South Mid")
colnames(BatMatrix2) <- c("Brown long-eared bat", "Common pipistrelle", "Lesser noctule", "Nathusius pipistrelle", "Soprano pipistrelle")

# create matrix
bat_long <- as.data.frame(BatMatrix2) %>%
  mutate(Site = rownames(BatMatrix2)) %>% 
  pivot_longer(
    cols = -Site,
    names_to = "Species",
    values_to = "Count"
  )

bat_long$Count <- as.numeric(bat_long$Count)
bat_long$Site <- factor(bat_long$Site,
                        levels = c("North Low", "North Mid", "North High", "South Low", "South Mid"))
bat_long$Presence <- ifelse(bat_long$Count > 0, 1, 0)

BatMatrixPlot <- ggplot(bat_long, aes(x = Site, y = Species, fill = factor(Presence))) +
  geom_tile(color = "black") +   # black borders around each square
  scale_fill_manual(
    values = c("0" = "white", "1" = "red"),  # 0 = absent (white), 1 = present (green)
    labels = c("Absent", "Present"),
    name = "Presence"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Bat Species Presence by Site",
    x = "Site",
    y = "Species"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

BatMatrixPlot

ggsave("BatMatrixPlot.png", plot = BatMatrixPlot, width = 10, height = 8, dpi = 300)
