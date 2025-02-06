# Load packages
library(ggplot2)
library(dplyr)
library(tidyr) 
library(lubridate)
library(reshape2)

# Read in data
spotify <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/Spotify_nationality - Sheet1.csv")

# Recognise date column appropriately
spotify$Week <- as.Date(spotify$Week, format="%d-%m-%Y")
str(spotify)

# Collate nationality data appropriately
spotify_count <- spotify %>%
  group_by(Week) %>%
  summarise(
    Ukrainian = sum(Nationality == "UA"),
    Russian = sum(Nationality == "RU"),
    Other = sum(!Nationality %in% c("UA", "RU"))
  )

# Convert data from wide to long format
long_spotify <- spotify_count %>%
  pivot_longer(cols = c(Ukrainian, Russian, Other), 
               names_to = "Nationality", 
               values_to = "Count")

# Plot 1: Heatmap (Weekly)
ggplot(long_spotify, aes(x = Week, y = Nationality, fill = Count)) +
  geom_tile(color = "white") +  
  scale_fill_gradient(low = "white", high = "#FF00D4") +  
  scale_x_date(date_breaks = "1 month", date_labels = "%B %Y") + 
  labs(
    title = "Heatmap of Nationality Counts Over Time",
    x = "Week",
    y = "Nationality",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text = element_text(size = 10), 
    plot.title = element_text(hjust = 0.5, size = 14)  
  ) +
  coord_fixed(ratio = 30)  

# Plot 2: Line plot
long_spotify <- spotify_count %>%
  pivot_longer(cols = c(Ukrainian, Russian, Other),
               names_to = "Nationality",
               values_to = "Count")

spotify_plot <- ggplot(long_spotify, aes(x = as.Date(Week), y = Count, group = Nationality)) +
  geom_line(aes(color = Nationality), size = 1) +
  scale_x_date(
    date_breaks = "6 months",  
    date_labels = "%b %Y",     
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(long_spotify$Count, na.rm = TRUE), by = 2),
    limits = c(0, max(long_spotify$Count, na.rm = TRUE)), 
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = c(
      "Russian" = "#ae1722",
      "Ukrainian" = "#e0ab26",
      "Other" = "#254252"
    ),
    breaks = c("Russian", "Ukrainian", "Other")
  ) +
  labs(
    title = "Ukrainian Spotify language preferences",
    subtitle = "Language spoken by the top 10 Spotify artists in Ukraine per week",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Arial"),
    axis.text.y = element_text(family = "Arial"),
    plot.title = element_text(hjust = 0, size = 18, family = "Arial", face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(hjust = 0, size = 12, family = "Arial", margin = margin(b = 20)),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(family = "Arial"),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_blank()
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 30, r = 20, b = 30, l = 20))

# Plot the final result
print(spotify_plot)

# Plot 3: Area plot
long_spotify <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/long_spotify.csv")

long_spotify$Week <- as.Date(long_spotify$Week, format = "%d/%m/%Y")

ggplot(long_spotify, aes(x = Week, y = Count, fill = Nationality)) + 
  geom_area(alpha = 1) + 
  labs(
    title = "Ukrainian Spotify user language preferences",
    subtitle = "Language spoken by the top 10 charting Spotify artists in Ukraine each week",
    fill = "Nationality"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, family = "Arial", color = "black", face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0, family = "Arial", color = "black", size = 12, face = "plain"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(family = "Arial", color = "black"),
    axis.ticks = element_blank(),
    axis.text.y = element_text(margin = margin(r = 1)),  
    panel.grid = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(family = "Arial", color = "black")
  ) + 
  scale_fill_manual(
    values = c("Ukrainian" = "#b01424", "Russian" = "#80b4e4", "Other" = "#254252"),
    breaks = c("Ukrainian", "Russian", "Other")
  ) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1))  