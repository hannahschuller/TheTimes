# Load packages
library(ggplot2)
library(dplyr)
library(tidyr) 
library(lubridate)

# Read in data
spotify <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/Spotify_nationality - Sheet1.csv")

# Recognise date column appropriately
spotify$Week <- as.Date(spotify$Week, format="%d-%m-%Y")
str(spotify)

# Collate nationality data appropriately
new_spotify <- spotify %>%
  group_by(Week) %>%
  summarise(
    Ukrainian = sum(Nationality == "UA"),
    Russian = sum(Nationality == "RU"),
    Other = sum(!(Nationality %in% c("UA", "RU")))
  )

# Convert data from wide to long format
long_spotify <- new_spotify %>%
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

# Calculating monthly average language track counts by weekly data and storing in new df 
monthly_spotify <- new_spotify %>%
  mutate(Month = format(as.Date(Week), "%Y-%m")) %>%  
  group_by(Month) %>%
  summarise(
    Ukrainian = ceiling(mean(Ukrainian, na.rm = TRUE)),  # Round up to the next integer
    Russian = ceiling(mean(Russian, na.rm = TRUE)),     
    Other = ceiling(mean(Other, na.rm = TRUE))         
  )

# Turning this into long format
long_monthly_spotify <- monthly_spotify %>%
  tidyr::pivot_longer(cols = c(Ukrainian, Russian, Other),
                      names_to = "Nationality",
                      values_to = "Count")

# Plot 2: Heatmap (monthly)
ggplot(long_monthly_spotify, aes(x = Month, y = Nationality, fill = Count)) +   
  geom_tile(color = "white") +     
  scale_fill_gradient(     
    low = "white",      
    high = "#80b4e4",      
    limits = c(0, 10),       
    breaks = c(0, 10),       
    labels = c("0", "10"),       
    na.value = "white"     
  ) +   
  labs(     
    title = "Language spoken by Ukraine's Top 10 weekly Spotify artists",   
    x = "Month",     
    y = "Nationality",     
    fill = "Count"   
  ) +   
  theme_minimal() +   
  theme(     
    plot.title = element_text(hjust = 0, size = 16, family = "Arial", color = "black", margin = margin(b = 15)),  
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Arial", color = "black", size = 6),       
    axis.text.y = element_text(family = "Arial", color = "black", size = 8),       
    axis.title.x = element_blank(),       
    axis.title.y = element_blank(),       
    panel.grid.major = element_blank(),      
    panel.grid.minor = element_blank(),      
    legend.position = "top",       
    legend.text = element_text(color = "black", family = "Arial", size = 8),       
    legend.title = element_blank(),      
    legend.key.height = unit(0.3, "cm"),      
    legend.key.width = unit(0.7, "cm"),  
    legend.margin = margin(t = 10) 
  ) +   
  coord_fixed(ratio = 1.2)

# Plot 3: Line plot
long_spotify <- monthly_spotify %>%
  pivot_longer(cols = c(Ukrainian, Russian, Other),
               names_to = "Nationality",
               values_to = "Count")

spotify_plot <- ggplot(long_spotify, aes(x = as.Date(paste0(Month, "-01")), y = Count, group = Nationality)) +
  geom_line(aes(color = Nationality), size = 1) +
  scale_x_date(
    breaks = as.Date(paste0(unique(substr(monthly_spotify$Month, 1, 4)), "-01-01")),
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    limits = c(0, 10),
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
    subtitle = "Language spoken by the top 10 Spotify artists in Ukraine per month",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, family = "Arial"),
    axis.text.y = element_text(family = "Arial"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
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

# Save the plot as an image
ggsave("Spotify_preferences.png", plot = spotify_plot, width = 10, height = 6, dpi = 300)
