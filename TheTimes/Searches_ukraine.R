# Load packages
library(ggplot2)
library(dplyr)
library(tidyr) 
library(lubridate)

# Read in data
searches <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/Google_search.csv")

# Count the number of appearances each language makes in yearly search rankings
language_count <- searches %>%
  mutate(Language = ifelse(Language %in% c("UA", "RU", "Both"), Language, "Other")) %>%
  group_by(Year, Language) %>%
  summarise(Count = n(), .groups = "drop")

# Save as a csv file
write.csv(language_count, "language_count.csv", row.names = FALSE)

# Read in data again to avoid having to re-run
languages <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/language_count.csv")

# Reorder the dataset so UA, RU, Both, and Other appear in that order for each year
language_count <- language_count %>%
  mutate(Language = factor(Language, levels = c("UA", "RU", "Both", "Other"))) %>%
  arrange(Year, Language)

# Replace values in the Language column
language_count <- language_count %>%
  mutate(Language = recode(Language,
                           "UA" = "Ukrainian",
                           "RU" = "Russian",
                           "Both" = "Shared spelling",
                           "Other" = "Other language"))

# Plot 1: Range plot
ggplot(language_count, aes(
  x = Count, y = factor(Year, levels = rev(unique(Year))), fill = factor(Language, levels = c("Ukrainian", "Russian", "Shared spelling", "Other language"))
)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  labs(
    title = "Language used in Ukraine's top 25 Google searches"
  ) +
  scale_fill_manual(
    values = c("Ukrainian" = "#b01424", "Russian" = "#80b4e4", "Shared spelling" = "#f6c55e", "Other language" = "#254252"),
    labels = c("Ukrainian", "Russian", "Shared spelling", "Other language"),
    name = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 10, family = "Arial", color = "black"),
    axis.text.x = element_text(size = 8, family = "Arial", color = "black"),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold", size = 14, family = "Arial"),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 8, family = "Arial"),
    plot.title.position = "plot"
  )

