# Load packages
library(ggplot2)
library(dplyr)
library(tidyr) 
library(lubridate)

book_revenue <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/book_revenue.csv")

# Reshaping data
book_revenue <- book_revenue %>%
  pivot_longer(cols = -X, 
               names_to = "Year", 
               values_to = "Revenue Change") %>%
  mutate(Year = as.integer(sub("X", "", Year))) 

# Plotting the data
ggplot(book_revenue, aes(x = factor(Year), y = `Revenue Change`, fill = X)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Book revenue change in Ukraine", 
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.box.spacing = unit(0.5, "cm")) +
  scale_fill_manual(values = c("Audiobooks" = "#254252", 
                               "eBooks" = "#80b4e4", 
                               "Physical books" = "#f6c55e", 
                               "Total" = "#b01424"))

