# Load packages
library(ggplot2)
library(dplyr)
library(tidyr) 
library(lubridate)
library(sf)

book_revenue <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/book_revenue.csv")

# Reshaping data
book_revenue <- book_revenue %>%
  pivot_longer(cols = -X, 
               names_to = "Year", 
               values_to = "Revenue Change") %>%
  mutate(Year = as.integer(sub("X", "", Year))) 

# Plot 1: Bar chart
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


bookshops <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/Bookshops.csv")
ukraine <- st_read("/Users/hannah/Desktop/TheTimes/TheTimes/gadm41_UKR_0.shp")

# Converting dataset into a spatial object
bookshops <- bookshops %>%
  separate(point, into = c("latitude", "longitude"), sep = ", ", convert = TRUE)
bookshops_sf <- st_as_sf(bookshops, coords = c("longitude", "latitude"), crs = 4326)

# Make translated version
bookshops_translated <- bookshops_sf %>%
  mutate(name = recode(name,
                       "Дім книги" = "House of Books",
                       "Мережа книгарень Vivat" = "Vivat",
                       "Мережа книгарень VALLIZA" = "Valliza",
                       "Клуб Сімейного Дозвілля" = "KSD Book Club",
                       "Книгарні-кав'ярні старого лева" = "Old Lion",
                       "Мережа книгарень Книголенд" = "Knigoland Books",
                       "Мережа Книгарень «Є»" = "The Ye Bookstore",
                       "Мережа книгарень Буква" = "Bukva",
                       "Немережеві книгарні" = "Other"
  ))

# Recoding for datawrapper
bookshops_recoded <- bookshops %>%
  mutate(name = recode(name,
                       "Дім книги" = "House of Books",
                       "Мережа книгарень Vivat" = "Vivat",
                       "Мережа книгарень VALLIZA" = "Valliza",
                       "Клуб Сімейного Дозвілля" = "KSD Book Club",
                       "Книгарні-кав'ярні старого лева" = "Old Lion",
                       "Мережа книгарень Книголенд" = "Knigoland Books",
                       "Мережа Книгарень «Є»" = "The Ye Bookstore",
                       "Мережа книгарень Буква" = "Bukva",
                       "Немережеві книгарні" = "Other"
  ))

write.csv(bookshops_recoded, "bookshops_recoded.csv", row.names = FALSE)

# Plot 2: Map of bookshops
ggplot() +
  geom_sf(data = ukraine, fill = NA, color = "black") +
  geom_sf(data = bookshops_translated, aes(color = name), size = 1) +
  scale_color_manual(values = c(
    "House of Books" = "#ae1722",
    "Vivat" = "#7fb1e2",
    "Valliza" = "#f37f2f",
    "KSD Book Club" = "#e0ab26",
    "Old Lion" = "#f6c55e",
    "Knigoland Books" = "#feea82",
    "The Ye Bookstore" = "#254252",
    "Bukva" = "#ed5156",
    "Other" = "#578ab9"
  )) +
  labs(title = "Bookshops across Ukraine", color = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, family = "Arial", color = "black"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(family = "Arial", color = "black"),
    legend.title = element_text(family = "Arial", color = "black"),
    legend.position = "top"
  )
