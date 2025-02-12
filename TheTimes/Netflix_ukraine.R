# Load relevant packages
library(ggplot2)
library(dplyr)
library(treemap)

# Loading in data
netflix <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/netflix.csv")

# Recognising it as date format
netflix$Week <- as.Date(netflix$Week, format = "%d-%m-%Y")

# Dropping unnecessary column
netflix <- netflix[, -4]

# Producing a pre-war dataset
pre_war <- netflix[netflix$Week <= as.Date("2022-02-22"), 

# Producing count of each genre appearance
genre_counts <- pre_war %>%
  count(Genre)

# Create proportional box chart
treemap(genre_counts, 
        index = "Genre", 
        vSize = "n", 
        title = "Proportional Box Chart of Genres in 2021",
        palette = "Set3",
        border.col = "white")

# Doing the same for post-war
post_war <- netflix[netflix$Week > as.Date("2022-02-22"), ]

genre_counts_post <- post_war %>%
  count(Genre)

treemap(genre_counts_post, 
        index = "Genre", 
        vSize = "n", 
        title = "Proportional Box Chart of Genres after invasion",
        palette = "Set3",
        border.col = "white")

# Saving these as a csv file
write.csv(genre_counts, "/Users/hannah/Desktop/TheTimes/TheTimes/genre_counts_pre_war.csv", row.names = FALSE)

write.csv(genre_counts_post, "/Users/hannah/Desktop/TheTimes/TheTimes/genre_counts_post_war.csv", row.names = FALSE)
