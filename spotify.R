# Installing necessary packages
install.packages("spotifyr")
install.packages("textcat")
install.packages("cld2")
install.packages("cld3")
library(spotifyr)
library(textcat)
library(cld2)
library(cld3)

# Running a test on week 1
setwd <-("/Users/hannah/Desktop/spotify_ukraine")
w1 <- read.csv("/Users/hannah/Desktop/datawrapper/spotify/regional-ua-weekly-2025-01-02.csv")
w1$week <- as.Date('2025-01-02')
w1 <- w1[, c(1, 2, 3, 4, 9, 10)]

# Linking with Spotify API
Sys.setenv(SPOTIFY_CLIENT_ID = "c30dcecf9dbc4943845c82916416f7c8")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "2e907d131b2c46dab310ac74a4a16099")

access_token <- get_spotify_access_token()

w1$lang <- cld3::detect_language(w1$track_name)


