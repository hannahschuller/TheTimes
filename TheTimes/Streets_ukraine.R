# Load in libraries
library(rvest)
library(xml2)
library(dplyr)
library(sf)
library(stringr)
library(tidyr)
library(ggplot2)
library(leaflet)

# Defining the URL of the Wikipedia page
url <- "https://en.wikipedia.org/wiki/List_of_Ukrainian_toponyms_that_were_changed_as_part_of_derussification"

# Read the webpage
webpage <- read_html(url)

# Extracting all tables from the URL
tables <- webpage %>% html_nodes("table")

# Convert tables to data frames
table_list <- lapply(tables, function(tbl) {
  table_df <- tbl %>% html_table(fill = TRUE)
  return(table_df)
})

# Save tables as CSV files
for (i in seq_along(table_list)) {
  write.csv(table_list[[i]], file = paste0("table_", i, ".csv"), row.names = FALSE)
}

# Print message when completed
cat("All tables have been scraped and saved as CSV files.\n")

# Link with shapefiles
# In Ukraine you have oblasts which are regions (24 of them), within which are raions (district regions, 136 of them)
country <- st_read("/Users/hannah/Desktop/TheTimes/TheTimes/gadm41_UKR_0.shp")
oblast <- st_read("/Users/hannah/Desktop/TheTimes/TheTimes/gadm41_UKR_1.shp")
raion <- st_read("/Users/hannah/Desktop/TheTimes/TheTimes/gadm41_UKR_2.shp")

cherkasy <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/cherkasy.csv")
chernihiv <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/chernihiv.csv")
dnipropetrovsk <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/dnipropetrovsk.csv")
donetsk <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/donetsk.csv")
kharkiv <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/kharkiv.csv")
kherson <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/kherson.csv")
khmelnytskyi <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/khmelnytskyi.csv")
kirovohrad <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/kirovohrad.csv")
kyiv <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/kyiv.csv")
luhansk <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/luhansk.csv")
lviv <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/lviv.csv")
mykolaiv <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/mykolaiv.csv")
odesa <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/odesa.csv")
poltava <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/poltava.csv")
rivne <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/rivne.csv")
sumy <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/sumy.csv")
ternopil <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/ternopil.csv")
vinnytsia <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/vinnytsia.csv")
volyn <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/volyn.csv")
zakarpattia <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/zakarpattia.csv")
zaporizhzhia <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/zaporizhzhia.csv")
zhytomyr <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/zhytomyr.csv")
raions <- read.csv("/Users/hannah/Desktop/TheTimes/TheTimes/raions.csv")

# Want to produce a count of the number of name changes per raion (district region) 
# Cleaning datasets
raion <- raion %>%
  select(5, 7, 14)

raion <- raion %>%
  slice(-1)

raion <- raion %>%
  rename(Oblast = NAME_1, Raion = NAME_2)

oblast <- oblast %>%
  select(4, 12)

oblast <- oblast %>%
  slice(-1)

oblast <- oblast %>%
  rename(Oblast = NAME_1)

raions <- raions %>%
  select(1:3)

raions <- raions %>%
  rename(
    `Old name` = Old.name,
    `New name` = New.name
  )

raions$Oblast <- gsub(" Oblast", "", raions$Oblast)
raions$`Old name` <- gsub(" Raion", "", raions$`Old name`)
raions$`New name` <- gsub(" Raion", "", raions$`New name`)

# 1. Cherkasy
cherkasy <- cherkasy %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Zvenyhorods'kyi" = "Zvenyhorodka",
                        "Zolotonis'kyi" = "Zolotonosha",
                        "Cherkas'kyi" = "Cherkasy"
  ))

# 2. Chernihiv
chernihiv <- chernihiv %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Nizhyns'ka" = "Nizhyn",
                        "Pryluts'ka" = "Pryluky",
                        "Chernihivs'ka" = "Chernihiv"
  ))

# 3. Dnipropetrovsk
dnipropetrovsk <- dnipropetrovsk %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Dnipropetrovs'ka" = "Dnipro",
                        "Kamians'ko-Dniprovs'kyi" = "Kamianske",
                        "KryvyiRig" = "Kryvyi_Rih",
                        "Nikopol's'ka" = "Nikopol",
                        "Novomoskovs'ka" = "Samar",
                        "Pavlograd" = "Pavlohrad",
                        "Synel'nykivs'ka" = "Synelnykove"
  ))

# 4. Donetsk
donetsk <- donetsk %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Bakhchysarais'kyi" = "Bakhmut",
                        "Volnovas'kyi" = "Volnovakha",
                        "Horlivs'ka" = "Horlivka",
                        "Kramators'ka" = "Kramatorsk",
                        "Mariupol's'ka" = "Mariupol",
                        "Pokrovs'kyi" = "Pokrovsk"
  ))

# 5. kharkiv
kharkiv <- kharkiv %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Bohodukhivs'kyi" = "Bohodukhiv",
                        "Iziums'ka" = "Izium",
                        "Krasnohrads'kyi" = "Krasnohrad",
                        "Kupians'ka" = "Kupiansk",
                        "Lozivs'ka" = "Lozova",
                        "Kharkivs'ka" = "Kharkiv",
                        "Chuhu‹vs'ka" = "Chuhuiv"
  ))

# 6. kherson
kherson <- kherson %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Beryslavs'kyi" = "Beryslav",
                        "Heniches'kyi" = "Henichesk",
                        "Kakhovs'kyi" = "Kakhovka",
                        "Skadovs'kyi" = "Skadovsk",
                        "Khersons'ka" = "Kherson"
  ))

# 7. khmelnytskyi
khmelnytskyi <- khmelnytskyi %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Kamianets'-Podil's'ka" = "Kamianets-Podilskyi",
                        "Khmel'nyts'ka" = "Khmelnytskyi",
                        "Shepetivs'ka" = "Shepetivka"
  ))

# 8. kirovohrad
kirovohrad <- kirovohrad %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Kompani‹vs'kyi" = "Kropyvnytskyi",
                        "Novoukra‹ns'kyi" = "Novoukrainka",
                        "Oleksandriis'kyi" = "Oleksandriia"
  ))

# 9. kiev
kyiv <- kyiv %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Boryspil's'ka" = "Boryspil",
                        "Bilotserkivs'ka" = "Bila Tserkva",
                        "Brovars'ka" = "Brovary",
                        "Vyshhorods'kyi" = "Vyshhorod",
                        "Obukhivs'kyi" = "Obukhiv",
                        "Fastivs'ka" = "Fastiv"
  ))

# 10. luhansk
luhansk <- luhansk %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Alchevs'ka" = "Alchevsk",
                        "Luhans'ka" = "Luhansk",
                        "Svativs'kyi" = "Svatove",
                        "Sieverodonets'ka" = "Sievierodonetsk",
                        "Starobil's'kyi" = "Starobilsk"
  ))


# 11. lviv
lviv <- lviv %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Zolochivs'kyi" = "Zolochiv",
                        "L'vivs'ka" = "Lviv",
                        "Stryis'ka" = "Stryi",
                        "Chervonohrads'ka" = "Chervonohrad"
  ))

# 12. mykolaiv
mykolaiv <- mykolaiv %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Bashtans'kyi" = "Bashtanka",
                        "Voznesens'ka" = "Voznesensk",
                        "Mykola‹vs'ka" = "Mykolaiv",
                        "Pervomais'ka" = "Pervomaisk"
  ))

# 13. odesa
odesa <- odesa %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Berezivs'kyi" = "Berezivka",
                        "Bilhorod-Dnistrovs'ka" = "Bilhorod-Dnistrovskyi Raion",
                        "Bolhrads'kyi" = "Bolhrad",
                        "Izma‹l's'ka" = "Izmail",
                        "Odes'ka" = "Odesa",
                        "Rozdil'nias'kyi" = "Rozdilna"
  ))

# 14. poltava
poltava <- poltava %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Kremenchuts'ka" = "Kremenchuk",
                        "Lubens'ka" = "Lubny",
                        "Myrhorods'kyi" = "Myrhorod",
                        "Poltavs'ka" = "Poltava"
  ))

# 15. rivne
rivne <- rivne %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Dubnivs'ka" = "Dubno"
  ))

# 16. sumy
sumy <- sumy %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Shostkins'ka" = "Shostka",
                        "Konotops'ka" = "Konotop",
                        "Okhtyrs'ka" = "Okhtyrka",
                        "Romens'ka" = "Romny",
                        "Sums'ka" = "Sumy"
  ))

# 17. ternopil
ternopil <- ternopil %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Ternopil's'ka" = "Ternopil"
  ))

# 18. vinnytsia
vinnytsia <- vinnytsia %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Vinnyts'ka" = "Vinnytsia",
                        "Mohyliv-Podil's'ka" = "Mohyliv-Podilskyi",
                        "Tul'chyns'kyi" = "Tulchyn",
                        "Khmil'nyts'kyi" = "Khmilnyk"
  ))

# 19. volyn
volyn <- volyn %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Volodymyr-Volyns'kyi" = "Volodymyr-Volynskyi",
                        "Kovel's'ka" = "Kovel"
  ))

# 20. zakarpattia
zakarpattia <- zakarpattia %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Tiachivs'kyi" = "Tiachiv",
                        "Berehivs'kyi" = "Berehove",
                        "Uzhhorods'ka" = "Uzhhorod"
  ))

# 21. zaporizhzhia
zaporizhzhia <- zaporizhzhia %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Melitopol's'ka" = "Melitopol",
                        "Vasylivs'kyi" = "Vasylivka",
                        "Zaporiz'ka" = "Zaporizhzhia",
                        "Polohivs'kyi" = "Polohy"
  ))

# 22. zhytomyr
zhytomyr <- zhytomyr %>%
  mutate(Raion = str_replace(Raion, " Raion$", ""))

raion <- raion %>%
  mutate(Raion = recode(Raion,
                        "Novohrad-Volyns'ka" = "Novohrad-Volynskyi",
                        "Berdychivs'ka" = "Berdychiv",
                        "Zhytomyrs'ka" = "Zhytomyr",
                        "Korostens'ka" = "Korosten"
  ))

# Joining the now-cleaned oblast lists together 
oblasts <- c("cherkasy", "chernihiv", "dnipropetrovsk", "donetsk", "kharkiv", 
             "kherson", "khmelnytskyi", "kirovohrad", "kyiv", "luhansk", "lviv", 
             "mykolaiv", "odesa", "poltava", "rivne", "sumy", "ternopil", 
             "vinnytsia", "volyn", "zakarpattia", "zhytomyr")

# Producing a dataset containin a count of name changes of raions
# Changing the name in my raion dataset so it'll be compatible with the data I want to merge it with
raion <- raion %>%
  mutate(Raion = case_when(
    Raion == "Volodymyr-Volynskyi" ~ "Volodymyr",
    Raion == "Novohrad-Volynskyi" ~ "Zviahel",
    Raion == "Krasnohrad" ~ "Berestyn",
    Raion == "Sievierodonetsk" ~ "Siverskodonetsk",
    Raion == "Chervonohrad" ~ "Sheptytskyi",
    TRUE ~ Raion 
  ))

# Left-joining my data
name_change_raions <- raion %>%
  left_join(raions %>% select(`New name`, `Old name`), by = c("Raion" = "New name"))

# Saving as a csv file
write.csv(name_change_raions, "/Users/hannah/Desktop/TheTimes/TheTimes/name_change_raions.csv", row.names = FALSE)

# Plotting as an interactive map
# Define color mapping: fill #f6c55e for those with an 'Old name', otherwise white
name_change_raions <- name_change_raions %>%
  mutate(fill_color = ifelse(!is.na(`Old name`), "#cc0a06", "white"),
         popup_text = ifelse(!is.na(`Old name`), 
                             paste("<b>Old name:</b>", `Old name`, "<br><b>New name:</b>", Raion), 
                             ""))  

# Create an interactive leaflet map
leaflet(name_change_raions) %>%
  addPolygons(
    fillColor = ~fill_color,  
    fillOpacity = 0.7,
    color = "black",          
    weight = 1,
    popup = ~popup_text 
  ) %>%
  addLegend(
    colors = c("#cc0a06", "white"),
    labels = c("Has Old Name", "No Old Name"),
    title = "Raion Name Changes",
    position = "bottomright"
  )

# Producing a dataset containing a count of name changes per raion
# Combine all oblast datasets into one
combined_oblasts <- bind_rows(lapply(oblasts, get))

# Count occurrences of each Raion across all oblast datasets
raion_counts <- combined_oblasts %>%
  group_by(Raion) %>%
  summarise(Count = n(), .groups = "drop")

# Join the count data to the original raion dataset
raion_with_counts <- raion %>%
  left_join(raion_counts, by = "Raion")

# Plotting as a map
ggplot(data = raion_with_counts) +
  geom_sf(aes(fill = Count), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") + 
  theme_minimal() + 
  labs(
    title = "Map of Raions with Count Data",
    fill = "Count"
  )

# Saving as a csv file
write.csv(raion_with_counts, "/Users/hannah/Desktop/TheTimes/TheTimes/raion_with_counts.csv", row.names = FALSE)

# Producing a dataset containing the nature of name changes per raion
combined_oblast_data <- bind_rows(lapply(oblasts, get))

# Saving as a csv file
write.csv(combined_oblast_data, "/Users/hannah/Desktop/TheTimes/TheTimes/combined_oblast_data.csv", row.names = FALSE)

# Plot as an interactive map
# Define color palette: Light Grey to Red (#cc0a06), white for NA
raion_with_counts <- raion_with_counts %>%
  left_join(combined_oblast_data %>% select(Raion, Type, `Old.Name`, `New.Name`, Notes), by = "Raion")

# Define color palette: Light Grey to Red (#cc0a06), white for NA
color_palette <- colorNumeric(
  palette = c("lightgrey", "#cc0a06"),  # Color gradient
  domain = raion_with_counts$Count,     # Use 'Count' for color scaling
  na.color = "white"                    # NA values appear in white
)

# Create popups based on 'Raion' match with 'combined_oblast_data'
raion_with_counts <- raion_with_counts %>%
  mutate(popup_text = ifelse(!is.na(Count),
                             paste0(
                               "<b>Toponym change: </b>", Raion, "<br>",
                               "<b>Type: </b>", coalesce(Type, "Unknown"), "<br>",
                               "<b>Old name: </b>", coalesce(`Old.Name`, "N/A"), "<br>",
                               "<b>New name: </b>", coalesce(`New.Name`, "N/A"), "<br>",
                               "<b>Reason: </b>", coalesce(Notes, "No reason provided")
                             ), ""))  # Empty string for NA values

# Create an interactive leaflet map WITHOUT a basemap and with a white background
leaflet(raion_with_counts, options = leafletOptions(zoomControl = TRUE)) %>%
  addPolygons(
    fillColor = ~color_palette(Count),  # Fill based on Count values
    fillOpacity = 0.8,
    color = "black",        # Thin black borders
    weight = 0.5,           # Reduce border thickness
    popup = ~popup_text     # Uses joined data for popups
  ) %>%
  addLegend(
    pal = color_palette,
    values = raion_with_counts$Count,
    title = "Count",
    position = "bottomright"
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      map.getContainer().style.background = 'white'; // Set background to white
    }
  ")