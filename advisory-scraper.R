# Travel Advisory Scraper
# Alexandra Harris


# Set up ----
library(tidyverse)
library(rvest)
library(googlesheets4)
library(ggmap)
library(leaflet)

# Import geocoded data
geocode_roads <- read.csv("geocode-roads.csv")


# All regions scraper ----

# Read the webpage
webpage <- read_html("https://www.thruway.ny.gov/travelers/map/text/twytextwta.cgi?region=ALLREGIONS")

# Parse all p elements
parsed_webpage <- html_nodes(webpage, "p")

# Extract the info
info <- html_text(parsed_webpage)

# Separate the info
info_list <- strsplit(info, "\n")

# Filter
info_list <- info_list[grep("^Road Status:", info_list)]

# Parse all h4 elements
parsed_webpage2 <- html_nodes(webpage, "h4")

# Extract the info
info2 <- html_text(parsed_webpage2)

# Separate the info
info_list2 <- strsplit(info2, "\n")

# Filter
info_list2 <- info_list2[grep("^I-\\d|^Exits", info_list2)]

# Extract road_status
road_status <- character()
for (i in 1:length(info_list)) {
  road_status[i] <- gsub("Pavement\\s*", "", str_extract(info_list[i], "Road Status:\\s*(.*?)(?:Pavement\\s*)"))
}
road_status <- gsub("Road Status:\\s*", "", road_status)
print(road_status)

# Extract pavement_conditions
pavement_conditions <- character()
for (i in 1:length(info_list)) {
  pavement_conditions[i] <- gsub("Pavement\\s*", "", str_extract(info_list[i], "Pavement Conditions:\\s*(.*?)(?:Weather\\s*)"))
}
pavement_conditions <- gsub("Conditions:\\s*", "", pavement_conditions)
pavement_conditions <- gsub("Weather\\s*", "", pavement_conditions)
print(pavement_conditions)

# Extract weather_conditions
weather_conditions <- character()
for (i in 1:length(info_list)) {
  weather_conditions[i] <- gsub("Road\\s*Status:\\s*.*Pavement\\s*", "", str_extract(info_list[i], "Weather Conditions:\\s*(.*)"))
}
weather_conditions <- gsub("Weather Conditions:\\s*", "", weather_conditions)
print(weather_conditions)

# Create data frame
all <- data.frame(road_name = character(length(info_list2)), road_exits = character(length(info_list2)))

# Extract road_name and road_exits
for (i in 1:length(info_list2)) {
  if(i %% 2 == 1) {
    all$road_name[i] <- gsub("(.*)I-(.*?) NYS Thruway.*","\\1I-\\2 NYS Thruway", info_list2[i])
    all$road_exits[i] <- gsub("(.*)Exits (.*) to (.*)","\\1\\2 to \\3", info_list2[i+1])
  }
}

all <- all[!(all$road_name == "" & all$road_exits == ""),]

all$road_status <- road_status
all$pavement_conditions <- pavement_conditions
all$weather_conditions <- weather_conditions

# Concatenate road details
all <- all %>% 
  mutate(road_details = paste0(road_name, " - ", road_exits)) %>% 
  select(road_name, road_exits, road_details, road_status, pavement_conditions, weather_conditions)

# Add geocoded data
all <- left_join(all, geocode_roads, by = "road_details") %>% 
  select(road_name.x, road_exits.x, road_details, exit_one, lat_exit_one, lon_exit_one, exit_two, lat_exit_two, lon_exit_two, road_status, pavement_conditions, weather_conditions) %>%
  rename(road_name = road_name.x, road_exits = road_exits.x)


# Albany region scraper ----

# Read the webpage
webpage <- read_html("https://www.thruway.ny.gov/travelers/map/text/twytextwta.cgi?region=AL")

# Parse all p elements
parsed_webpage <- html_nodes(webpage, "p")

# Extract the info
info <- html_text(parsed_webpage)

# Separate the info
info_list <- strsplit(info, "\n")

# Filter
info_list <- info_list[grep("^Road Status:", info_list)]

# Parse all h4 elements
parsed_webpage2 <- html_nodes(webpage, "h4")

# Extract the info
info2 <- html_text(parsed_webpage2)

# Separate the info
info_list2 <- strsplit(info2, "\n")

# Filter
info_list2 <- info_list2[grep("^I-\\d|^Exits", info_list2)]

# Extract road_status
road_status <- character()
for (i in 1:length(info_list)) {
  road_status[i] <- gsub("Pavement\\s*", "", str_extract(info_list[i], "Road Status:\\s*(.*?)(?:Pavement\\s*)"))
}
road_status <- gsub("Road Status:\\s*", "", road_status)
print(road_status)

# Extract pavement_conditions
pavement_conditions <- character()
for (i in 1:length(info_list)) {
  pavement_conditions[i] <- gsub("Pavement\\s*", "", str_extract(info_list[i], "Pavement Conditions:\\s*(.*?)(?:Weather\\s*)"))
}
pavement_conditions <- gsub("Conditions:\\s*", "", pavement_conditions)
pavement_conditions <- gsub("Weather\\s*", "", pavement_conditions)
print(pavement_conditions)

# Extract weather_conditions
weather_conditions <- character()
for (i in 1:length(info_list)) {
  weather_conditions[i] <- gsub("Road\\s*Status:\\s*.*Pavement\\s*", "", str_extract(info_list[i], "Weather Conditions:\\s*(.*)"))
}
weather_conditions <- gsub("Weather Conditions:\\s*", "", weather_conditions)
print(weather_conditions)

# Create data frame
albany <- data.frame(road_name = character(length(info_list2)), road_exits = character(length(info_list2)))

# Extract road_name and road_exits
for (i in 1:length(info_list2)) {
  if(i %% 2 == 1) {
    albany$road_name[i] <- gsub("(.*)I-(.*?) NYS Thruway.*","\\1I-\\2 NYS Thruway", info_list2[i])
    albany$road_exits[i] <- gsub("(.*)Exits (.*) to (.*)","\\1\\2 to \\3", info_list2[i+1])
  }
}

albany <- albany[!(albany$road_name == "" & albany$road_exits == ""),]

albany$road_status <- road_status
albany$pavement_conditions <- pavement_conditions
albany$weather_conditions <- weather_conditions

# Concatenate road details
albany <- albany %>% 
  mutate(road_details = paste0(road_name, " - ", road_exits)) %>% 
  select(road_name, road_exits, road_details, road_status, pavement_conditions, weather_conditions)

# Add geocoded data
albany <- left_join(albany, geocode_roads, by = "road_details") %>% 
  select(road_name.x, road_exits.x, road_details, exit_one, lat_exit_one, lon_exit_one, exit_two, lat_exit_two, lon_exit_two, road_status, pavement_conditions, weather_conditions) %>%
  rename(road_name = road_name.x, road_exits = road_exits.x)


# Geocode ----
 
# # Register Google API key
# register_google(key = "my_key")
# 
# # Split exits into two points
# all <- all %>%
#   mutate(exit_one = paste0("Exit ", str_extract(road_exits, '(.*)\\s\\(.*\\)(?=\\sto)'), " "),
#          exit_two = paste0("Exit ",str_extract(road_exits, '(?<=to\\s)(.*)\\s\\(.*\\)'), " ")) %>%
#   select(road_name, road_exits, road_details, exit_one, exit_two)
# 
# # Geocode exit_one
# all$coordinates <- geocode(paste(all$road_name, all$exit_one, "New York", "NY"))
# all$lon_exit_one <- all$coordinates[[1]]
# all$lat_exit_one <- all$coordinates[[2]]
# all <- all %>%
#   select(road_name, road_exits, road_details, exit_one, lat_exit_one, lon_exit_one, exit_two)
# 
# # Geocode exit_two
# all$coordinates <- geocode(paste(all$road_name, all$exit_two, "New York", "NY"))
# all$lon_exit_two <- all$coordinates[[1]]
# all$lat_exit_two <- all$coordinates[[2]]
# all <- all %>%
#   select(road_name, road_exits, road_details, exit_one, lat_exit_one, lon_exit_one, exit_two, lat_exit_two, lon_exit_two)
# 
# # Test exit coordinates in a map
# leaflet(all) %>%
#   addTiles() %>%
#   addMarkers(~lon_exit_one, ~lat_exit_one, popup = ~road_details) %>%
#   addMarkers(~lon_exit_two, ~lat_exit_two, popup = ~road_details)
# 
# # Save and export geocoded roads and comment code for future use as needed
# write.csv(all, "geocode-roads.csv", row.names=FALSE)
# sheet_write(all, ss = "link", sheet = "geocode")
 

# Export ----

# Authorize
gs4_auth("email")

# Export data to Google Sheet
sheet_write(all, ss = "link", sheet = "all")
sheet_write(albany, ss = "link", sheet = "albany")

