##### Setup
library(dplyr)
library(sdm)
library(terra)
library(scales)
library(lubridate)
library(leaflet)
library(vctrs)

if(!dir.exists("inputs")){dir.create("inputs")}
if(!dir.exists("outputs")){dir.create("outputs")}
grid <- rast("../../grid/outputs/grid_marine_250m.tif")

##### Import and filter observation data
#df <- read.csv("inputs/ICES/ICES_fish_and_decapod_species_obs_DATRAS_08112024.csv", sep = "|")
df <- read.csv("inputs/ICES_benthic_species_zoobenthos_23082024.csv", sep = "|")
df$date <- ymd_hms(df$date)

# Assume for a given sampling event, if a species is not observed, it is absent.
# i.e. that the sampling was recording all species present in the dataset

# Use only specific quantity types
#df <- filter(df, q_unit == "individuals")

df <- df %>%
  group_by(date, start_lat, start_long) %>%
  reframe(scien_name = c(scien_name, "_absence"),
          origin_id = c(origin_id, NA),
          station_id = c(station_id, NA),
          sample_id = c(sample_id, NA),
          quantity = c(quantity, 0),
          q_type = c(q_type, NA),
          q_unit = c(q_unit, "individuals")
  )

df <- df %>%
  group_by(date, start_lat, start_long) %>%
  mutate(group_id = cur_group_id()) %>%
  relocate(group_id)

# Get number of presence records per species
summary(as.factor(df$scien_name))

##### Filter by species
spec <- "Macoma balthica"
df <- filter(df, scien_name == spec | scien_name == "_absence") # Make sure to include absence records

# Set absences to species name, absence = when quantity is 0
df$scien_name[df$scien_name == "_absence"] <- spec

# Calculate species count in each sample
obs <- df %>%
  group_by(group_id, scien_name) %>%
  summarise(count = sum(quantity), lat = mean(start_lat), long = mean(start_long)) %>%
  ungroup()

# Convert to binary
obs$count[obs$count > 0] <- 1

# Plot the points
pres_obs <- obs[which(obs$count >= 0),]
palette <- colorNumeric(palette = "viridis", domain = pres_obs$count, reverse = TRUE)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = as.data.frame(pres_obs),
                   ~long, ~lat,
                   color = ~palette(count),
                   popup = ~paste("Count:", count),
                   radius = 2,
                   fillOpacity = 0.7) %>%
  addLegend("bottomright", pal = palette, values = pres_obs$count,
            title = "Count")

##### ... proceed with modelling
