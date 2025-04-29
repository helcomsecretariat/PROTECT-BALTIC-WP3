
### First draft of workflow intended for Mytilus edulis for the workshop ###
### This workflow will be generalized to a species list for general modelling purposes ### 

# Load required packages
require(dplyr)
require(sf)
require(terra) 
require(lubridate)

# Read input data 
# These datasets were directly downloaded from the sharepoint.
# Please, add your own path
EE <- read.csv2("Your path/EE_benthic_species_observations_19062024.csv", 
                sep=",", dec=".")
DE <- read.csv2("Your path/DE_benthic_species_observations_24032025.csv", 
                sep="|", dec=".")
ICES <- read.csv2("Your path/ICES_benthic_species_zoobenthos_23082024.csv", sep=",", dec=".")

# Select only the relevant fields from each dataset and rename columns 
EE <- EE %>% 
  select(c("station_id", "sample_id", "date", "quantity", "q_type", 
                      "samp_metho", "start_lat", 
                      "start_long", "scien_name")) %>%
  rename(start_decimal_latitude = start_lat, 
         start_decimal_longitude = start_long, 
         sampling_method = samp_metho,
         scientific_name = scien_name,
         quantity_type = q_type)

DE <- DE %>% 
  select(c("station_id", "sample_id", "date", "quantity", "quantity_type", 
                      "sampling_method", "start_decimal_latitude", 
                      "start_decimal_longitude", "scientific_name"))

ICES <- ICES %>% 
  select(c("station_id", "sample_id", "date", "quantity", "q_type", 
                      "s_method", "start_lat", 
                      "start_long", "scien_name")) %>%
  rename(start_decimal_latitude = start_lat, 
         start_decimal_longitude = start_long, 
         sampling_method = s_method,
         scientific_name = scien_name,
         quantity_type = q_type)

# Ensure station_id and sample_id are treated as characters
EE$station_id <- as.character(EE$station_id)
EE$sample_id <- as.character(EE$sample_id)
DE$station_id <- as.character(DE$station_id)
DE$sample_id <- as.character(DE$sample_id)
ICES$station_id <- as.character(ICES$station_id)
ICES$sample_id <- as.character(ICES$sample_id)

# Merge all datasets into a single table
BalticSea <- bind_rows(EE, DE, ICES)

# Create dummy station IDs where missing, based on date and coordinates 
BalticSea <- BalticSea %>%
  mutate(station_id = ifelse(station_id == "" | is.na(station_id), NA, station_id)) %>%
  group_by(date, start_decimal_latitude, start_decimal_longitude) %>%
  mutate(station_id = ifelse(all(is.na(station_id)),
                             paste0("st_dummy_", cur_group_id()),
                             station_id)) %>%
  ungroup()

# Create a helper column counting the number of samples per station to further remove duplicates
BalticSea <- BalticSea %>%
  group_by(date, start_decimal_latitude, start_decimal_longitude) %>%
  mutate(n_samples = n_distinct(sample_id)) %>%
  ungroup()

# Remove duplicates based on date, location, species, quantity, and number of samples 
BalticSea <- BalticSea %>%
  distinct(date, start_decimal_latitude, start_decimal_longitude, 
           scientific_name, quantity, n_samples, .keep_all = TRUE) %>%
  select(-n_samples)

# Filter dataset for specific quantity types and sampling methods
BalticSea <- BalticSea %>%
  filter(quantity_type %in% c("Count of individuals") & 
           grepl("grab sampler", sampling_method, ignore.case = TRUE))

# Aggregate data: calculate mean quantity per station, date, sampling method, lat, lon, and species 
BalticSea_aggregated <- BalticSea %>%
  group_by(station_id, date, quantity_type, sampling_method, 
           start_decimal_latitude, start_decimal_longitude, scientific_name) %>%
  summarise(quantity = mean(quantity, na.rm = TRUE), .groups = "drop")

# Define the list of target species for presence-absence analysis 
species_list <- c("Mytilus edulis")

# Create presence-absence table based on selected species 
PresenceAbsence <- BalticSea_aggregated %>%
  mutate(presence = ifelse(scientific_name %in% species_list, 1, 0)) %>%
  group_by(date, quantity_type, start_decimal_latitude, start_decimal_longitude) %>%
  summarise(presence = max(presence), .groups = "drop")

# Convert PresenceAbsence table to spatial object (Simple Feature) 
PresenceAbsence_sf <- PresenceAbsence %>%
  st_as_sf(coords = c("start_decimal_longitude", "start_decimal_latitude"), crs = 3067) # WGS84 (EPSG:4326)

# Export to Shapefile 
st_write(PresenceAbsence_sf, "PresenceAbsence.shp", delete_layer = TRUE)

# Load compete set of predictors stored in the raster stack produced by SLU and uploaded in github. 
predictor_stack <- rast("Your path/predictor_stack.tif")

# Extract the relevant predictors for benthic invertebrates and fish
n <- c("bpi_4000_20000_250m",
       "bpi_500_5000_250m",
       "depth_250m",
       "o2_bottom_mean_resample_250m",
       "o2_bottom_sd_resample_250m",
       "salinity_bottom_mean_resample_250m",
       "salinity_bottom_sd_resample_250m",
       "secchi_depth_mean_resample_250m",
       "sediment_resample_250m",
       "slope_250m",
       "temperature_bottom_mean_resample_250m",
       "temperature_bottom_sd_resample_250m",
       "aspect_250m",
       "chloro_a_mean_resample_250m",
       "nitrate_surface_mean_resample_250m",
       "secchi_depth_sd_resample_250m",
       "wave_max_height_sd_resample_250m",
       "sea_ice_concentration_mean_resample_250m")

predictor_stack <- predictor_stack[[n]]

# Add extra predictors calculated by SYKE and selected for benthic invertebrates and fish: Isaeus Surface wave exposure 250m, seafloor ruggedness, and wave exposure at seafloor
dae <- rast("Your path/depth_attenuated_exposure_at_seafloor.tif")
swe <- rast("Your path/Surface_wave_exposure.tif")
names(swe)<-"surface_wave_exposure"
rug <- rast("Your path/ruggedness_3cells.tif")
names(rug)<-"ruggedness"

predictor_stack <- c(predictor_stack, dae, swe, rug)

# Transform the PresenceAbsence spatial object to the CRS of the raster 
PresenceAbsence_sf <- PresenceAbsence_sf %>%
  st_transform(crs(predictor_stack))

# Convert sf object to terra SpatVector 
PresenceAbsence_vect <- vect(PresenceAbsence_sf)

# Rasterize stations: 1 if at least one presence, else 0 
Presence_raster <- rasterize(PresenceAbsence_vect, predictor_stack[[1]], field = "presence",
                             fun = function(x, ...) if (any(x == 1)) 1 else 0)

writeRaster(Presence_raster, "Presence_raster.tif")

# Now convert raster back to points (only non-NA cells) 
Presence_points <- as.points(Presence_raster, values = TRUE, na.rm = TRUE)

# Convert points to sf 
Presence_points_sf <- st_as_sf(Presence_points)

# Save final shapefile 
st_write(Presence_points_sf, "PresenceAbsence_points.shp", delete_layer = TRUE)

# Assigning presence/absence records to the nearest raster cell centroid 

# 1. Convert raster cells to point centroids
raster_centroids <- as.points(predictor_stack, values = FALSE) %>% 
  st_as_sf() %>%
  mutate(cell_id = 1:n())  # Add unique cell ID

# 2. Find nearest raster centroid for each presence point. Spatial join to nearest centroid
points_with_centroids <- Presence_points_sf %>%
  st_join(raster_centroids, join = st_nearest_feature)

# 3. Add raster values to the points. Extract raster values at the matched centroids 
points_with_centroids <- terra::extract(predictor_stack, vect(points_with_centroids), bind = TRUE) 
head(points_with_centroids)

# Save raster_centroids (for further checks)
st_write(raster_centroids, "centroids/raster_centroids.shp")

# Save points_with_centroids (this shapefile contains the final dataset for modelling) 
writeVector(points_with_centroids, "./centroids/Presence_points_with_centroids.shp", overwrite=TRUE)



# End of script






