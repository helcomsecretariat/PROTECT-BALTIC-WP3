
# Load packages ----
library(terra)       # For raster data
library(sf)          # For vector data
library(patchwork)   # For combining plots
library(ggspatial)   # For scale bars and north arrows
library(stars)
library(tidyverse)
library(paletteer)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggtext)

# At work
setwd("D:/Nextcloud/WP3 - Jade/Predictors/PROTECT-BALTIC-WP3-main/models/sdms/benthic_species")
dir2save <- "D:/Nextcloud/WP3 - Jade/BIODIV/Map layouts"

# At home
setwd("C:/Users/j_len/Nextcloud2/WP3 - Jade/Predictors/PROTECT-BALTIC-WP3-main/models/sdms/benthic_species")
dir2save <- "C:/Users/j_len/Nextcloud2/WP3 - Jade/BIODIV/Map layouts"

#load(".RData")

countries <- ne_countries(scale = "medium", returnclass = "sf")

# Maps for Macoma balthica ----

mb_pr <- rast("outputs/Macoma_balthica/Macoma_balthica_ensemble_probability.tif")
mb_pr <- project(mb_pr, "EPSG:3035")

mb_pr_map <- mb_pr %>% 
  aggregate(mb_pr, fact = 20, fun = mean) %>% 
  terra::as.data.frame(xy=T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = ensemble_weighted)) +
  geom_sf(data = countries, fill = "gray95", color = "black", linewidth = 0.2) +
  coord_sf(xlim = c(4200932, 5641416), ylim = c(3388676, 4889667), crs = st_crs(3035),  expand = FALSE) +
  #scale_fill_gradientn(colours = paletteer_c("ggthemes::Classic Red", 30), name = "Probability") + 
  scale_fill_gradientn(colours = colorRampPalette(c("white", "darkgreen"))(30), name = "Probability") +
  theme_minimal() +
  labs(title = "Probability map") +
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering, 
                         height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"))

# presence-absence map Macoma balthica
mb_pa <- rast("outputs/Macoma_balthica/Macoma_balthica_ensemble_pa.tif")
mb_pa <- project(mb_pa, "EPSG:3035")

mb_pa_map <- mb_pa %>% 
  aggregate(round(mb_pr), fact = 20, fun = mean) %>% 
  terra::as.data.frame(xy=T) %>% round() %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = as.factor(ensemble_weighted))) +
  geom_sf(data = countries, fill = "gray95", color = "black", linewidth = 0.2) +
  coord_sf(xlim = c(4200932, 5641416), ylim = c(3388676, 4889667), crs = st_crs(3035),  expand = FALSE) +
  scale_fill_manual(values = c("0" = "white", "1" = "darkgreen"), labels = c("Absence", "Presence"), 
                    name = "Occurrence", na.value = "white") +
  theme_minimal() +
  labs(title = "Presence-absence map") +
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering, 
                         height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"))

# disagreement map Macoma balthica
mb_dis <- rast("outputs/Macoma_balthica/Macoma_balthica_ensemble_allpredictions.tif")
mb_dis <- project(mb_dis, "EPSG:3035")

mb_dis_map <- mb_dis %>% 
  aggregate(mb_dis, fact = 20, fun = mean, na.rm=T) %>% 
  terra::as.data.frame(xy=T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = sd)) +
  geom_sf(data = countries, fill = "gray95", color = "black", linewidth = 0.2) +
  coord_sf(xlim = c(4200932, 5641416), ylim = c(3388676, 4889667), crs = st_crs(3035),  expand = FALSE) +
  scale_fill_gradientn(colours = colorRampPalette(c("white", "#C21807"))(30), name = "SD") +
  theme_minimal() +
  labs(title = "Disagreement map") +
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering, 
                         height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"))

caption_text <- "Coordinate Reference System: EPSG:3035 (ETRS89 / LAEA Europe)\nData source: ICES data base.\n Methodological approach: Ensemble modelling combining GLM, GAM, GBM, and RF methods."

mb_combined_map <- mb_pr_map + mb_pa_map + mb_dis_map +
  plot_layout(ncol = 3) +  # Side-by-side layout
  plot_annotation(title = "<i>Macoma balthica</i> distribution models", 
                  caption = caption_text,
                  tag_levels = "A",
                  theme = theme(plot.title = element_markdown(hjust = 0.5, size = 14, face = "bold"),
                                plot.subtitle = element_text(hjust = 0.5, size = 12),
                                plot.caption = element_text(size = 10, hjust = 1, face = "italic")))

#ggsave(filename = paste0(dir2save,"mb_pr_map.png"), plot = mb_pr_map, 
       #device = "png", width = 13, height = 5, units = "in", dpi = 600, bg = "white")
#ggsave(filename = paste0(dir2save,"mb_pa_map.png"), plot = mb_pa_map, 
       #device = "png", width = 13, height = 5, units = "in", dpi = 600, bg = "white")
#ggsave(filename = paste0(dir2save,"mb_dis_map.png"), plot = mb_dis_map, 
       #device = "png", width = 13, height = 5, units = "in", dpi = 600, bg = "white")

ggsave(filename = paste0(dir2save,"/mb_layout_map.png"), plot = mb_combined_map, 
       device = "png", width = 13, height = 5, units = "in", dpi = 900, bg = "white")


# End of script ----