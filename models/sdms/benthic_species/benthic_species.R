# Setup
## Load packages, etc.
library(dplyr)
library(sdm)
library(terra)
library(scales)
library(lubridate)
library(vctrs)

if(!dir.exists("inputs")){dir.create("inputs")}
if(!dir.exists("outputs")){dir.create("outputs")}
grid <- rast("../../../grid/outputs/grid_250m.tif")
plot_agg <- 5 # Factor by which to decrease plotting resolution
pred_agg <- 1 # Factor by which to decrease predictor resolution
ncores <- 8 #Specify number of cores to use for parallel processing

## Install packages for ensemble modelling (if not already installled)
#sdm::installAll(update = TRUE) # Install all modeling packages if not done already
#sdm::getmethodNames()

spec_list <- c("Macoma balthica", "Scoloplos armiger", "Pygospio elegans",
               "Monoporeia affinis", "Hediste diversicolor", "Mya arenaria",
               "Corbula gibba", "Saduria entomon", "Halicryptus spinulosus",
               "Mysella bidentata", "Mytilus edulis", "Hydrobia ulvae",
               "Nephtys hombergii", "Amphiura filiformis", "Abra alba",
               "Limecola balthica", "Corophium volutator", "Nucula nitidosa",
               "Heteromastus filiformis", "Cerastoderma glaucum", "Bylgides sarsi",
               "Diastylis rathkei", "Tubificoides benedii")

for(i in 1:length(spec_list)){
#for(i in 9:9){

  # Import and filter observation data
  df <- read.csv("inputs/ICES_benthic_species_zoobenthos_23082024.csv", sep = "|")
  df$date <- ymd_hms(df$date)


  ## Assuming any samples where a species was not observed are absences.
  ## i.e. that the sampling was recording all species present
  # Use only counts of individuals
  df <- filter(df, q_unit == "individuals")

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
    mutate(group_id = cur_group_id())

  df <- relocate(df, group_id)


  # Specify species
  ## Get number of presence records per species
  #summary(as.factor(df$scien_name))

  ## Filter by species
  spec <- spec_list[i]
  sdir <- paste0("outputs/", gsub(" ", "_", spec))
  mod_dir <- paste0(sdir, "/", gsub(" ", "_", spec), "_model.sdm")
  if(!dir.exists(sdir)){dir.create(sdir)}
  df <- filter(df, scien_name == spec | scien_name == "_absence") # Make sure to include absence records

  # Set absences to species name, absence = when count is 0
  df$scien_name[df$scien_name == "_absence"] <- spec

  # Calculate species count in each sample
  obs <- df %>%
    group_by(group_id, scien_name) %>%
    summarise(count = sum(quantity), lat = mean(start_lat), long = mean(start_long)) %>%
    ungroup()

  # Generate species distribution model
  ## Import predictors/explanatory variables
  l <- list.files("../../../predictors/_predictor_stack_vif", full.names = T, pattern = "\\.tif$")
  preds <- rast(l)
  names(preds) <- tools::file_path_sans_ext(basename(l))

  # Remove categorical predictors for now
  cat_preds <- c("biozone_resample_250m", "sediment_resample_250m")
  pred_keep <- setdiff(names(preds), cat_preds)
  preds <- preds[[pred_keep]]

  # Rescale predictors from 0 to 1 (if desired)
  #for(i in 1:nlyr(preds)){
  # v <- terra::values(preds[[i]])
  #bs <- as.data.frame(obs) v <- scales::rescale(v, to = c(0,1))
  # preds[[i]][] <- v
  # cat("\nPredictor layer", i , "rescaling done")
  #}

  # Optional: change to lower resolution to speed up processing
  # Aggregate by 4 changes 250m to 1km
  if(pred_agg > 1){preds <- aggregate(preds, fact = pred_agg, fun = "mean")}

  ## Change to presence/absence and convert to vector
  obs$count[which(obs$count > 0)] <- 1
  obs <- dplyr::select(obs, count, long, lat)
  obs <- vect(obs, geom = c("long", "lat"), crs="+proj=longlat +datum=WGS84")
  obs <- project(obs, crs(preds))

  # Determine predictor values at observation points
  obs <- extract(preds, obs, bind = T)

  # Remove observations with NAs in predictors
  obs <- obs[vec_detect_complete(as.data.frame(obs)), ]
  obs_df <- as.data.frame(obs)

  ## Generate the model
  dat <- sdmData(formula = count~., train = obs[,1], predictors = preds)
  mod <- sdm(count~., data = dat, methods = c("rf", "svm", "brt"), replicatin ='sub', test.percent = 30, n = 1)
  write.sdm(mod, mod_dir, overwrite = T)

  ## Save ROC curves
  png(file = paste0(sdir, "/", gsub(" ", "_", spec), "_roc.tif"))
  print(roc(mod))
  dev.off()

  ## Save variable importance
  vi <- getVarImp(mod, id = "ensemble", wtest = "training")
  temp <- vi@varImportance
  temp$AUCtest <- temp$AUCtest * 100
  temp$corTest <- temp$corTest * 100
  write.csv(temp, paste0(sdir, "/", gsub(" ", "_", spec), "_var_importance.csv"), row.names = FALSE)

  ## Import model (if necessary) and examine ROC curve
  mod <- read.sdm(mod_dir)

  ## Generate model predictions for each method
  ### Initiate cluster
  p <- predict(mod, newdata = preds, parallelSetting = list(ncore = ncores, method = "future", strategy = "data"))

  ## Generate ensemble predictions
  e <- ensemble(mod, newdata = p)

  ## Convert to presence/absence based on threshold
  presab <- pa(e, mod, id = "ensemble", opt = 5) # try opt = 9 also

  ## Modify according to thresholds (if desired)

  # Import depth
  dep <- preds[["depth_250m"]]

  # Here there are two options:
  # Option 1: Remove predictions from grid cells with a depth greater than
  # where the species has been observed OR
  # Option 2: Remove predictions from grid cells with a depth greater than
  # where sampling occurred (i.e. even if the species was absent)

  # Option 1
  depth_thresh <- max(obs_df$depth_250m[obs_df$count > 0], na.rm = TRUE)

  # Option 2
  #depth_thresh <- max(obs_df$depth_250m, na.rm = TRUE)

  # Implement depth threshold (if desired)
  e[dep > depth_thresh] <- 0; presab[dep > depth_thresh] <- 0

  ## Export model predictions
  # Scale 0 to 100 and convert to integer to reduce file size
  e <- e*100
  fp <- paste0(sdir, "/", gsub(" ", "_", spec), "_ensemble_probability.tif")
  writeRaster(e, fp, overwrite = TRUE,
              datatype = "INT2S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"))

  fp <- paste0(sdir, "/", gsub(" ", "_", spec), "_ensemble_pa.tif")
  writeRaster(presab, fp, overwrite = TRUE,
              datatype = "INT2S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"))

  cat("\nIteration", i, "complete - ", spec_list[i], "\n")

}


