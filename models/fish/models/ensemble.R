########## Info ##########
# When installing biomod2, use install.packages("biomod2", dependencies = TRUE)
# to make sure each of the different model packages are also installed

########## Settings ##########
mod_id <- "run1" # Unique ID for model runs
run_name <- "All data, absencences generated across all data, relevant predictors including CV, Bigboss default settings, except custom XGBoost"
# Models to include in the ensemble
#mod_methods <- c("GAM", "GLM", "RF", "GBM", "XGBOOST", "CTA", "MARS") # all options
mod_methods <- c("RF", "XGBOOST", "GBM", "CTA", "GAM", "GLM") # Subset
n_rep <- 5 # Number of cross-validation repetitions
cv_perc <- 0.7 # Percentage of training/testing data

########## Setup ##########
# Packages
setwd("C:/Users/edse0001/OneDrive - Sveriges lantbruksuniversitet/pb_fish/models") # For the batch script
library(terra)
library(biomod2)
library(caret)
library(dplyr)
library(tidyr)
library(FNN)
library(vroom)

source("extract_near.R") # Function to extract raster values to point from nearest cell
pdf(NULL)
start_time <- Sys.time()

# Create directories
if(!dir.exists("outputs")){dir.create("outputs")} # Main outputs from biomod for use in the quarto document
if(!dir.exists("outputs_extra")){dir.create("outputs_extra")} # Extra outputs for quick evaluation
start_dir <- paste0("outputs_extra/", mod_id, "/_run_started")
finish_dir <- paste0("outputs_extra/", mod_id, "/_run_finished")
if(!dir.exists(start_dir)){dir.create(start_dir, recursive = TRUE)}
if(!dir.exists(finish_dir)){dir.create(finish_dir, recursive = TRUE)}

# Import species list
spec_list <- read.csv("spec_list.csv")
l <- list.files(start_dir)
spec_list$run_initiated[spec_list$scientific_name %in% l] <- 1
spec_miss <- spec_list %>%
  filter(run_initiated == 0) %>%
  filter(quantity > 100)

spec_full <- first(spec_miss$scientific_name)
spec_full_ <- gsub(" ", ".", spec_full)
spec <- spec_full
if(nchar(spec) > 25){spec <- substr(spec, 1, 25)} # shorten name if too long (issues writing files)
spec_ <- gsub(" ", ".", spec)
write.csv(NA, paste0(start_dir, "/", spec_full)) # Record that run was initiated in _run_started folder
spec_list$run_initiated[spec_list$scientific_name == spec] <- 1 
#write.csv(spec_list, "spec_list.csv", row.names = FALSE)

# Biomod file/folder names and creation of output folders
resp_n <- gsub(" ", "_", spec)
proj_n <- mod_id
out_dir <- paste0("outputs/", mod_id)
if(!dir.exists(out_dir)){dir.create(out_dir, recursive = TRUE)}
extra_dir <- paste0("outputs_extra/", mod_id, "/", spec_)
if(!dir.exists(extra_dir)){dir.create(extra_dir, recursive = TRUE)}

########## Observation data ########## (This part you will probably need to modify according to the species group/datasets)
obs <- read.csv("../observations/outputs/observations_all.csv")

# Generate absences for all date/lat/long combinations
obs <- obs %>%
  group_by(date, latitude, longitude) %>%
  reframe(scientific_name = c(scientific_name, "_absence"),
          quantity = c(quantity, 0),
          quantity_unit = c(quantity_unit, first(quantity_unit)),
          gear_type = c(gear_type, first(gear_type)),
          gear_description = c(gear_description, first(gear_description)),
          soak_time_hours = c(soak_time_hours, mean(soak_time_hours, na.rm = TRUE)),
          dataset = c(dataset, first(dataset))
  )

obs <- obs %>%
  group_by(date, latitude, longitude) %>%
  mutate(group_id = cur_group_id()) %>%
  relocate(group_id)

# Remove absence observations if the species is not in the dataset
# This part just makes sure that we are not assigning absences for species
# that are never observed in a given dataset
# NOTE: you may or may not want to do this depending on the data
# e.g., if there are no porpoises in the video dataset, and you never saw one, 
# but if you had seen one, you would have recorded it. Then it would be fair to keep absences for porpoises
# Here you could replace dataset with any other grouping that you want to use
# E.g. if you only want to generate absences from the same country/dataset and the same gear type
# you can create a new column that combines these two and replace dataset with that column

obs$dataset <- "All" # Keep this line if you do want to generate absences across all the data
obs_dataset <- unique(obs$dataset[obs$scientific_name == spec_full])

obs <- obs %>%
  filter(scientific_name == spec_full | scientific_name == "_absence") %>%
  filter(dataset %in% obs_dataset | 
           (!dataset %in% obs_dataset & scientific_name != "_absence")) %>%
  filter(!is.na(quantity))

# Set absences to species name, absence = when quantity is 0
obs$scientific_name[obs$scientific_name == "_absence"] <- spec_full

# Calculate species count in each sample
obs <- obs %>%
  group_by(group_id, scientific_name) %>%
  summarise(quantity = sum(quantity), latitude = mean(latitude), longitude = mean(longitude)) %>%
  ungroup()

# Convert to binary
obs$quantity[obs$quantity > 0] <- 1

########## Predictors ##########
grid <- rast("../grid/outputs/grid_marine_250m_v2.tif")
l <- list.files("../predictors/predictor_stack_filter", pattern = ".tif$", full.names = TRUE)
ls <- list.files("../predictors/predictor_stack_filter", pattern = ".tif$", full.names = FALSE)
preds <- rast(l)
names(preds) <- tools::file_path_sans_ext(ls)

# Extract predictor data from nearest cells
# for those falling outside the grid
# NOTE: you may not want to do this if you are worried some points might be very far away from the grid
# or you simply think it is a bad idea, then you can just run the normal terra::extract function
xy <- vect(obs[, c("longitude", "latitude")], geom=c("longitude", "latitude"), crs = "EPSG:4326")
xy <- project(xy, grid)
df_preds <- extract_near(grid, preds, xy)

# Presence/absence map export
clrs <- ifelse(obs$quantity == 1, "red", "black")
#temp_plot <- plot(xy, col = clrs, pch = 16, cex = 1.5)
pdf(file = paste0(extra_dir, "/", spec_, "_point_map.pdf"),
    width = 8,
    height = 8)
plot(grid, col = "lightgrey")
plot(xy, col = clrs, pch = 16, cex = 0.3, add = TRUE)
dev.off()

########## Modeling ##########
## Prepare model
dat <- BIOMOD_FormatingData(resp.var = obs$quantity,
                            expl.var = df_preds,
                            resp.name = resp_n,
                            dir.name = out_dir,
                            resp.xy = terra::crds(xy))

dat

# Set training/testing data
part <- createDataPartition(obs$quantity, times = n_rep, list = FALSE, p = cv_perc)
calib <- matrix(nrow = length(obs$quantity), ncol = n_rep)
calib <- as.data.frame(calib)
colnames(calib) <- paste0("_allData_RUN", 1:n_rep)

for(i in 1:n_rep){
  calib[,i] <- FALSE
  calib[part[,i],i] <- TRUE
}

mod_cv <- bm_CrossValidation(bm.format = dat,
                             strategy = 'user.defined',
                             user.table = calib)

# Custom model settings (Bigboss as default otherwise)
# Use OptionsBigBoss to see default settings
xg_nrounds <- 1000
xg_early_stopping_rounds <- 20
xg_max_depth <- 4
xg_eta <- 0.01

user.XGBOOST <- vector("list", n_rep)
names(user.XGBOOST) <- paste0("_allData_RUN", 1:n_rep)
user.XGBOOST <- lapply(user.XGBOOST, function(x) list(nrounds = xg_nrounds,
                                                      early_stopping_rounds = xg_early_stopping_rounds,
                                                      params = list(max_depth = xg_max_depth, eta = xg_eta)))

user_val <- list(XGBOOST.binary.xgboost.xgboost = user.XGBOOST)

user_opt <- bm_ModelingOptions(data.type = 'binary',
                               models = mod_methods,
                               strategy = "user.defined",
                               user.val = user_val,
                               user.base = "bigboss",
                               bm.format = dat,
                               calib.lines = mod_cv)

# Single models
mod <- BIOMOD_Modeling(bm.format = dat,
                       models = mod_methods,
                       modeling.id = mod_id,
                       CV.strategy = 'user.defined',
                       CV.user.table = mod_cv,
                       OPT.user = user_opt,
                       CV.do.full.models = FALSE,
                       var.import = 1,
                       metric.eval = c('TSS', 'ROC'),
                       do.progress = FALSE)

# Plot stratification (this will plot which points are training/testing)
#clrs <- ifelse(get_calib_lines(mod)[,1], "blue", "red")
#plot(xy, col = clrs, pch = 16, cex = 1.5)

# Model evaluation plot export
bm_plot <- bm_PlotEvalMean(mod, dataset = "validation", do.plot = FALSE)
temp_plot <- bm_plot[["plot"]]
pdf(file = paste0(extra_dir, "/", spec_, "_evaluation.pdf"),
    width = 8,
    height = 8)
print(temp_plot)
dev.off()

# Ensemble model
# Remove models with large calibration-validation differential (over-fitting)
mod_eval <- get_evaluations(mod)
mod_eval <- filter(mod_eval, is.na(validation) == FALSE)
mod_eval$overfit <- mod_eval$calibration - mod_eval$validation
mod_eval <- filter(mod_eval, metric.eval == "TSS")
write.csv(mod_eval, paste0(extra_dir, "/", spec_, "_validation_scores.csv"), row.names = FALSE)
mod_eval <- filter(mod_eval, overfit < 1)
mod_select <- mod_eval$full.name

emod <- BIOMOD_EnsembleModeling(bm.mod = mod,
                                models.chosen = mod_select,
                                em.by = 'all',
                                #em.algo = c('EMwmean'), # Weighted mean for ensemble probabilities
                                em.algo = c('EMwmean', 'EMca'), # Committee averaging
                                metric.eval = c('TSS'),
                                metric.select = c('ROC'),
                                metric.select.thresh = c(0.7), # Cutoff for inclusion/removal
                                var.import = 1,
                                EMci.alpha = 0.05,
                                EMwmean.decay = 'proportional')

# Response curves plot export
bm_plot <- bm_PlotResponseCurves(bm.out = emod, 
                                 models.chosen = get_built_models(emod)[1],
                                 fixed.var = 'median',
                                 do.progress = FALSE,
                                 do.plot = FALSE)
temp_plot <- bm_plot[["plot"]]
pdf(file = paste0(extra_dir, "/", spec_, "_response_curves.pdf"),
    width = nlyr(preds)*1.4,
    height = nlyr(preds)*0.9)
print(temp_plot)
dev.off()

# Variable importance plot export
bm_plot <- bm_PlotVarImpBoxplot(mod,
                                group.by = c("algo", "expl.var", "expl.var"),
                                do.plot = TRUE)

temp_plot <- bm_plot[["plot"]]
pdf(file = paste0(extra_dir, "/", spec_, "_var_importance.pdf"),
    width = nlyr(preds)*1.4,
    height = nlyr(preds)*0.9)
print(temp_plot)
dev.off()

# Predictions
mod_proj <- BIOMOD_Projection(bm.mod = mod,
                              proj.name = proj_n,
                              new.env = preds,
                              models.chosen = mod_select,
                              metric.binary = 'TSS',
                              build.clamping.mask = FALSE,
                              keep.in.memory = FALSE,
                              do.stack = FALSE, # Check to see if this help reduce memory usage
                              omit.na = FALSE
)

# Individual projections plot export
temp_plot <- plot(mod_proj, do.plot = FALSE)
pdf(file = paste0(extra_dir, "/", spec_, "_individual_projections.pdf"),
    width = n_rep*10,
    height = length(mod_methods)*5)
print(temp_plot)
dev.off()

emod_proj <- BIOMOD_EnsembleForecasting(bm.em = emod,
                                        bm.proj = mod_proj,
                                        models.chosen = 'all',
                                        metric.binary = 'TSS',
                                        keep.in.memory = FALSE,
                                        do.stack = FALSE,
)

# Ensemble projections plot export
temp_plot <- plot(emod_proj, do.plot = FALSE)
pdf(file = paste0(extra_dir, "/", spec_, "_ensemble_projections.pdf"),
    width = 15,
    height = 10)
print(temp_plot)
dev.off()

# Record run time and export run details
end_time <- Sys.time()
run_time <- as.numeric(difftime(end_time, start_time, units = "hours"))

#spec_list <- read.csv("spec_list.csv")
spec_list$run_completed[spec_list$scientific_name == spec_full] <- 1
spec_list$run_time[spec_list$scientific_name == spec_full] <- run_time
spec_list$models[spec_list$scientific_name == spec_full] <- paste(mod_methods, collapse = ", ")
spec_list$num_presence[spec_list$scientific_name == spec_full] <- nrow(obs[obs$quantity == 1, ])
spec_list$num_absence[spec_list$scientific_name == spec_full] <- nrow(obs[obs$quantity == 0, ])
spec_list$run_name[spec_list$scientific_name == spec_full] <- run_name
write.csv(spec_list[spec_list$scientific_name == spec_full,], paste0(finish_dir, "/", spec_full_, ".csv"), row.names = FALSE)

# Save full run log
l <- list.files(start_dir)
ls <- list.files(start_dir, full.names = TRUE)
lf <- list.files(finish_dir, full.names = TRUE)
lf <- lf[!grepl("_details.csv", lf, fixed = TRUE)]
spec_finish <- vroom(lf)
spec_list <- read.csv("spec_list.csv")
spec_list$run_initiated[spec_list$scientific_name %in% l] <- 1
m1 <- match(spec_list$scientific_name, spec_finish$scientific_name)
m2 <- !is.na(m1)
spec_list[m2, ] <- spec_finish[m1[m2], ]
#write.csv(spec_list, paste0(finish_dir, "/_", mod_id, "_details.csv"), row.names = FALSE)

# Only when all species completed? (might not work if some models fail)
if(length(ls) == length(lf)){
  #spec_finish <- vroom(lf)
  write.csv(spec_list, paste0(finish_dir, "/_", mod_id, "_details.csv"), row.names = FALSE)
}

# Record species completion (now redundant)
#write.csv(spec_list, "spec_list.csv", row.names = FALSE)
#write.csv(spec_list, paste0("outputs/", mod_id, "_details.csv"), row.names = FALSE)
