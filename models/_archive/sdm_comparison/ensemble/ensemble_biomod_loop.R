library(dplyr)
library(terra)
library(biomod2)
library(pROC)
source("bm_ROC.R")

if(!dir.exists("inputs")){dir.create("inputs")}
if(!dir.exists("outputs")){dir.create("outputs")}
grid <- rast("inputs/grid/grid_250m.tif")
start_time <- Sys.time()

spec_list <- c("Abra alba", "Bathyporeia pilosa", "Macoma balthica")

for(j in spec_list){

  ## Settings
  ## Species: "Abra alba", "Bathyporeia pilosa", "Macoma balthica", "Marenzelleria neglecta"
  spec <- j
  spec_ <- gsub(" ", "_", spec)
  mod_methods <- c("XGBOOST", "RF", "GBM")
  sdir <- paste0("outputs/", gsub(" ", "_", spec))
  if(!dir.exists(sdir)){dir.create(sdir)}
  
  mod_id <- as.character(sample(1:999999999, 1))
  resp_n <- paste0("temp", mod_id)
  proj_n <- "misc"
  
  ## Import observations
  #mod_dir <- paste0(sdir, "/", gsub(" ", "_", spec), "_model.sdm")
  obs_dir <- paste0("../input_data/observations/", spec_, "_observations.csv")
  obs <- read.csv(obs_dir)
  train <- select(obs, train1, train2, train3)
  obs <- select(obs, count, long, lat)
  obs <- vect(obs, geom = c("long", "lat"), crs="+proj=longlat +datum=WGS84")
  obs <- project(obs, crs(grid))
  
  ## Import predictors
  l <- list.files("../input_data/predictors", full.names = T, pattern = "\\.tif$")
  preds <- rast(l)
  names(preds) <- tools::file_path_sans_ext(basename(l))
  
  # Aggregate predictors for faster testing
  #preds <- aggregate(preds, fact = 5, fun = "max", na.rm = T)
  
  ## Prepare model
  dat <- BIOMOD_FormatingData(resp.var = obs,
                              expl.var = preds,
                              resp.name = resp_n,
                              dir.name = "outputs")
  
  dat
  plot(dat)
  
  ## Specify training/testing
  colnames(train) <- c("_allData_RUN1", "_allData_RUN2", "_allData_RUN3")
  train <- train %>% mutate_all(~as.logical(.))
  mod_cv <- bm_CrossValidation(bm.format = dat,
                               strategy = 'user.defined',
                               user.table = train)
  
  ## Bigboss model
  #mod_bb <- BIOMOD_Modeling(bm.format = dat,
  #                          models = mod_methods,
  #                          modeling.id = mod_id,
  #                          CV.strategy = 'user.defined',
  #                          CV.user.table = mod_cv,
  #                          OPT.strategy = 'bigboss',
  #                          var.import = 2,
  #                          metric.eval = c('TSS','ROC'),
  #                          do.progress = FALSE,
  #                          CV.do.full.models = FALSE)
  
  ## Customized model
  opt.b <- bm_ModelingOptions(data.type = 'binary',
                              models = mod_methods,
                              strategy = 'bigboss')
  
  user.RF <- vector("list", 3)
  names(user.RF) <- paste0("_allData_RUN", 1:3)
  user.RF <- lapply(user.RF, function(x) list(ntree = 500))
  
  user.ANN <- vector("list", 3)
  names(user.ANN) <- paste0("_allData_RUN", 1:3)
  user.ANN <- lapply(user.ANN, function(x) list(size = 8, decay = 0.2, maxit = 1000))
  
  user.XGBOOST <- vector("list", 3)
  names(user.XGBOOST) <- paste0("_allData_RUN", 1:3)
  user.XGBOOST <- lapply(user.XGBOOST, function(x) list(nrounds = 10))
  
  user_val <- list(RF.binary.randomForest.randomForest = user.RF,
                   ANN.binary.nnet.nnet = user.ANN,
                   XGBOOST.binary.xgboost.xgboost = user.XGBOOST)
  
  user_opt <- bm_ModelingOptions(data.type = 'binary',
                                 models = mod_methods,
                                 strategy = "user.defined",
                                 user.val = user_val,
                                 user.base = "bigboss",
                                 bm.format = dat,
                                 calib.lines = mod_cv)
  
  mod <- BIOMOD_Modeling(bm.format = dat,
                         modeling.id = mod_id,
                         models = mod_methods,
                         CV.strategy = 'user.defined',
                         CV.user.table = mod_cv,
                         OPT.user = user_opt,
                         metric.eval = c('TSS','ROC'),
                         var.import = 2,
                         CV.do.full.models = FALSE)
  
  
  ## Plot and export ROC curves
  source("bm_ROC.R")
  train <- get_calib_lines(mod)
  if(any(colnames(train) == "_allData_allRun")){
    train <- train[ , -which(colnames(train) %in% c("_allData_allRun"))]
  }
  cv_runs <- ncol(train)
  
  pdf(file = paste0(sdir, "/", gsub(" ", "_", spec), "_ROC_curves.pdf"),
      width = cv_runs*3,
      height = length(mod_methods)*3)
  bm_ROC(mod, mod_methods)
  dev.off()
  
  ## Individual model performance
  mod_eval <- get_evaluations(mod)
  mod_eval <- mod_eval %>%
    select(algo, run, metric.eval, calibration, validation)
  colnames(mod_eval) <- c("algo", "cv_run", "metric", "calibration", "validation")
  write.csv(mod_eval, paste0(sdir, "/", spec_, "_individial_model_performance.csv"), row.names = FALSE)
  png(file = paste0(sdir, "/", spec_, "_evaluation_plot.png"))
  bm_PlotEvalMean(mod)
  dev.off()
  
  ## Ensemble model
  emod <- BIOMOD_EnsembleModeling(bm.mod = mod,
                                  models.chosen = 'all',
                                  em.by = 'PA+run',
                                  em.algo = c('EMwmean'),
                                  metric.select = c('ROC'),
                                  metric.select.thresh = c(0.7),
                                  metric.eval = c('TSS', 'ROC'),
                                  var.import = 2,
                                  EMci.alpha = 0.05,
                                  EMwmean.decay = 'proportional')
  
  ## Ensemble performance
  emod_eval <- get_evaluations(emod)
  emod_eval <- emod_eval %>%
    select(algo, merged.by.run, metric.eval, calibration, validation)
  emod_eval$algo <- "ensemble"
  colnames(emod_eval) <- c("algo", "cv_run", "metric", "calibration", "validation")
  emod_eval$diff <- emod_eval$calibration - emod_eval$validation
  write.csv(emod_eval, paste0(sdir, "/", spec_, "_ensemble_model_performance.csv"), row.names = FALSE)
  
  # Tjur's R2
  true_obs <- as.logical(obs$count)
  tjur_df <- data.frame("algo" = "ensemble", "run" = paste0("RUN", 1:3), "tjurR2" = NA)
  for(i in 1:3){
    r <- paste0("RUN", i)
    emod_preds <- get_predictions(emod) %>%
      filter(merged.by.run == r)
    mt <- mean(emod_preds$pred[which(true_obs)])/1000
    mf <- mean(emod_preds$pred[which(!true_obs)])/1000
    
    tjur_df$tjurR2[i] <- mt - mf 
  }
  write.csv(tjur_df, paste0(sdir, "/", spec_, "_ensemble_tjurR2.csv"), row.names = FALSE)
  
  ## Ensemble model ROC curves
  pdf(file = paste0(sdir, "/", gsub(" ", "_", spec), "_ensemble_ROC_curves.pdf"),
      width = cv_runs*3,
      height = 3)
  
  bm_ROC(mod, mod_methods, emod)
  dev.off()
  
  ## Response curves
  bm_plot <- bm_PlotResponseCurves(bm.out = emod, 
                                   models.chosen = get_built_models(emod),
                                   fixed.var = 'median',
                                   do.progress = FALSE,
                                   do.plot = FALSE)
  pdf(file = paste0(sdir, "/", spec_, "_response_curves.pdf"),
      width = 20,
      height = 20)
  bm_plot[["plot"]]
  dev.off()
  
  ## Predictions
  mod_proj <- BIOMOD_Projection(bm.mod = mod,
                                proj.name = proj_n,
                                new.env = preds,
                                models.chosen = 'all',
                                metric.binary = 'all',
                                metric.filter = 'all',
                                build.clamping.mask = FALSE,
                                nb.cpu = 8)
  
  plot(mod_proj)
  
  emod_proj <- BIOMOD_EnsembleForecasting(bm.em = emod,
                                          bm.proj = mod_proj,
                                          models.chosen = 'all',
                                          metric.binary = 'all',
                                          metric.filter = 'all',
                                          nb.cpu = 8)
  #plot(emod_preds)
  
  ## Export relevant files
  # Model files
  dir <- paste0("outputs/", spec_, "/models")
  if(!dir.exists(dir)){dir.create(dir)}
  mod_dir <- paste0("outputs/", resp_n, "/models/", mod_id)
  file.copy(list.files(mod_dir, full.names = T), dir, overwrite = TRUE)
  
  # Individual model projections
  mod_stack <- terra::unwrap(mod_proj@proj.out@val)
  n <- names(mod_stack)
  p <- paste0(resp_n, "_allData")
  n <- gsub(p, spec_, n)
  names(mod_stack) <- n
  dir <- paste0("outputs/", spec_, "/individual_projections")
  if(!dir.exists(dir)){dir.create(dir)}
  for(i in 1:nlyr(mod_stack)){
    writeRaster(mod_stack[[i]], filename = paste0(dir, "/", names(mod_stack)[i], ".tif"), overwrite = T)
  }
  
  # Ensemble model projections
  mod_stack <- terra::unwrap(emod_proj@proj.out@val)
  n <- names(mod_stack)
  n <- gsub(resp_n, spec_, n)
  names(mod_stack) <- n
  dir <- paste0("outputs/", spec_, "/ensemble_projections")
  if(!dir.exists(dir)){dir.create(dir)}
  for(i in 1:nlyr(mod_stack)){
    writeRaster(mod_stack[[i]], filename = paste0(dir, "/", names(mod_stack)[i], ".tif"), overwrite = T)
  }
  
  # Ensemble model projections
  dir <- paste0("outputs/", spec_, "/ensemble_projections")
  proj_list <- emod_proj@proj.out@link
  file.copy(proj_list, dir, overwrite = TRUE)
  
  # Observations, CV set & predictions
  predictions <- get_predictions(emod, model.as.col = T, algo = 'EMwmean')
  obs <- read.csv(paste0("../input_data/observations/", spec_, "_observations.csv"))
  train <- select(obs, train1, train2, train3)
  pa_predictions <- predictions
  cut <- get_evaluations(emod, metric.eval = "TSS")
  
  for(i in 1:3){
    pa_predictions[,i] <- ifelse(pa_predictions[,i] > cut$cutoff[i], 1, 0)
  }
  
  df <- data.frame("true_observation" = obs$count,
                   "train1" = train$train1,
                   "train2" = train$train2,
                   "train3" = train$train3,
                   "predictions_train1" = predictions[,1]/1000,
                   "predictions_train2" = predictions[,2]/1000,
                   "predictions_train3" = predictions[,3]/1000,
                   "pa_predictions_train1" = pa_predictions[,1],
                   "pa_predictions_train2" = pa_predictions[,2],
                   "pa_predictions_train3" = pa_predictions[,3]
  )
  
  write.csv(obs, paste0("outputs/", spec_, "/", spec_, "_observations.csv"), row.names = F)
  write.csv(train, paste0("outputs/", spec_, "/", spec_, "_CVset.csv"), row.names = F)
  write.csv(df, paste0("outputs/", spec_, "/", spec_, "_predictions.csv"), row.names = F)
  
  # Record run time
  stop_time <- Sys.time()
  run_time <- difftime(stop_time, start_time, units = "hours")
  run_df <- data.frame(species = spec, run_time = run_time, models = paste(mod_methods, collapse = ","))
  write.csv(run_df, paste0(sdir, "/", spec_, "_run_time.csv"), row.names = FALSE)
  
  # Delete biomod2 created directory
  dir <- paste0("outputs/", resp_n)
  unlink(dir, recursive = T)

}
