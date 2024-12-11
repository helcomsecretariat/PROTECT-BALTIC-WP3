# mod = BIOMOD.models.out object
# mod_methods = vector of algos, e.g. c("GBM", "XGBOOST")

require(pROC)

bm_ROC <- function(mod, mod_methods, ensemble = NULL){
  
  if(class(ensemble)[1] == "BIOMOD.ensemble.models.out"){
    ens <- TRUE} else {ens <- FALSE}
  
  obs <- get_formal_data(mod)@data.species
  train <- get_calib_lines(mod)
  if(any(colnames(train) == "_allData_allRun")){
    train <- train[ , -which(colnames(train) %in% c("_allData_allRun"))]
  }
  cv_runs <- ncol(train)
  
  if(ens){mod_methods <- "ensemble"}
  
  par(mfrow = c(length(mod_methods), cv_runs), mar = c(12, 5, 5, 5))

  for(i in 1:length(mod_methods)){
    for(j in 1:ncol(train)){
      if(ens){
        predictions <- get_predictions(ensemble, model.as.col = T, algo = NULL)
      } else {
        predictions <- get_predictions(mod, model.as.col = T, algo = mod_methods[i])
      }
      predictions <- predictions / 1000
      p_train <- predictions[train[,j],j]
      p_test <- predictions[!train[,j],j]
      r_train <- obs[train[,j]]
      r_test <- obs[!train[,j]]
      
      n <- paste0(mod_methods[i], "_RUN", j)
      pROC::plot.roc(r_train, p_train, col = "blue", main = n, quiet = T)
      pROC::plot.roc(r_test, p_test, col = "red", add = T, quiet = T)
      mtext("Train", col = "blue", adj = 0.95, side = 1, line = 5, cex = 0.7)
      mtext("Test", col = "red", adj = 0.95, side = 1, line = 6, cex = 0.7)
    }
  }
}