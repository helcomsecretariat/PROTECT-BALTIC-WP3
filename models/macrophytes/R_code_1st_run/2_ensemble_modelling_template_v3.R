# v3 fixing swe pseudoabs generation. this is out-commented -> only oscar's p/a interpretation

start.time = Sys.time()

# parameters for testing, remove 

species_name = 'species_name_placeholder'
# species_name = "Alisma gramineum" # testing 
workdir = "models"
method = 'method_name_placeholder'
# method = "D" # testing 
nb.cpu = 1
run_name <- "1_core_96Gb_GAM_GLM_RF_CTA_XGBOOST" # move here for easier testing 

# workspace = "local" # testing 
workspace = "puhti" # production

cat(
  paste("Modelling run start time: ", start.time)
)

cat(
  paste0("Species: ", species_name, ", n.cpu = ", nb.cpu)
)

print("Loading libraries & setting parameters---")
# 1. Libraries & wd----

if (workspace == "puhti") {
  .libPaths(c("/projappl/project_2014296/project_rpackages_4.4.2", .libPaths()))
  libpath <- .libPaths()[1]
  options(encoding = "ISO-8859-1")  
}

# install.packages("blockCV", lib = libpath)

library(terra)
library(tidyterra)
library(tidyverse)
library(biomod2) # make sure to use the latest ver 
library(data.table)

### Parameters -----

# include extra absences (empty sampling sites w no vegetation)? yes/no
includeAbsences = "yes"

# include deep benchic faunal sampling absences? 
includeDeepAbsences = "yes" 

# include swedish data? for testing yes/no 
includeSweden = "yes"

# include German data? for testing yes/no. Only presences from Helcom bd database
includeGermany = "yes"

balance.Data = "no" # balance presence/absence classes. random sampling of absences (and pseudo-abs)

env.thinning = "yes" # should we thin observations by env gradients 
# country.thinning = FALSE # thin by country / dataset? Should set either of these to TRUE 

if (workspace == "puhti") {
  datafile1 = "/scratch/project_2014296/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/data_call_data_for_modelling_04_08_2025.rds"
  datafile2 = "/scratch/project_2014296/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/swedish_macrophytes_2000_2024_ver5.rds"
  absence_file = "/scratch/project_2014296/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/empty_sites_no_vegetation_FI_EE.rds.rds"
  deep_absence_file = "/scratch/project_2014296/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/deep_40m_absences_DE_DK_EE_FI.rds"
  swe_deep_absence_file = "/scratch/project_2014296/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/swe_deep_100m_grab_samples.rds"
  ger_data_file = "/scratch/project_2014296/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/german_presences_from_bd_database.rds"
  
  # raster stack for prediction 
  env.stack.location = "/scratch/project_2014296/Protect_BALTIC/WP3/data/predictors/predictor_stack.tif"
} else {
  datafile1 = "D:/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/data_call_data_for_modelling_04_08_2025.rds"
  datafile2 = "D:/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/swedish_macrophytes_2000_2024_ver5.rds"
  absence_file = "D:/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/empty_sites_no_vegetation_FI_EE.rds.rds"
  deep_absence_file = "D:/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/deep_40m_absences_DE_DK_EE_FI.rds"
  swe_deep_absence_file = "D:/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/swe_deep_100m_grab_samples.rds"
  ger_data_file = "D:/Protect_BALTIC/WP3/data/occurrence_data/processed_occurrence_data/german_presences_from_bd_database.rds"
  
  # raster stack for prediction 
  env.stack.location = "D:/Protect_BALTIC/WP3/data/predictors_250m/predictor_stack_02_07_2025/predictor_stack_02_07_2025.tif"
}

#this is fixed for all macrophytes
envarlist =  c("bpi_4000_20000_250m",
               "bpi_500_5000_250m",
               "depth_250m",
               "soft_substrate_resample_250m",
               "sand_substrate_resample_250m",
               "hard_substrate_resample_250m",
               "coarse_substrate_resample_250m",
               "slope_250m",
               "aspect_250m",
               "vector_ruggedness_3cells",
               "depth_attenuated_exposure_at_seafloor",
               "secchi_depth_mean_resample_250m",
               "secchi_depth_cv_resample_250m",
               "chloro_a_mean_resample_250m",
               "nitrate_surface_mean_resample_250m",
               "ph_surface_mean_resample_250m",
               "ph_surface_cv_resample_250m",
               "wave_significant_height_cv_resample_250m",
               "salinity_surface_mean_resample_250m",
               "sea_ice_fraction_mean_resample_250m",
               "temperature_surface_mean_resample_250m",
               "temperature_surface_cv_resample_250m"
               
)

print("Loading data---")
### read and process observations ----

# load data 
phyto.in = readRDS(datafile1) # national datasets from data call + ICES 
swe.in = readRDS(datafile2) # swedish data 
empty.sites = readRDS(absence_file) # sites with no vegetation in Fi and EE data 
deep.abs = readRDS(deep_absence_file) # depth > 40m faunal samples (observed depth)
deep.abs.swe = readRDS(swe_deep_absence_file) # grab samples from swedish data where mean depth (from raster) > 100m 
german.obs = readRDS(ger_data_file) # german presences from Helcom bd database 

# fetch number of observations per dataset
n.obs.phyto = phyto.in %>% filter(scien_name == species_name) %>% nrow()
n.obs.swe = swe.in %>% filter(taxon_name == species_name) %>% nrow()
n.obs.ge = german.obs %>% filter(scien_name == species_name) %>% nrow()

# raster stack 
env.stack = rast(env.stack.location)
env.stack = env.stack[[envarlist]] 

print("Generating absences & formatting modelling data...")
### pseudo-absence generation ----

# species absence observable by dive data only (method = "D") 
# (includes dive comparable methods like Framenet)
if (method == "D") {
  
  # presence + pseudoabsence data 
  dat.pa = phyto.in %>% 
    dplyr::filter(
      (country == "EE" & samp_metho %in% c("Frame sample", "FRAMENET", "DIV")) |
        (country == "EE" & method_cat == "Diver") |
        (country == "FI" & samp_metho == "Visual sample" & method_cat == "Vegetation mapping") |
        (country == "LT" & samp_metho == "FRAMENET") |
        (country == "PL" & samp_metho == "FRAMENET") |
        (country == "SE" & samp_metho == "FRAMENET")
    ) 
  
  # group by lat, long and year (could be date but we'll go for the same as swedish data )
  # calculate mean abundance per investigation. We convert the data to p/a so just 
  # brutally summarise over different methods 
  
  # create a grouping id 
  dat.pa = dat.pa %>% 
    dplyr::mutate(
      investig_id = paste(lat, lon, year, sep = "_")
    )
  
  # env vars 
  dat.pa.env = dat.pa %>% 
    dplyr::select(all_of(c("investig_id", envarlist))) %>% 
    unique() # 255 865 rows without unique, 71646 otherwise 
  
  # summarise all species obs at specific site at year y
  dat.pa.grouped = dat.pa %>% 
    dplyr::group_by(lat, lon, year, scien_name, investig_id) %>% 
    dplyr::summarise(quantity = 1)  # all quantities are positive
  
  # on some rare species we might need to round the coordinates -> will 
  # increase prevalence of rare species 
  
  # widen & create pseudoabsences
  dat.pa.grouped = dat.pa.grouped %>% 
    pivot_wider(
      names_from = scien_name, 
      values_from =  quantity, 
      values_fill = 0
    )
  
  # if the species has been observed in the data, fetch the obs & pseudoabs 
  if (n.obs.phyto > 0) {
    
    # join with env data 
    dat.pa = full_join(
      dat.pa.grouped %>% dplyr::select(all_of(c(species_name, "lat", "lon", "year", "investig_id"))), 
      dat.pa.env
    )
    colnames(dat.pa)[1] <- "y"  
    
  } else { # if the species has not been observed in the data, all the obs are 0 
    
    dat.pa.grouped = dat.pa.grouped %>% dplyr::select(all_of(c("lat", "lon", "year", "investig_id")))
    dat.pa.grouped$y <- 0
    dat.pa.grouped <- dat.pa.grouped[,c("y", "lat", "lon", "year", "investig_id")]
    
    dat.pa = full_join(
      dat.pa.grouped, 
      dat.pa.env
    )
  }
  
  rm(dat.pa.env, dat.pa.grouped)
  
  # swedish data, species observable by video 
  # create pseudo abs for these, remove pres abs from swe data 
  
  ### pseudo-absence generation from dive/scrape data -- 
  # if(n.obs.swe > 0) {
  #   
  #   swe.dive = swe.in %>% 
  #     filter(method_no %in% c(5,7,8,9) & origin_id != -1) # -1 -> "singular" obs not for pseudo abs generation 
  #   
  #   swe.in = swe.in %>% filter(!(method_no %in% c(5,7,8,9) & origin_id != -1))
  #   
  #   # all potential sites where the species could have been observed by the method
  #   # & their env data values, set presence -> 0 as (some of) these will be absences 
  #   swe.env = swe.dive[,c("site_id", envarlist)] %>%
  #     unique() %>% # don't duplicate absences per site id 
  #     mutate(presence = 0, 
  #            taxon_name = species_name
  #     ) 
  #   
  #   # where the species has been observed (site_id)
  #   swe.dive.pa = swe.dive[,c("presence", "taxon_name", "site_id", envarlist)] %>% 
  #     filter(taxon_name == species_name)
  #   
  #   # filter out the observed sites from the generated absences & env vars 
  #   swe.env = swe.env %>% 
  #     filter(!site_id %in% swe.dive.pa$site_id)
  #   
  #   swe.pa = rbind(swe.env, swe.dive.pa)
  #   rm(swe.env, swe.dive.pa)
  #   
  # }
}


# species which are observable on dive and video data (method = "DV") 
if (method == "DV") {
  
  # presence + pseudoabsence data 
  dat.pa = phyto.in %>% 
    dplyr::filter(
      (country == "EE" & samp_metho %in% c("Frame sample", "FRAMENET", "DIV", "VID-DROP", "Visual sample")) |
        (country == "EE" & method_cat == "Diver") |
        (country == "FI" & samp_metho == "Visual sample") |
        samp_metho %in% c("VID", "FRAMENET")
    ) 
  
  # group by lat, long and year (could be date but we'll go for the same as swedish data )
  # calculate mean abundance per investigation. We convert the data to p/a so just 
  # brutally summarise over different methods 
  
  # create a grouping id 
  dat.pa = dat.pa %>% 
    mutate(
      investig_id = paste(lat, lon, year, sep = "_")
    )
  
  # env vars 
  dat.pa.env = dat.pa %>% 
    dplyr::select(all_of(c("investig_id", envarlist))) %>% 
    unique() 
  
  # summarise all species obs at specific site at year y
  dat.pa.grouped = dat.pa %>% 
    dplyr::group_by(lat, lon, year, scien_name, investig_id) %>% 
    dplyr::summarise(quantity = 1)  # all quantities are positive
  
  # on some rare species we might need to round the coordinates -> will 
  # increase prevalence of rare species 
  
  # widen & create pseudoabsences
  dat.pa.grouped = dat.pa.grouped %>% 
    pivot_wider(
      names_from = scien_name, 
      values_from =  quantity, 
      values_fill = 0
    )
  
  # if the species has been observed in the data, feth the obs & pseudoabs 
  if (n.obs.phyto > 0) {
    
    # join with env data 
    dat.pa = full_join(
      dat.pa.grouped %>% dplyr::select(all_of(c(species_name, "lat", "lon", "year", "investig_id"))), 
      dat.pa.env
    )
    colnames(dat.pa)[1] <- "y"  
    
  } else { # if the species has not been observed in the data, all the obs are 0 
    
    dat.pa.grouped = dat.pa.grouped %>% dplyr::select(all_of(c("lat", "lon", "year", "investig_id")))
    dat.pa.grouped$y <- 0
    dat.pa.grouped <- dat.pa.grouped[,c("y", "lat", "lon", "year", "investig_id")]
    
    dat.pa = full_join(
      dat.pa.grouped, 
      dat.pa.env
    )
  }
  
  rm(dat.pa.env, dat.pa.grouped)
  
  ### pseudo-absence generation from dive/scrape + video data -- 
  # if(n.obs.swe > 0) {
  #   
  #   swe.dive.video = swe.in %>% 
  #     filter(method_no %in% c(5:11, 13) & origin_id != -1) # -1 -> "singular" obs not for pseudo abs generation 
  #   
  #   swe.in = swe.in %>% filter(!(method_no %in% c(5:11, 13) & origin_id != -1))
  #   
  #   # all potential sites where the species could have been observed by the method
  #   # & their env data values, set presence -> 0 as (some of) these will be absences 
  #   swe.env = swe.dive.video[,c("site_id", envarlist)] %>%
  #     unique() %>% # don't duplicate absences per site id 
  #     mutate(presence = 0, 
  #            taxon_name = species_name
  #     ) 
  #   
  #   # where the species has been observed (site_id)
  #   swe.dive.video.pa = swe.dive.video[,c("presence", "taxon_name", "site_id", envarlist)] %>% 
  #     filter(taxon_name == species_name)
  #   
  #   # filter out the observed sites from the generated absences & env vars 
  #   swe.env = swe.env %>% 
  #     filter(!site_id %in% swe.dive.video.pa$site_id)
  #   
  #   swe.pa = rbind(swe.env, swe.dive.video.pa)
  #   rm(swe.env, swe.dive.video.pa)
  #   
  # }
  
}

# create the modelling data

data.in = dat.pa



### add observed presences and absences ---- 

### presence data (observed presences but can't infer absences)
### rakes, tube corers etc 

# from national datasets + ICES 
dat.p = phyto.in %>% 
  dplyr::filter(
    samp_metho %in% c("Grab sampler", "Rake", "DTR", "BRNG")
  ) %>% 
  dplyr::filter(scien_name == species_name) %>% 
  dplyr::mutate(
    investig_id = paste(lat, lon, year, sep = "_"), 
    quantity = 1
  )

if (nrow(dat.p) > 0) {
  
  dat.p = dat.p %>% 
    dplyr::select(all_of(c("quantity", "investig_id", "lat", "lon", "year", envarlist))) 
  colnames(dat.p)[1] <- "y"
  
  data.in = rbind(data.in, dat.p)
  rm(phyto.in)
  
}

# German presences from Helcom db 
if (includeGermany == "yes") {
  
  dat.ge = german.obs %>% 
    dplyr::filter(scien_name == species_name) %>% 
    dplyr::mutate(
      investig_id = paste(lat, lon, year, sep = "_"), 
      quantity = 1
    ) %>% 
    dplyr::select(all_of(c("quantity", "investig_id", "lat", "lon", "year", envarlist))) 
  
  if (nrow(dat.ge) > 0) {
    colnames(dat.ge)[1] <- "y"
    data.in = rbind(data.in, dat.ge)
  }
}

rm(german.obs)

# True absences 
### add true absences? (no species recorded at sampling site) 
if (includeAbsences == "yes") {
  
  dat.empty = empty.sites %>% 
    dplyr::mutate(
      investig_id = paste(lat, lon, year, sep = "_"), 
      quantity = 0
    ) %>% 
    dplyr::select(all_of(c("quantity", "investig_id", "lat", "lon", "year", envarlist))) 
  colnames(dat.empty)[1] <- "y"
  
  data.in = rbind(data.in, dat.empty)
  rm(empty.sites)
  
} 

# add deep (> 40m) faunal grab samples with no vegetation
if (includeDeepAbsences == "yes") {
  
  # FI, DK, DE, EE 
  dat.deep.abs = deep.abs %>% 
    dplyr::mutate(
      investig_id = paste(lat, lon, year, sep = "_")
    ) %>% 
    dplyr::select(all_of(c("quantity", "investig_id", "lat", "lon", "year", envarlist))) %>% 
    dplyr::mutate(quantity = 0)
  
  colnames(dat.deep.abs)[1] <- "y"
  data.in = rbind(data.in, dat.deep.abs)
  rm(deep.abs)
  
  #SWE 
  dat.deep.abs.swe = deep.abs.swe %>%  
    dplyr::select(all_of(c("presence", "year", envarlist, "site_id"))) %>% 
    dplyr::mutate(presence = 0) %>% 
    dplyr::rename("investig_id" = "site_id") %>% # to match with others 
    dplyr::mutate(
      investig_id = as.character(investig_id)
    )
  colnames(dat.deep.abs.swe)[1] <- "y"
  
  data.in = rbind(data.in, dat.deep.abs.swe)
  rm(deep.abs.swe)
  
}


### including swedish data ----

# what is the method of absence generation? Swe data has only "true" absences -> effect  on prevalence? 
# prevalence of Fucus is about 13% with presence, absence, and pseudoabs data 
# but swedish data has Fucus prevalence of 24 %
# which will distort the probabilities towards sweden 

if (includeSweden == "yes") {
  
  swe.pa.data = swe.in %>% 
    filter(taxon_name == species_name) %>% 
    select(all_of(c("presence", "year", envarlist, "site_id"))) %>% 
    rename("investig_id" = "site_id")# to match with others 
  
  swe.pa.data$investig_id = as.character(swe.pa.data$investig_id)
  colnames(swe.pa.data)[1] <- "y"
  
  data.in = rbind(data.in, swe.pa.data)
  rm(swe.in)
  
}

# print data rows to log 
print(paste("Added national & ICES presences", sum(dat.pa$y)))
print(paste("Added national & ICES pseudo-absences", (nrow(dat.pa) - sum(dat.pa$y))))
print(paste("Added national (FI, EE, DK, DE) & ICES", species_name, "presences (rakes, corers etc):", nrow(dat.p)))
print(paste("Added German presences from bd database:", species_name, ":", nrow(dat.ge)))
print(paste("Added empty sites (no vegetation, FI, EE):", nrow(dat.empty)))
print(paste("Added deep absences from faunal grab samples (FI, EE, DE, DK):", nrow(dat.deep.abs)))
print(paste("Added deep absences from faunal grab samples (SE)", nrow(dat.deep.abs.swe)))
print(paste("Added", species_name, "presence/absence observations from SGU (SE):", nrow(swe.pa.data)))

rm(dat.pa, dat.p, dat.ge, dat.empty, dat.deep.abs, dat.deep.abs.swe)


# remove rows of data with empty env var valus (swe data) 
data.in = data.in %>% 
  filter(!is.na(salinity_surface_mean_resample_250m))

# print n presences / absences 
n.presences = sum(data.in$y)
n.absences = nrow(data.in)-sum(data.in$y)
prevalence = (n.presences/(n.presences + n.absences)) %>% round(digits = 3)

print(paste("Number of presences before env thinning: ", n.presences))
print(paste("Number of absences before env thinning: ", n.absences))
print(paste("Prevalence before env thinning: ", prevalence))

gc()

# balance the data? prevalence = 0.5 ----
# for testing, does this make sense. should we condition by prevalence?  
if(balance.Data == "yes") {
  print("Balancing the dataset ---")
  
  n.pres = sum(data.in$y)
  n.abs = nrow(data.in)
  
  pres.data = data.in[data.in$y == 1,]
  abs.data = data.in[data.in$y == 0,]
  
  abs.data = abs.data[sample(1:nrow(abs.data), size = n.pres),]
  
  data.in = rbind(pres.data, abs.data)
  data.in = data.in[sample(1:nrow(data.in), size = nrow(data.in)),]
  
  rm(pres.data, abs.data)
}

### environmental thinning? 

if (env.thinning == "yes") {
  print("Environmental thinning ---")
  
  # Ants's code ---
  
  preds = data.in[,envarlist]
  fulldata = data.in[,!colnames(data.in) %in% envarlist]
  
  large=30000 #when do we consider using sampling from the full dataset necessary
  clustno=100 #how many clusters
  set.seed(1000)
  if(dim(preds)[1]>large){ #environmentally balanced sampling if the amount of data is too large
    clust=kmeans(scale(preds),centers=clustno)
    dataclust=data.table(nr=1:dim(preds)[1],clustnr=clust$cluster) #clust$cluster gives cluster number
    selection=dataclust[,sample(nr,min(round(large/clustno),.N)),clustnr]$V1 #large/clustno gives how many from each cluster
    fulldata=fulldata[selection,]
    preds=preds[selection,]
  }
  
  data.in = cbind(fulldata, preds)
  
  # print n presences / absences 
  n.presences = sum(data.in$y)
  n.absences = nrow(data.in)-sum(data.in$y)
  prevalence = (n.presences/(n.presences + n.absences)) %>% round(digits = 3)
  
  print(paste("Number of presences after env thinning: ", n.presences))
  print(paste("Number of absences after env thinning: ", n.absences))
  print(paste("Prevalence after env thinning: ", prevalence))
  
}


data.out = tibble(
  species = species_name, 
  prevalence = sum(data.in[,1])/nrow(data.in), 
  n.pres = sum(data.in[,1]), 
  n.abs = nrow(data.in) - sum(data.in[,1])
)



########## Modelling ########## ----
print("Modelling start ---")

######## Model Settings ##########

spec_ <- gsub(" ", "_", species_name) # format for saving files
spec <- gsub(" ", ".", species_name) # format for biomod 
spec <- gsub("_", ".", spec) # 
if(nchar(spec) > 25){spec <- substr(spec, 1, 25)} # shorten name if too long (issues writing files)

date_stamp = format(Sys.Date(), "%d%m%Y")
mod_id <- paste(spec_, date_stamp, sep = "_") # Unique ID for model runs
# run_name <- "All data, absencences generated across all data, relevant predictors including CV, Bigboss default settings, except custom XGBoost"
# Models to include in the ensemble
#mod_methods <- c("GAM", "GLM", "RF", "GBM", "XGBOOST", "CTA", "MARS") # all options
mod_methods <- c("RF", "XGBOOST", "CTA", "GAM", "GLM") # Subset
# mod_methods <- c("GAM", "GLM") # Subset
n_rep <- 5 # Number of cross-validation repetitions
cv_perc <- 0.7 # Percentage of training/testing data

pdf(NULL)
start_time <- Sys.time()

# working dir 
if (workspace == "puhti") {
  setwd("/scratch/project_2014296/Protect_BALTIC/WP3/models/")
} else {
  setwd(paste0("D:/Protect_BALTIC/WP3/", workdir))
}

# Create subdirectories
if(!dir.exists("outputs")){dir.create("outputs")} # Main outputs from biomod for use in the quarto document
if(!dir.exists("outputs_extra")){dir.create("outputs_extra")} # Extra outputs for quick evaluation
# start_dir <- paste0("outputs_extra/", mod_id, "/_run_started")
# finish_dir <- paste0("outputs_extra/", mod_id, "/_run_finished")
# if(!dir.exists(start_dir)){dir.create(start_dir, recursive = TRUE)}
# if(!dir.exists(finish_dir)){dir.create(finish_dir, recursive = TRUE)}

# write.csv(NA, paste0(start_dir, "/", spec)) # Record that run was initiated in _run_started folder
# spec_list$run_initiated[spec_list$scientific_name == spec] <- 1 
#write.csv(spec_list, "spec_list.csv", row.names = FALSE)

# Biomod file/folder names and creation of output folders
resp_n <- spec
proj_n <- mod_id
out_dir <- paste0("outputs/", mod_id) 
if(!dir.exists(out_dir)){dir.create(out_dir, recursive = TRUE)}
extra_dir <- paste0("outputs_extra/", mod_id, "/", spec)
if(!dir.exists(extra_dir)){dir.create(extra_dir, recursive = TRUE)}

colnames(data.in)[1] <- resp_n
data.in = as.data.frame(data.in)
data.in$quantity = data.in[,1]

# Presence/absence map export

# # grid for plotting 
# grid <- rast("..//data/predictors_250m/grid_marine_250m_v2.tif")
# 
# # points for plotting 
# xy <- vect(data.in[, c("lon", "lat")], geom=c("lon", "lat"), crs = "EPSG:4326")
# xy <- project(xy, grid)
# 
# # plot (ED) 
# clrs <- ifelse(data.in[,1]== 1, "red", "black")
# #temp_plot <- plot(xy, col = clrs, pch = 16, cex = 1.5)
# pdf(file = paste0(extra_dir, "/", spec_, "_point_map.pdf"),
#     width = 6,
#     height = 6)
# plot(grid, col = "lightgrey")
# plot(xy, col = clrs, pch = 16, cex = 0.3, add = TRUE)
# dev.off()
# 
# tiff(file = paste0(extra_dir, "/", spec_, "_point_map.pdf"),
#      width = 6,
#      height = 6, compression = "lzw")
# plot(grid, col = "lightgrey")
# plot(xy, col = clrs, pch = 16, cex = 0.3, add = TRUE)
# dev.off()

## Prepare model
dat <- BIOMOD_FormatingData(resp.var = data.in[,resp_n],
                            expl.var = data.in[,envarlist],
                            resp.name = resp_n,
                            dir.name = out_dir,
                            resp.xy = data.in[,c("lon", "lat")]
)

# Set training/testing data
part <- caret::createDataPartition(data.in$quantity, times = n_rep, list = FALSE, p = cv_perc)
calib <- matrix(nrow = length(data.in$quantity), ncol = n_rep)
calib <- as.data.frame(calib)
colnames(calib) <- paste0("_allData_RUN", 1:n_rep)

for(i in 1:n_rep){
  calib[,i] <- FALSE
  calib[part[,i],i] <- TRUE
}

print("Biomod cv ---")
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

print("Building single models ---")
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
                       do.progress = FALSE, 
                       nb.cpu = nb.cpu)


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
print("Ensemble modelling ---")
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
                                EMwmean.decay = 'proportional', 
                                nb.cpu = nb.cpu)

# Response curves plot export
bm_plot <- bm_PlotResponseCurves(bm.out = emod, 
                                 models.chosen = get_built_models(emod)[1],
                                 fixed.var = 'median',
                                 do.progress = FALSE,
                                 do.plot = FALSE)
temp_plot <- bm_plot[["plot"]]
pdf(file = paste0(extra_dir, "/", spec_, "_response_curves.pdf"),
    width = length(envarlist)*1.4,
    height = length(envarlist)*0.9)
print(temp_plot)
dev.off()

# Variable importance plot export
bm_plot <- bm_PlotVarImpBoxplot(mod,
                                group.by = c("algo", "expl.var", "expl.var"),
                                do.plot = TRUE)

temp_plot <- bm_plot[["plot"]]
pdf(file = paste0(extra_dir, "/", spec_, "_var_importance.pdf"),
    width = length(envarlist)*1.4,
    height = length(envarlist)*0.9)
print(temp_plot)
dev.off()

### spatial prediction ---- 

# remove unused data? 
# rm(data.in)
gc()

mod_proj <- BIOMOD_Projection(bm.mod = mod,
                              proj.name = proj_n,
                              new.env = env.stack,
                              models.chosen = mod_select,
                              metric.binary = 'TSS',
                              build.clamping.mask = FALSE,
                              keep.in.memory = FALSE,
                              do.stack = FALSE, # Check to see if this help reduce memory usage
                              omit.na = FALSE, 
                              nb.cpu = nb.cpu
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

end.time = Sys.time()
elapsed = end.time-start.time

elapsed <- as.duration(elapsed)

print(
  paste("Modelling run end time: ", end.time, "elapsed: ", print(elapsed))
)

# write modelling time to disk 

data.out2 = tibble(
  elapsed_time = elapsed,
  algorithms = stringr::str_flatten(string = (mod_methods), collapse = ", "), 
  cv_rep = n_rep
)

data.out = cbind(data.out, data.out2)

## move log, R and sh files to out folder 

file.copy(from = paste0(spec_, ".R"), to = paste0(out_dir, "/", mod_id, ".R"))
file.copy(from = paste0(spec_, ".sh"), to = paste0(out_dir, "/", mod_id, ".sh"))
file.copy(from = paste0(spec_, "_ERROR.txt"), to = paste0(out_dir, "/", mod_id, "_ERROR.txt"))
file.copy(from = paste0(spec_, "_OUT.txt"), to = paste0(out_dir, "/", mod_id, "_OUT.txt"))

file.remove(paste0(spec_, ".R"))
file.remove(paste0(spec_, ".sh"))
file.remove(paste0(spec_, "_ERROR.txt"))
file.remove(paste0(spec_, "_OUT.txt"))

# if successfull, write modelling summary to name.txt 

write.csv2(t(data.out) %>% 
             as.data.frame() %>% 
             rownames_to_column(var = "variable"), 
           file = paste0("tracking/", mod_id, "_modelling_summary.csv")
           )

