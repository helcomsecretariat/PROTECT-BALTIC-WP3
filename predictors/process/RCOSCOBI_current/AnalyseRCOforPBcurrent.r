setwd("/scratch/project_2013009/Data")
# Load required libraries


library(ncdf4)
library(abind, include.only = c("abind"))

source("./scripts/process_data_forPB.R") # Script for processing
source("./scripts/save_to_nc_forPB.R") # Script for saving
source("./scripts/Bottom_values.R") # Script for bottom values

load("./Input/depth_indices.RData")
load("./Input/NAids.RData")

# List all .nc files
file_paths <- list.files(path = "./dataset_RCP85_SSP2_Mean/Data/", pattern = "*.nc", full.names = TRUE)
check_nc <- c('.nc')

files <- file_paths[grepl(paste0("(", paste(check_nc, collapse="|"), ")$"), basename(file_paths))]

# Define variables to process and their conditions
variables <- list(
  VAR165 = "SigWaveHeight",
    VAR004 = "temp",
   VAR005 = "salinity",
   VAR016 = "nitrate",
   VAR017 = "phosphate",
   VAR018 = "oxygen",
  VAR011 = "chlorophyll",
  VAR051 = "SeaIceThick",
  VAR050 = "SeaIceConc"
)

# Define months for selection
annual <- c(1:12)

# depths for surface
selected_depth <- 1

for (ii in 1:length(variables)) {

  var <- names(variables)[ii]
  var_name <- variables[var]
  
   ## Surface
   data_out <- process_data_forPB(file_path4=files, varname=var, time_period = annual, depth = selected_depth,naids)
    
   # Save output with appropriate filename
   output_filename <- paste0("./Output/PB/",var_name, "_PB_", 
                             min(data_out$years), "-", max(data_out$years), 
                             "_", 
                             min(data_out$months), "-", max(data_out$months),
                             "_depth_Surface.nc")
   save_to_nc_forPB(data_out,output_filename, var)
   
  ## Bottom
  data_out2 <- process_data_forPB(file_path4=files, varname=var, time_period = annual, depth = depth_indices,naids)
     
  # Save output with appropriate filename
  output_filename2 <- paste0("./Output/PB/",var_name, "_PB_",
                            min(data_out2$years), "-", max(data_out2$years),
                            "_",
                            min(data_out2$months), "-", max(data_out2$months),
                            "_depth_Bottom.nc")
     
  save_to_nc_forPB(data_out2,output_filename2, var)
}

library(terra)
library(stringr)

grid <- rast("./Input/PB_grid_250m.tif")


source("./scripts/resint.R") # Script for resampling

## Get lat lon
load("./Input/RCO_Coordinates.RData")

startfiles <- list.files(path = "./Output/PB", pattern = "*.nc", full.names = TRUE)
get_resampled <- function(ncfile,varname,  outputfilename, rco.lon, rco.lat ){
  mean_SSS <- ncvar_get(ncfile, varname)
  rownames(mean_SSS) <- rco.lon
  colnames(mean_SSS) <- rco.lat
  XYZ <- as.data.frame(as.table(as.matrix(mean_SSS)))
  r <- rast(XYZ)
  projrco <- "+proj=longlat +zone=35 +north +ellps=WGS84 +datum=WGS84 +no_defs"
  crs(r) <- projrco 
  r_1000<- project(r, crs(grid), res=c(1000,1000),method = "cubic")
  mp_res <- resint(r_1000, grid, nit = 50, win_size = 4, method = "cubic")
  writeRaster(mp_res, paste0("./EnvLayers/PB_250_",outputfilename), overwrite = TRUE, 
              datatype = "FLT4S", gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"))
}

for(jj in 1:length(startfiles)){
  ncfile <- nc_open(startfiles[jj])
  print(startfiles[jj])
  outputfilename <- paste0(str_sub(basename(startfiles[jj]),1,-4),"_PBmean.tif")
  get_resampled(ncfile,"mean",  outputfilename, rco.lon, rco.lat)

    outputfilename <- paste0(str_sub(basename(startfiles[jj]),1,-4),"_PBsd.tif")
    get_resampled(ncfile,"sd",  outputfilename, rco.lon, rco.lat)

  nc_close(ncfile)
}
