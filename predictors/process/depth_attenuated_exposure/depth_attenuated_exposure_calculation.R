library(terra)
library(tidyverse)


# Calculating wave exposure at seafloor (depth attenuated exposure), based on 
# depth and surface wave exposure. 
# AT 31.3.2025
# The method is described in Bekkby et al. 2008:
# Bekkby, T., P. E. Isachsen, M. Isæus, and V. Bakkestuen. 2008. GIS modeling of
# wave exposure at the seabed: a depth-attenuated wave exposure model. Marine Geodesy 31:117-127.

# input rasters: 

# Protect Baltic depth raster
r.depth = terra::rast("data/predictors_250m/predictor_stack.tif", lyrs = 10)

# surface wave exposure raster
r.swm = terra::rast("data/predictors_250m/Syke_predictors/Surface_wave_exposure/swm_250m_PB_grid.tif")

# 22^2 = 484 

# kp raster
r.kp = terra::app(
  r.swm, 
  fun = function(x) {
    (484*((1/x)^(2/3))*(9.82^(1/3)))
  }
)

# plot(r.swm)
# plot(r.kp)

# wave exposure at seafloor 

# combine depth, swm and kp layers to a stack 
r.d.s.kp = c(r.depth, r.swm, r.kp) 

# wave exposure at seafloor raster 
# -> apply function to stack with lapp
swm.depth = terra::lapp(
  r.d.s.kp,
  fun = function(depth,swm,kp) {
    
    r.out = swm*exp(kp*depth*-1) # depth should be negative (-> -1)
    return(r.out)
    
  }
)

# plot(r.depth)
# plot(swm.depth)

# focalize to match the shoreline of PB grid 

swm.depth.f = terra::focal(swm.depth, 
               w= 3, 
               fun = mean, 
               na.rm = T, 
               na.policy = "only"
)


for (i in 1:50) {
  swm.depth.f = terra::focal(swm.depth.f, 
                 w= 3, 
                 fun = mean, 
                 na.rm = T, 
                 na.policy = "only"
  )
}

# plot(swm.depth.f)

# mask to grid 
swm.depth.f = terra::mask(swm.depth.f, r.depth)

# there is an area in Denmark which is super sheltered and out of grid 

# all the cells which don't have values after focal 
temp = terra::mask(r.depth, swm.depth.f, inverse = T)

# set these manually to 0 
temp[cells(temp)] <- 0

# merge to focalized raster 
swm.depth.f.merge = terra::merge(swm.depth.f, temp)

# plot(swm.depth.f)
# plot(swm.depth.f.merge)

# write out the raster 
names(swm.depth.f.merge) = "depth_attenuated_exposure"
dir.create("data/predictors_250m/Syke_predictors/Depth_attenuated_exposure/output")

terra::writeRaster(swm.depth.f.merge, 
            filename = "data/predictors_250m/Syke_predictors/Depth_attenuated_exposure/depth_attenuated_exposure_at_seafloor.tif", 
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"), 
            NAflag = -3.4e+38, 
            overwrite=TRUE
            )









