l <- list.files("inputs/temp", full.names = TRUE)
r <- rast(l[14])
#r <- r[[270:360]]
r <- app(r, fun = "mean")
temp <- r

temp <- project(temp, grid, method = "near")
temp <- temp*grid

writeRaster(temp, "C:/GitHub/EdSacre/rdir/sandbox_r/gis_temp/predictors/SST_error_lakes.tif")
