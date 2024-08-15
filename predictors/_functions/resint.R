# nit is the number of iterations through which the raster will "expand", i.e.
# the moving window will move. You will need more iterations if you want to
# extrapolate to cells distant from the existing data.
# The win_size is the radius as the number of pixels around the central pixel
# from which the mean values are taken in the moving window. It is best to
# experiment with this value to see what works best.


resint <- function(source, grid, nit = 100, win_size = 2, method = "cubic"){

  win_size <- terra::res(grid)[1] * win_size
  source <- terra::project(source, grid, method = method, progress = 0)

  for(i in 1:nit){
    sp <- source
    w <- terra::focalMat(source, win_size, "circle")
    w[w > 0] <- 1
    f <- terra::focal(source, w = w, fun = "mean", na.policy = "only", na.rm=TRUE)
    source[is.na(source)] <- f[is.na(source)]
    source[is.na(grid)] <- NA
    perc <- (i/nit)*100
    cat("\f")
    cat("\r", perc, "% complete")

    if(identical(terra::values(sp), terra::values(source))){
      cat("\f") 
      cat("\r100% complete")
      cat("\nExtrapolation complete after", i-1, "iterations")
      break
      }
  }

  if(!identical(terra::values(sp), terra::values(source))){
    cat("\n")
    warning("Reached max iterations without finishing extrapolation. Consider
            increasing iteration number.", call. = FALSE)
    }
  return(source)

}
