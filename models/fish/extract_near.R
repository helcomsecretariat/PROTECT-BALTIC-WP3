# Packages needed: terra, FNN
# grid = single layer SpatRaster
# predictors = multiple layer SpatRaster
# points = SpatVector of points

extract_near <- function(grid, predictors, points){

  vals_cells <- terra::extract(predictors, points, cells = TRUE)
  mcol <- ncol(vals_cells)-1
  vals <- vals_cells[,2:mcol]  # Extracted values
  cells <- vals_cells[,1]  # Cell numbers (for reference)
  
  # Find indices of points with NA values
  na_indices <- as.integer(rownames(vals[rowSums(is.na(vals)) > 0,]))
  
  # Coordinates of NA points
  na_points_coords <- terra::crds(points[na_indices, ])
  
  # Coordinates and values of all non-NA raster cells
  non_na_cells <- which(!is.na(terra::values(grid)))
  non_na_coords <- terra::xyFromCell(grid, non_na_cells)
  non_na_vals <- terra::extract(predictors, non_na_cells)
  
  nn <- FNN::get.knnx(non_na_coords, na_points_coords, k = 1)
  vals[na_indices,] <- non_na_vals[nn$nn.index,]
  
  return(vals)
}