#' This function automatically defines a threshold value in order to create a
#' binary raster. Needs an index GeoTIFF raster and produce a GeoTIFF binary
#' output. This function requires the "raster" package.
#' Example usage:
#' input_file <- "./Output/S2A2A_20210619_122_AOI_BOA_10_MNDWI.tif"
#' output_file <- "Binary.tif"
#' IndexRasterThresholding(input_file, output_file)

IndexRasterThresholding <- function(input_file, output_file) {
  # Load the index GeoTIFF.
  IndexRaster <- raster(input_file)
  # Compute the histogram.
  HistValues <- raster::hist(raster::values(IndexRaster),
                             breaks = 200, plot = FALSE)
  # Compute the cumulative sum of frequencies.
  CumsumFreq <- base::cumsum(HistValues$counts)
  # Compute the cumulative sum of intensities.
  CumsumIntensity <- base::cumsum(HistValues$mids * HistValues$counts)
  # Compute between-class variance.
  BetweenClassVariance <- (CumsumIntensity^2) / CumsumFreq -
                          ((CumsumIntensity / CumsumFreq)^2)
  # Find the threshold that maximizes between-class variance.
  Threshold <- HistValues$mids[which.max(BetweenClassVariance)]
  # Apply the threshold to classify water and non-water pixels.
  Binary <- IndexRaster > Threshold
  # Save the binary image.
  writeRaster(Binary, filename = output_file,
              format = "GTiff", overwrite = TRUE)
}
