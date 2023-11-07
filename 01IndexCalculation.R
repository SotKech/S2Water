#'
#'                      MGI Internship  :   S2Water - 01IndexCalculation.R
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-06-21
#'                      Last update     :   2023-08-30
#'                      R Version       :   4.3.1
#'                      Packages        :   raster, ggplot2, sf
#'                      LICENSE         :   CC BY-NC-SA 4.0
#'

                            #### Package Import ####
#  pkgTest is a function that loads packages and installsthem only when they
#  are not installed yet.
pkgTest <- function(x) {
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}

neededPackages <- c("raster", "ggplot2", "sf")              # Necessary Packages

for (package in neededPackages) {
  pkgTest(package)
}

                          #### Set up directories ####
getwd()
# setwd("C:/Projects/S2Water")                          # Set working directory.
# Get a list of all TIF files in the working directory.
tif_files <- list.files(path = "./Data/BOA", pattern = "\\.tif$",
                        full.names = TRUE)
AOI_b <- sf::st_read("./Data/Berambadi_reservoirs.geojson")

                           #### Indices functions ####
calculate_NDVI <- function(B4, B8) {
  NDVI <- (B8 - B4) / (B8 + B4)
  return(NDVI)
}
calculate_SWI <- function(B5, B11) {
  SWI <- (B5 - B11) / (B5 + B11)
  return(SWI)
}
calculate_NDWI <- function(B3, B8) {
  NDWI <- (B3 - B8) / (B3 + B8)
  return(NDWI)
}
calculate_MNDWI <- function(B3, B11) {
  MNDWI <- (B3 - B11) / (B3 + B11)
  return(MNDWI)
}
calculate_AWEI <- function(B3, B8, B11, B12) {                                   
  AWEI <- (0.004 * (B3 - B11) - (0.00025 * B8 + 0.00275 * B12))        # AWEInsh
  return(AWEI)      # Also AWEIsh: B2 + 2.5 * B3 - 1.5 * (B8 + B11) - 0.25 * B12
}
calculate_LSWI <- function(B8A, B11) {
  MNDWI <- (B8A - B11) / (B8A + B11)
  return(MNDWI)
}
calculate_MBWI <- function(B3, B4, B8, B11, B12) {
  MBWI <- 2 * B3 - B4 - B8 - B11 - B12
  return(MBWI)
}
calculate_cloud  <- function(B1) {                                   
  B1_1500 <- B1[[1]] >= 1500
  return(B1_1500)
}

                          #### Create Index Images ####
i = 0
# Loop through each TIF files
for (tif_file in tif_files) {
  i = i + 1                                            # Create progression bar.
  j = round((i/as.double(length(tif_files))*100), 2) ; cat(paste0("\r", j, "%"))
  # Load the image
  image <- raster::stack(tif_file)
  # Extract the necessary bands.
  B1  <- image[[1]]  ;  B3  <- image[[3]] ; B4  <- image[[4]]
  B5  <- image[[5]]  ;  B8  <- image[[8]] ; B8A  <- image[[9]]
  B11 <- image[[10]] ;  B12 <- image[[11]];

  # Calculate indices.
  NDVI    <- calculate_NDVI(B4, B8)   ;  SWI   <- calculate_SWI(B5, B11)
  NDWI    <- calculate_NDWI(B3, B8)   ;  MNDWI <- calculate_MNDWI(B3, B11)
  LSWI    <- calculate_LSWI(B8A, B11) ; B1_1500 <- calculate_cloud(B1)
  AWEI    <- calculate_AWEI(B3, B8, B11, B12) 
  MBWI    <- calculate_MBWI(B3, B4, B8, B11, B12)
  
  
  # List of raster objects and corresponding file names.
  raster_list <- list(
    list(raster_obj = B1_1500,  filename_suffix = "_B1_1500"),
    list(raster_obj = NDVI,     filename_suffix = "_NDVI"),
    list(raster_obj = SWI,      filename_suffix = "_SWI"),
    list(raster_obj = NDWI,     filename_suffix = "_NDWI"),
    list(raster_obj = MNDWI,    filename_suffix = "_MNDWI"),
    list(raster_obj = AWEI,     filename_suffix = "_AWEI"),
    list(raster_obj = MBWI,     filename_suffix = "_MBWI"),
    list(raster_obj = LSWI,     filename_suffix = "_LSWI")
  )
  # Loop through the list and save the rasters.
  for (raster_info in raster_list) {
    raster_obj <- raster_info$raster_obj
    filename_suffix <- raster_info$filename_suffix
    # Create the full file name.
    full_filename <- paste0(sub(".tif", "", basename(tif_file)),
                            filename_suffix, ".tif")
    full_filepath <- file.path("./Output", full_filename)
    # Save raster.
    raster::writeRaster(raster_obj, filename = full_filepath,
                        format = "GTiff", overwrite = TRUE)
  }
}