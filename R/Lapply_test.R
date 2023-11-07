#'
#'                      MGI Internship  :   S2Water - 03IndexDataframe
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-09-14
#'                      Last update     :   2023-09-15
#'                      R Version       :   4.3.1
#'                      Packages        :   raster, sf, dplyr
#'                      LICENSE         :   CC BY-NC-SA 4.0
#'


                           #### Package Import ####
# pkgTest function loads and installs packages only when they are not installed yet.
neededPackages <- c("raster", "sf", "dplyr")
pkgTest <- function(x){
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)
}
for (package in neededPackages){pkgTest(package)}


                          #### Set up directories ####
# Get a list of all TIF files in the ./Data/BOA/ directory.
getwd()
tif_files <- list.files(path = "./Data/BOA", pattern = "\\.tif$",
                        full.names = TRUE)
# Load boundaries of reservoirs
AOI_b <- sf::st_read("./Data/Berambadi_reservoirs.geojson")


                            #### Load Index Images ####
# Function to get a list of TIF
get_tif_files <- function(suffix) {              
  list.files(path = "./Output",
             pattern = paste0("\\_",suffix, "\\.tif$"),
             full.names = TRUE)
}

# Get lists of TIF files with different suffixes
NDVI_images  <- get_tif_files("NDVI")  ; NDWI_images <- get_tif_files("NDWI")
MNDWI_images <- get_tif_files("MNDWI") ; AWEI_images <- get_tif_files("AWEI")
SWI_images   <- get_tif_files("SWI")   ; B1_1500_images<- get_tif_files("B1_1500")
MBWI_images  <- get_tif_files("MBWI")  ; LSWI_images <- get_tif_files("LSWI")


                   #### Count Pixels And Create Dataframe ####
# Function to count pixels within a reservoir.
CountPixels <- function(img, resrv) {
  img <- raster(img)
  mask <- rasterize(resrv,raster(extent(img),
                                 ncols = ncol(img),
                                 nrows = nrow(img)))
  img_masked <- img * mask
  px <- sum(values(img_masked) < 0, na.rm = TRUE)
  return(px)
}

# Count pixels and create a dataframe.
CountPixelsAndCreateDataframe <- function(Index_images, AOI_b, output_file) {
  # Initialize an empty data frame to store the results.
  result_df <- data.frame(Date = character(0), ImageName = character(0))
  # Iterate the images.
  for (i in 1:length(Index_images)) {
    img <- Index_images[i]
    date_str <- substr(basename(img), 7, 14)
    formatted_date <- paste(substr(date_str, 1, 4), substr(date_str, 5, 6),
                            substr(date_str, 7, 8), sep = "-")
    row <- data.frame(Date = formatted_date, ImageName = basename(img))
    row_index <- nrow(result_df) + 1  # Track the current row index.
    
    # Iterate the reservoirs using lapply.
    reservoir_columns <- lapply(1:nrow(AOI_b), function(j) {
      reservoir <- AOI_b[AOI_b$id == j, ]
      pixels <- CountPixels(img, reservoir)
      col_px <- paste("Reservoir", j, "px", sep = "_")
      col_area <- paste("Reservoir", j, "area(m2)", sep = "_")
      list(col_px = pixels, col_area = pixels * 1e-6)
    })
    
    # Unlist the results and add them to the row.
    reservoir_columns <- do.call(rbind, reservoir_columns)
    result_df[row_index, paste("Reservoir", 1:nrow(AOI_b), "px", sep = "_")] <- reservoir_columns[, "col_px"]
    result_df[row_index, paste("Reservoir", 1:nrow(AOI_b), "area(m2)", sep = "_")] <- reservoir_columns[, "col_area"]
    
    # Update progress.
    progress <- round((i / length(Index_images)) * 100, 2)
    cat(paste0("\r", progress, "%"))
  }
  write.csv(result_df, file = output_file)
}

# Create and export Dataframes
CountPixelsAndCreateDataframe(SWI_images,   AOI_b, paste0("SWI",   ".csv"))
