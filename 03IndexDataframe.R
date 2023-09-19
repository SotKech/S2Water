#'
#'                      MGI Internship  :   S2Water - Analysis_C
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-09-14
#'                      Last update     :   2023-09-15
#'                      R Version       :   4.3.1
#'                      Packages        :   raster, sf, dplyr
#'                      LICENSE         :   CC BY-NC-SA 4.0
#'


                           #### Package Import ####
# pkgTest function loads and install packages only when are not installed yet.
neededPackages <- c("raster", "sf", "dplyr")
pkgTest <- function(x){
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)
} for (package in neededPackages){pkgTest(package)}


                          #### Set up directories ####
getwd()
# Get a list of all TIF files in the ./Data/BOA/ directory.
tif_files <- list.files(path = "./Data/BOA", pattern = "\\.tif$",
                        full.names = TRUE)
# Load boundaries of reservoirs
AOI_b <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")


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
SWI_images   <- get_tif_files("SWI") ; B1_1500_images<- get_tif_files("B1_1500")

                   #### Count Pixels And Create Dataframe ####
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
    # Iterate the reservoirs.
    for (j in 1:nrow(AOI_b)) {
      reservoir <- AOI_b[AOI_b$id == j, ]
      pixels <- CountPixels(img, reservoir)
      col_px <- paste("Reservoir", j, "px", sep = "_")
      col_area <- paste("Reservoir", j, "area(ha)", sep = "_")
      row[[col_px]] <- pixels
      row[[col_area]] <- pixels * 1e-4 # (1e-6 for Km2)
    }
    # Append row.
    result_df <- dplyr::bind_rows(result_df, row) 
    # Update progress.
    progress <- round((i / length(Index_images)) * 100, 2)
    cat(paste0("\r", progress, "%"))
  }
  write.csv(result_df, file = output_file)
}

CountPixelsAndCreateDataframe(B1_1500_images,  AOI_b, paste0("B1_1500",  ".csv"))
CountPixelsAndCreateDataframe(NDVI_images,  AOI_b, paste0("NDVI",     ".csv"))
CountPixelsAndCreateDataframe(NDWI_images,  AOI_b, paste0("NDWI",     ".csv"))
CountPixelsAndCreateDataframe(MNDWI_images, AOI_b, paste0("MNDWI",    ".csv"))
CountPixelsAndCreateDataframe(SWI_images,   AOI_b, paste0("SWI",      ".csv"))
CountPixelsAndCreateDataframe(AWEI_images,  AOI_b, paste0("AWEI",     ".csv"))
