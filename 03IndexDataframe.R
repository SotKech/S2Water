#' MGI Internship  :   S2Water - 03IndexDataframe.R
#' Author          :   Sotirios Kechagias
#' Created         :   2023-09-14
#' Last update     :   2023-12-18
#' R Version       :   4.3.1
#' Packages        :   raster, sf, dplyr
#' LICENSE         :   CC BY-NC-SA 4.0
#' Input           :   TIF images of spectral indices in ./Output/
#'                     * previously calculated by 01IndexCalculation
#' Output          :   Creation of dataframes (csv) files of each index for all
#'                     the reservoirs and images in ./Indices/
#'                     * WARNING: These dataframes contain dates with cloud 
#'                                contamination

#### Package Import ####
# pkgTest function loads and install packages only when are not installed yet.
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
AOI <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")

# Create Indcies folder
Indices_path <- "./Indices"
if (!dir.exists(Indices_path)){dir.create(Indices_path)} 

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
SWI_images   <- get_tif_files("SWI")   ; MBWI_images  <- get_tif_files("MBWI")
B1_1500_images<- get_tif_files("B1_1500")


#### Count Pixels And Create Dataframe ####
# Function to count pixels within a reservoir.
CountPixels <- function(img, resrv, IndexName) {
  img <- raster(img)
  mask <- rasterize(resrv, raster(extent(img), ncols = ncol(img),
                                  nrows = nrow(img)))
  img_masked <- img * mask
  if (IndexName == "NDVI") {
    # Logic specific to SWI images
    px <- sum(values(img_masked) < 0, na.rm = TRUE)
  } else if (IndexName == "B1_1500") {
    # Logic specific to MBWI images
    px <- sum(values(img_masked) == 1, na.rm = TRUE)
  } else {
    # Generic logic for other images
    px <- sum(values(img_masked) >= 0, na.rm = TRUE)
  }
  return(px)
}

# Count pixels and create dataframe.
CountPixelsAndCreateDataframe <- function(Index_images, AOI, IndexName) {
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
    for (j in 1:nrow(AOI)) {
      reservoir <- AOI[AOI$id == j, ]
      pixels <- CountPixels(img, reservoir, IndexName)
      col_px <- paste("Reservoir", j, "px", sep = "_")
      col_area <- paste("Reservoir", j, "area_ha", sep = "_")
      row[[col_px]] <- pixels
      row[[col_area]] <- pixels * 1e-2
      # px -> m2 => 1px = 1e+2 m2
      # m2 -> ha => 1ha = 1e-4m2
    }
    # Append row.
    result_df <- dplyr::bind_rows(result_df, row) 
    # Update progress.
    progress <- round((i / length(Index_images)) * 100, 2)
    cat(paste0("\r", progress, "%"))
  }
  write.csv(result_df,
            file = file.path(paste0("./Indices/", IndexName, ".csv")))
}

# Create and export Dataframs
CountPixelsAndCreateDataframe(SWI_images,   AOI, "SWI")
CountPixelsAndCreateDataframe(AWEI_images,  AOI, "AWEI")
CountPixelsAndCreateDataframe(MNDWI_images, AOI, "MNDWI")
CountPixelsAndCreateDataframe(NDWI_images,  AOI, "NDWI")
CountPixelsAndCreateDataframe(MBWI_images,  AOI, "MBWI")
CountPixelsAndCreateDataframe(NDVI_images,  AOI, "NDVI")
CountPixelsAndCreateDataframe(B1_1500_images, AOI, "B1_1500")
