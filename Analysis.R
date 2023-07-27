#                                                                              #
#               {}|{}                                                          #
#               /o|o\                                                          #
#  ___________:/o|||o\}___________    MGI Internship  | S2Water                #
#   =|=|=|=|:S|":|||:"|K:|=|=|=|=     Author          : Sotirios Kechagias     #
#    """"""""""\:|||:/""""""""""      Created         : July 21, 2023          #
#               \.|./                 Last update     : July 27, 2023          #
#               /o.o\                 R Version       : 4.3.1                  #
#              /o.o.o\                LICENSE         : CC BY-NC-SA 4.0        #
#             |o.o.o.o|                                                        #
#                                                                              #
################################ Package Import ################################

pkgTest <- function(x) { #     pkgTest is a helper function to load packages and
                         # install packages only when they are not installed yet
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}

neededPackages <- c("raster", "ggplot2", "sf")

for (package in neededPackages) {
  pkgTest(package)
}

############################## Set up directories ##############################
# Set working directory.
setwd("C:/Projects/S2Water")

# Get a list of all tif files in the working directory.
tif_files <- list.files(path = "./Data/BOA", pattern = "\\.tif$",
                        full.names = TRUE)

AOIb <- sf::st_read("./Data/Lebna_reservoir_buffered.geojson")

############################## Indices functions ###############################

calculate_NDVI <- function(B4, B8) {
  NDVI <- (B8 - B4) / (B8 + B4)
  return(NDVI)
}

calculate_SWI <- function(B5, B11) {
  SWI <- (B5 - B11) / (B5 + B11)
  return(SWI)
}

calculate_NDWI <- function(B3, B8) {
  NDWI <- (B3 - B8)/(B3 + B8)
  return(NDWI)
}

calculate_MNDWI <- function(B3, B11) {
  MNDWI <- (B3 - B11)/(B3 + B11)
  return(MNDWI)
}

calculate_AWEI <- function(B2, B3, B8, B11, B12) {              
  AWEI <- 4 * (B3 - B11) - 0.25 * B8 - 2.75 * B12              # This is AWEInsh
  return(AWEI)      # Also AWEIsh: B2 + 2.5 * B3 - 1.5 * (B8 + B11) - 0.25 * B12
}














# Create an empty list to store average NDVI values for each image
NDVI_avg_list <- list()
# Initialize an empty data frame to store filenames and NDVI values
NDVI_df <- data.frame(FileName = character(), NDVI = numeric(),
                      Date = character())




i = 0
# Loop through each tif file
for (tif_file in tif_files) {
  # Create progression bar.
  i = i + 1
  j = round((i/as.double(length(tif_files))), 2) ; cat(paste0("\r", j, "%"))
  
  # Load the image
  image <- raster::stack(tif_file)
  
  # Extract the necessary bands.
  B2  <- image[[2]] ;  B3  <- image[[3]] ; B4  <- image[[4]]
  B5  <- image[[5]] ;  B8  <- image[[8]] ; B11 <- image[[11]]
  B12 <- image[[12]]

  # Calculate indices.
  NDVI  <- calculate_NDVI(B4, B8) ;  SWI   <- calculate_SWI(B5, B11)
  NDWI  <- calculate_NDWI(B3, B8) ;  MNDWI <- calculate_MNDWI(B3, B11)
  AWEI  <- calculate_AWEI(B2, B3, B8, B11, B12)
  
  # Plot NDVI map.
  # raster::plot(NDVI)
  # NDVI_filename <- paste0(sub(".tif", "", basename(tif_file)), "_NDVI.png")
  # dev.copy(png, NDVI_filename) ; dev.off()
  
  # Calculate the average NDVI for the entire image
  avg_NDVI <- cellStats(NDVI, mean)
  
  
  
  # Save the raster
  SWI_filename <- paste0(sub(".tif", "", basename(tif_file)), "_WWI.tif")
  
  raster::writeRaster(SWI, filename = file.path("./Output",SWI_filename),
                      format = "GTiff", overwrite = TRUE)   
  
  
  # Save the raster
  NDWI_filename <- paste0(sub(".tif", "", basename(tif_file)), "_NDWI.tif")
  
  raster::writeRaster(NDWI, filename = file.path("./Output",NDWI_filename),
                      format = "GTiff", overwrite = TRUE)  
  
  
  
  # Save the raster
  NDVI_filename <- paste0(sub(".tif", "", basename(tif_file)), "_NDVI.tif")

  raster::writeRaster(NDVI, filename = file.path("./Output",NDVI_filename),
                      format = "GTiff", overwrite = TRUE)
  
  
  
  
  # Add NDVI values and filename to the data frame
  date_str <- substr(basename(tif_file), 7, 14)
  NDVI_avg_list[[basename(tif_file)]] <- avg_NDVI
}








# Convert the NDVI_avg_list to a data frame and extract the date from filenames
NDVI_df <- data.frame(FileName = names(NDVI_avg_list),
                      AvgNDVI = unlist(NDVI_avg_list))

# Extract the date from the filename
NDVI_df$Date <- as.Date(substr(NDVI_df$FileName, 7, 14), format = "%Y%m%d")

# Create the graph
ggplot(NDVI_df, aes(x = Date, y = AvgNDVI)) +
  geom_line() +
  labs(title = "Average NDVI Through Time",
       x = "Date",
       y = "Average NDVI")






# ///




# Get a list of all tif files in the working directory
NDVI_images <- list.files(path = "./Output",
                        pattern = "\\.tif$",
                        full.names = TRUE)


x = raster(NDVI_images[1])

# Convert raster image to a data frame
raster_df <- as.data.frame(x, xy = TRUE)

AOIb <- sf::st_transform(AOIb, crs = sf::st_crs(x))
# Convert the reprojected raster to a data frame


# Create the plot
ggplot() +
  # Add the raster image as a background
  geom_raster(data = raster_df, aes(x = x, y = y, fill = S2A2A_20220105_122_AOI_BOA_10_NDVI)) +
  scale_fill_viridis_c() +  # Use a color scale, you can change it as needed
  # Add the GeoJSON polygon on top
  geom_sf(data = AOIb, fill = "transparent", color = "red", size = 1) +
  # Adjust the aspect ratio and theme as needed
  coord_sf() +
  theme_minimal()




