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

# plots_directory <- "./Output/Plots"
# if (!dir.exists(plots_directory))   {dir.create(plots_directory)}

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

calculate_AWEI <- function(B3, B8, B11, B12) {              
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
  j = round((i/as.double(length(tif_files))*100), 2) ; cat(paste0("\r", j, "%"))
  
  # Load the image
  image <- raster::stack(tif_file)
  
  # Extract the necessary bands.
  B3  <- image[[3]] ; B4  <- image[[4]]  ; B5  <- image[[5]]
  B8  <- image[[8]] ; B11 <- image[[10]] ;  B12 <- image[[11]]
  # NOTE: (Sen2r issue) Band 10 is missing from Level-2A products thus bands 8 
  # and 8a are alternatively present (band 8 if the output resolution is < 20m,
  # band 8a if >= 20m).

  # Calculate indices.
  NDVI  <- calculate_NDVI(B4, B8) ;  SWI   <- calculate_SWI(B5, B11)
  NDWI  <- calculate_NDWI(B3, B8) ;  MNDWI <- calculate_MNDWI(B3, B11)
  AWEI  <- calculate_AWEI(B3, B8, B11, B12)
  
  # Plot NDVI map.
  # raster::plot(NDVI)
  # NDVI_filename <- paste0(sub(".tif", "", basename(tif_file)), "_NDVI.png")
  # dev.copy(png, NDVI_filename) ; dev.off()
  
  # Calculate the average NDVI for the entire image
  avg_NDVI <- cellStats(NDVI, mean)
  
  
  
  # List of raster objects and corresponding filenames
  raster_list <- list(
    list(raster_obj = NDVI, filename_suffix = "_NDVI"),
    list(raster_obj = SWI, filename_suffix = "_SWI"),
    list(raster_obj = NDWI, filename_suffix = "_NDWI"),
    list(raster_obj = MNDWI, filename_suffix = "_MNDWI"),
    list(raster_obj = AWEI, filename_suffix = "_AWEI")
  )
  
  # Loop through the list and save the rasters
  for (raster_info in raster_list) {
    raster_obj <- raster_info$raster_obj
    filename_suffix <- raster_info$filename_suffix
    
    # Create the full filename
    full_filename <- paste0(sub(".tif", "", basename(tif_file)), filename_suffix, ".tif")
    full_filepath <- file.path("./Output", full_filename)
    
    # Save the raster
    raster::writeRaster(raster_obj, filename = full_filepath, format = "GTiff", overwrite = TRUE)
  }
  
  
  
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




# Function to get a list of TIF files with a specific suffix in the working directory
get_tif_files <- function(suffix) {
  list.files(path = "./Output", pattern = paste0("\\_",suffix, "\\.tif$"),
             full.names = TRUE)
}

# Get lists of TIF files with different suffixes
NDVI_images <- get_tif_files("NDVI")
NDWI_images <- get_tif_files("NDWI")
MNDWI_images <- get_tif_files("MNDWI")
SWI_images <- get_tif_files("SWI")
AWEI_images <- get_tif_files("AWEI")



# Function to create plots
create_ndvi_plot <- function(image) {
  x <- raster(image)
  # Convert raster image to a data frame
  raster_df <- as.data.frame(x, xy = TRUE)
  
  # Create the plot
  p <- ggplot() +
    # Add the raster image as a background
    geom_raster(data = raster_df, aes(x = x, y = y, fill = raster_df[, 3])) +
    scale_fill_viridis_c() +  # Use a color scale, you can change it as needed
    # Add the GeoJSON polygon on top
    geom_sf(data = AOIb, fill = "transparent", color = "red", size = 1) +
    # Adjust the aspect ratio and theme as needed
    coord_sf() +
    theme_minimal()
  # Export the plot as a PNG file
  ggsave(filename = paste0("plot_", basename(image), ".png"), plot = p,
         width = 10, height = 8, dpi = 300)
  
}

# Function to create plots for raster images
create_plots <- function(image_list, plot_function) {
  lapply(image_list[1:5], plot_function)
}

# Call the function for each image list with its corresponding plot function
create_plots(NDVI_images, create_ndvi_plot)
create_plots(NDWI_images, create_ndwi_plot)
create_plots(MNDWI_images, create_mndwi_plot)
create_plots(SWI_images, create_swi_plot)
create_plots(AWEI_images, create_awei_plot)



