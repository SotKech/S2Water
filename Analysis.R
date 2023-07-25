#                                                                              #
#               {}|{}                                                          #
#               /o|o\                                                          #
#  ___________:/o|||o\}___________    MGI Internship  | S2Water                #
#   =|=|=|=|:S|":|||:"|K:|=|=|=|=     Author          : Sotirios Kechagias     #
#    """"""""""\:|||:/""""""""""      Created         : July 21, 2023          #
#               \.|./                 Last update     : July 21, 2023          #
#               /o.o\                 R Version       : 4.3.1                  #
#              /o.o.o\                LICENSE         : CC BY-NC-SA 4.0        #
#            #|o.o.o.o|#                                                       #
#                                                                              #


#### Package Import ####
pkgTest <- function(x){   #    pkgTest is a helper function to load packages and
  # install packages only whenthey are not installed yet
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)}
neededPackages <- c("raster", "ggplot2", "sf")
for (package in neededPackages){pkgTest(package)}

# Set working directory to the folder containing tif images
setwd("C:/Projects/S2Water")


calculate_ndvi <- function(nir, red) {
  ndvi <- (nir - red) / (nir + red)
  return(ndvi)
}



# Get a list of all tif files in the working directory
tif_files <- list.files(path = "./Data/BOA",
                        pattern = "\\.tif$",
                        full.names = TRUE)

# Create an empty list to store average NDVI values for each image
ndvi_avg_list <- list()
# Initialize an empty data frame to store filenames and NDVI values
ndvi_df <- data.frame(FileName = character(), NDVI = numeric(),
                      Date = character())

i = 0
# Loop through each tif file
for (tif_file in tif_files) {
  i = i + 1
  j = round((i/as.double(length(tif_files))), 2) ; cat(paste0("\r", j, "%"))
  
  # Load the image
  image <- stack(tif_file)
  
  # Extract the necessary bands (assuming NIR is  8th, red is 4th)
  nir <- image[[8]] ; red <- image[[4]]
  
  # Calculate & plot NDVI
  ndvi <- calculate_ndvi(nir, red)
  raster::plot(ndvi)
  ndvi_filename <- paste0(sub(".tif", "", basename(tif_file)), "_NDVI.png")
  dev.copy(png, ndvi_filename) ; dev.off()
  
  # Calculate the average NDVI for the entire image
  avg_ndvi <- cellStats(ndvi, mean)
  
  # Save the NDVI raster
  ndvi_filename <- paste0(sub(".tif", "", basename(tif_file)), "_NDVI.tif")

  raster::writeRaster(ndvi, filename = file.path("./Output",ndvi_filename),
                      format = "GTiff", overwrite = TRUE)
  
  # Add NDVI values and filename to the data frame
  date_str <- substr(basename(tif_file), 7, 14)
  ndvi_avg_list[[basename(tif_file)]] <- avg_ndvi
}

# Convert the ndvi_avg_list to a data frame and extract the date from filenames
ndvi_df <- data.frame(FileName = names(ndvi_avg_list),
                      AvgNDVI = unlist(ndvi_avg_list))

# Extract the date from the filename
ndvi_df$Date <- as.Date(substr(ndvi_df$FileName, 7, 14), format = "%Y%m%d")

# Create the graph
ggplot(ndvi_df, aes(x = Date, y = AvgNDVI)) +
  geom_line() +
  labs(title = "Average NDVI Through Time",
       x = "Date",
       y = "Average NDVI")



# ///


AOI <- sf::st_read("./Data/Lebna_reservoir_buffered.geojson")


# Get a list of all tif files in the working directory
ndvi_images <- list.files(path = "./Output",
                        pattern = "\\.tif$",
                        full.names = TRUE)


x = raster(ndvi_images[1])

# Convert raster image to a data frame
raster_df <- as.data.frame(x, xy = TRUE)

AOI <- sf::st_transform(AOI, crs = sf::st_crs(x))
# Convert the reprojected raster to a data frame


# Create the plot
ggplot() +
  # Add the raster image as a background
  geom_raster(data = raster_df, aes(x = x, y = y, fill = S2A2A_20220105_122_AOI_BOA_10_NDVI)) +
  scale_fill_viridis_c() +  # Use a color scale, you can change it as needed
  # Add the GeoJSON polygon on top
  geom_sf(data = AOI, fill = "transparent", color = "red", size = 1) +
  # Adjust the aspect ratio and theme as needed
  coord_sf() +
  theme_minimal()




