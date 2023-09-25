#'
#'                      MGI Internship  :   S2Water - Analysis
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-06-21
#'                      Last update     :   2023-08-30
#'                      R Version       :   4.3.1
#'                      Packages        :   base, raster, ggplot2, sf
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
AOI_b <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")

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
  B1  <- image[[1]] ;  B3  <- image[[3]] ; B4  <- image[[4]]
  B5  <- image[[5]] ;  B8  <- image[[8]] ; B11 <- image[[10]]
  B12 <- image[[11]]
  # NOTE: (Sen2r issue) Band 10 is missing from Level-2A products thus bands 8 
  # and 8a are alternatively present (band 8 if the output resolution is < 20m,
  # band 8a if >= 20m).
  
  # Calculate indices.
  NDVI    <- calculate_NDVI(B4, B8) ;  SWI   <- calculate_SWI(B5, B11)
  NDWI    <- calculate_NDWI(B3, B8) ;  MNDWI <- calculate_MNDWI(B3, B11)
  AWEI    <- calculate_AWEI(B3, B8, B11, B12) ; B1_1500 <- calculate_cloud(B1)
  
  
  # List of raster objects and corresponding file names.
  raster_list <- list(
    list(raster_obj = B1_1500,  filename_suffix = "_B1_1500"),
    list(raster_obj = NDVI,     filename_suffix = "_NDVI"),
    list(raster_obj = SWI,      filename_suffix = "_SWI"),
    list(raster_obj = NDWI,     filename_suffix = "_NDWI"),
    list(raster_obj = MNDWI,    filename_suffix = "_MNDWI"),
    list(raster_obj = AWEI,     filename_suffix = "_AWEI")
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

#                             #### Load Index Images ####
# 
# get_tif_files <- function(suffix) {              # Function to get a list of TIF
#   list.files(path = "./Output",
#              pattern = paste0("\\_",suffix, "\\.tif$"),
#              full.names = TRUE)
# }
# 
# 
# 
#                           #### Plotting Index Images####
# # Get lists of TIF files with different suffixes
# NDVI_images  <- get_tif_files("NDVI")   ; NDWI_images <- get_tif_files("NDWI")
# MNDWI_images <- get_tif_files("MNDWI")  ; SWI_images  <- get_tif_files("SWI")
# AWEI_images  <- get_tif_files("AWEI")
# 
# # Function to create and export raster plots
# create_raster_plot <- function(image, fill_colors, index, lim, brk) {
#   x <- raster(image)                      # Convert raster image to a data frame
#   raster_df <- as.data.frame(x, xy = TRUE)
#   p <- ggplot() +
#     geom_raster(data = raster_df, aes(x = x, y = y, fill = raster_df[, 3])) +
#     scale_fill_gradientn(
#       colors = fill_colors,
#       limits = lim,
#       na.value = "transparent",
#       breaks = brk,
#     ) +
#     geom_sf(data = AOI_b, fill = "transparent", color = "red", size = 1) +
#     coord_sf() +
#     theme_minimal() +
#     labs(
#       title = as.Date(substr(basename(image), 7, 14), format = "%Y%m%d"),
#       fill = index
#     ) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     xlab("") +
#     ylab("")
#   
#   ggsave(                                        # Export the plot as a PNG file
#     filename = paste0("./Output/Plots/plot_", basename(image), ".png"),
#     plot = p, width = 10,  height = 10, dpi = 300, bg = "white"
#   )
#   return(p)
# }
# 
# # Function to create plots for raster images
# create_plots <- function(img_list, plot_function, fill_colors, index, lim, brk){
#   lapply(img_list[1:3], function(image) {
#     plot_function(image, fill_colors, index, lim, brk)
#   })
# }
# 
# # Define the fill colors and fill names for NDVI and NDWI
# ndvi_col <- c("red", "red", "orange", "green", "darkgreen")
# other_cols <- c("darkgreen" ,"green", "white", "blue", "blue")
# lims <- c(-1, 1)        ; brks <- c(-1, -0.5, 0, 0.5, 1)
# AWEI_lims <- c(-20, 20) ; AWEI_brks <- c(-20, -10, 0, 10, 20)
# 
# # Call the create_plots function with the appropriate arguments
# create_plots(NDVI_images,  create_raster_plot, ndvi_col,   "NDVI",  lims, brks)
# create_plots(NDWI_images,  create_raster_plot, other_cols, "NDWI",  lims, brks)
# create_plots(MNDWI_images, create_raster_plot, other_cols, "MNDWI", lims, brks)
# create_plots(SWI_images,   create_raster_plot, other_cols, "SWI",   lims, brks)
# create_plots(AWEI_images,  create_raster_plot, other_cols, "AWEI",  AWEI_lims,
#              AWEI_brks)
# 
#               #### Pixel/Area Count Per Reservoir Dataframe ####
# 
# # Function to count pixels within a reservoir.
# count_pixels <- function(img, resrv) {
#   img <- raster(img)
#   mask <- rasterize(resrv,raster(extent(img),
#                                  ncols = ncol(img),
#                                  nrows = nrow(img)))
#   img_masked <- img * mask
#   px <- sum(values(img_masked) > 0, na.rm = TRUE) # > 0 for AWEI
#   return(px)
# }
# 
# # Initialize an empty data frame to store the results.
# result_df <- data.frame(Date = character(0), ImageName = character(0))
# 
# k = 0
# # Iterate through each image file.
# for (img in AWEI_images) {       # <-------------------- AWEI
#   date_str <- substr(basename(img), 7, 14)
#   formatted_date <- paste(substr(date_str, 1, 4), substr(date_str, 5, 6),
#                           substr(date_str, 7, 8), sep = "-")
#   # Create a row for the result dataframe.
#   row <- data.frame(Date = formatted_date, ImageName = basename(img))
#   # Iterate through each reservoir in AOI_b and count negative pixels.
#   for (i in 1:nrow(AOI_b)) {
#     reservoir <- AOI_b[AOI_b$id == i, ]
#     pixels <- count_pixels(img, reservoir)
#     col_px <- paste("Reservoir", i, "px", sep = "_")
#     col_area <- paste("Reservoir", i, "area(Km2)", sep = "_")
#     row[[col_px]] <- pixels
#     row[[col_area]] <- pixels * 1e-6 # Convert to square kilometers.
#   }
#   # Add the row to the result dataframe.
#   result_df <- rbind(result_df, row)
#   # Create progression bar.
#   k = k + 1 ; j = round((k/as.double(length(NDWI_images))*100), 2)
#   cat(paste0("\r", j, "%"))
# }
# 
# # Print the result dataframe and write it to a CSV file.
# result_df$Date <- as.Date(result_df$Date)
# cat("Result Dataframe:\n") ; print(result_df)
# write.csv(result_df, file = "./result_df.csv")
# 
#                       #### Plotting Pixel Count Graphs ####
# 
# # Define a function to generate and display plots
# generate_and_display_plots <- function(data, reservoirs, ylm) {
#   # Convert Date column to Date type
#   data$Date <- as.Date(data$Date)
#   # Create a list to store plots
#   plots <- list()
#   # Create plots for each reservoir
#   for (reservoir in reservoirs) {
#     p <- ggplot(data, aes(x = Date, y = .data[[reservoir]])) +
#       geom_line(colour = "darkgreen") +
#       geom_area(fill = "lightblue", alpha = 0.5) +
#       geom_point() +
#       geom_smooth(method = "auto",
#                   se = TRUE,
#                   fullrange = FALSE,
#                   level = 0.95,
#                   color = 'darkgrey', alpha = 0.5) +
#       scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y") +
#       ylim(0, ylm) + 
#       theme(axis.text.x = element_text(angle = 90, hjust = 1),
#             plot.title = element_text(hjust = 0.5)) +
#       labs(title = paste("Reservoir", which(reservoirs == reservoir))) +
#       xlab("Date") +
#       ylab("Water Pixel Count")
#     plots[[reservoir]] <- p
#   }
#   # Display plots
#   for (reservoir in reservoirs) {
#     print(plots[[reservoir]])
#   }
# }
# 
# generate_and_display_plots(result_df, "Reservoir_1_px", 60000)
# generate_and_display_plots(result_df, c("Reservoir_1_px","Reservoir_2_px", # add 1 for correct numbering.
#                                         "Reservoir_3_px",
#                                         "Reservoir_4_px",
#                                         "Reservoir_5_px"), 5000)
# 
# 

