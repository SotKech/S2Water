#'
#'                      MGI Internship  :   S2Water - Analysis_b
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-09-14
#'                      Last update     :   2023-09-14
#'                      R Version       :   4.3.1
#'                      Packages        :   base, raster, ggplot2, sf
#'                      LICENSE         :   CC BY-NC-SA 4.0
#'



                           #### Package Import ####
# pkgTest is a helper function to load packages and  
# install packages only when are not installed yet.
pkgTest <- function(x){
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)
}
neededPackages <- c("raster", "sf", "doParallel", "foreach")
for (package in neededPackages){pkgTest(package)}


                          #### Set up directories ####
getwd()
# folder to store Plots
Plots_path   <- "./Output/Plot/"                         
if (!dir.exists(Plots_path)){dir.create(Plots_path)}
# Get a list of all TIF files in the working directory.
tif_files <- list.files(path = "./Data/BOA", pattern = "\\.tif$",
                        full.names = TRUE)
# Load boundaries of reservoirs
AOI_b <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")


                            #### Load Index Images ####
get_tif_files <- function(suffix) {              # Function to get a list of TIF
  list.files(path = "./Output",
             pattern = paste0("\\_",suffix, "\\.tif$"),
             full.names = TRUE)
}

# Get lists of TIF files with different suffixes
NDVI_images  <- get_tif_files("NDVI")   ; NDWI_images <- get_tif_files("NDWI")
MNDWI_images <- get_tif_files("MNDWI")  ; SWI_images  <- get_tif_files("SWI")
AWEI_images  <- get_tif_files("AWEI")


################################################################################
              #### Pixel/Area Count Per Reservoir Dataframe ####
# Function to count pixels within a reservoir.
CountPixels <- function(img, resrv) {
  img <- raster(img)
  mask <- rasterize(resrv,raster(extent(img),
                                 ncols = ncol(img),
                                 nrows = nrow(img)))
  img_masked <- img * mask
  px <- sum(values(img_masked) > 0, na.rm = TRUE) # > 0 for AWEI
  return(px)
} # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Initialize an empty data frame to store the results.
result_df <- data.frame(Date = character(0), ImageName = character(0))

k = 0
# Iterate through each image file.
for (img in NDWI_images) {
  date_str <- substr(basename(img), 7, 14)
  formatted_date <- paste(substr(date_str, 1, 4), substr(date_str, 5, 6),
                          substr(date_str, 7, 8), sep = "-")
  # Create a row for the result dataframe.
  row <- data.frame(Date = formatted_date, ImageName = basename(img))
  # Iterate through each reservoir in AOI_b and count negative pixels.
  for (i in 1:nrow(AOI_b)) {
    reservoir <- AOI_b[AOI_b$id == i, ]
    pixels <- CountPixels(img, reservoir)
    col_px <- paste("Reservoir", i, "px", sep = "_")
    col_area <- paste("Reservoir", i, "area(Km2)", sep = "_")
    row[[col_px]] <- pixels
    row[[col_area]] <- pixels * 1e-6 # Convert to square kilometers.
  }
  # Add the row to the result dataframe.
  result_df <- rbind(result_df, row)
  # Create progression bar.
  k = k + 1 ; j = round((k/as.double(length(NDWI_images))*100), 2)
  cat(paste0("\r", j, "%"))
}

# Print the result dataframe and write it to a CSV file.
result_df$Date <- as.Date(result_df$Date)
cat("Result Dataframe:\n")
print(result_df)
write.csv(result_df, file = "./result_df.csv")



################################################################################

# Print the result dataframe and write it to a CSV file.
result_df$Date <- as.Date(result_df$Date)
cat("Result Dataframe:\n") ; print(result_df)
write.csv(result_df, file = "./result_df.csv")


                      #### Plotting Pixel Count Graphs ####
# Define a function to generate and display plots
generate_and_display_plots <- function(data, reservoirs, ylm) {
  # Convert Date column to Date type
  data$Date <- as.Date(data$Date)
  # Create a list to store plots
  plots <- list()
  # Create plots for each reservoir
  for (reservoir in reservoirs) {
    p <- ggplot(data, aes(x = Date, y = .data[[reservoir]])) +
      geom_line(colour = "darkgreen") +
      geom_area(fill = "lightblue", alpha = 0.5) +
      geom_point() +
      geom_smooth(method = "auto",
                  se = TRUE,
                  fullrange = FALSE,
                  level = 0.95,
                  color = 'darkgrey', alpha = 0.5) +
      scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y") +
      ylim(0, ylm) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = paste("Reservoir", which(reservoirs == reservoir))) +
      xlab("Date") +
      ylab("Water Pixel Count")
    plots[[reservoir]] <- p
  }
  # Display plots
  for (reservoir in reservoirs) {
    print(plots[[reservoir]])
  }
}
generate_and_display_plots(result_df, "Reservoir_1_px", 60000)
generate_and_display_plots(result_df, c("Reservoir_1_px", # add Reservoir_1_px for correct numbering.
                                        "Reservoir_2_px", 
                                        "Reservoir_3_px",
                                        "Reservoir_4_px",
                                        "Reservoir_5_px"), 5000)

