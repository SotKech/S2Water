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
neededPackages <- c("raster", "sf", "doParallel", "foreach", "parallel", "iterators")
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

