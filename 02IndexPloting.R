#'
#'                      MGI Internship  :   S2Water - Analysis_a
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-09-13
#'                      Last update     :   2023-09-13
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
neededPackages <- c("raster", "sf")
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
NDVI_images  <- get_tif_files("NDVI")  ; NDWI_images <- get_tif_files("NDWI")
MNDWI_images <- get_tif_files("MNDWI") ; SWI_images  <- get_tif_files("SWI")
AWEI_images  <- get_tif_files("AWEI")  ; B1_1500     <- get_tif_files("B1_1500")


                          #### Plotting Index Images####
# Function to create and export raster plots
create_raster_plot <- function(image, fill_colors, index, lim, brk) {
  x <- raster(image)                      # Convert raster image to a data frame
  raster_df <- as.data.frame(x, xy = TRUE)
  p <- ggplot() +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = raster_df[, 3])) +
    scale_fill_gradientn(
      colors = fill_colors,
      limits = lim,
      na.value = "transparent",
      breaks = brk,
    ) +
    geom_sf(data = AOI_b, fill = "transparent", color = "red", size = 1) +
    coord_sf() +
    theme_minimal() +
    labs(
      title = as.Date(substr(basename(image), 7, 14), format = "%Y%m%d"),
      fill = index
    ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("") +
    ylab("")
  
  ggsave(                                        # Export the plot as a PNG file
    filename = paste0("./Output/Plots/plot_", basename(image), ".png"),
    plot = p, width = 10,  height = 10, dpi = 300, bg = "white"
  )
  return(p)
}

# Function to create plots for raster images
create_plots <- function(img_list, plot_function, fill_colors, index, lim, brk){
  lapply(img_list[1:41], ##### you need to adjust for the images
         function(image) {plot_function(image, fill_colors, index, lim, brk)})
}

# Define the fill colors and fill names for NDVI and NDWI
ndvi_col <- c("red", "red", "orange", "green", "darkgreen")
other_cols <- c("darkgreen" ,"green", "white", "blue", "blue")
lims <- c(-1, 1)        ; brks <- c(-1, -0.5, 0, 0.5, 1)
AWEI_lims <- c(-20, 20) ; AWEI_brks <- c(-20, -10, 0, 10, 20)

# Call the create_plots function with the appropriate arguments
create_plots(NDVI_images,  create_raster_plot, ndvi_col,   "NDVI",  lims, brks)
create_plots(NDWI_images,  create_raster_plot, other_cols, "NDWI",  lims, brks)
create_plots(MNDWI_images, create_raster_plot, other_cols, "MNDWI", lims, brks)
create_plots(SWI_images,   create_raster_plot, other_cols, "SWI",   lims, brks)
create_plots(AWEI_images,  create_raster_plot, other_cols, "AWEI",  AWEI_lims,
             AWEI_brks)