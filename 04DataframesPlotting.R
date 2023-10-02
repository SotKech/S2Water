#'
#'                      MGI Internship  :   S2Water - 04DataframePlotting.R
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-09-14
#'                      Last update     :   2023-09-15
#'                      R Version       :   4.3.1
#'                      Packages        :   raster, sf, dplyr
#'                      LICENSE         :   CC BY-NC-SA 4.0
#'


                            #### Package Import ####
# pkgTest function loads and install packages only when are not installed yet.
neededPackages <- c("raster", "sf", "dplyr", "ggplot2")
pkgTest <- function(x){
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)
}
for (package in neededPackages){pkgTest(package)}


                          #### Set up directories ####
getwd()
# Get a list of all TIF files in the ./Data/BOA/ directory.
tif_files <- list.files(path = "./Data/BOA", pattern = "\\.tif$",
                        full.names = TRUE)
# Load boundaries of reservoirs
AOI_b <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")


                          #### Set up directories ####
generate_and_display_merged_plots <- function(data1, data2, data3,
                                              data4, data5, data6, reservoir) {
  # Convert Date column to Date type for both dataframes
  for (i in 1:6) {
    assign(paste("data", i, sep = ""),
           within(get(paste("data", i, sep = "")),
                  Date <- as.Date(Date)))
  }
  # Create the merged plot
  p <- ggplot() +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_line(data = data1,
              aes(x = Date, y = .data[[reservoir]], color = "AWEI")) +
    geom_bar( data = data2, alpha = 0.5, stat = "identity",
              aes(x = Date, y = .data[[reservoir]], fill = "B1_1500")) +
    geom_line(data = data3,
              aes(x = Date, y = .data[[reservoir]], color = "MNDWI")) +
    geom_line(data = data4,
              aes(x = Date, y = .data[[reservoir]], color = "NDVI")) +
    geom_line(data = data5,
              aes(x = Date, y = .data[[reservoir]], color = "NDWI")) +
    geom_line(data = data6,
              aes(x = Date, y = .data[[reservoir]], color = "SWI")) +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_point(data = data1,
               aes(x = Date, y = .data[[reservoir]], color = "AWEI")) +
    geom_point(data = data3,
               aes(x = Date, y = .data[[reservoir]], color = "MNDWI")) +
    geom_point(data = data4,
               aes(x = Date, y = .data[[reservoir]], color = "NDVI")) +
    geom_point(data = data5,
               aes(x = Date, y = .data[[reservoir]], color = "NDWI")) +
    geom_point(data = data6,
               aes(x = Date, y = .data[[reservoir]], color = "SWI")) +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xlim(as.Date("2020-01-01"), as.Date("2023-01-01")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
           plot.title = element_text(hjust = 0.5)) +
    labs(title = paste(reservoir), color = "Indices") +
    scale_fill_manual(name = " ", values = c("B1_1500" = "orange")) +
    xlab("Date") +
    ylab("Water Pixel Count") +
    theme(legend.position = "top") ###### is this nessesary

  # plot(p)
  ggsave(paste0("./Outpot/Graphs/Lebna_",reservoir,".png", sep = ""),
         plot = p, width = 12, height = 6.5, dpi = 300,)
}

# Read and assign CSV files to individual variables
for (i in 1:7) {
  file_path <- paste("./", c("AWEI", "B1_1500", "MNDWI", "MBWI", "NDVI", "NDWI", "SWI")[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}

# Use a loop to generate the reservoir names and add them to the vector
for (i in 1:12) {
  reservoir_name <- paste0("Reservoir_", i, "_px")
  reservoirs <- c(character(0), reservoir_name)
  generate_and_display_merged_plots(result_df1, result_df2, result_df3,
                                    result_df4, result_df5, result_df6,
                                    reservoirs)
  progress <- round((i / 12) * 100, 2)
  cat(paste0("\r", progress, "%"))
}

