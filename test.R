#'
#'                      MGI Internship  :   S2Water
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   July 21, 2023
#'                      Last update     :   August 28, 2023
#'                      R Version       :   4.3.1
#'                      LICENSE         :   CC BY-NC-SA 4.0
#'

# Package Import ----
pkgTest <- function(x) { #         pkgTest is a function that loads packages and 
  #  installs them  only when they are not installed yet.
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}

neededPackages <- c("raster", "ggplot2", "sf")              # Necessary Packages

for (package in neededPackages) {
  pkgTest(package)
}

# Set up directories ----
# setwd("C:/Projects/S2Water") # Set working directory.
getwd()
# Get a list of all TIF files in the working directory.
tif_files <- list.files(path = "./Data/BOA", pattern = "\\.tif$",
                        full.names = TRUE)

AOI_b <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")


# Other ----

get_tif_files <- function(suffix) {              # Function to get a list of TIF
  list.files(path = "./Output",
             pattern = paste0("\\_",suffix, "\\.tif$"),
             full.names = TRUE)
}

NDWI_images  <- get_tif_files("NDWI")

# Function to count negative pixels within a reservoir.
count_pixels <- function(img, resrv) {
  img <- raster(img)
  mask <- rasterize(resrv,raster(extent(img),
                                 ncols = ncol(img),
                                 nrows = nrow(img)))
  img_masked <- img * mask
  px <- sum(values(img_masked) < 0, na.rm = TRUE)
  return(px)
}

# Initialize an empty data frame to store the results.
result_df <- data.frame(Date = character(0), ImageName = character(0))

k = 0
# Iterate through each image file.
for (img in NDVI_images) {
  date_str <- substr(basename(img), 7, 14)
  formatted_date <- paste(substr(date_str, 1, 4),
                          substr(date_str, 5, 6),
                          substr(date_str, 7, 8), sep = "-")
  
  # Create a row for the result dataframe.
  row <- data.frame(Date = formatted_date, ImageName = basename(img))
  
  # Iterate through each reservoir in AOI_b and count negative pixels.
  for (i in 1:nrow(AOI_b)) {
    reservoir <- AOI_b[AOI_b$id == i, ]
    pixels <- count_pixels(img, reservoir)
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
cat("Result Dataframe:\n") ; print(result_df)
write.csv(result_df, file = "./result_df.csv")

# Plotting ----
result_df <- read.csv("./result_df.csv")
result_df$Date <- as.Date(result_df$Date)


ggplot(result_df, aes(x = Date, y = Reservoir_1_px)) +
  geom_line(colour = "darkgreen") +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_point() +
  geom_smooth(method="auto",
              se=TRUE,
              fullrange=FALSE,
              level=0.95,
              color = 'darkgrey', alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y") +
  # limits = as.Date(c("2021-06-01", "2023-02-01")
  ylim(0, 60000) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Fig 1")

ggplot(result_df, aes(x = Date, y = Reservoir_2_px)) +
  geom_line(colour = "darkgreen") +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y") +
  ylim(0, 3000) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Fig 2")

ggplot(result_df, aes(x = Date, y = Reservoir_3_px)) +
  geom_line(colour = "darkgreen") +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y") +
  ylim(0, 3000) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Fig 3")

ggplot(result_df, aes(x = Date, y = Reservoir_4_px)) +
  geom_line(colour = "darkgreen") +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y") +
  ylim(0, 3000) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Fig 4")

ggplot(result_df, aes(x = Date, y = Reservoir_10_px)) +
  geom_line(colour = "darkgreen") +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y") +
  ylim(0, 3000) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Fig 10")