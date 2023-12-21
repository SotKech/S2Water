#' Continuation of LOESS_plot.R
#' 
#' This script plot the LOESS estimation WITH the min-max points and exports 
#' them. Although the script is problematic
#' After that the script finds and export the Standard Deviation but this is
#' also problematic


#### Package Import ####
# pkgTest function loads and install packages only when are not installed yet.
neededPackages <- c("raster", "sf", "dplyr", "ggplot2", "lubridate")
pkgTest <- function(x){
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)
}
for (package in neededPackages){pkgTest(package)}

#### Set directory ####
getwd() # setwd("C:/Projects/S2Water")

# Read and assign CSV files to individual variables
indices <- c("AWEI", "B1_1500", "MBWI", "MNDWI", "NDVI", "NDWI", "SWI")
for (i in seq_along(indices)) {
  file_path <- paste("./Indices/", indices[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}
# Load boundaries of reservoirs
AOI <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")
# Load reservoir names
reservoir_name <- AOI$Name
# # Read and adjsut insitu data
# insitu <- read.csv("./Data/Surface_Volume_Kamech_2017-2023.csv")
# insitu$Date <- as.Date(insitu$Date)
# insitu$S_ha <- insitu$S_m2 * 1e-4


# Define the column number of reservoir
res <- 27
j <- 12
# # Hellping if as there is no loop yet
# if (res == 23) {
#   j <- 10
# } else if (res == 5) {
#   j <- 1
# }
# Remove rows where a condition is met in result_df2
rows_to_remove <- which(result_df2[, res] > 0.001)
f_df1 <- result_df1[-rows_to_remove, ]
f_df3 <- result_df3[-rows_to_remove, ]
f_df4 <- result_df4[-rows_to_remove, ]
f_df5 <- result_df5[-rows_to_remove, ]
f_df6 <- result_df6[-rows_to_remove, ]
f_df7 <- result_df7[-rows_to_remove, ]

# Define data and styling information
data_frames <- list(f_df1, f_df3, f_df4, f_df5, f_df6, f_df7) # insitu
linetype_vector <- c("dashed", "dashed", "solid","solid",
                     "solid", "dotdash")
my_colors <- c('#f8766d', '#9e854e', '#2bd4d6', '#4daf4a',
               '#377eb8', '#f564e3')
labels <- c('AWEI', 'MBWI', 'MNDWI', 'NDVI', 'NDWI', 'SWI')

calculate_loess <- function(df, linetype, my_color, label) {
  df <- df[order(df$Date), ]
  df$Date <- as.Date(df$Date)
  x <- as.numeric(df$Date)
  
  if (colnames(df)[1] == "X") {
    y <- df[, res]
  } else {
    y <- df$S_ha
  }
  
  loess_fit <- loess(y ~ x, span = 0.15, data = df)
  df$y_pred <- predict(loess_fit, data.frame(x = x))
  
  window_size <- 8
  
  peaks <- c()
  valleys <- c()
  
  # Iterate over the indices with a window to find peaks and valleys
  for (i in (window_size + 1):(length(df$y_pred) - window_size)) {
    # Check for missing values in the window
    if (!any(is.na(df$y_pred[(i - window_size):(i + window_size)]))) {
      # Check if the current point is a peak
      if (df$y_pred[i] == max(df$y_pred[(i - window_size):(i + window_size)])) {
        peaks <- c(peaks, i)
      } 
      # Check if the current point is a valley
      else if (df$y_pred[i] == min(df$y_pred[(i - window_size):(i + window_size)])) {
        valleys <- c(valleys, i)
      }
    }
  }
  
  df_peaks <- df[peaks, c("Date", "y_pred")]
  df_peaks$Label <- label
  
  df_valleys <- df[valleys, c("Date", "y_pred")]
  df_valleys$Label <- label
  
  return(list(df = df, df_peaks = df_peaks, df_valleys = df_valleys,
              linetype = linetype, my_color = my_color, label = label))
}


# calculate_loess(insitu, "solid", "black", "insitu")

# Create empty data frames to store peaks and valleys
all_peaks <- data.frame()
all_valleys <- data.frame()
# Define breaks for the x-axis
breaks.vec <- seq(lubridate::ymd("2017-01-01"),
                  lubridate::ymd("2023-12-01"), by = "3 months")
# Create an empty plot
combined_plot <- ggplot() + labs(title = reservoir_name[j],
                                 x = "Date", y = "Water Surface (ha)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title =  element_text(hjust = 0.5)) +
  scale_x_date(breaks = breaks.vec, date_labels = "%m-%Y") +
  theme(axis.text.x =   element_text(angle = 45, hjust = 1))
# Loop through the list of data frames
for (i in seq_along(data_frames)) {
  result <- calculate_loess(data_frames[[i]], linetype_vector[i],
                            my_colors[i],     labels[i])
  df         <- result$df
  df_peaks   <- result$df_peaks
  df_valleys <- result$df_valleys
  linetype   <- result$linetype
  my_color   <- result$my_color
  label      <- result$label
  # Append peaks and valleys to the overall data frames
  all_peaks <- rbind(all_peaks, df_peaks)
  write.csv(all_peaks, file = file.path(paste0("./Output/Peaks", j,"_", reservoir_name[j], ".csv")))
  all_valleys <- rbind(all_valleys, df_valleys)
  write.csv(all_valleys, file = file.path(paste0("./Output/Valleys", j,"_", reservoir_name[j], ".csv")))
  
  # Add lines and points to the combined plot
  combined_plot <- combined_plot +
    geom_line(data = df, aes(x = Date, y = y_pred),
              color = my_color,
              linetype = linetype) +
    geom_point(data = df_peaks, aes(x = Date, y = y_pred, color = "Peaks")) +
    geom_point(data = df_valleys, aes(x = Date, y = y_pred, color = "Valleys"))
}

points_data <- data.frame(x = as.Date(f_df1$Date), y = 0)

combined_plot <- combined_plot + 
  geom_point(data = points_data, aes(x = x, y = y), color = 'black', shape = 124, size = 3)
  #+ geom_line(data = insitu, aes(x = Date, y = S_ha, color = "black"))

# Add a legend for colors
combined_plot <- combined_plot +
  scale_color_manual(values = c("Peaks" = "red", "Valleys" = "black"),
                     labels = c("Peaks", "Valleys"),
                     name = "Legend")

# Save the plot as an image
print(combined_plot)
ggsave(paste0("./Output/Graphs/Extremes_", paste(j,"_", sep = ""), # Change 
              reservoir_name[j],".png", sep = ""),
       plot = combined_plot, width = 17, height = 7, dpi = 400,)

# print(all_peaks)
# print(all_valleys)


################################################################################
# 
# calculate_standard_deviation <- function(df) {
#   # Convert the 'Date' column to date format
#   df$Date <- as.Date(df$Date)
#   
#   # Convert the date to numeric format
#   df$Numeric_Date <- as.numeric(df$Date)
#   
#   # Create a grouping variable every 6 lines
#   df$Group <- rep(1:ceiling(nrow(df)/6), each=6)[1:nrow(df)]
#   
#   # Now calculate the standard deviation of Y_Pred for each group
#   SD_value <- df %>%
#     group_by(Group) %>%
#     summarise(Std_Y_Pred = sd(y_pred))
#   
#   # Print the result
#   # print(SD_value)
#   
#   # Calculate the standard deviation of Y_Pred and Numeric_Date for each group
#   SD_Date <- df %>%
#     group_by(Group) %>%
#     summarise(Std_Y_Pred = sd(y_pred),
#               Std_Numeric_Date = sd(Numeric_Date))
#   
#   # Print the result
#   print(SD_Date)
# }
# 
# ################################################################################
# 
