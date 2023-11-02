# Load necessary packages
neededPackages <- c("raster", "sf", "dplyr", "ggplot2", "lubridate")

# Function to check and install packages
pkgTest <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
}

# Check and load necessary packages
invisible(sapply(neededPackages, pkgTest))

# Read and assign CSV files to individual variables
indices <- c("AWEI", "B1_1500", "MBWI", "MNDWI", "NDVI", "NDWI", "SWI")
for (i in seq_along(indices)) {
  file_path <- paste("./", indices[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}

# Initialize variables
res <- 15

# Remove rows where a condition is met in result_df2
rows_to_remove <- which(result_df2[, res] > 0.000100)
f_df1 <- result_df1[-rows_to_remove, ]
f_df3 <- result_df3[-rows_to_remove, ]
f_df4 <- result_df4[-rows_to_remove, ]
f_df5 <- result_df5[-rows_to_remove, ]
f_df6 <- result_df6[-rows_to_remove, ]
f_df7 <- result_df7[-rows_to_remove, ]

# Define data and styling information
data_frames <- list(f_df1, f_df3, f_df4, f_df5, f_df6, f_df7)
linetype_vector <- c("dashed", "dashed", "solid", "solid", "solid", "dotdash")
my_colors <- c('#f8766d', '#9e854e', '#2bd4d6', '#4daf4a', '#377eb8', '#f564e3')
labels <- c('AWEI', 'MBWI', 'MNDWI', 'NDVI', 'NDWI', 'SWI')

# Function to calculate LOESS fit and identify peaks and valleys
calculate_loess <- function(df, linetype, my_color, label) {
  df <- df[order(df$Date), ]
  df$Date <- as.Date(df$Date)
  x <- as.numeric(df$Date)
  y <- df[, res]
  
  loess_fit <- loess(y ~ x, span = 0.15, data = df)
  df$y_pred <- predict(loess_fit, data.frame(x = x))
  
  window_size <- 4
  peaks <- c()
  valleys <- c()
  for (i in ((window_size + 1):(length(df$y_pred) - window_size))) {
    if (df$y_pred[i] == max(df$y_pred[(i - window_size):(i + window_size)])) {
      peaks <- c(peaks, i)
    } else if (df$y_pred[i] == min(df$y_pred[(i - window_size):(i + window_size)])) {
      valleys <- c(valleys, i)
    }
  }
  
  df_peaks <- df[peaks, c("Date", "y_pred")]
  df_peaks$Label <- label
  df_valleys <- df[valleys, c("Date", "y_pred")]
  df_valleys$Label <- label
  
  return(list(df = df, df_peaks = df_peaks, df_valleys = df_valleys, linetype = linetype, my_color = my_color, label = label))
}

# Create empty data frames to store peaks and valleys
all_peaks <- data.frame()
all_valleys <- data.frame()
# Define breaks for the x-axis
breaks.vec <- seq(lubridate::ymd("2017-01-01"),
                  lubridate::ymd("2023-12-01"), by = "3 months")

# Create an empty plot
combined_plot <- ggplot() + labs(title = "LOESS and local extremes",
                                 x = "Date", y = "Predicted Values") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(breaks = breaks.vec, date_labels = "%m-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Loop through the list of data frames
for (i in seq_along(data_frames)) {
  result <- calculate_loess(data_frames[[i]], linetype_vector[i], my_colors[i], labels[i])
  df <- result$df
  df_peaks <- result$df_peaks
  df_valleys <- result$df_valleys
  linetype <- result$linetype
  my_color <- result$my_color
  label <- result$label
  
  # Append peaks and valleys to the overall data frames
  all_peaks <- rbind(all_peaks, df_peaks)
  all_valleys <- rbind(all_valleys, df_valleys)
  
  # Add lines and points to the combined plot
  combined_plot <- combined_plot +
    geom_line(data = df, aes(x = Date, y = y_pred),
              color = my_color,
              linetype = linetype) +
    geom_point(data = df_peaks, aes(x = Date, y = y_pred, color = "Peaks")) +
    geom_point(data = df_valleys, aes(x = Date, y = y_pred, color = "Valleys"))
}

# Add a legend for colors
combined_plot <- combined_plot +
  scale_color_manual(values = c("Peaks" = "red", "Valleys" = "black"),
                     labels = c("Peaks", "Valleys"),
                     name = "Legend")


# Print the final combined plot
print(combined_plot)

# Print all peaks and valleys
print(all_peaks)
print(all_valleys)
