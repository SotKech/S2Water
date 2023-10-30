# Load necessary libraries
library(ggplot2) # For creating plots
library(zoo) # For handling time series data

# Read and assign CSV files to individual variables
for (i in 1:7) {
  file_path <- paste("./", c("AWEI", "B1_1500", "MBWI", "MNDWI", "NDVI", "NDWI", "SWI")[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path)) # Assigns the data to respective variables
}

# Define the column number to be considered
res <- 27

# Remove rows where a condition is met in result_df2
rows_to_remove <- which(result_df2[, res] > 0.000100)
f_df1 <- result_df1[-rows_to_remove, ]
f_df2 <- result_df2
f_df3 <- result_df3[-rows_to_remove, ]
f_df4 <- result_df4[-rows_to_remove, ]
f_df5 <- result_df5[-rows_to_remove, ]
f_df6 <- result_df6[-rows_to_remove, ]
f_df7 <- result_df7[-rows_to_remove, ]

# Function to calculate loess fit for each data frame
calculate_loess <- function(df) {
  # Arrange data by Date
  df <- df[order(df$Date), ]
  df$Date <- as.Date(df$Date)
  x <- as.numeric(df$Date)
  y <- df[, res]
  
  # Calculate the loess fit
  loess_fit <- loess(y ~ x, span = 0.15, data = df)
  y_pred <- predict(loess_fit, data.frame(x = x))
  
  return(list(x = as.Date(x), y_pred = y_pred))
}

# Function to add data to the plot
add_to_plot <- function(p, df, color, label, linetype, loess_data) {
  # Add line plot with specific color and linetype
  p <- p + geom_line(data = df, aes(x = loess_data$x, y = loess_data$y_pred, color = label), linetype = linetype, size = 0.75) +
    geom_text(data = data.frame(x = max(loess_data$x), y = max(loess_data$y_pred), label = label), 
              aes(x = max(loess_data$x), y = max(loess_data$y_pred), label = label), hjust = -20, vjust = 0)
  
  # Add specific points for AWEI data
  if (label == "AWEI") {
    points_data <- data.frame(x = as.Date(df$Date), y = 0, label = "Highlighted Dates")
    p <- p + geom_point(data = points_data, aes(x = x, y = y), color = 'black', shape = 124, size = 3)
  }
  
  return(p) # Return the modified plot
}

# Create an empty ggplot object
p <- ggplot()

# Prepare data and styling information
data_frames <- list(f_df1, f_df3, f_df4, f_df5, f_df6, f_df7)
linetype_vector <- c("dashed", "dashed", "solid", "solid", "solid", "dotdash")
my_colors <- c('#f8766d', '#9e854e', '#2bd4d6', '#4daf4a', '#377eb8', '#f564e3')
labels <- c('AWEI', 'MBWI', 'MNDWI', 'NDVI', 'NDWI', 'SWI')

# Calculate loess for each data frame
loess_data_frames <- lapply(data_frames, calculate_loess)

# Process each data frame and add to the plot
for (i in seq_along(data_frames)) {
  p <- add_to_plot(p, data_frames[[i]], my_colors[i], labels[i], linetype_vector[i], loess_data_frames[[i]])
}

# Define breaks for the x-axis
breaks.vec <- seq(lubridate::ymd("2017-01-01"),
                  lubridate::ymd("2023-12-01"), by = "3 months")

# Customize the plot
p <- p + xlab("Date") + ylab(paste0(colnames(f_df1)[res])) +
  theme(legend.position = "top") +
  scale_color_manual(values = my_colors) +
  scale_linetype_manual(values = linetype_vector) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = paste("Title"), color = "Indices") +
  scale_x_date(breaks = breaks.vec, date_labels = "%m-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
print(p)
ggsave(paste0("./Graph_", paste0(colnames(f_df1)[res]), ".png", sep = ""),
       plot = p, width = 17, height = 7, dpi = 300) # Save the plot as an image file
