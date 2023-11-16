#'
#'                      MGI Internship  :   S2Water - LOESS_plot
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-09-21
#'                      Last update     :   2023-11-07
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
# Necessary Packages
neededPackages <- c("zoo", "ggplot2")

for (package in neededPackages) {
  pkgTest(package)
}

                            #### Set directory ####
getwd()
# setwd("C:/Projects/S2Water")

reservoir_name <- list("Lebna", "Akrane", "Ain Soudan", "Gombar", "Errouiguet",
                       "El Hajl", "Ben Salem", "El Guitoun", "Gbail", "Kamech",
                       "Reservoir 11", "Ennar")


                            #### Load index csv ####
# Read and assign CSV files to individual variables
for (i in 1:7) {
  file_path <- paste("./Indices/",
                     c("AWEI", "B1_1500", "MBWI",
                       "MNDWI", "NDVI", "NDWI", "SWI")[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}

              #### Define reservoir and remove cloudy days ####
# Define the column number of reservoir
res <- 23
# Hellping if as there is no loop yet
if (res == 23) {
  j <- 10
} else if (res == 5) {
  j <- 1
}


# Remove rows where a condition is met in result_df2 (B1_1500)
rows_to_remove <- which(result_df2[, res] > 0.001)
f_df1 <- result_df1[-rows_to_remove, ]
f_df2 <- result_df2
f_df3 <- result_df3[-rows_to_remove, ]
f_df4 <- result_df4[-rows_to_remove, ]
f_df5 <- result_df5[-rows_to_remove, ]
f_df6 <- result_df6[-rows_to_remove, ]
f_df7 <- result_df7[-rows_to_remove, ]

                            #### Basic functions ####
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
  p <- p + geom_line(data = df, aes(x = loess_data$x, y = loess_data$y_pred,
                                    color = label),
                     linetype = linetype, size = 0.75) +
    
    geom_text(data = data.frame(x = max(loess_data$x),
                                y = max(loess_data$y_pred), label = label),
              aes(x = max(loess_data$x), y = max(loess_data$y_pred),
                  label = label), hjust = -20, vjust = 0)
  return(p)
}

                            #### Initiate plot ####
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
  p <- add_to_plot(p, data_frames[[i]], my_colors[i],labels[i],
                   linetype_vector[i], loess_data_frames[[i]])
}

# Define breaks for the x-axis
breaks.vec <- seq(lubridate::ymd("2017-01-01"),
                  lubridate::ymd("2023-12-01"), by = "3 months")
# Set dates point in x-axis
points_data <- data.frame(x = as.Date(f_df1$Date), y = 0)


# Customize the plot
p <- p + geom_point(data = points_data, aes(x = x, y = y),
                    color = 'black', shape = 124, size = 3) +
  xlab("Date") + ylab("Water Surface ha") +
  theme(legend.position = "top") +
  scale_color_manual(values = my_colors) +
  scale_linetype_manual(values = linetype_vector) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = paste0(reservoir_name[j]), color = "Indices") +
  scale_x_date(breaks = breaks.vec, date_labels = "%m-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
print(p)
ggsave(paste0("./Output/Graphs/LOESS_", paste(j,"_", sep = ""), # Change 
              reservoir_name[j],".png", sep = ""),
       plot = p, width = 17, height = 7, dpi = 400,)




