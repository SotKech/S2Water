# Read and assign CSV files to individual variables
for (i in 1:7) {
  file_path <- paste("./", c("AWEI", "B1_1500", "MBWI", "MNDWI", "NDVI", "NDWI", "SWI")[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}


zero_rem1 <- which(result_df1[, 5] == 0)
zero_rem3 <- which(result_df3[, 5] == 0)
zero_rem4 <- which(result_df4[, 5] == 0)
zero_rem5 <- which(result_df5[, 5] == 0)
zero_rem6 <- which(result_df6[, 5] == 0)
zero_rem7 <- which(result_df7[, 5] == 0)

rows_to_remove <- which(result_df2[, 5] > 0.000100)

f_df1 <- result_df1[-rows_to_remove, ]
f_df1 <- result_df1[-zero_rem1, ]
f_df2 <- result_df2
f_df3 <- result_df3[-rows_to_remove, ]
# f_df3 <- result_df1[-zero_rem3, ]
f_df4 <- result_df4[-rows_to_remove, ]
# f_df4 <- result_df4[-zero_rem4, ]
f_df5 <- result_df5[-rows_to_remove, ]
f_df5 <- result_df5[-zero_rem5, ]
f_df6 <- result_df6[-rows_to_remove, ]
f_df6 <- result_df6[-zero_rem6, ]
f_df7 <- result_df7[-rows_to_remove, ]
# f_df7 <- result_df7[-zero_rem7, ]


library(ggplot2)
library(zoo)

# Read the CSV file into a data frame
data <- f_df7
data <- data[order(data$Date), ]
data$Date <- as.Date(data$Date)
data$Date <- as.numeric(data$Date)
x <- data$Date
y <- data[,5]

# Median Rolling Average
M_roll <- c(y[1] ,y[1],
            zoo::rollapply(y, width=5, FUN=median),
            y[length(y)], y[length(y)])

# Calculate the residuals
residuals <- y - M_roll

# Calculate the mean distance from the residuals
mean_distance <- mean(abs(residuals))           # Mean absolute error (MAE) 

# For plotting
loess_fit <- loess(y ~ x, span = 0.15, data = data)
y_pred <- predict(loess_fit)
data$Date <- as.Date(data$Date)

# Creating the ggplot object
ggplot(data, aes(x = Date, y = y)) +
  geom_point(color = "black") +
  geom_line(aes(y = M_roll), color = "red", size = 0.5) +
  geom_line(aes(y = y_pred), color = "darkgreen", size = 0.5) +
  geom_segment(aes(xend = Date, yend = M_roll), color = "blue", linetype = "dashed") +
  labs(x = "Dates", y = "Lebna_area.Km2.", title = "LOESS with Residuals")

# Print the mean distance
print(paste("Mean absolute error =", mean_distance))





###############################################################################################
# Read and assign CSV files to individual variables
for (i in 1:7) {
  file_path <- paste("./", c("AWEI", "B1_1500", "MBWI", "MNDWI", "NDVI", "NDWI", "SWI")[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}

zero_rem1 <- which(result_df1[, 9] == 0)
zero_rem3 <- which(result_df3[, 9] == 0)
zero_rem4 <- which(result_df4[, 9] == 0)
zero_rem5 <- which(result_df5[, 9] == 0)
zero_rem6 <- which(result_df6[, 9] == 0)
zero_rem7 <- which(result_df7[, 9] == 0)

rows_to_remove <- which(result_df2[, 9] > 0.000100)

f_df1 <- result_df1[-rows_to_remove, ]
f_df1 <- result_df1[-zero_rem1, ]
f_df2 <- result_df2
f_df3 <- result_df3[-rows_to_remove, ]
f_df3 <- result_df1[-zero_rem3, ]
f_df4 <- result_df4[-rows_to_remove, ]
f_df4 <- result_df4[-zero_rem4, ]
f_df5 <- result_df5[-rows_to_remove, ]
f_df5 <- result_df5[-zero_rem5, ]
f_df6 <- result_df6[-rows_to_remove, ]
f_df6 <- result_df6[-zero_rem6, ]
f_df7 <- result_df7[-rows_to_remove, ]
f_df7 <- result_df7[-zero_rem7, ]


library(ggplot2)
library(zoo)

data_frames <- list(f_df1, f_df3, f_df4, f_df5, f_df6, f_df7)


# Create an empty vector to store mean distances
mean_distances <- c()

# Iterate through each data frame
for (i in 1:length(data_frames)) {
  data <- data_frames[[i]]
  data <- data[order(data$Date), ]
  data$Date <- as.Date(data$Date)
  data$Date <- as.numeric(data$Date)
  x <- data$Date
  y <- data[,9]
  
  # Median Rolling Average
  M_roll <- c(y[1] ,y[1],
              zoo::rollapply(y, width=5, FUN=median),
              y[length(y)], y[length(y)])
  
  # Calculate the residuals
  residuals <- y - M_roll
  
  # Calculate the mean distance from the residuals
  mean_distance <- mean(abs(residuals))           # Mean absolute error (MAE) 
  
  # Save the mean distance in the vector
  mean_distances <- c(mean_distances, mean_distance)
}

# Print the mean distances
print(mean_distances)

