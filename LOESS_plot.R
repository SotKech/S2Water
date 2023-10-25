library(ggplot2)
library(zoo)



# Read and assign CSV files to individual variables
for (i in 1:7) {
  file_path <- paste("./", c("AWEI", "B1_1500", "MBWI", "MNDWI", "NDVI", "NDWI", "SWI")[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}

# colnames(f_df1)
res = 27


zero_rem1 <- which(result_df1[, res] == 0)
zero_rem3 <- which(result_df3[, res] == 0)
zero_rem4 <- which(result_df4[, res] == 0)
zero_rem5 <- which(result_df5[, res] == 0)
zero_rem6 <- which(result_df6[, res] == 0)
zero_rem7 <- which(result_df7[, res] == 0)

rows_to_remove <- which(result_df2[, res] > 0.000100)

f_df1 <- result_df1[-rows_to_remove, ]
f_df1[zero_rem1, ] <- NA
f_df2 <- result_df2

f_df3 <- result_df3[-rows_to_remove, ]
f_df3[zero_rem3, ] <- NA
f_df4 <- result_df4[-rows_to_remove, ]
f_df4[zero_rem4, ] <- NA
f_df5 <- result_df5[-rows_to_remove, ]
f_df5[zero_rem5, ] <- NA
f_df6 <- result_df6[-rows_to_remove, ]
f_df6[zero_rem6, ] <- NA
f_df7 <- result_df7[-rows_to_remove, ]
f_df7[zero_rem7, ] <- NA



# Function to process each data frame and add to the plot
process_data_frame <- function(df, color, p, label) {
  df <- df[order(df$Date), ]
  df$Date <- as.Date(df$Date)
  x <- as.numeric(df$Date)
  y <- df[, res]
  
  loess_fit <- loess(y ~ x, span = 0.15, data = df)
  y_pred <- predict(loess_fit, data.frame(x = x))
  x <- as.Date(x)
  p <- p + geom_line(data = df, aes(x = x, y = y_pred, color = label), size = 0.75) +
    geom_text(data = data.frame(x = max(x), y = max(y_pred), label = label), 
              aes(x = x, y = y, label = label), hjust = -20, vjust = 0, color = color)
  return(p)
}

# Create an empty ggplot object
p <- ggplot()

# List of data frames
data_frames <- list(f_df1, f_df3, f_df4, f_df5, f_df6, f_df7)
colors <- c('black', 'blue', 'green', 'orange', 'purple', 'brown')
labels <- c('AWEI', 'MBWI', 'MNDWI', 'NDVI', 'NDWI', 'SWI')

# Process each data frame and add to the plot
for (i in seq_along(data_frames)) {
  p <- process_data_frame(data_frames[[i]], colors[i], p, labels[i])
}

# Display the plot with labels
p <- p + xlab("Date") + ylab(paste0(colnames(f_df1)[res])) + theme(legend.position = "top") 
print(p)

ggsave(paste0("./Graph_",paste0(colnames(f_df1)[res]),".png", sep = ""),
       plot = p, width = 12, height = 6.5, dpi = 300,)



