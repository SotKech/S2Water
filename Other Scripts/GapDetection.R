# starting from a csv file with spectral indices shared on the
# shared-data directory. This line of code displays the dates after which
# there is a gap of more than 30 days

# Read the CSV file into a data frame
data <- read.csv('NDVI.csv',sep=",")
data <- data[order(data$Date), ]
# Convert the "Date" column to Date objects
data$Date <- as.Date(data$Date, "%Y-%m-%d")
# Calculate the number of days passed from the previous row
data$DaysPassed <- c(NA, diff(data$Date))
data$DaysPassed



# Write the 'data' DataFrame to a new CSV file
write.csv(data, file = "output.csv", row.names = FALSE)