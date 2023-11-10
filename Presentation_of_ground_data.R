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
res <- 5

# Remove rows where a condition is met in result_df2 (B1_1500)
rows_to_remove <- which(result_df2[, res] > 0.001)
f_df1 <- result_df1[-rows_to_remove, ]
f_df2 <- result_df2
f_df3 <- result_df3[-rows_to_remove, ]
f_df4 <- result_df4[-rows_to_remove, ]
f_df5 <- result_df5[-rows_to_remove, ]
f_df6 <- result_df6[-rows_to_remove, ]
f_df7 <- result_df7[-rows_to_remove, ]

# In-situ data
insitu <- read.csv("./Data/Surface_Volume_Lebna_2017-2023.csv")
insitu$Date <- as.Date(insitu$Date)
insitu$S_ha <- insitu$S_m2 * 1e-4
Surf <- insitu$S_ha




points_data <- data.frame(x = as.Date(f_df1$Date), y = 0)

breaks.vec <- seq(lubridate::ymd("2017-01-01"), lubridate::ymd("2023-12-01"),
                  by = "3 months")
  
f = insitu$V_m3 *1e-4
f

# Creating a ggplot with two different scales
plot <- ggplot(insitu, aes(x = Date)) +
  geom_line(aes(y = V_m3 *1e-4, color = "Volume"), size = 1) +
  geom_line(aes(y = S_ha, color = "Surface Area"), size = 1) +
  scale_y_continuous(
    name = "Surface Area (ha)",
    sec.axis = sec_axis(~ . * 1 , name = expression(paste("Volume", frac('m³', '10000'))))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
  scale_x_date(breaks = breaks.vec, date_labels = "%m-%Y") +
  geom_point(data = points_data, aes(x = x, y = y), color = 'black', shape = 124, size = 3)

print(plot)
