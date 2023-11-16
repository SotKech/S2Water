#'
#'                      MGI Internship  :   S2Water - 05InsituComparison.R
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-11-05
#'                      Last update     :   2023-11-07
#'                      R Version       :   4.3.1
#'                      Packages        :   lubridate, dplyr, ggplot2
#'                      LICENSE         :   CC BY-NC-SA 4.0
#'


#### Package Import ####
# pkgTest function loads and install packages only when are not installed yet.
neededPackages <- c("lubridate", "dplyr", "ggplot2")
pkgTest <- function(x){
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)
}
for (package in neededPackages){pkgTest(package)}


#### Set up directories ####
getwd()
# Load boundaries of reservoirs
AOI_b <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")

# Read and assign CSV files to individual variables
for (i in 1:7) {
  file_path <- paste("./Indices/",
                     c("AWEI", "B1_1500", "MNDWI",
                       "NDVI", "NDWI", "SWI", "MBWI")[i], ".csv", sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}

reservoir_name <- list("Lebna", "Akrane", "Ain Soudan", "Gombar", "Errouiguet",
                       "El Hajl", "Ben Salem", "El Guitoun", "Gbail", "Kamech",
                       "Reservoir 11", "Ennar")


# In-situ data
insitu <- read.csv("./Data/Surface_Volume_Kamech_2017-2023.csv")
insitu$Date <- as.Date(insitu$Date)
insitu$S_ha <- insitu$S_m2 * 1e-4




generate_and_display_merged_plots <- function(data1, data2, data3,
                                              data4, data5, data6,
                                              data7, insitu, reservoir) {
  # Convert Date column to Date type for both dataframes
  for (i in 1:7) {
    assign(paste("data", i, sep = ""),
           within(get(paste("data", i, sep = "")),
                  Date <- as.Date(Date)))
  }
  breaks.vec <- seq(lubridate::ymd("2017-01-01"), lubridate::ymd("2023-12-01"),
                    by = "3 months")
  # Create the merged plot
  p <- ggplot() +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_line(data = insitu,
              aes(x = Date, y = S_ha, color = "INSITU")) +
    geom_line(data = data1,
              aes(x = Date, y = .data[[reservoir]], color = "AWEI"),
              linetype = "dashed") +
    geom_line(data = data3,
              aes(x = Date, y = .data[[reservoir]], color = "MNDWI")) +
    geom_line(data = data4,
              aes(x = Date, y = .data[[reservoir]], color = "NDVI")) +
    geom_line(data = data5,
              aes(x = Date, y = .data[[reservoir]], color = "NDWI")) +
    geom_line(data = data6,
              aes(x = Date, y = .data[[reservoir]], color = "SWI"),
              linetype = "dotdash") +
    geom_line(data = data7,
              aes(x = Date, y = .data[[reservoir]], color = "MBWI"),
              linetype = "dashed") +
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
    geom_point(data = data7,
               aes(x = Date, y = .data[[reservoir]], color = "MBWI")) +
    geom_point(data = insitu,
               aes(x = Date, y = S_ha, color = "INSITU"), size=0.9) +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    scale_x_date(breaks = breaks.vec, date_labels = "%m-%Y") +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = reservoir_name[j], color = "Indices") +
    scale_color_manual(values = c("AWEI" = "#f8766d",     "MNDWI" = "#2bd4d6",
                                  "NDVI" = "#4daf4a",     "NDWI" = "#377eb8",
                                  "SWI" = "#f564e3",      "MBWI" = "#9e854e",
                                  "INSITU" = "black")) +
    xlab("Date") +
    ylab(bquote("Water Surface ha")) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot(p)
  ggsave(paste0("./Output/Graphs/Insitu_", paste(j,"_", sep = ""),
                reservoir_name[j],".png", sep = ""),
         plot = p, width = 17, height = 7, dpi = 400,)
}



# Define j. (j = 1) for Lebna | (j = 10) for Kamech
j <- 10
# Define the column name
reservoir_col <- paste0("Reservoir_", j, "_area_ha")
reservoirs <- c(character(0), reservoir_col)

# Find row indices in df1 where the column value is greater than 0
rows_to_remove <- which(result_df2[reservoirs] > 0.001)

# Remove corresponding rows from all data frames
f_df1 <- result_df1[-rows_to_remove, ]
f_df2 <- result_df2
f_df3 <- result_df3[-rows_to_remove, ]
f_df4 <- result_df4[-rows_to_remove, ]
f_df5 <- result_df5[-rows_to_remove, ]
f_df6 <- result_df6[-rows_to_remove, ]
f_df7 <- result_df7[-rows_to_remove, ]
insitu_new <- insitu[-rows_to_remove, ]

# Plot the current Reservoir column
generate_and_display_merged_plots(f_df1, f_df2, f_df3,
                                  f_df4, f_df5, f_df6,
                                  f_df7, insitu, reservoirs)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Store the dataframes in a list
df_list <- list(f_df1, f_df3, f_df4, f_df5, f_df6, f_df7)

# Create empty vectors to store the results
indices <- c("AWEI", "MNDWI", "NDVI", "NDWI", "SWI", "MBWI")
rmse_vec <- c()
r2_vec <- c()
bias_vec <- c()

# Loop through the dataframes
for (i in seq_along(df_list)) {
  # Merge dataframes based on common dates
  common_dates_df <- merge(df_list[[i]], insitu_new, by = "Date")
  # Extract the common data
  common_set1 <- common_dates_df[[paste0(reservoir_col)]]
  common_set2 <- common_dates_df$S_ha
  # Calculate RMSE
  rmse <- sqrt(mean((common_set1 - common_set2)^2))
  rmse_vec <- c(rmse_vec, rmse)
  # Calculate R2
  mean_common_set2 <- mean(common_set2)
  ssr <- sum((common_set1 - common_set2)^2)
  sst <- sum((common_set1 - mean_common_set2)^2)
  r2 <- 1 - (ssr / sst)
  r2_vec <- c(r2_vec, r2)
  # Calculate bias
  bias <- mean(common_set1 - common_set2)
  bias_vec <- c(bias_vec, bias)
}

# Create a dataframe
Validation_df <- data.frame(Indices = indices, RMSE = rmse_vec,
                            R2 = r2_vec, Bias = bias_vec)

# Print the results dataframe
print(Validation_df)
write.csv(Validation_df, paste0("./Output/Indices_", paste(j,"_", sep = ""),
                                reservoir_name[j], ".csv", sep = ""))
                                







