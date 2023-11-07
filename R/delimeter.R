# Read and assign CSV files to individual variables
result_dfs <- list()
for (i in 1:8) {
  file_path <- paste("./Indices/", c("AWEI", "B1_1500", "LSWI", "MBWI", "MNDWI", "NDVI", "NDWI", "SWI")[i], ".csv", sep = "")
  result_dfs[[i]] <- read.csv(file_path)
}

# Function to multiply specific columns by 100
multiply <- function(df) {
  for (i in 1:12) {
    col_name <- paste("Reservoir_", i, "_area.Km2.", sep = "")
    df[[col_name]] <- df[[col_name]] * 100
  }
  return(df) # return the modified data frame
}

# Applying the function to the data frames and exporting to CSV
for (i in 1:8) {
  df <- result_dfs[[i]]
  modified_df <- multiply(df)
  file_name <- paste("./Indices/", c("AWEI", "B1_1500", "LSWI", "MBWI", "MNDWI", "NDVI", "NDWI", "SWI")[i],".csv", sep = "")
  write.csv(modified_df, file_name, row.names = FALSE)
}
