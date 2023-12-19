# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Flags - Level and >10% cloud
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            #### Package Import ####
# pkgTest function loads and install packages only when are not installed yet.
neededPackages <- c("raster", "lubridate", "sf", "dplyr", "ggplot2")
pkgTest <- function(x){
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)
}
for (package in neededPackages){pkgTest(package)}


                          #### Set up directories ####
getwd()
# Create Graphs folder if does not exist
Graphs_path   <- "./Output/Graphs"
if (!dir.exists(Graphs_path))   {dir.create(Graphs_path)}
# Load boundaries of reservoirs
AOI_b <- sf::st_read("./Data/Lebna_reservoirs_buffered.geojson")
# Load flags
data <- read.csv('./Indices/Flags.csv', sep=",")
# Load reservoir names
# reservoir_name <- list("Lebna", "Akrane", "Ain Soudan", "Gombar", "Errouiguet",
#                        "El Hajl", "Ben Salem", "El Guitoun", "Gbail", "Kamech",
#                        "Reservoir 11", "Ennar")


reservoir_name <- list("Maddur Tank (NW)", "Berambadi Dam",
                       "Channamallipura_1 (NW)", "Channamallipura_2 (NW)",
                       "Channamallipura_3 (NW)", "No_Name_6 (NW)",
                       "Kunagahalli Dam (SW)", "Berambadi Temple Tank",
                       "Gopalaswamy Temple Tank (SW)", "No_Name_10 (NE)",
                       "No_Name_11 (Center)", "Kannegala Village Tank",
                       "Kannegala River Tank", "Honnegowdanahalli Tank (SE)",
                       "id_15", "Kunagahalli Tank (SW)", "id_17", "id_18",
                       "id_19", "No_Name_20 (NW)", "No_Name_22 (SW)",
                       "No_Name_23 (SW)", "No_Name_24 (SW)", "Parvatana Katte",
                       "Gopalapura Tank", "Devarahalli Dam (SE)", 
                       "Motana katte", "No_Name_29 (Center)", "Kutunur Tank",
                       "No_Name_31 (Center)", "No_Name_32 (Center)",
                       "No_Name_33 (Center)", "No_Name_34 (NE)",
                       "No_Name_35 (NE)", "Gopalaswami Dam (SE)",
                       "Kaggaladundi Temple Tank (NW)", "id_37",
                       "Maramma Temple - Tank (NE)", "No_Name_40 (NE)",
                       "No_Name_41 (SE)", "No_Name_43 (NW)", "Belavina Katte",
                       "Kallipura Tank (SE)", "Forest Tank (SE)",
                       "No_Name_47 (Center)", "No_Name_48 (SE)", "id_47",
                       "No_Name_50 (SW)", "Lakki Katte", "Navilugundi Kere",
                       "Kuruti katte")




# Read and assign CSV files to individual variables
for (i in 1:7) {
  file_path <- paste("./Indices/", c("AWEI", "B1_1500", "MNDWI", "NDVI", "NDWI",
                                     "SWI",  "MBWI")[i], ".csv",  sep = "")
  assign(paste("result_df", i, sep = ""), read.csv(file_path))
}

# Use interchangeably
Level <- data$Level
Manual <- data$Above_10_cloud


generate_and_display_merged_plots <- function(data1, data2, data3,
                                              data4, data5, data6,
                                              data7, reservoir, level) {
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
    geom_line(data = data1,
              aes(x = Date, y = .data[[reservoir]], color = "AWEI")) +
    # geom_bar( data = data2, alpha = 0.4, stat = "identity",
    #           aes(x = Date, y = .data[[reservoir]], fill = "B1_1500")) +
    geom_line(data = data3,
              aes(x = Date, y = .data[[reservoir]], color = "MNDWI")) +
    geom_line(data = data4,
              aes(x = Date, y = .data[[reservoir]], color = "NDVI")) +
    geom_line(data = data5,
              aes(x = Date, y = .data[[reservoir]], color = "NDWI")) +
    geom_line(data = data6,
              aes(x = Date, y = .data[[reservoir]], color = "SWI")) +
    geom_line(data = data7,
              aes(x = Date, y = .data[[reservoir]], color = "MBWI")) +
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
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_point(data = data1, aes(x = Date, y = .data[[reservoir]],
                  shape = ifelse(level == 1, "FLAG", "NORM"),
                  color = ifelse(level == 1, "FLAG", "AWEI"))) +
    geom_point(data = data3,
               aes(x = Date, y = .data[[reservoir]],
                   shape = ifelse(level == 1, "FLAG", "NORM"),
                   color = ifelse(level == 1, "FLAG", "MNDWI"))) +
    geom_point(data = data4,
               aes(x = Date, y = .data[[reservoir]],
                   shape = ifelse(level == 1, "FLAG", "NORM"),
                   color = ifelse(level == 1, "FLAG", "NDVI"))) +
    geom_point(data = data5,
               aes(x = Date, y = .data[[reservoir]],
                   shape = ifelse(level == 1, "FLAG", "NORM"),
                   color = ifelse(level == 1, "FLAG", "NDWI"))) +
    geom_point(data = data6,
               aes(x = Date, y = .data[[reservoir]],
                   shape = ifelse(level == 1, "FLAG", "NORM"),
                   color = ifelse(level == 1, "FLAG", "SWI"))) +
    geom_point(data = data7,
               aes(x = Date, y = .data[[reservoir]],
                   shape = ifelse(level == 1, "FLAG", "NORM"),
                   color = ifelse(level == 1, "FLAG", "MBWI"))) +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    scale_x_date(breaks = breaks.vec, date_labels = "%m-%Y") +
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = reservoir_name[j], color = "Indices") +
    # scale_fill_manual(name = " ", values = c("B1_1500" = "orange")) +
    scale_color_manual(values = c("AWEI" = "#f8766d",     "MNDWI" = "#2bd4d6",
                                  "NDVI" = "#4daf4a", "NDWI" = "#377eb8",
                                  "SWI" = "#f564e3",     "MBWI" = "#9e854e",
                                  "FLAG" = 'black')) +
    scale_shape_manual(values=c("NORM" = 16, "FLAG" = 1)) +
    guides(shape = "none")+


    xlab("Date") +
    ylab("Water Surface (ha)") +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # plot(p)
  ggsave(paste0("./Output/Graphs/L1C_", paste(j,"_", sep = ""), # Change 
                reservoir_name[j],".png", sep = ""),
         plot = p, width = 17, height = 7, dpi = 400,)
}


# Use a loop to generate the reservoir names and add them to the vector
# for (j in 1:12) {
#   reservoir_col <- paste0("Reservoir_", j, "_area_ha")
#   reservoirs <- c(character(0), reservoir_col)
#   generate_and_display_merged_plots(result_df1, result_df2, result_df3,
#                                     result_df4, result_df5, result_df6,
#                                     result_df7, reservoirs, Manual)
#                                                             # Level/Manual
#   progress <- round((j / 12) * 100, 2)
#   cat(paste0("\r", progress, "%"))
# }
# 
# Loop through Reservoir columns from 1 to 12
for (j in 1:51) {
  # Define the column name
  reservoir_col <- paste0("Reservoir_", j, "_area_ha")
  reservoirs <- c(character(0), reservoir_col)

  # Find row indices in df1 where the column value is greater than 0
  rows_to_remove <- which(result_df2[, reservoirs] > 0.001)

  # Remove corresponding rows from all data frames
  f_df1 <- result_df1[-rows_to_remove, ]
  f_df2 <- result_df2
  f_df3 <- result_df3[-rows_to_remove, ]
  f_df4 <- result_df4[-rows_to_remove, ]
  f_df5 <- result_df5[-rows_to_remove, ]
  f_df6 <- result_df6[-rows_to_remove, ]
  f_df7 <- result_df7[-rows_to_remove, ]
  data1  <- data[-rows_to_remove, ] ;  Level <- data1$Level
                                       #Select: Level/Manual

  # Plot the current Reservoir column
  generate_and_display_merged_plots(f_df1, f_df2, f_df3,
                                    f_df4, f_df5, f_df6,
                                    f_df7, reservoirs, Level) # Level/Manual
  progress <- round((j / 51) * 100, 2)
  cat(paste0("\r", progress, "%"))
}

