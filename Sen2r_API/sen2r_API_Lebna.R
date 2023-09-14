#'  MGI Internship  :   S2Water - sen2r_API_Lebna
#'  Author          :   Sotirios Kechagias
#'  Created         :   2023-06-07
#'  Last update     :   2023-09-12
#'  R Version       :   4.3.1
#'  Packages        :   base, raster, sen2r
#'  License         :   CC BY-NC-SA 4.0
#'  Description     :   This  algorithm utilizing  "sen2r" package to  download, 
#'                      stack bands & clip to AOI based on the given parameters.
#'

#### Package Import ####
# pkgTest is a helper function to load packages and  
# install packages only when are not installed yet.
pkgTest <- function(x){
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)
} ; neededPackages <- c("raster", "sen2r")
for (package in neededPackages){pkgTest(package)}

#### Data Preparation ####
# Create directories.
getwd() ; setwd("C:/Projects/S2Water")
Data_path   <- "./Data"
# Output_path <- "./Output"
Temp_path   <- "./sen2r_temp" # folder to store downloaded SAFE
if (!dir.exists(Data_path))   {dir.create(Data_path)}
# if (!dir.exists(Output_path)) {dir.create(Output_path)}
if (!dir.exists(Temp_path))    {dir.create(Temp_path)} 
# Load Area Of Interest.
AOI <- sf::st_read("./Data/Lebna_catchment_boundaries.geojson")

#### Search for available S2 products####
# Create a CSV with the available images according to the parameters.
list <- sen2r::s2_list(
  tile =          "32SPF",
  orbit =         122,
  time_interval = c(as.Date("2017-01-30"),
                    as.Date("2017-08-5")),
  level =         "auto",
  server =        "scihub",
  # apihub = NA,                 # (Optional)   !! I can define apihub.txt 
  service =       "apihub",
  max_cloud =     10,
  availability =  "check",
  output_type =   "deprecated"
) ; my_df <- as.data.frame(list) ; write.csv(my_df, "./Lebna.csv")
# Order images (Optional)
sen2r::s2_order(list)

#### Start sen2r Download ####
start_time <- Sys.time() ; expo <- sen2r(
  gui =               FALSE,
  sel_sensor =        c("s2a", "s2b"),
  server =            "scihub",
  order_lta =         TRUE,
  step_atmcorr =      "l2a",    # "auto" !!!
  max_cloud_safe =    10,
  timewindow =        c(as.Date("2018-11-07"),
                        as.Date("2019-03-12")),
  extent =            AOI,
  extent_name =       "AOI",
  s2tiles_selected =  c("32SPF"),
  s2orbits_selected = c("122"),
  list_prods =        c("BOA"),
  list_rgb =          c("RGB432B"),
  # list_indices =      c("NDVI", "NDWI", "NDWI2"),
  # index_source =      "BOA",
  mask_type =         NA,      # because of glint
  max_mask =          100, 
  clip_on_extent =    TRUE,
  extent_as_mask =    TRUE,    # !Maybe I need this FALSE cause of NA of bbox!
  overwrite =         FALSE,
  path_l1c =          Temp_path,
  path_l2a =          Temp_path,
  path_out =          Data_path,
  thumbnails =        FALSE,
  parallel =          3,       # MAX speed / CPU load
  processing_order =  "mixed"  # MAX speed / CPU load
) ; end_time <- Sys.time()
cat("Runtime: ", round(as.numeric(difftime(end_time,start_time, units = "min")),
                                  digits = 3), "m", sep = "")

