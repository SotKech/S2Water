#                                                                              #
#               {}|{}                                                          #
#               /o|o\                                                          #
#  ___________:/o|||o\}___________    MGI Internship  | S2Water                #
#   =|=|=|=|:S|":|||:"|K:|=|=|=|=     Author          : Sotirios Kechagias     #
#    """"""""""\:|||:/""""""""""      Created         : July 07, 2023          #
#               \.|./                 Last update     : July 07, 2023          #
#               /o.o\                 R Version       : 4.3.1                  #
#              /o.o.o\                LICENSE         : CC BY-NC-SA 4.0        #
#            #|o.o.o.o|#                                                       #
#                                                                              #


#### Package Import ####
pkgTest <- function(x){   #    pkgTest is a helper function to load packages and
                          # install packages only whenthey are not installed yet
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)}
neededPackages <- c("sen2r", "sf")
for (package in neededPackages){pkgTest(package)}



#### Data Preparation ####
# Create directories
setwd("C:/Projects/S2Water")
getwd()                       # check if your working directory is correctly set
data_path   <- "./Data"
output_path <- "./Output"
if (!dir.exists(data_path))   {dir.create(data_path)}
if (!dir.exists(output_path)) {dir.create(output_path)}

safe_dir <- tempfile(pattern = "sen2r_safe_")  # folder to store downloaded SAFE


#                                  NOTES
#-------------------------------------------------------------------------------     
# In order to allor Sen2Cor performing topographic correction, use functions
# sen2cor() and sen2r() with the following arguments: sen2cor(...,
# use_dem = TRUE) sen2r(..., sen2cor_use_dem = TRUE)
#
# In order to use (sen2cor {sen2r}) you need a directory of L1C - so the plan is
# to check with (s2_list) the amount of L1C and L2A images and decide based on
# the most amount of images
#-------------------------------------------------------------------------------






AOI <- sf::st_read("./Data/Lebna_catchment_boundaries.geojson")

list <- s2_list(
  tile = "32SPF",
  orbit = 122,
  time_interval = c(as.Date("2017-01-01"), as.Date("2023-07-01")),
  level = "auto",
  server = "scihub",
  service = "apihub",
  max_cloud = 10,
  availability = "check",
  output_type = "deprecated"
) # ; my_df <- as.data.frame(list) ; write.csv(my_df, "./outputM.csv")



start_time <- Sys.time()
expo <- sen2r(
  gui = FALSE,
  sel_sensor = c("s2a", "s2b"),
  server = "scihub",
  step_atmcorr = "auto", # means that L2A is first searched on SciHub: if found,
                         # it is downloaded, if not, the corresponding Level-1C 
                         # is downloaded and   sen2cor is used to produce L2A
  sen2cor_use_dem = TRUE,
  # sen2cor_gipp = NA,   #             Ground Image Processing Parameters (GIPP)
  max_cloud_safe = 10,
  timewindow = c(as.Date("2022-12-20"), as.Date("2023-01-5")),
  extent = AOI,
  extent_name = "AOI",
  s2tiles_selected = c("32SPF"),
  s2orbits_selected = c("122"),
  list_prods = c("BOA"),
  # list_rgb = c("RGB432B"),
  list_indices = c("NDWI"),
  index_source = "BOA",
  mask_type = "cloud_and_shadow",
  max_mask = 10, 
  clip_on_extent= TRUE,
  extent_as_mask= TRUE,
  # overwrite = TRUE
  path_l1c = safe_dir,
  path_l2a = safe_dir,
  path_out = data_path,
  thumbnails = FALSE,
) ; end_time <- Sys.time()
cat("Runtime: ", round(as.numeric(difftime(end_time,start_time, units = "min")),
                                                      digits = 3),"s", sep = "")


