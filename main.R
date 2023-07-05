#
#               {}|{}
#               /o|o\
#  ___________:/o|||o\}___________    MGI Internship  | S2Water
#   =|=|=|=|:S|":|||:"|K:|=|=|=|=     Author          : Sotirios Kechagias
#    """"""""""\:|||:/""""""""""      Created         : July 07, 2023
#               \.|./                 Last update     : July 07, 2023
#               /o.o\                 R Version       : 4.3.1
#              /o.o.o\                LICENSE         : CC BY-NC-SA 4.0
#            #|o.o.o.o|#
#
#-------------------------------------------------------------------------------

#### Package Import ####
pkgTest <- function(x){
  # pkgTest is a helper function to load packages and install packages only when
  # they are not installed yet.
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("sen2r", "sf")
for (package in neededPackages){pkgTest(package)}



#### Data Preparation ####
# Create directories
setwd("C:/Projects/S2Water")
getwd() # check if your working directory is correctly set
data_path   <- "./Data"
output_path <- "./Output"
if (!dir.exists(data_path))   {dir.create(data_path)}
if (!dir.exists(output_path)) {dir.create(output_path)}


# Set paths
safe_dir <- tempfile(pattern = "sen2r_safe_")  # folder to store downloaded SAFE

AOI <- sf::st_read("./Data/Lebna_catchment_boundaries.geojson")

list <- s2_list(
  tile = "32SPF",
  orbit = 122,
  time_interval = c(as.Date("2022-12-20"), as.Date("2023-02-14")),
  level = "L2A",
  server = "scihub",
  service = "apihub",
  max_cloud = 10,
  output_type = "deprecated"
) ; list

list_SAFE <- safe_is_online(list) ; list_SAFE


start_time <- Sys.time()
expo <- sen2r(
  gui = FALSE,
  s2tiles_selected = "32SPF",
  step_atmcorr = "auto", # means that L2A is first searched on SciHub: if found, it is downloaded, if not, the corresponding Level-1C is downloaded and sen2cor is used to produce L2A
  extent = AOI,
  extent_name = "AOI",
  timewindow = c(as.Date("2022-12-20"), as.Date("2023-02-14")),
  list_prods = c("BOA"),
  # list_indices = c("NDVI","MSAVI2"),
  # list_rgb = c("RGB432B"),
  max_cloud_safe = 10,
  mask_type = "cloud_and_shadow",
  max_mask = 10, 
  path_l2a = safe_dir,
  path_out = output_path,
  clip_on_extent= TRUE,
  extent_as_mask= TRUE,
  overwrite = TRUE
) ; end_time <- Sys.time()
cat("Runtime: ", round(as.numeric(
  difftime(end_time,start_time, units = "secs")),
  digits = 3),"s", sep = "")


