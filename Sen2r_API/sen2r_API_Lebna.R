#                                                                              #
#               {}|{}                                                          #
#               /o|o\                                                          #
#  ___________:/o|||o\}___________    MGI Internship  | S2Water                #
#   =|=|=|=|:S|":|||:"|K:|=|=|=|=     Author          : Sotirios Kechagias     #
#    """"""""""\:|||:/""""""""""      Created         : July 07, 2023          #
#               \.|./                 Last update     : August 11, 2023        #
#               /o.o\                 R Version       : 4.3.1                  #
#              /o.o.o\                LICENSE         : CC BY-NC-SA 4.0        #
#            #|o.o.o.o|#                                                       #
#                                                                              #


#### Package Import ####
pkgTest <- function(x){  #     pkgTest is a helper function to load packages and
                         # install packages only when they are not installed yet
  if (x %in% rownames(installed.packages()) == FALSE){
    install.packages(x, dependencies= TRUE)}
  library(x, character.only = TRUE)}
neededPackages <- c("raster", "methods", "sf", "stars", "data.table", "XML",
                    "jsonlite", "geojsonio", "foreach", "parallel", "sen2r",
                    "doParallel", "httr", "RcppTOML")
for (package in neededPackages){pkgTest(package)}

#### Data Preparation ####
# Create directories
setwd("C:/Projects/S2Water")       # ("C:/Projects/S2Water")
getwd()                                        # check working directory
# setwd('..')                                    # Go back one level
data_path   <- "./Data"
output_path <- "./Output"
if (!dir.exists(data_path))   {dir.create(data_path)}
if (!dir.exists(output_path)) {dir.create(output_path)}

safe_dir <- tempfile(pattern = "sen2r_safe_")  # folder to store downloaded SAFE


AOI <- sf::st_read("./Data/Lebna_catchment_boundaries.geojson")

list <- sen2r::s2_list(
  tile =          "32SPF",
  orbit =         122,
  time_interval = c(as.Date("2022-01-01"),
                    as.Date("2023-01-01")),
  level =         "auto",
  server =        "scihub",
  service =       "apihub",
  max_cloud =     10,
  availability =  "check",
  output_type =   "deprecated"
)  ; my_df <- as.data.frame(list) ; write.csv(my_df, "./Lebna.csv")

sen2r::s2_order(list)

start_time <- Sys.time()
expo <- sen2r(
  gui =               FALSE,
  sel_sensor =        c("s2a", "s2b"),
  server =            "scihub",
  step_atmcorr =      "auto", # means that L2A is first searched on SciHub: if 
                              # found, it is downloaded, if not, the L1C is
                              # downloaded and sen2cor applied
  # sen2cor_use_dem = F       # This produce Level 2AP
  # sen2cor_gipp =    NA,        
  max_cloud_safe =    10,
  timewindow =        c(as.Date("2022-01-01"),
                        as.Date("2023-01-01")),
  extent =            AOI,
  extent_name =       "AOI",
  s2tiles_selected =  c("32SPF"),
  s2orbits_selected = c("122"),
  list_prods =        c("BOA"),
  list_rgb =          c("RGB432B"),
  # list_indices =    c("NDWI", "NDWI2"),
  index_source =      "BOA",
  # mask_type =       NA, # because of glint
  max_mask =          100, 
  clip_on_extent =    TRUE,
  extent_as_mask =    TRUE,
  # overwrite =       TRUE,
  path_l1c =          safe_dir,
  path_l2a =          safe_dir,
  path_out =          data_path,
  thumbnails =        FALSE,
  parallel =          3
) ; end_time <-       Sys.time()
cat("Runtime: ", round(as.numeric(difftime(end_time,start_time, units = "min")),
                                  digits = 3),"m", sep = "")

