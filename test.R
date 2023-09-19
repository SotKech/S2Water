# Load required libraries
library(raster)

# Load your RGB image
rgb_image <- raster("./Data/BOA/S2B2A_20210714_122_AOI_BOA_10.tif")
# S2B2A_20210714_122_AOI_RGB432B_10

B1 <- rgb_image[[1]]
plot(B1)

CloudMask <- B1[[1]] > 1500 ; plot(CloudMask)



#'
#'                      MGI Internship  :   S2Water - Analysis
#'                      Author          :   Sotirios Kechagias
#'                      Created         :   2023-06-21
#'                      Last update     :   2023-08-30
#'                      R Version       :   4.3.1
#'                      Packages        :   base, raster, ggplot2, sf
#'                      LICENSE         :   CC BY-NC-SA 4.0
#'




