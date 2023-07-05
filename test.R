# Set paths
out_dir_1  <- tempfile(pattern = "sen2r_out_1_") # output folder
safe_dir <- tempfile(pattern = "sen2r_safe_")  # folder to store downloaded SAFE

myextent_1 <- system.file("extdata/vector/barbellino.geojson", package = "sen2r") 

AOI <- sf::st_read("./Data/Lebna_catchment_boundaries.geojson")


library(sen2r)
out_paths_1 <- sen2r(
  gui = FALSE,
  step_atmcorr = "l2a",
  extent = AOI,
  extent_name = "AOI",
  timewindow = c(as.Date("2023-06-27"), as.Date("2023-06-30")),
  list_prods = c("BOA","SCL"),
  # list_indices = c("NDVI","MSAVI2"),
  list_rgb = c("RGB432B"),
  mask_type = "cloud_and_shadow",
  max_mask = 10, 
  path_l2a = safe_dir,
  path_out = out_dir_1,
  clip_on_extent= TRUE,
  extent_as_mask= TRUE,
  overwrite = TRUE
)
