
# Load libraries
library(pacman)
pacman::p_load(raster, rgdal, tidyverse)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
run <- '_run1'
years <- '_2050'
yrs <- paste0('../_rf/_output/_run1/_results/_process/_mixed/_2050/') %>%
  list.files(full.names = TRUE, pattern = '.asc$') %>% 
  stack()

# To make the modal
lyr.mdl <- raster::modal(lyrs)
writeRaster(lyr.mdl, paste0('../_RF/_output/_run1/_results/_process/_mixed/RF_3classes_unc_future.asc'), overwrite = TRUE)
  

  




