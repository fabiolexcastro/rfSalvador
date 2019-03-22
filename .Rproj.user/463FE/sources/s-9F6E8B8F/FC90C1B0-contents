
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use
limitations <- function(path_lyr_prob, path_lyr_clust, path_output, nameOutput, no.clusters, no.absenceclasses){
  
  lyr_prob <- raster(path_lyr_prob)
  lyr_clust <- raster(path_lyr_clust)
  
  mtx_prob <- matrix(c(0, threshold, 0, threshold, 1, 2), ncol = 3, byrow = T)
  mtx_clust <- matrix(c(0.5, no.absenceclasses + 0.5, 0, no.absenceclasses + 0.5, no.absenceclasses + no.clusters + 0.5, 1), nrow = 2, byrow = T)
  
  lyr_prob_rcl <- raster::reclassify(lyr_prob, mtx_prob)
  lyr_clust_rcl <- raster::reclassify(lyr_clust, mtx_clust)
  
  print('To make the difference')
  diff <- lyr_prob_rcl - lyr_clust_rcl
  result <- lyr_clust
  result[which(diff[] == -1)] <- no.absenceclasses + no.clusters + 1
  result[which(diff[] == 2)]  <- no.absenceclasses + no.clusters + 1
  
  print('To Write')
  writeRaster(result, paste(path_output, nameOutput, sep = '/'))
  
}

# Load data
gcm <- 'current'
no.absenceclasses <- 2

# Calculating the threshold
load('../_rData/_run1/threshold_prob.rData')

# Applying the function
limitations(path_lyr_prob = '../_rf/_output/_run1/_results/_raw/RF_5Prob_current.asc',
            path_lyr_clust = '../_rf/_output/_run1/_results/_raw/RF_5Clust_current.asc',
            path_output = '../_rf/_output/_run1/_results/_process',
            nameOutput = paste0('RF_3_Clust_lim_', gcm, '.asc'),
            no.absenceclasses = 2, 
            no.clusters = 3)









