

# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, tidyverse)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use
mixedCategory <- function(pth.cls, pth.unc, pth.prb, threshold, no.absenceclasses){
  lyrClust <- raster(pth.cls)
  lyrUnc <- raster(pth.unc)
  lyrPrb <- raster(pth.prb)
  
  thrUnc <- raster::extract(lyrUnc, occ[,1:2])
  thrUnc <- thrUnc[!is.na(thrUnc)]
  thrUnc1  <- quantile(thrUnc, 0.1) %>% as.numeric()
  quantile(thrUnc, seq(0, 1, 0.01))
  min(thrUnc)
  
  save(thrUnc1, file = '../_rData/_run1/threshold_unc.rData')
  
  rslt <- lyrClust
  rslt[which(lyrUnc[] < thrUnc1 & lyrPrb[] > threshold)] <- max(unique(lyrClust[]), na.rm = T) + 1 
  print('To write the raster')
  writeRaster(rslt, paste0('../_rf/_output/_run1/_results/_process/RF_5Classes_unc_', gcm, '.asc'), overwrite = TRUE)
  print('¡Done!')
}

# Load data
run <- '_run1'
load('../_rData/_run1/threshold_prob.rData')
load('../_rData/_run1/clustereddata.rData')
gcm <- 'current'

pth.cls <- '../_rf/_output/_run1/_results/_process/_limitations/RF_3_Clust_lim_current.asc '
pth.unc <- '../_rf/_output/_run1/_results/_raw/RF_5Unc_current.asc'
pth.prb <- '../_rf/_output/_run1/_results/_raw/RF_5Prob_current.asc'
pth.out <- '../_rf/_output/_run1/_results/_raw'

no.absenceclasses <- 2
no.clusters <- 3

occ <- occ[,1:2]

mixedCategory(pth.cls, pth.unc, pth.prb, threshold, no.absenceclasses)

