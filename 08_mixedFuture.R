
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr, doSNOW, foreach)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use
mixedCategory <- function(pathClust, pathUnc, pathProb, thr.prb, thr.unc){
  
  lyrClust <- raster(pathClust)
  lyrUnc   <- raster(pathUnc)
  lyrProb  <- raster(pathProb)# To 2 criterios
  
  result <- lyrClust
  
  result[which(lyrUnc[] < thr.unc & lyrProb[] > thr.prb)] <- max(unique(lyrClust[]), na.rm = T) + 1
  
  return(result)
  
  
  
}

# Load data
pth.cls <- '../_rf/_output/_run1/_results/_process/_limitations/RF_3_Clust_lim_current.asc'
pth.unc <- '../_rf/_output/_run1/_results/_raw/RF_5Unc_current.asc'
pth.prb <- '../_rf/_output/_run1/_results/_raw/RF_5Prob_current.asc'
load('../_rData/_run1/threshold_prob.rData')
load('../_rData/_run1/threshold_unc.rData')

thr.prb <- threshold
thr.unc <- thrUnc1

models <- list.files('../_rf/_output/_run1/_results/_process/_limitations/_2050') %>%
  gsub('RF_3Clust_lim_', '', .) %>%
  gsub('.asc', '', .) 
no.clusters <- 3
years <- '_2050'

# Parallelizations
cl <- makeCluster(7) 
registerDoSNOW(cl)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach', 'sp', 'stringr'), .verbose = TRUE) %dopar% {
  
  print(models[i]) 
  
  print(years[j])
    
  path_clust <- paste('../_rf/_output/_run1/_results/_process/_limitations/_2050', sep = '/') %>%
      list.files(., full.names = T, pattern = '.asc') %>% 
      grep('lust', ., value = T) %>%
      grep(models[i], ., value = T, fixed = T) %>%
      .[1]
    
  path_Unc  <- paste('../_rf/_output/_run1/_results/_raw/_2050') %>%
      list.files(., full.names = T, pattern = '.asc') %>% 
      grep('Unc', ., value = T) %>%
      grep(models[i], ., value = T, fixed = T) %>%
      .[1]
    
  path_prob <- paste('../_rf/_output/_run1/_results/_raw/_2050', sep = '/') %>%
      list.files(., full.names = T, pattern = '.asc') %>% 
      grep('Prob', ., value = T) %>%
      grep(models[i], ., value = T, fixed = T) %>%
      .[1]
    
  mixta <- mixedCategory(pathClust = path_clust,
                         pathUnc = path_Unc,
                         pathProb = path_prob,
                         thr.unc = thr.unc,
                         thr.prb = thr.prb)
    
  writeRaster(mixta, paste0('../_rf/_output/_run1/_results/_process/_mixed/_2050/RF_', no.clusters, 'Classes_unc_', models[i], '.asc'), overwrite = T)
    
}
  





