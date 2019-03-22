
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr, foreach, parallel, doSNOW)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use
mkdirs <- function(fp) {
  
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
  
} 

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
  writeRaster(result, paste(path_output, nameOutput, sep   = '/'), overwrite = TRUE)
  
}

# Load data
load('../_rData/_run1/threshold_prob.rData')
load('../_rData/_run1/clustereddata.rData')
yrs <- '_2050'
lapply(paste0('../_rf/_output/_run1/_results/_process/_limitations/', yrs), mkdirs)

no.absenceclasses <- 2
no.clusters <- 3

gcm <- list.files('../_rf/_output/_run1/_results/_raw/_2050', pattern = 'Clust') %>% 
  gsub('RF_3Clust_', '', .) %>%
  gsub('_2030.asc', '', .) 
years <- '_2050'

cl <- makeCluster(length(6)) 
registerDoSNOW(cl) 

foreach(i = 1:length(gcm), .packages = c('raster', 'dplyr', 'gtools', 'foreach', 'sp', 'stringr'), .verbose = TRUE) %dopar% {
  
  print(gcm[i]) 
    
  path_lyr_prob <- paste('../_rf/_output/_run1/_results/_raw', years[1], sep = '/') %>%
      list.files(., full.names = T, pattern = '.asc') %>% 
      grep('rob', ., value = T) %>%
      grep(gcm[i], ., value = T, fixed = T) %>%
      .[1]
    
  path_lyr_clust <- paste('../_rf/_output/_run1/_results/_raw', years[1], sep = '/') %>%
      list.files(., full.names = T, pattern = '.asc') %>% 
      grep('Clust', ., value = T) %>%
      grep(gcm[i], ., value = T, fixed = T) %>%
      .[1]
    
  limitations(path_lyr_prob = path_lyr_prob, 
              path_lyr_clust = path_lyr_clust,
              path_output = paste0('../_rf/_output/_run1/_results/_process/_limitations/_2050/'),#_percentil0_5
              nameOutput = paste0('RF_', no.clusters, 'Clust_lim_', gcm[i], '.asc'),
              no.absenceclasses = 2, 
              no.clusters = no.clusters)
  
}






