

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, tidyverse, parallel, foreach, doSNOW, rgdal, cclust, outliers, dismo, gtools, multcomp, sp, rgeos, outliers, FactoMineR, pROC, randomForest, stringr)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 9999)
run <- '_run2'
source('FunctionsRFclustering.R')
myproj <- CRS('+proj=longlat +datum=WGS84')

# Load data ---------------------------------------------------------------
load('../_rData/_run1/clusterdata.rData')
load('../_rData/_run1/clustereddata.rData')
load('../_rData/_run1/rflist_3.rdata')

NumberOfClusters <- 3
ar5biofolder <- '../_rf/_input'
yearlist <- '_2050'
gcmlist <- list.files(paste0(ar5biofolder, '/', yearlist))
resultsfolder <- paste0('../_rf/_output/_run1/_results/_raw/') 
modelfolder <- '../_rf/_output/_run1/_models/'

rff <- do.call(randomForest::combine, rflist)

cl <- makeCluster(7)
registerDoSNOW(cl)

y <- 1
vrs <- paste0('bio_', 1:19, '.asc$')

foreach(i = 1:length(gcmlist), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach', 'randomForest', 'sp', 'stringr'), .verbose = TRUE) %dopar% {
  
  print(gcmlist[i]) 
  
  
  gcmfiles <- paste(ar5biofolder, yearlist[y], gcmlist[i], sep = '/') %>%
    list.files(., full.names = T, pattern = '.asc$') %>% 
    grep(paste0(vrs, collapse = '|'), ., value = T) %>%  
    mixedsort()
  
  climatelayers <- raster::stack(gcmfiles)
  climatevalues <- data.frame(getValues(climatelayers))
  
  print('Climate values')
  
  rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
  rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
  uncertainty <- apply(rasterProbs, 1, max)  
  
  rasterRFprob <- lyr[[1]]
  values(rasterRFprob) <- rasterRF 
  
  rasterRFuncertainty <- lyr[[1]]
  values(rasterRFuncertainty) <- uncertainty 
  
  rasterRF <- max.col(rasterProbs, 'first')
  rasterRFclass <- lyr[[1]]
  values(rasterRFclass) <- rasterRF
  
  print("Write Raster...")
  writeRaster(rasterRFclass, paste0('../_rf/_output/_run1/_results/_raw/',  yearlist[y], '/RF_', NumberOfClusters, 'Clust_', gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  writeRaster(rasterRFprob, paste0('../_rf/_output/_run1/_results/_raw/', yearlist[y], '/RF_', NumberOfClusters, 'Prob_',  gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  writeRaster(rasterRFuncertainty, paste('../_rf/_output/_run1/_results/_raw/', yearlist[y], '/RF_', NumberOfClusters, 'Unc_', gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  
  print('Done!')
  print(gcmlist[i])
  
}
#

mdl <- list.files('../_rf/_output/_run1/_results/_raw/_2050/', full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort() %>% 
  grep('Clust', ., value = T) %>% 
  stack()
mdl <- raster::modal(mdl)  
writeRaster(mdl, '../_rf/_output/_run1/_results/_raw/RF_5Clust_future.asc')


prb <- list.files('../_rf/_output/_run1/_results/_raw/_2050/', full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort() %>% 
  grep('Prob_', ., value = T) %>% 
  stack()
prb <- mean(prb)
writeRaster(mdl, '../_rf/_output/_run1/_results/_raw/RF_5Prob_future.asc', overwrite = TRUE)






