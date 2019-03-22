

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, raster, rgdal, cclust, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, velox, rgeos)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('FunctionsRFclustering.R')
run <- '_run1'
myproj <- CRS('+proj=longlat +datum=WGS84')

# Functions to use --------------------------------------------------------
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  # occ = back_swd; nforest = 50; ntrees = 500; nVars = 8; nclasses = 2
  datRF_presences <- occ[,3:ncol(occ)] %>% as.data.frame()
  print(nrow(datRF_presences))
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  return(list(labelRF, clusterdata))
}

# Load data ---------------------------------------------------------------
msk <- raster('../_rf/_input/_current/bio_1.asc') * 0
slv <- raster::getData('GADM', country = 'SLV', level = 1)
vrs <- paste0('bio_', 1:19, '.asc')

load(file = '../_rData/_run1/clustereddata.rData')

vrs <- colnames(occ[grep('bio', colnames(occ), value = F)]) %>% paste0(., '.asc$')
lyr <- list.files('../_rf/_input/_current', full.names = TRUE, pattern = '.asc$') %>%
  mixedsort() %>% 
  grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>%
  stack()

# Bias raster - process ---------------------------------------------------
SPspecies <- SpatialPoints(occ[,1:2]) 
crs(SPspecies) <- myproj
back_raster <- msk
speciescell <- raster::extract(msk, SPspecies, cellnumber = TRUE)
back_raster[speciescell[,1]]  <- NA #remove the cell with presences
samplesize <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0) 
NumberOfClusters <- max(clusteredpresdata$cluster) 
ratio <- NumberOfClusters/1
numberofpresences <- nrow(clusteredpresdata) 
crs(back_raster) <- myproj
back <- randomPoints(back_raster, 1*numberofpresences) %>%
  as_data_frame()
coordinates(back) <- ~ x + y
back_swd  <- raster::extract(lyr, back) %>% 
  cbind(coordinates(back), .)
nrow(back_swd) == nrow(back_swd[complete.cases(back_swd),])
write.csv(back_swd, '../_rf/_input/_points/_run1/back_swd.csv', row.names = FALSE)
write.csv(occ, '../_rf/_input/_points/_run1/occ_swd.csv', row.names = FALSE)

plot(slv, border = 'grey')
points(back_swd[,1], back_swd[,2], pch = 16, col = 'red')
back_swd <- back_swd[complete.cases(back_swd),]

# Cluster analysis to pseudoabsences
bckclust <- rf.clust(occ = back_swd, nforest = 50, ntrees = 500, nVars = 8, nclasses = 2)
datRF <- as.data.frame(back_swd[,3:ncol(back_swd)])
attach(datRF)
no.forests <- 50#raw = 25
no.trees <- 500
distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)# mtry1 = 4 raw  # es la cantidad de variables a utilizar en cada no
no.absenceclasses <- 2
labelRF <- pamNew(distRF$cl1, no.absenceclasses)
detach(datRF)
classdata <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])

presvalue_swd  <- clusteredpresdata[,3:ncol(clusteredpresdata)] %>%
  cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
  na.omit() %>%
  as.data.frame() %>%
  mutate(cluster = cluster + no.absenceclasses)
presvalue_swd <- dplyr::select(presvalue_swd, pb, bio_1:bio?19)
presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))
classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)]) # Background

dim(classdata_2); dim(presvalue_swd)
presvalue_swd <- presvalue_swd %>% dplyr::select(-cluster)

allclasses_swd <- rbind(classdata_2, presvalue_swd[,1:ncol(classdata_2)])
unique(allclasses_swd$pb)
write.csv(allclasses_swd, '../_rf/_input/_points/_run1/all_classes_swd.csv', row.names = FALSE)

# To make the random forest analysis --------------------------------------
vrs <- gsub('.asc', '', vrs) 
vrs <- gsub('\\$', '', vrs)
model1 <- as.formula(paste('factor(pb) ~', paste(paste(vrs), collapse = '+', sep =' ')))
rflist <- vector('list', 50) 
auc <- vector('list', 50)

for(repe in 1:50){ # 50 bosques
  
  print(repe)
  pressample <- list()
  
  for (i in 1:(NumberOfClusters+no.absenceclasses)){
    
    if(any(i==c(1:no.absenceclasses))) { 
      
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                     size = samplesize*NumberOfClusters/2/no.absenceclasses)
    } else {
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size=samplesize)
    }
    pressample[[i]] <- allclasses_swd[rows,] 
  }
  
  species <- na.omit(do.call(rbind, pressample)) 
  head(species)
  Samplesplit <- sample(rownames(species)) 
  
  envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
  envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
  
  rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
  
  save(rfmodel, file = paste('../_rf/_output/_run1/_models/', NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
  rflist[[repe]] <- rfmodel
  
  # AUC 
  predicted <- as.numeric(predict(rfmodel, envtest))
  observed <- as.vector(envtest[,'pb'])
  auc[[repe]] <- auc(observed, predicted) 
  rm(rfmodel)
  
  cat(auc[[repe]] ,'\n')
  
}

auc <- unlist(auc)
rff <- do.call(randomForest::combine, rflist)
importance <- as.data.frame(rff$importance)

save(rflist, file = paste('../_rData/', run, '/rflist_', NumberOfClusters, '.rdata', sep = ''))
save(importance, file = paste0('../_rData/', run, '/importanceRF.rData'))
save(auc, file = paste0('../_rData/', run, '/aucRF_dist.rData'))
save(rff, file = paste0('../_rData/', run, '/rff_dist.rData'))

# Predict modell
climatevalues  <- data.frame(getValues(lyr))
NumberOfClusters <- 3

rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
rasterProbs_na <- na.omit(rasterProbs)
sum_rasterProbs_na <- apply(rasterProbs_na, 1, sum)

rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
uncertainty <- apply(rasterProbs, 1, max)  

rasterRFprob <- lyr[[1]]
values(rasterRFprob) <- rasterRF 

rasterRFuncertainty <- lyr[[1]]
values(rasterRFuncertainty) <- uncertainty 

rasterRF <- max.col(rasterProbs, 'first')
rasterRFclass <- lyr[[1]]
values(rasterRFclass) <- rasterRF

writeRaster(rasterRFclass, paste0('../_rf/_output/_run1/_results/_raw/RF_5Clust_current.asc'), format = 'ascii', overwrite = T)
writeRaster(rasterRFprob, paste0('../_rf/_output/_run1/_results/_raw/RF_5Prob_current.asc'), format = 'ascii', overwrite = T)
writeRaster(rasterRFuncertainty, paste0('../_rf/_output/_run1/_results/_raw/RF_5Unc_current.asc'), format = 'ascii', overwrite = T)



