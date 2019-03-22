
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, gtools, tidyverse, outliers)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr, outliers, Hmisc, cclust, sf, randomForest, multcomp, dismo, magrittr, ggpubr, corrplot)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('FunctionsRFclustering.R')
run <- '_run1'

# Functions to use --------------------------------------------------------
dup_cell <- function(mask, df){
  cellNum <- raster::extract(mask, df[,c('Lon', 'Lat')], cellnumbers = T) 
  cells <- xyFromCell(mask, cellNum[,'cells'])
  dupvec <- duplicated(cells[,c('x', 'y')])
  occ_rmDupCell <- tbl_df(df[!dupvec,])
  occ_DupCell <- tbl_df(df[dupvec,])
  return(list(occ_rmDupCell, occ_DupCell))
}
rmvOutliers <- function(pnts){
  # pnts <- pnt2
  norm <- scores(pnts[,3:ncol(pnts)], 'z')
  norm_na <- norm
  norm_na[abs(norm_na) > 3.5] <- NA
  normpoints <- cbind(pnts[,c('x', 'y')], norm_na) %>% 
    na.omit() %>% 
    as_data_frame()
  print('Done...!')
  normpoints <- normpoints[,c('x', 'y')]
  return(normpoints)
}
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  
  datRF_presences <- occ[,3:ncol(occ)]
  print(nrow(datRF))
  
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

# Preparing variables
vrs <- paste0('bio_', 1:19, '.asc')
occ <- read.csv('../_rf/_input/_points/_run1/Presencepoints_final.csv') %>% 
  as.tibble()
fls <- list.files('../_rf/_input/_current', full.names = TRUE, pattern = '.asc$') %>% 
  grep(paste0(vrs, collapse = '|'), ., value = T) %>% 
  mixedsort()
stk <- stack(fls)
slv <- shapefile('../_data/_shp/SLV_adm1.shp')

# Extract the points for only El Salvador
msk <- stk[[1]] * 0
occ <- occ %>% filter(Country == 'El Salvador')
pnt <- occ
coordinates(pnt) <- ~ Longitude + Latitude
crs(pnt) <- crs(hnd)

occ_adm <- raster::extract(slv, occ[,3:4]) %>% 
  dplyr::select(NAME_0, NAME_1) %>% 
  as.tibble() %>% 
  mutate(NAME_0 = as.character(NAME_0),
         NAME_1 = as.character(NAME_1),
         Lon = occ %>% pull(3),
         Lat = occ %>% pull(4))
occ_adm <- occ_adm[complete.cases(occ_adm),]
write.csv(occ, '../_rf/_input/_points/_run1/occ_all.csv', row.names = FALSE)
write.csv(occ_adm, '../_rf/_input/_points/_run1/occ_raw.csv', row.names = FALSE)

# Remove duplicated by cell
occ_dup <- dup_cell(mask = msk, df = occ_adm)[[1]]
nrow(occ_adm) - nrow(occ_dup)
write.csv(occ_dup, '../_rf/_input/_points/_run1/occ_rmDup.csv', row.names = FALSE)

# Extracting values for the bioclimatic variables
occ_dup_vls <- raster::extract(stk, occ_dup[,c('Lon', 'Lat')]) %>%
  as.tibble() %>%
  mutate(x = occ_dup %>% pull(Lon),
         y = occ_dup %>% pull(Lat)) %>%
  dplyr::select(x, y, bio_1:bio_19)

# Remove outliers
occ_dup_vls <- occ_dup_vls[complete.cases(occ_dup_vls),]
occ_out <- rmvOutliers(pnts = occ_dup_vls)
occ_out_vls <- raster::extract(stk, occ_out) %>% 
  as_data_frame() %>%
  mutate(x = occ_out %>% pull(1),
         y = occ_out %>% pull(2)) %>%
  dplyr::select(x, y, bio_1:bio_19)

# Clustering using Random Forest
occ <- occ_out_vls
env_values <- as.matrix(occ[,3:ncol(occ)]); nrow(env_values)
datRF <- as.data.frame(occ[,3:ncol(occ)]); nrow(datRF)
d <- dist(datRF, method = "euclidean")  
rfClust <- rf.clust(occ = occ, nforest = 25, ntrees = 100, nVars = 8, nclasses = 3)
labelRF <- rfClust[[1]]
clusterdata <- rfClust[[2]]
classdata <- cbind(pb = as.factor(labelRF), occ[,3:ncol(occ)])
clusteredpresdata <- cbind(occ, cluster = labelRF) %>% na.omit() %>% tbl_df()
no.clusters <- 3

save(datRF, file = paste0('../_rData/', run, '/datRF.rData'))
save(clusterdata, file = paste0('../_rData/', run, '/clusterdata.rData'))
save(occ, clusteredpresdata, no.clusters, labelRF, file = paste0('../_rData/', run, '/clustereddata.rData'))



