
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, gtools, tidyverse)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
prj <- '+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs'
lbl <- data.frame(value = c(0, 3, 5), gradiente = c('No idoneo', 'Zonas de transformacion', 'Zonas de alta adaptacion'))

# Load data
imp <- raster('../_rf/_output/_run1/_results/_process/change_2050.tif')
adm <- shapefile('../_data/_shp/SLV_adm1.shp')

# Extract values for the administrative areas
pnt <- imp %>% 
  rasterToPoints() %>% 
  as_data_frame() %>%
  setNames(c('x', 'y', 'category'))
dpt <- raster::extract(adm, pnt[,1:2]) %>%
  as_data_frame() %>%
  dplyr::select(NAME_0, NAME_1) %>% 
  mutate(categoria = pnt %>% pull(3))

# Summarize
smm <- dpt %>% 
  group_by(NAME_1, categoria) %>%
  summarize(count = n()) %>%
  ungroup() %>% 
  mutate(NAME_1 = iconv(NAME_1, from = 'UTF-8', to = 'latin1'))

# Project the raster to know the resolution in meters
crs(imp) <- crs(adm)
imp_prj <- projectRaster(imp, crs = prj)

mts <- res(imp_prj)[1] * res(imp_prj)[2]
smm <- smm %>% 
  mutate(meters = count * mts,
         has = meters / 10000) %>%
  inner_join(., lbl, by = c('categoria' = 'value'))

smm <- smm %>% 
  dplyr::select(NAME_1, gradiente, has)
smm <- smm %>% 
  group_by(NAME_1) %>% 
  mutate(prc = has / sum(has) * 100) %>% 
  ungroup()
write.csv(smm, '../_data/_tbl/summarize_gradient.csv', row.names = FALSE)

smm %>%
  filter(gradiente != 'No idoneo') %>%
  group_by(NAME_1) %>% 
  filter(prc == max(prc))

