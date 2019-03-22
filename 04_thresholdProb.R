
# Load libraries
library(pacman)
pacman::p_load(raster, rgdal, data.table, tidyverse)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
load('../_rData/_run1/clustereddata.rData')
lyr <- raster('../_rf/_output/_run1/_results/_raw/RF_5Prob_current.asc')
occ <- tbl_df(occ)

# Extracting the values
vls <- raster::extract(lyr, occ[,1:2])
vls <- vls[!is.na(vls)]
length(vls)
qnt_05 <- quantile(vls, seq(0, 1, 0.01))

vls_df <- as.data.frame(qnt_05)
threshold <- as.numeric(subset(vls_df, rownames(vls_df) == '5%'))
save(threshold, file = '../_rData/_run1/threshold_prob.rData')

formals(save)
load('../_rData/_run2/threshold_prob.rData')
load('../_rData/_run2/threshold_unc.rData')