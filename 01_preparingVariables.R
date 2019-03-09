

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr)


# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())


# preparing variables