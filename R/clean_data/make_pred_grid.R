#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# - Make a prediction grid that is cropped to predict only on coordinates in the Baltic.
#   Covariates are added later
# 
# A. Load libraries
# 
# B. Prepare data
# 
# C. Fit models
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
#rm(list = ls())

# Load libraries, install if needed
library(tidyverse)
library(tidylog)
library(viridis)
library(mapdata)
library(rgdal)
library(raster)
library(sf)
library(marmap)

# Print package versions
# sessionInfo()
# other attached packages:
# [1] forcats_0.5.0    stringr_1.4.0    dplyr_1.0.0      purrr_0.3.4      readr_1.3.1
# tidyr_1.1.0      tibble_3.0.3    [8] ggplot2_3.3.2    tidyverse_1.3.0  glmmfields_0.1.4
# Rcpp_1.0.5.1    


# B. PREPARE DATA ==================================================================
# Get the boundaries
dat <- read.csv("data/mdat_cond.csv") 

# Make predictions on a grid - basic example
min(dat$lat)

# Make predictions on a grid - basic example
pred_grid <- expand.grid(
  lat = seq(54, 58, by = 0.05),
  lon = seq(12, 22, by = 0.1))

# Here I could convert to UTM to get even distance across space. But I am in two UTM
# zones, 33 and 34, so perhaps I will stick with lat-long... 

# Then remove Kattegatt...
pred_grid <- pred_grid %>% 
  mutate(kattegatt = ifelse(lat > 55.5 & lon < 14, "Y", "N")) %>% 
  filter(kattegatt == "N")

# Remove additional areas
pred_grid <- pred_grid %>% filter(lon > 12.4 & lon < 21.5 & lat > 54 & lat < 58)

# Now we need to avoid predicting over land. We overlay the countries but it messes up 
# the z and the legend... 
# https://stackoverflow.com/questions/23131359/r-keep-points-within-one-polygon
# https://stackoverflow.com/questions/21567028/extracting-points-with-polygon-in-r
# https://stackoverflow.com/questions/9333766/how-to-convert-a-spatial-dataframe-back-to-normal-dataframe

# Read in the BITS shapefiles
bits <- readOGR(dsn = "data/shapefiles")

water_dat <- pred_grid %>% dplyr::select(lat, lon) %>% data.frame()

coordinates(water_dat) <- ~ lon + lat

proj4string(water_dat) <- proj4string(bits)

water_dat <- water_dat[complete.cases(over(water_dat, bits)), ]

water_dat <- as.data.frame(water_dat)

head(water_dat)

# Now we need to remove areas that haven't been sampled due to being too deep
# To that by extracting depths of the pred grid
# https://gis.stackexchange.com/questions/242941/extracting-values-from-raster-to-get-elevation-values-in-r
baltic_sea <- getNOAA.bathy(lon1 = 12, lon2 = 22, lat1 = 53, lat2 = 60, resolution = 15)

plot(baltic_sea, image = TRUE)
scaleBathy(baltic_sea, deg = 2, x = "bottomleft", inset = 5)

latlong <- water_dat %>% dplyr::select(lon, lat)

data <- SpatialPoints(latlong)

plot(data, pch = 16, col = "red", add = TRUE)

sp <- get.depth(baltic_sea, latlong[, 1:2], locator = FALSE)

df <- sp

# Now make a new grid. Can't use expand grid
pred_grid <- data.frame(lon = rep(df$lon, length(unique(dat$year))),
                        lat = rep(df$lat, length(unique(dat$year))),
                        depth = rep(df$depth, length(unique(dat$year))),
                        year = rep(sort(unique(dat$year)), each = nrow(df)))

# Save
write.csv(pred_grid, file = "data/pred_grid.csv", row.names = FALSE)

