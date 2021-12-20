rm(list = ls())

library(tidyverse); theme_set(theme_light(base_size = 12))
library(readxl)
library(tidylog)
library(RCurl)
library(viridis)
library(RColorBrewer)
library(patchwork)
library(janitor)
library(icesDatras)
library(mapdata)
library(patchwork)
library(rgdal)
library(raster)
library(sf)
library(rgeos)
library(chron)
library(lattice)
library(ncdf4)
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapplots)
library(geosphere)
#remotes::install_github("pbs-assess/sdmTMB")
library(sdmTMB)

# Specify map ranges
ymin = 54; ymax = 58; xmin = 12; xmax = 22

map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", continent = "europe")

# Crop the polygon for plotting and efficiency:
# st_bbox(map_data) # find the rough coordinates
swe_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

# Transform our map into UTM 33 coordinates, which is the equal-area projection we fit in:
utm_zone33 <- 32633
swe_coast_proj <- sf::st_transform(swe_coast, crs = utm_zone33)

# Define plotting theme for facet_wrap map with years
theme_facet_map <- function(base_size = 10, base_family = "") {
  theme_light(base_size = 10, base_family = "") +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.text = element_text(size = 6),
      strip.text = element_text(size = 8, colour = 'black', margin = margin()),
      strip.background = element_rect(fill = "grey90")
    )
}

# Make default base map plot
plot_map_raster <- 
  ggplot(swe_coast_proj) + 
  geom_sf(size = 0.3) +
  labs(x = "Longitude", y = "Latitude") +
  theme_facet_map(base_size = 14)

# Read the tifs
saduria <- raster("data/saduria_tif/AbundanceI_HRmo_presHighweightcor_no0_bpprf5_prescor_prediction_newZi2.tif")
saduria_longlat = projectRaster(saduria, crs = ('+proj=longlat'))

crs(saduria)
extent(saduria)

plot(saduria_longlat)

# Save to check each year is ok! First convert the raster to points for plotting
# (so that we can use ggplot)
map_saduria <- rasterToPoints(saduria_longlat)

# Make the points a dataframe for ggplot
df_rast <- data.frame(map_saduria)

# Rename columns
colnames(df_rast) <- c("lon", "lat", "saduria_abundance")

# Add UTM coords to raster
# Function
LongLatToUTM <- function(x, y, zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

utm_coords <- LongLatToUTM(df_rast$lon, df_rast$lat, zone = 33)
df_rast$X <- utm_coords$X/1000 # for computational reasons
df_rast$Y <- utm_coords$Y/1000 # for computational reasons

# Now make the map
# plot_map_raster +
#   geom_raster(data = df_rast, aes(x = X*1000, y = Y*1000, fill = saduria_abundance)) +
#   scale_fill_viridis_c(trans = "sqrt") +
#   NULL
  
ggplot() + 
  geom_raster(data = df_rast, aes(x = lon, y = lat, fill = saduria_abundance)) +
  ylim(c(ymin, ymax)) + 
  xlim(c(xmin, xmax)) + 
  NULL

# Now extract the values from the saduria raster to the prediction grid (evenly spaced
# UTM grid, geom_raster shouldn't complain!!)

# And now read in predicted cod densities (the underlying data is pred grid)
pred_cod <- read.csv("output/predict_mcod_density.csv") %>% dplyr::select(-X.1)

# Extract saduria values
pred_cod$saduria_abundance <- extract(saduria_longlat, pred_cod[, 8:9])

# Convert Saduria from abundance to biomass using a simple equation
# pred_cod <- pred_cod %>%
#   mutate(saduria_biomass = 0.01*saduria_abundance^3)

# Now make the map 
plot_map_raster +
  geom_raster(data = pred_cod, aes(x = X*1000, y = Y*1000, fill = saduria_abundance)) +
  scale_fill_viridis_c(trans = "sqrt") +
  NULL

plot_map_raster +
  geom_raster(data = pred_cod, aes(x = X*1000, y = Y*1000, fill = saduria_biomass)) +
  scale_fill_viridis_c(trans = "sqrt") +
  facet_wrap(~subdiv)
  NULL

# Calculate biomass-weighted overlap index by year! (row-wise, which is one grid cell) per grid cell
# NOTE! Saduria biomass index is in unit g/m2
# we want kg/km^2
pred_cod2 <- pred_cod %>% 
  mutate(est_kg_km = exp(est),
         pred_i = est_kg_km,
         prey_i = saduria_abundance) %>% # response scale not link
  drop_na(pred_i) %>% 
  drop_na(prey_i) %>% 
  group_by(year) %>% 
  mutate(numerator = pred_i / max(pred_i) * prey_i / max(prey_i),
         denominator = prey_i / max(prey_i)) %>% 
  summarise(sum_num = sum(numerator),
            sum_den = sum(denominator),
            BWOI = sum_num / sum_den) %>% # max is here within a year)
  ungroup()

ggplot(pred_cod2) +
  geom_point(aes(year, BWOI, color = "BWOI"), size = 3) +
  theme_light(base_size = 14) + 
  stat_smooth(aes(year, BWOI, color = "BWOI"), method = "gam", formula = y ~ s(x, k = 3)) +
  NULL

# Calculate biomass-weighted overlap index

