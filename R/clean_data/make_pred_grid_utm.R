#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# - Make a prediction grid that is cropped to predict only on coordinates in the Baltic.
#   Covariates are added later
# 
# A. Load libraries
# 
# B. Basic grid with depth
# 
# C. Grid with oxygen & temperature
# 
# D. Add lat long
# 
# E. Add ICES areas
# 
# F. Save
#
# G. Make figures of environmental variables in prediction grid
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
rm(list = ls())

# Load libraries, install if needed
library(tidyverse); theme_set(theme_light(base_size = 12))
library(tidylog)
library(viridis)
library(mapdata)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ncdf4)
library(chron)
library(gganimate)
library(gifski)
library(png)
library(RCurl)
library(sdmTMB)
library(RColorBrewer)
library(terra)

# Print package versions
# sessionInfo()
# other attached packages:
# [1] forcats_0.5.0    stringr_1.4.0    dplyr_1.0.0      purrr_0.3.4      readr_1.3.1
# tidyr_1.1.0      tibble_3.0.3    [8] ggplot2_3.3.2    tidyverse_1.3.0  glmmfields_0.1.4
# Rcpp_1.0.5.1    

# B. BASIC GRID WITH DEPTH =========================================================
# Get the boundaries
dat <- read.csv("data/for_analysis/mdat_cond.csv") 

# Make predictions on a grid - basic example
min(dat$lat)
max(dat$lat)
min(dat$lon)
max(dat$lon)

# These are the ranges I'm thinking of using. Convert these to UTM!
ymin = 54
ymax = 58
xmin = 12
xmax = 22

# Function to go from lat long to UTM
LongLatToUTM <- function(x, y, zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

LongLatToUTM(11, 53, 33)
LongLatToUTM(11, 58, 33)
LongLatToUTM(22, 53, 33)
LongLatToUTM(22, 58, 33)

# Round values based on above
utm_x_min <- 230000
utm_x_max <- 960000
utm_y_min <- 5900000
utm_y_max <- 6450000

# Make the evenly spaced (on UTM) grid 
pred_grid <- expand.grid(
  X = seq(utm_x_min, utm_x_max, by = 4000),
  Y = seq(utm_y_min, utm_y_max, by = 4000)) # 4x4 km

# For adding maps to plots
world <- ne_countries(scale = "medium", returnclass = "sf")

map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", continent = "europe")

swe_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

# Transform our map into UTM 9 coordinates, which is the equal-area projection we fit in:
utm_zone33 <- 32633
swe_coast_proj <- sf::st_transform(swe_coast, crs = utm_zone33)

ggplot(swe_coast_proj) + geom_sf()

ggplot(swe_coast_proj) + geom_sf() +
  geom_point(data = pred_grid, aes(x = X, y = Y), alpha = 0.1, shape = 21, fill = NA) +
  theme_light() +
  labs(x = "Longitude", y = "Latitude")

# Looks OK!

# Now we need to add depth
west <- raster("data/depth_geo_tif/D5_2018_rgb-1.tif")
plot(west)

east <- raster("data/depth_geo_tif/D6_2018_rgb-1.tif")
plot(east)

dep_rast <- raster::merge(west, east)

plot(dep_rast)

# Do that by extracting depths of the pred grid
utm_coords <- pred_grid %>% dplyr::select(X, Y)

# Reproject the raster to fit the UTM pred grid...
# Define spatial reference 
sr <- "+proj=utm +zone=33  +datum=WGS84 +units=m " 

# Project Raster... This takes some time
projected_raster <- projectRaster(dep_rast, crs = sr)

utm_coords$depth <- extract(projected_raster, utm_coords[, 1:2])
max(utm_coords$depth)
min(utm_coords$depth)

# Convert to depth (instead of elevation)
ggplot(utm_coords, aes(depth)) + geom_histogram()
utm_coords$depth <- utm_coords$depth - max(utm_coords$depth)
ggplot(utm_coords, aes(depth)) + geom_histogram()

ggplot(swe_coast_proj) + geom_sf() +
  geom_point(data = filter(utm_coords, depth < 0), aes(x = X, y = Y, color = depth)) +
  theme_light() +
  scale_colour_viridis() + 
  labs(x = "Longitude", y = "Latitude")

df <- utm_coords %>% filter(depth < 0) %>% mutate(depth = depth*-1)

# Now make a new grid
pred_grid <- data.frame(X = rep(df$X, length(unique(dat$year))),
                        Y = rep(df$Y, length(unique(dat$year))),
                        depth = rep(df$depth, length(unique(dat$year))),
                        year = rep(sort(unique(dat$year)), each = nrow(df)))

pred_grid <- pred_grid %>% mutate(deep = ifelse(depth > 135, "Y", "N"))

ggplot(swe_coast_proj) + 
  geom_raster(data = filter(pred_grid, year == "1999"), aes(x = X, y = Y, fill = deep)) +
  geom_sf() +
  theme_light() +
  labs(x = "Longitude", y = "Latitude")

ggplot(swe_coast_proj) + 
  geom_raster(data = filter(pred_grid, year == "1999"), aes(x = X, y = Y, fill = depth)) +
  geom_sf() +
  theme_light() +
  labs(x = "Longitude", y = "Latitude")

hist(pred_grid$depth)


# C. GRID WITH OXYGEN & TEMPERATURE ================================================
# ** Oxygen ========================================================================
# Loop through each year and extract the oxygen levels
# Downloaded from here: https://resources.marine.copernicus.eu/?option=com_csw&view=details&product_id=BALTICSEA_REANALYSIS_BIO_003_012
# Extract raster points: https://gisday.wordpress.com/2014/03/24/extract-raster-values-from-points-using-r/comment-page-1/
# https://rpubs.com/boyerag/297592
# https://pjbartlein.github.io/REarthSysSci/netCDF.html#get-a-variable
# Open the netCDF file
ncin <- nc_open("data/NEMO_Nordic_SCOBI/dataset-reanalysis-scobi-monthlymeans_1610091357600.nc")

print(ncin)

# Get longitude and latitude
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)

# Get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt
tunits

# Get oxygen
dname <- "o2b"

oxy_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(oxy_array)

# Get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

# Convert time: split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])

# Here I deviate from the guide a little bit. Save this info:
dates <- chron(time, origin = c(tmonth, tday, tyear))

# Crop the date variable
months <- as.numeric(substr(dates, 2, 3))
years <- as.numeric(substr(dates, 8, 9))
years <- ifelse(years > 90, 1900 + years, 2000 + years)

# Replace netCDF fill values with NA's
oxy_array[oxy_array == fillvalue$value] <- NA

# We only use Quarter 4 in this analysis, so now we want to loop through each time step,
# and if it is a good month save it as a raster.
# First get the index of months that correspond to Q4
months

index_keep <- which(months > 9)

# Quarter 4 by keeping months in index_keep
oxy_q4 <- oxy_array[, , index_keep]

months_keep <- months[index_keep]

years_keep <- years[index_keep]

# Now we have an array with only Q4 data...
# We need to now calculate the average within a year.
# Get a sequence that takes every third value between 1: number of months (length)
loop_seq <- seq(1, dim(oxy_q4)[3], by = 3)

# Create objects that will hold data
dlist <- list()
oxy_10 <- c()
oxy_11 <- c()
oxy_12 <- c()
oxy_ave <- c()

# Loop through the vector sequence with every third value, then take the average of
# three consecutive months (i.e. q4)
for(i in loop_seq) {
  
  oxy_10 <- oxy_q4[, , (i)]
  oxy_11 <- oxy_q4[, , (i + 1)]
  oxy_12 <- oxy_q4[, , (i + 2)]
  
  oxy_ave <- (oxy_10 + oxy_11 + oxy_12) / 3
  
  list_pos <- ((i/3) - (1/3)) + 1 # to get index 1:n(years)
  
  dlist[[list_pos]] <- oxy_ave
  
}

# Now name the lists with the year:
names(dlist) <- unique(years_keep)

# Now I need to make a loop where I extract the raster value for each year...

# Filter years in the pred-grid data frame to only have the years I have oxygen for
d_sub_oxy <- pred_grid %>% filter(year %in% names(dlist)) %>% droplevels()

# Create data holding object
data_list <- list()

# Create factor year for indexing the list in the loop
d_sub_oxy$year_f <- as.factor(d_sub_oxy$year)

# Loop through each year and extract raster values for the pred-grid data points
for(i in unique(d_sub_oxy$year_f)) {
  
  # Subset a year
  oxy_slice <- dlist[[i]]
  
  # Create raster for that year (i)
  r <- raster(t(oxy_slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Flip...
  r <- flip(r, direction = 'y')
  
  plot(r, main = i)
  
  # Change projection to UTM (same as pred grid)
  proj_raster <- projectRaster(r, crs = sr)
  
  # Filter the same year (i) in the pred-grid data and select only coordinates
  d_slice <- d_sub_oxy %>% filter(year_f == i) %>% dplyr::select(X, Y)
  
  # Make into a SpatialPoints object
  data_sp <- SpatialPoints(d_slice, proj4string = CRS(sr))
  
  # Extract raster value (oxygen)
  rasValue <- raster::extract(proj_raster, data_sp)
  
  # Now we want to plot the results of the raster extractions by plotting the pred-grid
  # data points over a raster and saving it for each year.
  # Make the SpatialPoints object into a raster again (for pl)
  df <- as.data.frame(data_sp)
  
  # Add in the raster value in the df holding the coordinates for the pred-grid data
  d_slice$oxy <- rasValue
  
  # Add in which year
  d_slice$year <- i
  
  # Create a index for the data last where we store all years (because our loop index
  # i is not continuous, we can't use it directly)
  index <- as.numeric(d_slice$year)[1] - 1992
  
  # Add each years' data in the list
  data_list[[index]] <- d_slice
  
}

# Now create a data frame from the list of all annual values
pred_grid_oxy <- dplyr::bind_rows(data_list)

lims <- pred_grid_oxy %>% drop_na(oxy) %>% summarise(min = min(oxy),
                                                     max = max(oxy))
# Plot and compare with rasters
ggplot(swe_coast_proj) +
  geom_raster(data = pred_grid_oxy,
             aes(x = X, y = Y, fill = oxy)) +
  geom_sf() +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(lims$min, lims$max)) +
  theme_light() +
  facet_wrap(~ year) + 
  labs(fill = "Oxygen") +
  labs(x = "Longitude", y = "Latitude")

# Animation
# pred_grid_oxy$year <- as.integer(pred_grid_oxy$year)
# 
# p <- ggplot(swe_coast_proj) + geom_sf() +
#   geom_raster(data = pred_grid_oxy,
#               aes(x = X, y = Y, fill = oxy)) +
#   scale_fill_gradientn(colours = rev(terrain.colors(10)),
#                        limits = c(lims$min, lims$max)) +
#   theme_light() +
#   labs(fill = "Oxygen") +
#   labs(x = "Longitude", y = "Latitude")
# 
# # Here comes the gganimate specific bits
# anim <- p +
#   labs(title = 'Year: {frame_time}') +
#   transition_time(as.integer(year)) +
#   ease_aes('linear')
# 
# gganimate::animate(anim, height = 600, width = 600)
# 
# anim_save(filename = "output/gif/oxy.gif")

# Add in oxygen to the main prediction grid
pred_grid_oxy <- pred_grid_oxy %>% arrange(X, Y, year)
pred_grid <- pred_grid %>% arrange(X, Y, year)

str(pred_grid_oxy)
str(pred_grid)

pred_grid$oxy <- pred_grid_oxy$oxy

# Now the unit of oxygen is mmol/m3. I want it to be ml/L. The original model is in unit ml/L
# and it's been converted by the data host. Since it was converted without accounting for
# pressure or temperature, I can simply use the following conversion factor:
# 1 ml/l = 103/22.391 = 44.661 μmol/l -> 1 ml/l = 0.044661 mmol/l = 44.661 mmol/m^3 -> 0.0223909 ml/l = 1mmol/m^3
# https://ocean.ices.dk/tools/unitconversion.aspx

pred_grid$oxy <- pred_grid$oxy * 0.0223909


# ** Temperature ===================================================================
# Open the netCDF file
ncin <- nc_open("data/NEMO_Nordic_SCOBI/dataset-reanalysis-nemo-monthlymeans_1608127623694.nc")

print(ncin)

# Get longitude and latitude
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)

# Get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt
tunits

# Get temperature
dname <- "bottomT"

temp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(temp_array)

# Get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

# Convert time: split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])

# Here I deviate from the guide a little bit. Save this info:
dates <- chron(time, origin = c(tmonth, tday, tyear))

# Crop the date variable
months <- as.numeric(substr(dates, 2, 3))
years <- as.numeric(substr(dates, 8, 9))
years <- ifelse(years > 90, 1900 + years, 2000 + years)

# Replace netCDF fill values with NA's
temp_array[temp_array == fillvalue$value] <- NA

# We only use Quarter 4 in this analysis, so now we want to loop through each time step,
# and if it is a good month save it as a raster.
# First get the index of months that correspond to Q4
months

index_keep <- which(months > 9)

# Quarter 4 by keeping months in index_keep
temp_q4 <- temp_array[, , index_keep]

months_keep <- months[index_keep]

years_keep <- years[index_keep]

# Now we have an array with only Q4 data...
# We need to now calculate the average within a year.
# Get a sequence that takes every third value between 1: number of months (length)
loop_seq <- seq(1, dim(temp_q4)[3], by = 3)

# Create objects that will hold data
dlist <- list()
temp_10 <- c()
temp_11 <- c()
temp_12 <- c()
temp_ave <- c()

# Loop through the vector sequence with every third value, then take the average of
# three consecutive months (i.e. q4)
for(i in loop_seq) {
  
  temp_10 <- temp_q4[, , (i)]
  temp_11 <- temp_q4[, , (i + 1)]
  temp_12 <- temp_q4[, , (i + 2)]
  
  temp_ave <- (temp_10 + temp_11 + temp_12) / 3
  
  list_pos <- ((i/3) - (1/3)) + 1 # to get index 1:n(years)
  
  dlist[[list_pos]] <- temp_ave
  
}

# Now name the lists with the year:
names(dlist) <- unique(years_keep)

# Now I need to make a loop where I extract the raster value for each year...

# Filter years in the pred-grid data frame to only have the years I have temperature for
d_sub_temp <- pred_grid %>% filter(year %in% names(dlist)) %>% droplevels()

# Create data holding object
data_list <- list()

# Create factor year for indexing the list in the loop
d_sub_temp$year_f <- as.factor(d_sub_temp$year)

# Loop through each year and extract raster values for the pred-grid data points
for(i in unique(d_sub_temp$year_f)) {
  
  # Subset a year
  temp_slice <- dlist[[i]]
  
  # Create raster for that year (i)
  r <- raster(t(temp_slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Flip...
  r <- flip(r, direction = 'y')
  
  plot(r, main = i)
  
  proj_raster <- projectRaster(r, crs = sr)
  
  # Filter the same year (i) in the pred-grid data and select only coordinates
  d_slice <- d_sub_temp %>% filter(year_f == i) %>% dplyr::select(X, Y)
  
  # Make into a SpatialPoints object
  data_sp <- SpatialPoints(d_slice, proj4string = CRS(sr))
  
  # Extract raster value (temperature)
  rasValue <- raster::extract(proj_raster, data_sp)
  
  # Now we want to plot the results of the raster extractions by plotting the pred-grid
  # data points over a raster and saving it for each year.
  # Make the SpatialPoints object into a raster again (for pl)
  df <- as.data.frame(data_sp)
  
  # Add in the raster value in the df holding the coordinates for the pred-grid data
  d_slice$temp <- rasValue
  
  # Add in which year
  d_slice$year <- i
  
  # Create a index for the data last where we store all years (because our loop index
  # i is not continuous, we can't use it directly)
  index <- as.numeric(d_slice$year)[1] - 1992
  
  # Add each years' data in the list
  data_list[[index]] <- d_slice
  
}

# Now create a data frame from the list of all annual values
pred_grid_temp <- dplyr::bind_rows(data_list)

lims <- pred_grid_temp %>% drop_na(temp) %>% summarise(min = min(temp),
                                                       max = max(temp))

# Plot and compare with rasters
ggplot(swe_coast_proj) +
  geom_raster(data = pred_grid_temp,
              aes(x = X, y = Y, fill = temp)) +
  geom_sf() +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                       limits = c(lims$min, lims$max)) +
  theme_light() +
  facet_wrap(~ year) + 
  labs(fill = "Temperature") +
  labs(x = "Longitude", y = "Latitude")

# Animation
# pred_grid_temp$year <- as.integer(pred_grid_temp$year)
# 
# p <- ggplot(swe_coast_proj) + geom_sf() +
#   geom_raster(data = pred_grid_temp,
#               aes(x = X, y = Y, fill = temp)) +
#   scale_fill_gradientn(colours = rev(terrain.colors(10)),
#                        limits = c(lims$min, lims$max)) +
#   theme_light() +
#   labs(fill = "Temperature") +
#   labs(x = "Longitude", y = "Latitude")
# 
# # Here comes the gganimate specific bits
# anim <- p +
#   labs(title = 'Year: {frame_time}') +
#   transition_time(as.integer(year)) +
#   ease_aes('linear')
# 
# gganimate::animate(anim, height = 600, width = 600)
# 
# anim_save(filename = "output/gif/temp.gif")

# Add in temperature to the main prediction grid
pred_grid_temp <- pred_grid_temp %>% arrange(X, Y, year)
pred_grid <- pred_grid %>% arrange(X, Y, year)

str(pred_grid_temp)
str(pred_grid)

pred_grid$temp <- pred_grid_temp$temp


# E. ADD LATLONG ===================================================================

# Need to go from UTM to lat long for this one... 
# https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r
xy <- as.matrix(pred_grid[, 1:2])
v <- vect(xy, crs="+proj=utm +zone=33 +datum=WGS84  +units=m")
y <- project(v, "+proj=longlat +datum=WGS84")
lonlat <- geom(y)[, c("x", "y")]

pred_grid$lon <- lonlat[, 1]
pred_grid$lat <- lonlat[, 2]

pred_grid$X <- pred_grid$X/1000
pred_grid$Y <- pred_grid$Y/1000

# E. ADD ICES AREAS VIA SHAPEFILE ==================================================
# https://stackoverflow.com/questions/34272309/extract-shapefile-value-to-point-with-r
# https://gis.ices.dk/sf/
shape <- shapefile("data/ICES_StatRec_mapto_ICES_Areas/StatRec_map_Areas_Full_20170124.shp")
head(shape)

plot(shape, axes = TRUE)

pts <- SpatialPoints(cbind(pred_grid$lon, pred_grid$lat), 
                     proj4string = CRS(proj4string(shape)))

pred_grid$subdiv <- over(pts, shape)$Area_27
pred_grid$subdiv2 <- over(pts, shape)$AreasList

# Rename subdivisions to the more common names and do some more filtering (by sub div and area)
sort(unique(pred_grid$subdiv))

pred_grid2 <- pred_grid %>% 
  mutate(SubDiv = factor(subdiv),
         SubDiv = fct_recode(subdiv,
                             "24" = "3.d.24",
                             "25" = "3.d.25",
                             "26" = "3.d.26",
                             "27" = "3.d.27",
                             "28" = "3.d.28.1",
                             "28" = "3.d.28.2"),
         SubDiv = as.character(SubDiv)) %>% 
  filter(SubDiv %in% c("24", "25", "26", "27", "28")) %>% 
  filter(lat > 54 & lat < 58 & lon < 22)
  

# F. SAVE ==========================================================================
# Save

write.csv(pred_grid2, file = "data/for_analysis/pred_grid2.csv", row.names = FALSE)


# G. PLOT ==========================================================================
# Oxygen vs depth
ggplot(pred_grid2, aes(depth, oxy)) + geom_point()

# Oxygen vs year
pred_grid2 %>% 
  drop_na(oxy) %>% 
  group_by(year) %>% 
  summarise(mean_oxy = mean(oxy),
            sd_oxy = sd(oxy)) %>% 
  ggplot(., aes(year, mean_oxy)) +
  geom_point() + 
  geom_errorbar(aes(x = year, ymin = mean_oxy - sd_oxy, ymax = mean_oxy + sd_oxy, width = 0), alpha = 0.5) + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), color = "tomato") +
  labs(y = "Mean 02 [ml/L]", x = "Year") +  
  labs(y = expression(paste("Mean O" [2], " [ml/L]", sep = "")),
       x = "Year") + 
  theme(legend.position = c(0.8, 0.5))

ggsave("figures/supp/env_oxy.png", width = 6.5, height = 6.5, dpi = 600)

# Oxygen vs year and sd
pred_grid2 %>% 
  drop_na(oxy, SubDiv) %>% 
  filter(!SubDiv == 22) %>% 
  group_by(year, SubDiv) %>% 
  summarise(mean_oxy = mean(oxy),
            sd_oxy = sd(oxy)) %>% 
  ggplot(., aes(year, mean_oxy)) +
  geom_point() + 
  facet_wrap(~ SubDiv) +
  # geom_errorbar(aes(x = year, ymin = mean_oxy - sd_oxy, ymax = mean_oxy + sd_oxy, width = 0),
  #               alpha = 0.5) + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), color = "tomato") +
  labs(y = "Mean 02 [ml/L]", x = "Year") +  
  labs(y = expression(paste("Mean O" [2], " [ml/L]", sep = "")), x = "Year")

ggsave("figures/supp/env_oxy_sd.png", width = 6.5, height = 6.5, dpi = 600)

# Oxygen vs year and sd by depth
pred_grid2 %>% 
  drop_na(oxy, SubDiv) %>% 
  mutate(deep = ifelse(depth < 50, "N", "Y")) %>% 
  filter(!SubDiv == 22) %>% 
  group_by(year, SubDiv, deep) %>% 
  summarise(mean_oxy = mean(oxy),
            sd_oxy = sd(oxy)) %>% 
  ggplot(., aes(year, mean_oxy, color = deep)) +
  geom_point() + 
  facet_wrap(~ SubDiv) +
  # geom_errorbar(aes(x = year, ymin = mean_oxy - sd_oxy, ymax = mean_oxy + sd_oxy, width = 0),
  #               alpha = 0.5) + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
  labs(y = "Mean 02 [ml/L]", x = "Year") +  
  labs(y = expression(paste("Mean O" [2], " [ml/L]", sep = "")), x = "Year") +
  theme(strip.text = element_text(colour = 'black'),
        strip.background = element_rect(fill = "grey90"))

ggsave("figures/supp/env_oxy_sd_depth.png", width = 6.5, height = 6.5, dpi = 600)

