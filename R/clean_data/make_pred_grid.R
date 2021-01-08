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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
rm(list = ls())

# Load libraries, install if needed
library(tidyverse); theme_set(theme_classic())
library(tidylog)
library(viridis)
library(mapdata)
library(rgdal)
library(raster)
library(sf)
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ncdf4)
library(chron)
library(gganimate)
library(gifski)
library(png)

# Print package versions
# sessionInfo()
# other attached packages:
# [1] forcats_0.5.0    stringr_1.4.0    dplyr_1.0.0      purrr_0.3.4      readr_1.3.1
# tidyr_1.1.0      tibble_3.0.3    [8] ggplot2_3.3.2    tidyverse_1.3.0  glmmfields_0.1.4
# Rcpp_1.0.5.1    

world <- ne_countries(scale = "medium", returnclass = "sf")


# B. BASIC GRID ====================================================================
# Get the boundaries
dat <- read.csv("data/for_analysis/mdat_cond.csv") 

# Make predictions on a grid - basic example
min(dat$lat)
max(dat$lat)
min(dat$lon)
max(dat$lon)

ymin = 54
ymax = 58
xmin = 9.5
xmax = 22

# Make predictions on a grid - basic example
pred_grid <- expand.grid(
  lat = seq(ymin, ymax, by = 0.05),
  lon = seq(xmin, xmax, by = 0.1))

# Here I could convert to UTM to get even distance across space. But I am in two UTM
# zones, 33 and 34, so perhaps I will stick with lat-long... 

# Then remove Kattegatt...
pred_grid <- pred_grid %>% 
  mutate(kattegatt = ifelse(lat > 56 & lon < 14, "Y", "N")) %>% 
  filter(kattegatt == "N")

# Remove additional areas
# pred_grid <- pred_grid %>% filter(lon > 12.4 & lon < 21.5 & lat > 54 & lat < 58)

# Now we need to avoid predicting over land. We overlay the countries but it messes up 
# the z and the legend... 
# NOT NEEDED! Using the depth raster for that
# https://stackoverflow.com/questions/23131359/r-keep-points-within-one-polygon
# https://stackoverflow.com/questions/21567028/extracting-points-with-polygon-in-r
# https://stackoverflow.com/questions/9333766/how-to-convert-a-spatial-dataframe-back-to-normal-dataframe

# Read in the BITS shapefiles
# bits <- readOGR(dsn = "data/shapefiles")
# 
# water_dat <- pred_grid %>% dplyr::select(lat, lon) %>% data.frame()
# 
# coordinates(water_dat) <- ~ lon + lat
# 
# proj4string(water_dat) <- proj4string(bits)
# 
# water_dat <- water_dat[complete.cases(over(water_dat, bits)), ]
# 
# water_dat <- as.data.frame(water_dat)
# 
# head(water_dat)
# 
# ggplot(water_dat, aes(lon, lat)) + geom_point()

# Now we need to remove areas that haven't been sampled due to being too deep
west <- raster("data/depth_geo_tif/D5_2018_rgb-1.tif")
plot(west)

east <- raster("data/depth_geo_tif/D6_2018_rgb-1.tif")
plot(east)

dep_rast <- raster::merge(west, east)

plot(dep_rast)

# To that by extracting depths of the pred grid
# https://gis.stackexchange.com/questions/242941/extracting-values-from-raster-to-get-elevation-values-in-r
# baltic_sea <- getNOAA.bathy(lon1 = xmin, lon2 = xmax, lat1 = ymin, lat2 = ymax, resolution = 1)
# 
# plot(baltic_sea, image = TRUE)
# scaleBathy(baltic_sea, deg = 2, x = "bottomleft", inset = 5)
# 
# latlong <- water_dat %>% dplyr::select(lon, lat)
latlong <- pred_grid %>% dplyr::select(lon, lat)
 
# data <- SpatialPoints(latlong)
# 
# plot(data, pch = 16, col = "red", add = TRUE)
# 
# sp <- get.depth(baltic_sea, latlong[, 1:2], locator = FALSE)
latlong$depth <- extract(dep_rast, latlong[, 1:2])
max(latlong$depth)
min(latlong$depth)

# Convert to depth (instead of elevation)
ggplot(latlong, aes(depth)) + geom_histogram()
latlong$depth <- latlong$depth - max(latlong$depth)
ggplot(latlong, aes(depth)) + geom_histogram()

latlong %>% 
  filter(depth < 0) %>% 
  ggplot(., aes(lon, lat, color = depth)) + 
  scale_color_viridis() +
  geom_sf(data = world, inherit.aes = F, size = 0.2, fill = NA) +
  geom_point(size = 1) + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL

latlong %>% 
  filter(depth < 0) %>% 
  ggplot(., aes(lon, lat, color = depth)) + 
  scale_color_viridis() +
  geom_sf(data = world, inherit.aes = F, size = 0.2, fill = NA, color = NA) +
  geom_point(size = 1) + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL

latlong %>% 
  filter(depth < 0) %>% 
  ggplot(., aes(lon, lat, color = depth*-1)) + 
  scale_color_viridis() +
  geom_sf(data = world, inherit.aes = F, size = 0.2, fill = NA, color = NA) +
  geom_point(size = 1) + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL

df <- latlong %>% filter(depth < 0) %>% mutate(depth = depth*-1)

# Now make a new grid. Can't use expand grid
pred_grid <- data.frame(lon = rep(df$lon, length(unique(dat$year))),
                        lat = rep(df$lat, length(unique(dat$year))),
                        depth = rep(df$depth, length(unique(dat$year))),
                        year = rep(sort(unique(dat$year)), each = nrow(df)))

p1 <- pred_grid %>%
  filter(year == "1999") %>% 
  mutate(deep = ifelse(depth > 135, "Y", "N")) %>% 
  ggplot(., aes(y = lat, x = lon, color = deep)) +
  geom_point(size = 0.3) +
  geom_point(data = dat, aes(y = lat, x = lon),
             inherit.aes = FALSE, shape = 3, color = "grey") + 
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL

point <- data.frame(lon = 14, lat = 57)

p2 <- pred_grid %>%
  filter(year == "1999") %>% 
  ggplot(., aes(y = lat, x = lon)) +
  geom_sf(data = world, inherit.aes = F, size = 0.2, alpha = 0) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(size = 0.3) +
  geom_point(data = point, aes(y = lat, x = lon), color = "red") +
  NULL

p1 / p2

# Now see how well it matches depth in data
model_df <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cond.csv")

model_df$depth_bathy <- extract(dep_rast, model_df[, 4:3])

model_df <- model_df %>% mutate(depth_bathy = (depth_bathy - max(depth_bathy)) * -1) 

model_df %>% 
  drop_na(depth) %>% 
  drop_na(depth_bathy) %>% 
  ggplot(., aes(depth, depth_bathy)) +
  geom_point() +
  geom_abline(color = "red")

# Save
#write.csv(pred_grid, file = "data/for_analysis/pred_grid.csv", row.names = FALSE)


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

# Filter years in the condition data frame to only have the years I have oxygen for
d_sub_oxy <- pred_grid %>% filter(year %in% names(dlist)) %>% droplevels()

# Create data holding object
data_list <- list()

# Create factor year for indexing the list in the loop
d_sub_oxy$year_f <- as.factor(d_sub_oxy$year)

# Loop through each year and extract raster values for the condition data points
for(i in unique(d_sub_oxy$year_f)) {
  
  # Subset a year
  oxy_slice <- dlist[[i]]
  
  # Create raster for that year (i)
  r <- raster(t(oxy_slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Flip...
  r <- flip(r, direction = 'y')
  
  plot(r, main = i)
  
  # Filter the same year (i) in the condition data and select only coordinates
  d_slice <- d_sub_oxy %>% filter(year_f == i) %>% dplyr::select(lon, lat)
  
  # Make into a SpatialPoints object
  data_sp <- SpatialPoints(d_slice)
  
  # Extract raster value (oxygen)
  rasValue <- raster::extract(r, data_sp)
  
  # Now we want to plot the results of the raster extractions by plotting the condition
  # data points over a raster and saving it for each year.
  # Make the SpatialPoints object into a raster again (for pl)
  df <- as.data.frame(data_sp)
  
  # Add in the raster value in the df holding the coordinates for the condition data
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
ggplot(pred_grid_oxy, aes(lon, lat, color = oxy)) + 
  facet_wrap(~year) +
  scale_colour_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(lims$min, lims$max)) +
  geom_sf(data = world, inherit.aes = F, size = 0.2, alpha = 0) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(size = 0.1) +
  NULL
  
# Animation
pred_grid_oxy$year <- as.integer(pred_grid_oxy$year)

p <- ggplot(pred_grid_oxy, aes(lon, lat, fill = oxy)) + 
  geom_raster() +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(lims$min, lims$max)) +
  geom_sf(data = world, inherit.aes = F, size = 1, alpha = 0) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  
# Here comes the gganimate specific bits
anim <- p +
  labs(title = 'Year: {frame_time}') +
  transition_time(as.integer(year)) +
  ease_aes('linear') +
  theme_classic(base_size = 24)

animate(anim, height = 1200, width = 1200)

anim_save(filename = "output/gif/oxy.gif")

# Left join in the depth again
pred_grid <- left_join(pred_grid_oxy, pred_grid)

# Save
#write.csv(pred_grid, file = "data/for_analysis/pred_grid2.csv", row.names = FALSE)


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
# The condition data is called dat so far in this script

# Filter years in the condition data frame to only have the years I have temperature for
d_sub_temp <- pred_grid %>% filter(year %in% names(dlist)) %>% droplevels()

# Create data holding object
data_list <- list()

# Create factor year for indexing the list in the loop
d_sub_temp$year_f <- as.factor(d_sub_temp$year)

# Loop through each year and extract raster values for the condition data points
for(i in unique(d_sub_temp$year_f)) {
  
  # Subset a year
  temp_slice <- dlist[[i]]
  
  # Create raster for that year (i)
  r <- raster(t(temp_slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Flip...
  r <- flip(r, direction = 'y')
  
  plot(r, main = i)
  
  # Filter the same year (i) in the condition data and select only coordinates
  d_slice <- d_sub_temp %>% filter(year_f == i) %>% dplyr::select(lon, lat)
  
  # Make into a SpatialPoints object
  data_sp <- SpatialPoints(d_slice)
  
  # Extract raster value (temperature)
  rasValue <- raster::extract(r, data_sp)
  
  # Now we want to plot the results of the raster extractions by plotting the condition
  # data points over a raster and saving it for each year.
  # Make the SpatialPoints object into a raster again (for pl)
  df <- as.data.frame(data_sp)
  
  # Add in the raster value in the df holding the coordinates for the condition data
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
ggplot(pred_grid_temp, aes(lon, lat, color = temp)) + 
  facet_wrap(~year) +
  scale_colour_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(lims$min, lims$max)) +
  geom_sf(data = world, inherit.aes = F, size = 0.2, alpha = 0) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(size = 0.1) +
  NULL

# Animation
pred_grid_temp$year <- as.integer(pred_grid_temp$year)

p <- ggplot(pred_grid_temp, aes(lon, lat, fill = temp)) + 
  geom_raster() +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                       limits = c(lims$min, lims$max)) +
  geom_sf(data = world, inherit.aes = F, size = 1, alpha = 0) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))

# Here comes the gganimate specific bits
anim <- p +
  labs(title = 'Year: {frame_time}') +
  transition_time(as.integer(year)) +
  ease_aes('linear') +
  theme_classic(base_size = 24)

animate(anim, height = 1200, width = 1200)

anim_save(filename = "output/gif/temp.gif")

# Left join in the depth again
pred_grid <- left_join(pred_grid, pred_grid_temp)

head(pred_grid)

sort(unique(pred_grid$year))

# Save
write.csv(pred_grid, file = "data/for_analysis/pred_grid2.csv", row.names = FALSE)

