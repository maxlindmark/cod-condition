#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# - Make a prediction grid that is cropped to predict only on coordinates in the Baltic.
#   Covariates are added later
# 
# A. Load libraries
# 
# B. Basic grid
#
# C. Grid with oxygen
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
#rm(list = ls())

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
baltic_sea <- getNOAA.bathy(lon1 = xmin, lon2 = xmax, lat1 = ymin, lat2 = ymax, resolution = 1)

plot(baltic_sea, image = TRUE)
scaleBathy(baltic_sea, deg = 2, x = "bottomleft", inset = 5)

latlong <- water_dat %>% dplyr::select(lon, lat)

data <- SpatialPoints(latlong)

plot(data, pch = 16, col = "red", add = TRUE)

sp <- get.depth(baltic_sea, latlong[, 1:2], locator = FALSE)

df <- sp

df$depth <- df$depth*-1

# Now make a new grid. Can't use expand grid
pred_grid <- data.frame(lon = rep(df$lon, length(unique(dat$year))),
                        lat = rep(df$lat, length(unique(dat$year))),
                        depth = rep(df$depth, length(unique(dat$year))),
                        year = rep(sort(unique(dat$year)), each = nrow(df)))

# Plot nicer
world <- ne_countries(scale = "medium", returnclass = "sf")

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

# Plot depths
pred_grid %>%
  filter(year == "1999") %>% 
  mutate(deep = ifelse(depth > 70, "Y", "N")) %>% 
  ggplot(., aes(y = lat, x = lon, color = deep)) +
  geom_sf(data = world, inherit.aes = F, size = 0.2, alpha = 0) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(size = 0.1) +
  NULL

# Now see how well it matches depth in data
model_df <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cond.csv")

latlong2 <- model_df %>% dplyr::select(lon, lat)

data2 <- SpatialPoints(latlong2)

plot(baltic_sea, image = TRUE)
scaleBathy(baltic_sea, deg = 2, x = "bottomleft", inset = 5)
plot(data2, pch = 16, col = "red", add = TRUE)

sp2 <- get.depth(baltic_sea, latlong2[, 1:2], locator = FALSE)

df2 <- sp2

df2$depth <- df2$depth*-1

df2 <- df2 %>% rename("depth_raster" = "depth")

df2$depth_bits <- model_df$depth

ggplot(df2, aes(depth_raster, depth_bits)) +
  geom_point() + 
  geom_abline(color = "red")

df2 %>% 
  mutate(resid = depth_raster - depth_bits) %>% 
  ggplot(., aes(resid)) +
  geom_histogram()
  
df2 %>% 
  mutate(resid = depth_raster - depth_bits) %>% 
  ggplot(., aes(depth_bits, resid)) +
  geom_point()

# Plot on map
df2 %>% 
  mutate(resid = depth_raster - depth_bits) %>% 
  ggplot(., aes(lon, lat, color = resid)) +
  geom_point() +
  scale_color_gradient2(midpoint = 0)


# Save
#write.csv(pred_grid, file = "data/for_analysis/pred_grid.csv", row.names = FALSE)

# C. GRID WITH OXYGEN ==============================================================
pred_grid2 <- pred_grid

# Loop through each year and extract the oxygen levels
# Downloaded from here: https://resources.marine.copernicus.eu/?option=com_csw&view=details&product_id=BALTICSEA_REANALYSIS_BIO_003_012
# Extract raster points: https://gisday.wordpress.com/2014/03/24/extract-raster-values-from-points-using-r/comment-page-1/
# https://rpubs.com/boyerag/297592
# https://pjbartlein.github.io/REarthSysSci/netCDF.html#get-a-variable
# Open the netCDF file
ncin <- nc_open("data/NEMO_Nordic_SCOBI/dataset-reanalysis-scobi-monthlymeans_1603971995426.nc")

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
d_sub_oxy <- pred_grid2 %>% filter(year %in% names(dlist)) %>% droplevels()

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
pred_grid2 <- dplyr::bind_rows(data_list)

# Plot and compare with rasters
ggplot(pred_grid2, aes(lon, lat, color = oxy)) + 
  facet_wrap(~year) +
  scale_colour_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(-200, 400)) +
  geom_sf(data = world, inherit.aes = F, size = 0.2, alpha = 0) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_point(size = 0.1) +
  NULL
  
# Animation
pred_grid2$year <- as.integer(pred_grid2$year)

p <- ggplot(pred_grid2, aes(lon, lat, fill = oxy)) + 
  geom_raster() +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(min(drop_na(pred_grid2)$oxy), max(drop_na(pred_grid2)$oxy))) +
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
pred_grid2 <- left_join(pred_grid2, pred_grid)

# Save
#write.csv(pred_grid2, file = "data/for_analysis/pred_grid2.csv", row.names = FALSE)
