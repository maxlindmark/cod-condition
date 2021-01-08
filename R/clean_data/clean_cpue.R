#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2021.01.07: Max Lindmark
#
# - Code to clean CPUE data and add temperature and oxygen from NEMO NORDIC SCOBII MODEL
#   https://resources.marine.copernicus.eu/?option=com_csw&task=results
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
rm(list = ls())

# Load libraries, install if needed
library(tidyverse); theme_set(theme_classic())
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
library(sdmTMB) # remotes::install_github("pbs-assess/sdmTMB")
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapplots)
library(geosphere)

# Print package versions
# sessionInfo()
# other attached packages:
# [1] sf_0.9-5           raster_3.3-13      rgdal_1.5-12       sp_1.4-2           mapdata_2.3.0      maps_3.3.0         ggsidekick_0.0.2  
# [8] icesDatras_1.3-0   janitor_2.0.1      patchwork_1.0.1    RColorBrewer_1.1-2 viridis_0.5.1      viridisLite_0.3.0  RCurl_1.98-1.2    
# [15] tidylog_1.0.2      readxl_1.3.1       forcats_0.5.0      stringr_1.4.0      dplyr_1.0.0        purrr_0.3.4        readr_1.3.1       
# [22] tidyr_1.1.0        tibble_3.0.3       ggplot2_3.3.2      tidyverse_1.3.0 

# For adding maps to plots
world <- ne_countries(scale = "medium", returnclass = "sf")

# Specify map ranges
# ymin = 54; ymax = 58; xmin = 9.5; xmax = 22
ymin = 54; ymax = 58; xmin = 12.5; xmax = 22

# Make plot function
plot_map_raster <- function(dat, column = "est") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    facet_wrap(~year) +
    coord_fixed() +
    scale_fill_viridis_c() +
    geom_sf(data = world, inherit.aes = F, size = 0.2) +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
}


# B. READ HAUL DATA ================================================================
# Read data (length-specific CPUE) 
# Unit is #Catch in numbers per hour of hauling"
# In theory I could convert to "Catch per km2" or rather "Biomass per km2" to get the
# same unit as in Sean Anderson...
cpue <- read.csv("data/DATRAS_cpue_length_haul/CPUE per length per haul per hour_2020-09-25 16_15_36.csv")

head(cpue)

# Create a new ID column to distinguish hauls
# Filter by species and convert length to cm, then add in ID
cpue <- cpue %>%
  mutate(length_cm = LngtClass/10) %>% # All are of the same LngtCode
  mutate(ID = paste(Year, Quarter, Ship, Gear, HaulNo, ShootLat, ShootLong, sep = "."))

length(unique(cpue$ID))
cpue <- cpue %>% filter(Species == "Gadus morhua")
length(unique(cpue$ID))  

# First check if this is unique by haul. Then I should get 1 row per ID and size...
cpue %>%
  group_by(ID, LngtClass) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct(n)

# # Test it's the same number of valid hauls as in the raw haul data
# bits_hh <- read.csv("data/DATRAS_exchange/bits_hh.csv") %>% 
#   mutate(ID = paste(Year, Quarter, Ship, Gear, HaulNo, ShootLat, ShootLong, sep = "."))
# 
# # Filter only valid hauls:
# bits_hh <- bits_hh %>% filter(HaulVal == "V")
# 
# # Check that per ID, there's only one row
# bits_hh %>%
#   group_by(ID) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   distinct(n)
# 
# bits_hh %>%
#   group_by(ID) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   filter(n == 2)
# # Only 10 rows with n = 2, will remove
# bits_hh <- bits_hh %>% 
#   group_by(ID) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   filter(n == 1) %>% 
#   dplyr::select(-n)
# 
# length(unique(bits_hh$ID))
# length(unique(cpue$ID))
# 
# sort(unique(bits_hh$Year))
# sort(unique(cpue$Year))
# 
# # Filter so that we have the same years (1991-2019)
# bits_hh <- bits_hh %>% filter(Year < 2020 & Quarter == 4)
# 
# length(unique(bits_hh$ID))
# length(unique(cpue$ID))
# 
# sort(unique(bits_hh$Year))
# sort(unique(cpue$Year))
# 
# # Ok, which ID's are in the haul data but not in the CPUE data?
# missing_ids <- bits_hh$ID[!bits_hh$ID %in% cpue$ID]
# missing_ids
# 
# # "2019.4.26HF.TVS.68.57.0275.10.7215"
# # ID = paste(Year, Quarter, Ship, Gear, HaulNo, ShootLat, ShootLong
# 
# # Check this ID in the cpue data... (it doesn't exist there but we can filter by almost all variables)
# cpue %>% filter(Year == 2019 & Quarter == 4 & Ship == "26HF", Gear == "TVS") %>% arrange(desc(HaulNo)) %>% distinct(HaulNo)
# 
# # Ok, so there is no 68 HaulNo in the cpue data. Not sure why, but I will continue assuming that all the 0 hauls are present there...

# Remove hauls from outside the study area and select only quarter 4
cpue <- cpue %>% 
  filter(ShootLat < 58) %>% 
  mutate(kattegatt = ifelse(ShootLat > 56 & ShootLong < 14, "Y", "N")) %>% 
  filter(kattegatt == "N") %>% 
  filter(Quarter == 4) %>% 
  dplyr::select(-kattegatt)

# Now, calculate the weight based no their lengths, given the length-weight relationship
# I have estimated in the condition script.
cpue$log_length_cm <- log(cpue$length_cm)
cpue$log_weight_g <- -4.6 + cpue$log_length_cm*2.98
cpue$weight_g <- exp(cpue$log_weight_g)
cpue$weight_kg <- cpue$weight_g / 100

# Ensure we have 0 catches in the data as well
#cpue %>% arrange(CPUE_number_per_hour)
#cpue %>% filter(CPUE_number_per_hour == 0)

# Calculate CPUE not in numbers per hour bu kg per hour
cpue <- cpue %>% mutate(CPUE_weight_per_hour = CPUE_number_per_hour*weight_kg)

# Now calculate the sum of all CPUEs for each length class per haul
cpue_tot <- cpue %>% 
  group_by(ID) %>% 
  mutate(CPUE_kg_hour_tot = sum(CPUE_weight_per_hour)) %>% 
  ungroup() %>% 
  distinct(ID, .keep_all = TRUE)

# Test it worked, first by plotting n rows per ID
cpue_tot %>% group_by(ID) %>% mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

# Next by calculating an example
id <- unique(cpue_tot$ID)[99]

cpue_tot %>%
  filter(ID == id) %>%
  dplyr::select(ID, CPUE_kg_hour_tot)

sum(filter(cpue, ID == id)$CPUE_weight_per_hour)
# Correct! The data subset yields the same 

#-- Testing if we can calculate area swept and by that remove the time dimension and 
# go from catch per effort to biomasss per area
# ** Looks like we can't to many rows missing door spread and distance trawled values
# Now we need to join the haul-level data so that we can convert from cpue_kg/h to 
# # kg/km^2
# bits_hh <- read.csv("data/DATRAS_exchange/bits_hh.csv") %>%
#   mutate(ID = paste(Year, Quarter, Ship, Gear, HaulNo, ShootLat, ShootLong, sep = ".")) %>% 
#   filter(HaulVal == "V") # Filter valid hauls
# 
# # Filter the ID's that are already in the cpue data (see hashtagged code above)
# bits_hh <- bits_hh %>% 
#   filter(ID %in% cpue_tot$ID) %>% 
#   arrange(ID)
#   
# # bits_hh2 %>% group_by(ID) %>% mutate(n = n()) %>% ungroup() %>% distinct(n)
# # length(unique(bits_hh2$ID))
# # length(unique(cpue_tot$ID))
# 
# bits_hh %>% filter(Distance > 0 & DoorSpread > 0)
# # Ok, some rows without a distance measurement (57%!). Need to calculate distance for these
# # rowwise based on the shoot and haul coordinates
# 
# # Short test
# # df <- data.frame(long = 17.9483, lat = 57.0632)
# # my_long <- 17.9385
# # my_lat <- 57.0133
# # df %>%
# #   rowwise() %>%
# #   mutate(dist = distm(c(my_long, my_lat), c(long, lat), fun=distHaversine))
# # head(bits_hh2, 1)
# 
# # Calculate distance based on coordinates
# distances <- bits_hh %>%
#   rowwise() %>%
#   mutate(dist_calculated = distm(c(ShootLong, ShootLat), c(HaulLong, HaulLat), fun = distHaversine)) %>% 
#   mutate(dist_calculated = as.integer(dist_calculated)) %>% 
#   dplyr::select(dist_calculated)
# 
# # I do not really trust the Distance column in the data set and will therefore use the
# # calculated distance based on coordinates, so that it's the same for all rows and because
# # I know there aren't any missing coordinates since then I can even calculate the ID properly
# # The above plot shows it's OK though (roughly similar distances)
# # Some rows have extremely small distance even when the coordinates clearly differ, check
# # e.g. here: https://gps-coordinates.org/distance-between-coordinates.php
# 
# bits_hh$dist_calculated <- distances$dist_calculated
# 
# str(bits_hh)
# 
# # Plot and compare distances from the two sources
# bits_hh %>% 
#   filter(Distance > 0 & dist_calculated > 0) %>% 
#   ggplot(., aes(Distance, dist_calculated)) + geom_point()
# 
# bits_hh %>% 
#   filter(Distance > 0 & dist_calculated > 0) %>% 
#   dplyr::select(Distance, dist_calculated)
# 
# bits_hh %>% 
#   filter(Distance > 0 & dist_calculated > 0 & dist_calculated < 15000) %>% 
#   ggplot(., aes(Distance, dist_calculated)) + geom_point()
# 
# # The reason the calculated distance is blowing up is because some HAUL coordinates are NA
# bits_hh %>% 
#   filter(dist_calculated > 15000)
# 
# # To deal with that, I will assign the distance based on the original distance column
# bits_hh <- bits_hh %>% mutate(dist2 = ifelse(dist_calculated > 10000, Distance, dist_calculated))
# 
# ggplot(bits_hh, aes(Distance, dist2)) + geom_point()
# 
# ggplot(bits_hh, aes(dist2)) + geom_histogram()
# 
# # Ok, now that I have distance 
# test <- bits_hh %>% filter(dist2 > 0)
# 
# # Do I have a do
# test2 <- test %>% filter(DoorSpread > 0)
# 
# # Meh, if wewant data with a door spread estimate we omit 50% of rows!!! Cannot accept that
#-- End test

# Inspect
# length(unique(t$ID))

dat <- cpue_tot %>%
  rename("cpue" = "CPUE_kg_hour_tot",
         "year" = "Year",
         "lat" = "ShootLat",
         "lon" = "ShootLong",
         "quarter" = "Quarter",
         "depth" = "Depth") %>% 
  dplyr::select(cpue, year, lat, lon, quarter, depth)


# C. READ AND JOIN OCEANOGRAPHIC DATA ==============================================
# ** Oxygen ========================================================================
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
# The condition data is called dat so far in this script

# Filter years in the condition data frame to only have the years I have oxygen for
d_sub_oxy <- dat %>% filter(year %in% names(dlist)) %>% droplevels()

# Create data holding object
data_list <- list()

# ... And for the oxygen raster
raster_list <- list()

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
  
  # Save to check each year is ok! First convert the raster to points for plotting
  # (so that we can use ggplot)
  map.p <- rasterToPoints(r)
  
  # Make the points a dataframe for ggplot
  df_rast <- data.frame(map.p)
  
  # Rename y-variable and add year
  df_rast <- df_rast %>% rename("oxy" = "layer") %>% mutate(year = i)
  
  # Add each years' raster data frame in the list
  raster_list[[index]] <- df_rast
  
  # Make appropriate column headings
  colnames(df_rast) <- c("Longitude", "Latitude", "oxy")
  
  # Now make the map
  ggplot(data = df_rast, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = oxy)) +
    geom_point(data = d_slice, aes(x = lon, y = lat, fill = oxy),
               color = "black", size = 5, shape = 21) +
    theme_bw() +
    geom_sf(data = world, inherit.aes = F, size = 0.2) +
    coord_sf(xlim = c(min(dat$lon), max(dat$lon)),
             ylim = c(min(dat$lat), max(dat$lat))) +
    scale_colour_gradientn(colours = rev(terrain.colors(10)),
                           limits = c(-200, 400)) +
    scale_fill_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(-200, 400)) +
    NULL
  
  ggsave(paste("figures/supp/cpue_oxygen_rasters/", i,".png", sep = ""),
         width = 6.5, height = 6.5, dpi = 600)
  
}

# Now create a data frame from the list of all annual values
big_dat_oxy <- dplyr::bind_rows(data_list)
big_raster_dat_oxy <- dplyr::bind_rows(raster_list)

# Plot data, looks like there's big inter-annual variation but a negative
big_raster_dat_oxy %>%
  group_by(year) %>%
  drop_na(oxy) %>%
  summarise(mean_oxy = mean(oxy)) %>%
  mutate(year_num = as.numeric(year)) %>%
  ggplot(., aes(year_num, mean_oxy)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm") +
  NULL

big_raster_dat_oxy %>%
  group_by(year) %>%
  drop_na(oxy) %>%
  mutate(dead = ifelse(oxy < 0, "Y", "N")) %>%
  filter(dead == "Y") %>%
  mutate(n = n(),
         year_num = as.numeric(year)) %>%
  ggplot(., aes(year_num, n)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm") +
  NULL

# Now add in the new oxygen column in the original data:
str(d_sub_oxy)
str(big_dat_oxy)

# Create an ID for matching the oxygen data with the condition data
dat$id_oxy <- paste(dat$year, dat$lon, dat$lat, sep = "_")
big_dat_oxy$id_oxy <- paste(big_dat_oxy$year, big_dat_oxy$lon, big_dat_oxy$lat, sep = "_")

# Which id's are not in the cpue data (dat)?
ids <- dat$id_oxy[!dat$id_oxy %in% c(big_dat_oxy$id_oxy)]

unique(ids)

# Select only the columns we want to merge
big_dat_sub_oxy <- big_dat_oxy %>% dplyr::select(id_oxy, oxy)

# Remove duplicate ID (one oxy value per id)
big_dat_sub_oxy2 <- big_dat_sub_oxy %>% distinct(id_oxy, .keep_all = TRUE)
# big_dat_sub_oxy %>% group_by(id_oxy) %>% mutate(n = n()) %>% arrange(desc(n))

# Join the data with raster-derived oxygen with the full condition data
dat <- left_join(dat, big_dat_sub_oxy2, by = "id_oxy")


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
d_sub_temp <- dat %>% filter(year %in% names(dlist)) %>% droplevels()

# Create data holding object
data_list <- list()

# ... And for the temperature raster
raster_list <- list()

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
  
  # Save to check each year is ok! First convert the raster to points for plotting
  # (so that we can use ggplot)
  map.p <- rasterToPoints(r)
  
  # Make the points a dataframe for ggplot
  df_rast <- data.frame(map.p)
  
  # Rename y-variable and add year
  df_rast <- df_rast %>% rename("temp" = "layer") %>% mutate(year = i)
  
  # Add each years' raster data frame in the list
  raster_list[[index]] <- df_rast
  
  # Make appropriate column headings
  colnames(df_rast) <- c("Longitude", "Latitude", "temp")
  
  # Now make the map
  ggplot(data = df_rast, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = temp)) +
    geom_point(data = d_slice, aes(x = lon, y = lat, fill = temp),
               color = "black", size = 5, shape = 21) +
    theme_bw() +
    geom_sf(data = world, inherit.aes = F, size = 0.2) +
    coord_sf(xlim = c(min(dat$lon), max(dat$lon)),
             ylim = c(min(dat$lat), max(dat$lat))) +
    scale_colour_gradientn(colours = rev(terrain.colors(10)),
                           limits = c(2, 17)) +
    scale_fill_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(2, 17)) +
    NULL
  
  ggsave(paste("figures/supp/cpue_temp_rasters/", i,".png", sep = ""),
         width = 6.5, height = 6.5, dpi = 600)
  
}

# Now create a data frame from the list of all annual values
big_dat_temp <- dplyr::bind_rows(data_list)
big_raster_dat_temp <- dplyr::bind_rows(raster_list)

big_dat_temp %>% drop_na(temp) %>% summarise(max = max(temp))
big_dat_temp %>% drop_na(temp) %>% summarise(min = min(temp))

# Plot data, looks like there's big inter-annual variation but a positive trend
big_raster_dat_temp %>%
  group_by(year) %>%
  drop_na(temp) %>%
  summarise(mean_temp = mean(temp)) %>%
  mutate(year_num = as.numeric(year)) %>%
  ggplot(., aes(year_num, mean_temp)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm") +
  NULL

# Now add in the new temperature column in the original data:
str(d_sub_temp)
str(big_dat_temp)

# Create an ID for matching the temperature data with the condition data
dat$id_temp <- paste(dat$year, dat$lon, dat$lat, sep = "_")
big_dat_temp$id_temp <- paste(big_dat_temp$year, big_dat_temp$lon, big_dat_temp$lat, sep = "_")

# Which id's are not in the condition data (dat)? (It's because I don't have those years, not about the location)
ids <- dat$id_temp[!dat$id_temp %in% c(big_dat_temp$id_temp)]

unique(ids)

# Select only the columns we want to merge
big_dat_sub_temp <- big_dat_temp %>% dplyr::select(id_temp, temp)

# Remove duplicate ID (one temp value per id)
big_dat_sub_temp2 <- big_dat_sub_temp %>% distinct(id_temp, .keep_all = TRUE)

# Join the data with raster-derived oxygen with the full condition data
dat <- left_join(dat, big_dat_sub_temp2, by = "id_temp")

colnames(dat)

# Save data
write.csv(dat, file = "data/for_analysis/mdat_cpue.csv", row.names = FALSE)

