#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# - Code to clean and merge BITS HH (Record with detailed haul information),
#   and CA (Sex-maturity-age–length keys (SMALK's) for ICES subdivision) data
#   directly from DATRAS. We want to end up with a dataset of length-at-weight of cod,
#   with haul information.
# 
#   Next, we join in CPUE (Catch in numbers per hour of hauling) of flounder and cod
#   We calculate this for two size classes by species. If the haul is not in the catch
#   data, give a 0 catch
# 
#   After that, we join in the abundance of sprat and herring. This is available on 
#   an ICES rectangle-level, so many hauls will end up with the same abundance
# 
#   Lastly, we join in OCEANOGRAPHIC data (oxygen, temperature). This is model output
#   from NEMO_Nordic_SCOBI, downloaded from:
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
ymin = 54; ymax = 58; xmin = 9.5; xmax = 22


# B. READ HAUL DATA ================================================================
# Load HH data using the DATRAS package to get catches
# bits_hh <- getDATRAS(record = "HH", survey = "BITS", years = 1991:2020, quarters = 1:4)

# write.csv("data/bits_hh.csv")
bits_hh <- read.csv("data/DATRAS_exchange/bits_hh.csv")

# Create ID column
bits_hh <- bits_hh %>% 
  mutate(ID = paste(Year, Quarter, Ship, Gear, HaulNo, StNo, sep = "."))

# Check that per ID, there's only one row
bits_hh %>%
  group_by(ID) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(ID) %>% 
  as.data.frame()

# Check default availability of environmental data
ggplot(bits_hh, aes(BotSal)) + geom_histogram()
ggplot(bits_hh, aes(SurSal)) + geom_histogram()
ggplot(bits_hh, aes(BotTemp)) + geom_histogram()

# Plot haul-duration
ggplot(bits_hh, aes(HaulDur)) + geom_histogram()

# Select only useful columns, this is the dataframe used in the merge later on
bits_hh_filter <- bits_hh %>% dplyr::select(ID, ShootLat, ShootLong, StatRec, Depth,
                                            BotTemp,BotSal, Year, Quarter, HaulDur, 
                                            DataType, HaulVal)

# Test I only got 1 row per haul
bits_hh_filter %>% 
  group_by(ID) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()


# C. READ LENGTH-WEIGHT DATA =======================================================
# Load CA data using the DATRAS package to get catches
# Note we only want cod data here
# bits_ca <- getDATRAS(record = "CA", survey = "BITS", years = 1991:2020, quarters = 1:4)

# write.csv("data/bits_ca.csv")
bits_ca <- read.csv("data/DATRAS_exchange/bits_ca.csv")

# Filter only cod and positive length measurements
bits_ca <- bits_ca %>% filter(SpecCode %in% c("164712", "126436") & LngtClass > 0)

# Add new species-column
bits_ca$Species <- "Cod"

# Create ID column
bits_ca <- bits_ca %>% 
  mutate(ID = paste(Year, Quarter, Ship, Gear, HaulNo, StNo, sep = "."))

# Check that per ID AND LNGTCLASS, there's only one row
bits_ca %>% 
  mutate(TEST = paste(ID, LngtClass)) %>% 
  group_by(TEST) %>% 
  mutate(n = n()) %>% 
  ungroup() %>%
  ggplot(., aes(factor(n))) + geom_bar()

# Now I need to copy rows with NoAtLngt > 1 so that 1 row = 1 ind
# First make a small test
nrow(bits_ca)
head(filter(bits_ca, NoAtLngt == 5))
head(filter(bits_ca, ID == "1992.1.GFR.SOL.H20.33.42" & NoAtLngt == 5), 20)

bits_ca <- bits_ca %>% map_df(., rep, .$NoAtLngt)

head(data.frame(filter(bits_ca, ID == "1992.1.GFR.SOL.H20.33.42" & NoAtLngt == 5)), 20)
nrow(bits_ca)
# Looks ok!

# Standardize length
bits_ca <- bits_ca %>% 
  drop_na(IndWgt) %>% 
  drop_na(LngtClass) %>% 
  filter(IndWgt > 0 & LngtClass > 0) %>%  # Filter positive length and weight
  mutate(length_cm = ifelse(LngtCode == ".", 
                            LngtClass/10,
                            LngtClass)) %>% # Standardize length ((https://vocab.ices.dk/?ref=18))
  as.data.frame()
  
ggplot(bits_ca, aes(length_cm, fill = LngtCode)) + geom_histogram()


# D. JOIN CONDITION AND HAUL DATA ==================================================
# Check if any ID is in the HL but not HH data
# I will need to remove these because they do not have any spatial information
bits_ca$ID[!bits_ca$ID %in% bits_hh_filter$ID]

# And other way around (this is expected since we have hauls without catches or data 
# on condition)
bits_hh_filter$ID[!bits_hh_filter$ID %in% bits_ca$ID]

dat <- left_join(bits_ca, bits_hh_filter)

# Remove the NA latitudes and we remove all the IDs that were in the bits_ca but not 
# in the haul data
dat <- dat %>% drop_na(ShootLat)

# Plot spatial distribution of samples
# dat %>% 
#   ggplot(., aes(y = ShootLat, x = ShootLong)) +
#   geom_point(size = 0.3) +
#   facet_wrap(~ Year) + 
#   theme_bw() +
#   geom_sf(data = world, inherit.aes = F, size = 0.2) +
#   coord_sf(xlim = c(8, 25), ylim = c(54, 60)) +
#   NULL

# Lastly we can remove hauls from outside the study area (Kattegatt basically)
# select only quarter 4 and remove non-valid hauls
dat <- dat %>% 
  filter(ShootLat < 58) %>% 
  mutate(kattegatt = ifelse(ShootLat > 56 & ShootLong < 14, "Y", "N")) %>% 
  filter(kattegatt == "N",
         Quarter == 4,
         HaulVal == "V") %>% 
  dplyr::select(-kattegatt)

# Plot again:
# Plot spatial distribution of samples
# dat %>% 
#   ggplot(., aes(y = ShootLat, x = ShootLong)) +
#   geom_point(size = 0.3) +
#   facet_wrap(~ Year) + 
#   theme_bw() +
#   geom_sf(data = world, inherit.aes = F, size = 0.2) +
#   coord_sf(xlim = c(8, 25), ylim = c(54, 60)) +
#   NULL

min(dat$ShootLon)

dat %>% filter(ID == "1991.4.SOL.H20.34.49")


# E. READ AND JOIN THE COD AND FLOUNDER COVARIATES =================================
cov_dat <- read.csv("data/DATRAS_cpue_length_haul/CPUE per length per haul per hour_2020-09-25 16_15_36.csv")

# Remove hauls from outside the study area and select only quarter 4
cov_dat <- cov_dat %>% 
  filter(ShootLat < 58) %>% 
  mutate(kattegatt = ifelse(ShootLat > 56 & ShootLong < 14, "Y", "N")) %>% 
  filter(kattegatt == "N") %>% 
  filter(Quarter == 4) %>% 
  dplyr::select(-kattegatt)

# I am now going to assume that a haul that is present in the condition data but not
# in this covariate data means that the catch is 0
cov_dat %>% arrange(CPUE_number_per_hour)
cov_dat %>% filter(CPUE_number_per_hour == 0)

# Create a new ID column. Note that I can't define a single ID column that works for
# all data sets. The ID that I used for the Exchange data cannot be applied here. I
# need to come up with a new ID here. Run this to see common columns:
# colnames(dat)[colnames(dat) %in% colnames(cov_dat)]
# First filter by species and convert length to cm, then add in ID

cod <- cov_dat %>%
  filter(Species == "Gadus morhua") %>% 
  mutate(length_cm = LngtClass/10) %>% 
  filter(LngtClass > 0) %>% 
  mutate(ID2 = paste(Year, Quarter, Ship, Gear, HaulNo, Depth, ShootLat, ShootLong, sep = "."))

fle <- cov_dat %>%
  filter(Species == "Platichthys flesus") %>% 
  mutate(length_cm = LngtClass/10) %>% 
  filter(LngtClass > 0) %>% 
  mutate(ID2 = paste(Year, Quarter, Ship, Gear, HaulNo, Depth, ShootLat, ShootLong, sep = "."))

# First check if this is unique by haul. Then I should get 1 row per ID and size...
cod %>%
  group_by(ID2, LngtClass) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct(n, .keep_all = TRUE) %>% 
  as.data.frame()

fle %>%
  group_by(ID2, LngtClass) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct(n, .keep_all = TRUE) %>% 
  as.data.frame()

# And add also the same ID to dat (condition and haul data).
# Check if unique!
# It is with the exception of 8 rows. Not much I can do about that because I don't have
# any more unique columns I can add to the ID
test <- read.csv("data/DATRAS_exchange/bits_hh.csv")
test <- test %>%
  mutate(ID2 = paste(Year, Quarter, Ship, Gear, HaulNo, Depth, ShootLat, ShootLong, sep = "."))

test %>%
  mutate(ID2 = paste(Year, Quarter, Ship, Gear, HaulNo, Depth, ShootLat, ShootLong, sep = ".")) %>%
  group_by(ID2) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(!n==1) %>%
  as.data.frame()

# Test if these are in dat:
test_ids <- unique(test$ID2)

# Add in ID2
dat <- dat %>%
  mutate(ID2 = paste(Year, Quarter, Ship, Gear, HaulNo, Depth, ShootLat, ShootLong, sep = "."))

# No they are not, no need to filter.
filter(dat, ID2 %in% test_ids)

# Are there any ID's that are IN the covariate data that are not in the test
# (raw haul data) data?
cod$ID2[!cod$ID2 %in% test$ID2]
fle$ID2[!fle$ID2 %in% test$ID2]

filter(fle, ID2 %in% unique(test$ID2)) 
# Nope! All good.

# Now calculate the mean CPUE per haul and size group per species. For cod we use 30
# cm and for flounder 20 cm. This is because Neuenfeldt et al (2019) found that cod
# below 30cm are in a growth-bottleneck, and because Haase et al (2020) found that 
# flounder above 20cm start feeding a lot of saduria, which has been speculated to
# decline in cod stomachs due to interspecific competition and increased spatial
# overlap with flounder.
cod_above_30cm <- cod %>% 
  filter(length_cm >= 30) %>% 
  group_by(ID2) %>% 
  summarise(cpue_cod_above_30cm = sum(CPUE_number_per_hour)) %>% 
  ungroup()

cod_below_30cm <- cod %>% 
  filter(length_cm < 30) %>% 
  group_by(ID2) %>% 
  summarise(cpue_cod_below_30cm = sum(CPUE_number_per_hour)) %>% 
  ungroup()

fle_above_20cm <- fle %>% 
  filter(length_cm >= 20) %>% 
  group_by(ID2) %>% 
  summarise(cpue_fle_above_20cm = sum(CPUE_number_per_hour)) %>% 
  ungroup()

fle_below_20cm <- fle %>% 
  filter(length_cm < 20) %>% 
  group_by(ID2) %>% 
  summarise(cpue_fle_below_20cm = sum(CPUE_number_per_hour)) %>% 
  ungroup()

# Test it worked, first by plotting n rows per ID
cod_above_30cm %>% group_by(ID2) %>% mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

# Next by calculating an example
unique(cod_above_30cm$ID2)

cod_above_30cm %>% filter(ID2 == "2003.4.DAN2.TVL.13.66.55.3736.16.9982")
sum(filter(cod, ID2 == "2003.4.DAN2.TVL.13.66.55.3736.16.9982" &
             length_cm >= 30)$CPUE_number_per_hour)

# Correct! The data subset yields the same 

# Join covariates
cod_above_30cm
cod_below_30cm
fle_above_20cm
fle_below_20cm

# Some rows are not present in the condition data (dat) but are in the CPUE data.
# As I showed above, all ID2's in the CPUE data are in the raw haul data, so if they 
# aren't with us anymore that means they have been filtered away along the road.
# I don't need to remove those ID2's though because when using left_join I keep only
# rows in x!
# cod_above_30cm$ID2[!cod_above_30cm$ID2 %in% test$ID2]

# Left join dat and cpue data (cod_above_30cm)
dat <- left_join(dat, cod_above_30cm) 

# Left join dat and cpue data (cod_below_30cm)
dat <- left_join(dat, cod_below_30cm) 

# Left join dat and cpue data (fle_above_20cm)
dat <- left_join(dat, fle_above_20cm) 

# Left join dat and cpue data (fle_below_20cm)
dat <- left_join(dat, fle_below_20cm) 

head(dat)

dat

# Again, I'm assuming here that NA means 0 catch, because there were no catches by that 
# haul in the CPUE data. Testing a random ID2 that the covariate is repeated within haul
unique(dat$ID2)

filter(dat, ID2 == "2004.4.SOL2.TVS.53.42.55.1918.13.2392")
filter(dat, ID2 == "2003.4.BAL.TVL.23.62.54.55.15.65")

# Replace NA catches with 0
dat$cpue_cod_above_30cm[is.na(dat$cpue_cod_above_30cm)] <- 0
dat$cpue_cod_below_30cm[is.na(dat$cpue_cod_below_30cm)] <- 0
dat$cpue_fle_above_20cm[is.na(dat$cpue_fle_above_20cm)] <- 0
dat$cpue_fle_below_20cm[is.na(dat$cpue_fle_below_20cm)] <- 0

filter(dat, ID2 == "2004.4.SOL2.TVS.53.42.55.1918.13.2392")
filter(dat, ID2 == "2003.4.BAL.TVL.23.62.54.55.15.65")

# Create total CPUE column
dat <- dat %>% mutate(cpue_cod = cpue_cod_above_30cm + cpue_cod_below_30cm,
                      cpue_fle = cpue_fle_above_20cm + cpue_fle_below_20cm)

# Final check, use random ID2's and compare them in cod/fle and test
cod$ID2[cod$ID2 %in% dat$ID2]

cod %>% filter(ID2 == "1992.4.SOL.H20.42.20.54.5.14.2")
dat %>% filter(ID2 == "1992.4.SOL.H20.42.20.54.5.14.2")

sum(filter(cod, ID2 == "1991.4.SOL.H20.30.26.54.6.14.25" & length_cm >= 30)$CPUE_number_per_hour)
sum(filter(cod, ID2 == "1991.4.SOL.H20.30.26.54.6.14.25" & length_cm < 30)$CPUE_number_per_hour)
dat %>% filter(ID2 == "1991.4.SOL.H20.30.26.54.6.14.25") %>%
  dplyr:: select(ID2, cpue_cod_above_30cm, cpue_cod_below_30cm)

fle %>% filter(ID2 == "1992.4.SOL.H20.42.20.54.5.14.2")
dat %>% filter(ID2 == "1992.4.SOL.H20.42.20.54.5.14.2")

sum(filter(fle, ID2 == "1991.4.SOL.H20.30.26.54.6.14.25" & length_cm >= 20)$CPUE_number_per_hour)
sum(filter(fle, ID2 == "1991.4.SOL.H20.30.26.54.6.14.25" & length_cm < 20)$CPUE_number_per_hour)
dat %>% filter(ID2 == "1991.4.SOL.H20.30.26.54.6.14.25") %>%
  dplyr:: select(ID2, cpue_fle_above_20cm, cpue_fle_below_20cm)

# TEST
dat %>% filter(ID == "1991.4.SOL.H20.34.49")


# F. READ AND JOIN PELAGIC COVARIATES ==============================================
#** Rectangle level ================================================================
spr <- read_xlsx("data/BIAS/abundances_rectangles_1991-2019.xlsx",
                 sheet = 1) %>%
  rename("StatRec" = "Rec") %>%
  mutate(StatRec = as.factor(StatRec),
         Species = "Sprat",
         abun_spr = `Age 1`+`Age 2`+`Age 3`+`Age 4`+`Age 5`+`Age 6`+`Age 7`+`Age 8+`, # omitting `0+` here
         ID3 = paste(StatRec, Year, sep = ".")) # Make new ID)
  
her <- read_xlsx("data/BIAS/abundances_rectangles_1991-2019.xlsx",
                 sheet = 2) %>%
  as.data.frame() %>%
  rename("StatRec" = "Rect2") %>% # This is not called Rec in the data for some reason
  mutate(StatRec = as.factor(StatRec),
         Species = "Herring",
         abun_her = `Age 1`+`Age 2`+`Age 3`+`Age 4`+`Age 5`+`Age 6`+`Age 7`+`Age 8+`, # omitting `1+` here
         ID3 = paste(StatRec, Year, sep = ".")) # Make new ID

# Plot distribution over time in the whole area
spr %>%
  mutate(lon = ices.rect(spr$StatRec)$lon) %>%
  mutate(lat = ices.rect(spr$StatRec)$lat) %>%
  filter(! StatRec %in% c("41G0", "41G1", "41G2", "42G1", "42G2", "43G1", "43G2", "44G0", "44G1")) %>%
  ggplot(., aes(lon, lat, color = log(abun_spr))) +
  geom_point(size = 2.5, shape = 15) +
  scale_color_viridis() +
  facet_wrap(~ Year, ncol = 5) +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(x = "lon", y = "lat") +
  ggtitle("log(abun_spr)") +
  NULL

ggsave("figures/supp/spr_distribution.png", width = 10, height = 10, dpi = 600)

her %>%
  mutate(lon = ices.rect(her$StatRec)$lon) %>%
  mutate(lat = ices.rect(her$StatRec)$lat) %>%
  filter(! StatRec %in% c("41G0", "41G1", "41G2", "42G1", "42G2", "43G1", "43G2", "44G0", "44G1")) %>%
  ggplot(., aes(lon, lat, color = log(abun_her))) +
  geom_point(size = 2.5, shape = 15) +
  scale_color_viridis() +
  facet_wrap(~ Year, ncol = 5) +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(x = "lon", y = "lat") +
  ggtitle("log(abun_her)") +
  NULL

ggsave("figures/supp/her_distribution.png", width = 10, height = 10, dpi = 600)

# As can be seen in the above plots, we don't have data for all rectangles in all years
# It is important those rectangles are made NA - not 0 - when merging

# Check distribution of data
# https://www.researchgate.net/publication/47933620_Environmental_factors_and_uncertainty_in_fisheries_management_in_the_northern_Baltic_Sea/figures?lo=1
sort(unique(spr$SD))
sort(unique(her$SD))

# How many unique rows per ID3?
her %>%
  group_by(ID3) %>% 
  mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

spr %>%
  group_by(ID3) %>% 
  mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

# Ok, some ID's with two rows...
test_spr <- spr %>%
  group_by(ID3) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  ungroup() %>% 
  as.data.frame()

test_spr

# Seems to be due to rectangles somehow being in different sub divisions.
# I need to group by ID3 and summarize
# First check all rectangles with more than one row have two rows and not more
nrow(spr)
nrow(spr %>% group_by(ID3) %>% mutate(n = n()) %>% filter(n == 2))
nrow(spr %>% group_by(ID3) %>% mutate(n = n()) %>% filter(!n == 1))

spr_sum <- spr %>%
  group_by(ID3) %>% 
  summarise(abun_spr = sum(abun_spr)) %>% # Sum abundance within ID3
  distinct(ID3, .keep_all = TRUE) %>% # Remove duplicate ID3
  mutate(ID_temp = ID3) %>% # Create temporary ID3 that we can use to split in order
                            # to get Year and StatRect back into the summarized data
  separate(ID_temp, c("StatRec", "Year"), sep = 4)

nrow(spr_sum) 
nrow(spr)
nrow(spr %>% group_by(ID3) %>% mutate(n = n()) %>% filter(n == 2))

filter(spr_sum, ID3 == "39G2.1991")
filter(spr, ID3 == "39G2.1991")

# This should equal 1 (new # rows =  old - duplicated ID3)
nrow(spr_sum) / (nrow(spr) - 0.5*nrow(spr %>% group_by(ID3) %>% mutate(n = n()) %>% filter(n == 2)))

# How many rows per rectangle?
spr_sum %>%
  group_by(ID3) %>% 
  mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

# Now do the same for herring
nrow(her)
nrow(her %>% group_by(ID3) %>% mutate(n = n()) %>% filter(n == 2))
nrow(her %>% group_by(ID3) %>% mutate(n = n()) %>% filter(!n == 1))

her_sum <- her %>%
  group_by(ID3) %>% 
  summarise(abun_her = sum(abun_her)) %>% # Sum abundance within ID3
  distinct(ID3, .keep_all = TRUE) %>% # Remove duplicate ID3
  mutate(ID_temp = ID3) %>% # Create temporary ID3 that we can use to split in order
  # to get Year and StatRect back into the summarized data
  separate(ID_temp, c("StatRec", "Year"), sep = 4)

nrow(her_sum) 
nrow(her)
nrow(her %>% group_by(ID3) %>% mutate(n = n()) %>% filter(n == 2))

filter(her_sum, ID3 == "39G2.1991")
filter(her, ID3 == "39G2.1991")

# This should equal 1 (new # rows =  old - duplicated ID3)
nrow(her_sum) / (nrow(her) - 0.5*nrow(her %>% group_by(ID3) %>% mutate(n = n()) %>% filter(n == 2)))

# How many rows per rectangle?
her_sum %>%
  group_by(ID3) %>% 
  mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

# Join pelagic covariates
# Make StatRec a factor in the main data
dat <- dat %>% mutate(StatRec = as.factor(StatRec))
unique(is.na(dat$StatRec))

# Create ID3 in main data to match pelagics data
dat <- dat %>% mutate(ID3 = paste(StatRec, Year, sep = "."))

# Are there any StatRec that are in the condition data that are not in the pelagics data?
dat$StatRec[!dat$StatRec %in% her$StatRec]
dat$StatRec[!dat$StatRec %in% spr$StatRec]

# No, but not all ID3's are present
dat$ID3[!dat$ID3 %in% her$ID3]
dat$ID3[!dat$ID3 %in% spr$ID3]

filter(dat, ID3 == "44G8.1991")
filter(her, ID3 == "44G8.1991")

filter(dat, StatRec == "44G8")
filter(her, StatRec == "44G8")

# Select columns from pelagic data to go in dat
spr_sub <- spr_sum %>% dplyr::select(ID3, abun_spr)
her_sub <- her_sum %>% dplyr::select(ID3, abun_her)

# TEST
dat %>% filter(ID == "1991.4.SOL.H20.34.49")

# Now join dat and sprat data
dat <- left_join(dat, spr_sub)
nrow(dat)

# And herring..
dat <- left_join(dat, her_sub)

# TEST
dat %>% filter(ID == "1991.4.SOL.H20.34.49")

nrow(dat)

# Replace NA abundances with 0 (But not all! Some are true NA's)
dat$abun_her[is.na(dat$abun_her)] <- 0
dat$abun_spr[is.na(dat$abun_spr)] <- 0

unique(is.na(dat$abun_spr))
unique(is.na(dat$abun_her))

# Now, if an ID3 is not in the original sprat and herring data, it should in fact be NA
# Test for a StatRec that is not in Year 1993: spr %>% filter(StatRec == "39G5") %>% distinct(Year)
# Now get a vector of all the non-NA rectangles for spr and herring (should be the same tho)
spr_non_na_rect <- spr %>% distinct(ID3)
her_non_na_rect <- her %>% distinct(ID3)

# Now replace the 0's with NAs if they are NOT in the above vectors
dat <- dat %>% mutate(abun_spr2 = ifelse(ID3 %in% spr_non_na_rect$ID3, abun_spr, NA),
                      abun_her2 = ifelse(ID3 %in% her_non_na_rect$ID3, abun_her, NA))

unique(is.na(dat$abun_spr))
unique(is.na(dat$abun_her))

# Plot to see it's correct:
p1 <- dat %>%
  filter(Year < 1995) %>% 
  #filter(! StatRec %in% c("41G0", "41G1", "41G2", "42G1", "42G2", "43G1", "43G2", "44G0", "44G1")) %>%
  ggplot(., aes(ShootLong, ShootLat, color = abun_spr)) +
  geom_point(size = 2.5, shape = 15) +
  scale_color_viridis() +
  facet_wrap(~ Year, ncol = 2) +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL

p2 <- dat %>%
  filter(Year < 1995) %>% 
  #filter(! StatRec %in% c("41G0", "41G1", "41G2", "42G1", "42G2", "43G1", "43G2", "44G0", "44G1")) %>%
  ggplot(., aes(ShootLong, ShootLat, color = abun_spr2)) +
  geom_point(size = 2.5, shape = 15) +
  scale_color_viridis() +
  facet_wrap(~ Year, ncol = 2) +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL

p1+p2

# Plot to see it's correct:
p3 <- dat %>%
  filter(Year < 1995) %>% 
  #filter(! StatRec %in% c("41G0", "41G1", "41G2", "42G1", "42G2", "43G1", "43G2", "44G0", "44G1")) %>%
  ggplot(., aes(ShootLong, ShootLat, color = abun_her)) +
  geom_point(size = 2.5, shape = 15) +
  scale_color_viridis() +
  facet_wrap(~ Year, ncol = 2) +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL

p4 <- dat %>%
  filter(Year < 1995) %>% 
  #filter(! StatRec %in% c("41G0", "41G1", "41G2", "42G1", "42G2", "43G1", "43G2", "44G0", "44G1")) %>%
  ggplot(., aes(ShootLong, ShootLat, color = abun_her2)) +
  geom_point(size = 2.5, shape = 15) +
  scale_color_viridis() +
  facet_wrap(~ Year, ncol = 2) +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL

p3+p4

# Looks alright! We have NA's now in the variable abun_her2 and spr_her2. We can now replace
# abun_her and abun_spr with those columns

dat <- dat %>% mutate(abun_her = abun_her2, abun_spr = abun_spr2) %>% dplyr::select(-abun_spr2, -abun_her2)

# Test an ID3 in spr/her and dat
# > head(unique(dat$ID3))
# [1] "39G4.1991" "40G4.1991" "43G7.1991" "44G8.1991" "44G9.1991" "44G7.1991"
spr %>% filter(ID3 == "44G7.1991")
her %>% filter(ID3 == "44G7.1991")
dat %>% filter(ID3 == "44G7.1991")

# Plot distribution of abundances
ggplot(dat, aes(abun_spr)) + geom_histogram()
ggplot(dat, aes(abun_her)) + geom_histogram()

# TEST
dat %>% filter(ID == "1991.4.SOL.H20.34.49")


#** Sub-division level =============================================================
spr_sd <- read_xlsx("data/BIAS/abundance_sub_division_1991-2019.xlsx",
                 sheet = 1) %>%
  rename("Year" = "ANNUS") %>% 
  mutate(Sub_Div = as.factor(Sub_Div)) %>% 
  mutate(Sub_Div = recode(Sub_Div, "28_2" = "28")) %>% # Change 28_2 (main area) to simply 28 (which also includes 28_1)
  mutate(Species = "Sprat",
         abun_spr_sd = `AGE0`+`AGE1`+`AGE2`+`AGE3`+`AGE4`+`AGE5`+`AGE6`+`AGE7`+`AGE8+`,
         ID_sd = paste(Sub_Div, Year, sep = "."))

her_sd <- read_xlsx("data/BIAS/abundance_sub_division_1991-2019.xlsx",
                    sheet = 2) %>%
  rename("Year" = "ANNUS") %>% 
  mutate(Sub_Div = as.factor(Sub_Div)) %>% 
  mutate(Sub_Div = recode(Sub_Div, "28_2" = "28")) %>% # Change 28_2 (main area) to simply 28 (which also includes 28_1)
  mutate(Species = "Herring",
         abun_her_sd = `AGE0`+`AGE1`+`AGE2`+`AGE3`+`AGE4`+`AGE5`+`AGE6`+`AGE7`+`AGE8+`,
         ID_sd = paste(Sub_Div, Year, sep = "."))
  
# How many unique rows per ID3?
spr_sd %>%
  group_by(ID_sd) %>% 
  mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

her_sd %>%
  group_by(ID_sd) %>% 
  mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

# Now we need to add in the sub-divisions in the main data
# Somethings to note here: there are rectangles that are in more than 1 sub-division!
border_rec <- spr %>% group_by(ID3) %>% mutate(n = n()) %>% filter(n == 2)
unique(border_rec$StatRec)

# 39G2 is assigned to SD 24, not 23 (0 abundances in SD 23 from the Rectangle data)
spr %>% filter(ID3 == "39G2.1991")

# 39G4 is assigned to SD 24, not 25 (0 abundances in SD 23 from the Rectangle data)
spr %>% filter(ID3 == "39G4.1991")

# 41G0, 41G2 and 41G1 are not in the condition data
spr %>% filter(ID3 == "41G0.1991")

dat <- dat %>% 
  mutate(Sub_Div = NA) %>% 
  mutate(Sub_Div = ifelse(StatRec %in% c("37G0", "37G1",
                                         "38G0", "38G1", 
                                         "39F9", "39G0", "39G1",
                                         "40F9", "40G0", "40G1"), "22", Sub_Div)) %>% 
  mutate(Sub_Div = ifelse(StatRec == "40G2", "23", Sub_Div)) %>% 
  mutate(Sub_Div = ifelse(StatRec %in% c("37G2", "37G3", "37G4",
                                         "38G1", "38G2", "38G3", "38G4", 
                                         "39G1", "39G2", "39G3", "39G4",
                                         "40G1"), "24", Sub_Div)) %>% 
  mutate(Sub_Div = ifelse(StatRec %in% c("40G4",
                                         "37G5", "37G6", "37G7",
                                         "38G5", "38G6", "38G7",
                                         "39G5", "39G6", "39G7",
                                         "40G5", "40G6", "40G7",
                                         "41G5", "41G6", "41G7"), "25", Sub_Div)) %>% 
  mutate(Sub_Div = ifelse(StatRec %in% c("37G8", "37G9", "37H0",
                                         "38G8", "38G9", "38H0",
                                         "39G8", "39G9", "39H0",
                                         "40G8", "40G9", "40H0",
                                         "41G8", "41G9", "41H0"), "26", Sub_Div)) %>% 
  mutate(Sub_Div = ifelse(StatRec %in% c("42G6", "42G7",
                                         "43G6", "43G7",
                                         "44G6", "44G7", "44G8"), "27", Sub_Div)) %>% 
  mutate(Sub_Div = ifelse(StatRec %in% c("42G8", "42G9", "42H0", "42H1", "42H2",
                                         "43G8", "43G9", "43H0", "43H1", "43H2",
                                         "44G8", "44G9", "44H0", "44H1", "44H2"), "28", Sub_Div)) %>% 
  mutate(Sub_Div = factor(Sub_Div))

# Check if any NAs (if they weren't assigned an SD)
unique(is.na(dat$Sub_Div))

unique(spr_sd$Sub_Div)
unique(dat$Sub_Div)

# Now join dat and sprat data
# First add the new ID in dat
dat <- dat %>% mutate(ID_sd = paste(Sub_Div, Year, sep = "."))

# Select only key variables
spr_sd_sub <- spr_sd %>% dplyr::select(ID_sd, abun_spr_sd)
her_sd_sub <- her_sd %>% dplyr::select(ID_sd, abun_her_sd)

nrow(dat)
dat <- left_join(dat, spr_sd_sub)
dat <- left_join(dat, her_sd_sub)

nrow(dat)

# TEST
dat %>% filter(ID == "1991.4.SOL.H20.34.49")


#** Test if same as raw data =======================================================
# Sprat
test_sprat_raw <- spr_sd %>%
  dplyr::select(Year, Sub_Div, abun_spr_sd, ID_sd) %>%
  data.frame() %>%
  mutate(source = "raw")

test_sprat_dat <- dat %>%
  dplyr::select(Year, abun_spr_sd, Sub_Div, ID_sd) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(source = "dat")

test_sprat <- bind_rows(test_sprat_raw, test_sprat_dat)

test_sprat %>% 
  filter(ID_sd %in% test_sprat_dat$ID_sd) %>% 
  ggplot(., aes(Year, abun_spr_sd, shape = source, color = source)) +
  geom_point(alpha = 0.4, size = 4) + 
  facet_wrap(~ Sub_Div, scales = "free")

# Herring
test_herring_raw <- her_sd %>%
  dplyr::select(Year, Sub_Div, abun_her_sd, ID_sd) %>%
  data.frame() %>%
  mutate(source = "raw")

test_herring_dat <- dat %>%
  dplyr::select(Year, abun_her_sd, Sub_Div, ID_sd) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(source = "dat")

test_herring <- bind_rows(test_herring_raw, test_herring_dat)

test_herring %>% 
  filter(ID_sd %in% test_herring_dat$ID_sd) %>% 
  ggplot(., aes(Year, abun_her_sd, shape = source, color = source)) +
  geom_point(alpha = 0.4, size = 4) + 
  facet_wrap(~ Sub_Div, scales = "free")

# Yes they are the same.


#** Regarding sizes ================================================================
# How to select which ages to use as predictor variables?
# From Niiranen et al, it seems sprat in cod stomachs range between 50 and 150 mm and
# herring range between essentially 0 to 300 mm. Which ages does that correspond to? For
# that we need VBGE parameters. Following Lindmark et al (in prep), in which the VBGE 
# curves are plotted for these species for weight, and weight-length relationships are 
# estimated, we see the following:

# Sprat: a 5 and 15 cm sprat weighs 1 and 32 g respectively. 
# 0.0078*5^3.07=1.091271
# 0.0078*15^3.07=31.8196
# VBGE curves show that sprat at age 8 are on average <20g, hence we could probably include all ages

# Herring: a 1 and 30 cm herring weighs <1 and 182 g respectively. 
# 0.0042*1^3.14=0.0042
# 0.0042*30^3.14=182.5619
# VBGE curves show that sprat at age 8 are on average <100g, hence we could probably include all ages

# Conclusion: I will not filter any further as all could potentially be eaten by cod (given that the cod are large enough to eat herring (> 20 cm approx, which is majority of data)


# # G. READ AND JOIN ENVIRONMENTAL DATA ============================================
# ** Depth =========================================================================
west <- raster("data/depth_geo_tif/D5_2018_rgb-1.tif")
plot(west)

east <- raster("data/depth_geo_tif/D6_2018_rgb-1.tif")
plot(east)

dep_rast <- raster::merge(west, east)

dat$depth_rast <- extract(dep_rast, dat[, 32:31])

# Convert to depth (instead of elevation)
ggplot(dat, aes(depth_rast)) + geom_histogram()
dat$depth_rast <- (dat$depth_rast - max(dat$depth_rast)) *-1
ggplot(dat, aes(depth_rast)) + geom_histogram()

# Compare to built in depth data
ggplot(dat, aes(Depth, depth_rast)) + 
  geom_point() +
  geom_abline(color = "red")

dat %>% 
  filter(depth_rast > 0) %>% 
  ggplot(., aes(ShootLong, ShootLat, color = depth_rast)) + 
  scale_color_viridis() +
  geom_sf(data = world, inherit.aes = F, size = 0.2, fill = NA) +
  geom_point(size = 1) + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  NULL


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
d_sub_oxy <- dat %>% filter(Year %in% names(dlist)) %>% droplevels()

# Create data holding object
data_list <- list()

# ... And for the oxygen raster
raster_list <- list()

# Create factor year for indexing the list in the loop
d_sub_oxy$Year_f <- as.factor(d_sub_oxy$Year)

# Loop through each year and extract raster values for the condition data points
for(i in unique(d_sub_oxy$Year_f)) {

  # Subset a year
  oxy_slice <- dlist[[i]]

  # Create raster for that year (i)
  r <- raster(t(oxy_slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

  # Flip...
  r <- flip(r, direction = 'y')

  plot(r, main = i)

  # Filter the same year (i) in the condition data and select only coordinates
  d_slice <- d_sub_oxy %>% filter(Year_f == i) %>% dplyr::select(ShootLong, ShootLat)

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
    geom_point(data = d_slice, aes(x = ShootLong, y = ShootLat, fill = oxy),
               color = "black", size = 5, shape = 21) +
    theme_bw() +
    geom_sf(data = world, inherit.aes = F, size = 0.2) +
    coord_sf(xlim = c(min(dat$ShootLong), max(dat$ShootLong)),
             ylim = c(min(dat$ShootLat), max(dat$ShootLat))) +
    scale_colour_gradientn(colours = rev(terrain.colors(10)),
                           limits = c(-200, 400)) +
    scale_fill_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(-200, 400)) +
    NULL

    ggsave(paste("figures/supp/oxygen_rasters/", i,".png", sep = ""),
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
dat$id_oxy <- paste(dat$Year, dat$ShootLong, dat$ShootLat, sep = "_")
big_dat_oxy$id_oxy <- paste(big_dat_oxy$year, big_dat_oxy$ShootLong, big_dat_oxy$ShootLat, sep = "_")

# Which id's are not in the condition data (dat)?
ids <- dat$id_oxy[!dat$id_oxy %in% c(big_dat_oxy$id_oxy)]

unique(ids)

# Select only the columns we want to merge
big_dat_sub_oxy <- big_dat_oxy %>% dplyr::select(id_oxy, oxy)

# Remove duplicate ID (one oxy value per id)
big_dat_sub_oxy2 <- big_dat_sub_oxy %>% distinct(id_oxy, .keep_all = TRUE)

# Join the data with raster-derived oxygen with the full condition data
dat <- left_join(dat, big_dat_sub_oxy2, by = "id_oxy")

# Now the unit of oxygen is mmol/m3. I want it to be ml/L. The original model is in unit ml/L
# and it's been converted by the data host. Since it was converted without accounting for
# pressure or temperature, I can simply use the following conversion factor:
# 1 ml/l = 103/22.391 = 44.661 μmol/l -> 1 ml/l = 0.044661 mmol/l = 44.661 mmol/m^3 -> 0.0223909 ml/l = 1mmol/m^3
# https://ocean.ices.dk/tools/unitconversion.aspx

dat$oxy <- dat$oxy * 0.0223909


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
d_sub_temp <- dat %>% filter(Year %in% names(dlist)) %>% droplevels()

# Create data holding object
data_list <- list()

# ... And for the temperature raster
raster_list <- list()

# Create factor year for indexing the list in the loop
d_sub_temp$Year_f <- as.factor(d_sub_temp$Year)

# Loop through each year and extract raster values for the condition data points
for(i in unique(d_sub_temp$Year_f)) {
  
  # Subset a year
  temp_slice <- dlist[[i]]
  
  # Create raster for that year (i)
  r <- raster(t(temp_slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Flip...
  r <- flip(r, direction = 'y')
  
  plot(r, main = i)
  
  # Filter the same year (i) in the condition data and select only coordinates
  d_slice <- d_sub_temp %>% filter(Year_f == i) %>% dplyr::select(ShootLong, ShootLat)
  
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
    geom_point(data = d_slice, aes(x = ShootLong, y = ShootLat, fill = temp),
               color = "black", size = 5, shape = 21) +
    theme_bw() +
    geom_sf(data = world, inherit.aes = F, size = 0.2) +
    coord_sf(xlim = c(min(dat$ShootLong), max(dat$ShootLong)),
             ylim = c(min(dat$ShootLat), max(dat$ShootLat))) +
    scale_colour_gradientn(colours = rev(terrain.colors(10)),
                           limits = c(2, 17)) +
    scale_fill_gradientn(colours = rev(terrain.colors(10)),
                         limits = c(2, 17)) +
    NULL
  
  ggsave(paste("figures/supp/temp_rasters/", i,".png", sep = ""),
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
dat$id_temp <- paste(dat$Year, dat$ShootLong, dat$ShootLat, sep = "_")
big_dat_temp$id_temp <- paste(big_dat_temp$year, big_dat_temp$ShootLong, big_dat_temp$ShootLat, sep = "_")

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

# sort(unique(dat$Year))

# H. PREPARE DATA FOR ANALYSIS =====================================================
d <- dat %>%
  rename("weight_g" = "IndWgt",
         "lat" = "ShootLat",
         "lon" = "ShootLong",
         "year" = "Year",
         "sex" = "Sex",
         "depth" = "depth_rast") %>% 
  mutate(Fulton_K = weight_g/(0.01*length_cm^3), # not cod-specific
         sex = ifelse(sex == -9, "U", sex),
         sex = as.factor(sex),
         year_f = as.factor(year)) %>% 
  filter(Fulton_K < 3 & Fulton_K > 0.15) %>%  # Visual exploration, larger values likely data entry errors
  drop_na(oxy) %>% 
  drop_na(temp) %>% 
  filter(depth > 0) %>% 
  dplyr::select(-id_oxy, -id_temp) %>% 
  ungroup()

# filter(d, Fulton_K < 0.5) %>% dplyr::select(Fulton_K, length_cm, weight_g) %>% arrange(Fulton_K) %>% as.data.frame()
# filter(d, Fulton_K > 2.5) %>% dplyr::select(Fulton_K, length_cm, weight_g) %>% arrange(Fulton_K) %>% as.data.frame()

# Calculate average cod & flounder densities and oxygen and temperatures, by ICES rectangle
d <- d %>% 
  group_by(StatRec, year) %>% 
  mutate(oxy_rec = mean(oxy),
         temp_rec = mean(temp),
         cpue_cod_rec = mean(cpue_cod),
         cpue_fle_rec = mean(cpue_fle)) %>% 
  ungroup()

# Plot spatial distribution of samples by rectangle
# d %>%
#   ggplot(., aes(y = lat, x = lon, color = factor(StatRec))) +
#   geom_point(size = 1) +
#   theme_bw() +
#   geom_sf(data = world, inherit.aes = F, size = 0.2) +
#   coord_sf(xlim = c(8, 25), ylim = c(54, 60)) +
#   NULL

# Finally, select only the main variables to make the data file smaller (for GitHub)
d_analysis <- d %>% dplyr::select(year, depth, lat, lon, length_cm, weight_g, Fulton_K,
                                  cpue_cod, cpue_cod_rec, cpue_fle, cpue_fle_rec,
                                  oxy, oxy_rec, temp, temp_rec, 
                                  abun_spr, abun_spr_sd, abun_her, abun_her_sd)

# sort(unique(d_analysis$year))

write.csv(d_analysis, file = "data/for_analysis/mdat_cond.csv", row.names = FALSE)

