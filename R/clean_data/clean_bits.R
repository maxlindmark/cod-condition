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
#   Lastly, we join in the Oxygen data. This is still in progress as I need to decide
#   on the least bad way to do it... 
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
# rm(list = ls())

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
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# Print package versions
# sessionInfo()
# other attached packages:
# [1] sf_0.9-5           raster_3.3-13      rgdal_1.5-12       sp_1.4-2           mapdata_2.3.0      maps_3.3.0         ggsidekick_0.0.2  
# [8] icesDatras_1.3-0   janitor_2.0.1      patchwork_1.0.1    RColorBrewer_1.1-2 viridis_0.5.1      viridisLite_0.3.0  RCurl_1.98-1.2    
# [15] tidylog_1.0.2      readxl_1.3.1       forcats_0.5.0      stringr_1.4.0      dplyr_1.0.0        purrr_0.3.4        readr_1.3.1       
# [22] tidyr_1.1.0        tibble_3.0.3       ggplot2_3.3.2      tidyverse_1.3.0 


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

# Lastly we can remove hauls from outside the study area and select only quarter 4,
# and remove non-valid hauls
dat <- dat %>% 
  filter(ShootLat < 58 & ShootLong > 12.5) %>% 
  mutate(kattegatt = ifelse(ShootLat > 55.5 & ShootLong < 14, "Y", "N")) %>% 
  filter(kattegatt == "N",
         Quarter == 4,
         HaulVal == "V") %>% 
  dplyr::select(-kattegatt)


# E. READ AND JOIN THE COD AND FLOUNDER COVARIATES =================================
cov_dat <- read.csv("data/DATRAS_cpue_length_haul/CPUE per length per haul per hour_2020-09-25 16_15_36.csv")

# Remove hauls from outside the study area and select only quarter 4
cov_dat <- cov_dat %>% 
  filter(ShootLat < 58 & ShootLong > 12.5) %>% 
  mutate(kattegatt = ifelse(ShootLat > 55.5 & ShootLong < 14, "Y", "N")) %>% 
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

# Now calculate the mean CPUE per hauls and size group per species. For cod we use 30
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


# F. READ AND JOIN PELAGIC COVARIATES ==============================================
spr <- read_xlsx("data/from_mich/Abundances_rectangles_1984-2019_corrected.xlsx",
                 sheet = 1) %>%
  filter(Year > 1990) %>% 
  rename("StatRec" = "Rec") %>%
  mutate(StatRec = as.factor(StatRec),
         Species = "Sprat",
         abun_spr = `Age 0`+`Age 1`+`Age 2`+`Age 3`+`Age 4`+`Age 5`+`Age 6`+`Age 7`+`Age 8+`+`1+`,
         ID3 = paste(StatRec, Year, sep = ".")) # Make new ID)
  
head(spr)

her <- read_xlsx("data/from_mich/Abundances_rectangles_1984-2019_corrected.xlsx",
                 sheet = 2) %>%
  as.data.frame() %>%
  filter(Year > 1990) %>% 
  rename("StatRec" = "Rect2") %>% # This is not called Rec in the data for some reason
  mutate(StatRec = as.factor(StatRec),
         Species = "Herring",
         abun_her = `Age 0`+`Age 1`+`Age 2`+`Age 3`+`Age 4`+`Age 5`+`Age 6`+`Age 7`+`Age 8+`+`1+`,
         ID3 = paste(StatRec, Year, sep = ".")) # Make new ID

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

# Seems to be due to rectangles somehow being in different sub divisions. I need to
# group by ID3 and summarize
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

# Join pelagic covariates
# Make StatRec a factor in the main data
dat <- dat %>% mutate(StatRec = as.factor(StatRec))
unique(is.na(dat$StatRec))

# Create ID3 to match pelagics data
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
spr_sub <- spr %>% dplyr::select(ID3, abun_spr)
her_sub <- her %>% dplyr::select(ID3, abun_her)

# Now join dat and sprat data
dat <- left_join(dat, spr_sub)

# And herring..
dat <- left_join(dat, her_sub)

# REPLACE NA ABUNDANCES WITH 0
dat$abun_her[is.na(dat$abun_her)] <- 0
dat$abun_spr[is.na(dat$abun_spr)] <- 0

unique(is.na(dat$abun_spr))

# Test an ID3 in spr/her and dat
# > head(unique(dat$ID3))
# [1] "39G4.1991" "40G4.1991" "43G7.1991" "44G8.1991" "44G9.1991" "44G7.1991"
spr %>% filter(ID3 == "44G7.1991")
her %>% filter(ID3 == "44G7.1991")
dat %>% filter(ID3 == "44G7.1991")

# Plot distribution of abundances
ggplot(dat, aes(abun_spr)) + geom_histogram()
ggplot(dat, aes(abun_her)) + geom_histogram()

# How to select which ages to use as predictor variables?
# From Niiranen et al, it seem sprat in cod stomachs range between 50 and 150 mm and
# herring range between essentially 0 to 300 mm. Which ages does that correspond to? For
# that we need VBGE parameters. Following Lindmark et al (in prep), in which the VBGE 
# curves are plotted for these species for weight, and weight-length relationships are 
# estimated, we see the following:

# Sprat: a 5 and 15 cm sprat weighs 1 and 30 g respectively. 
0.0078*5^3.07
0.0078*15^3.07
# This covers all weights and ages in the sprat data. Moreover, in Niiranen et al it
# doesn't seem to be much variation between size classes of cod with respect to this.

# Herring: a 1 and 30 cm herring weighs >1 and 182 g respectively. 
0.0042*1^3.14
0.0042*30^3.14
# This covers all weights and ages in the herring data. Moreover, in Niiranen et al it
# doesn't seem to be much variation between size classes of cod with respect to this.

# Conclusion: I will not filter any further


# G. READ AND JOIN OCEANOGRAPHIC DATA =====================================================
# Read the standard daata (select "Oxygen" as parameter)
ocean_91_98 <- read.csv("data/OCEANOGRAPHY/OXYGEN/1991-1998.csv")
ocean_99_05 <- read.csv("data/OCEANOGRAPHY/OXYGEN/1999-2005.csv")
ocean_06_12 <- read.csv("data/OCEANOGRAPHY/OXYGEN/2006-2012.csv")
ocean_13_19 <- read.csv("data/OCEANOGRAPHY/OXYGEN/2013-2019.csv")

ocean <- rbind(ocean_91_98, ocean_99_05, ocean_06_12, ocean_13_19)

oxy <- ocean

# Split month column to year and month
oxy <- oxy %>% 
  mutate(yyyy.mm.ddThh.mm_temp = yyyy.mm.ddThh.mm) %>% 
  separate(yyyy.mm.ddThh.mm_temp, c("year", "month", "day_time"), sep = "([-])") %>% 
  filter(!DOXY..ml.l. %in% c("", "<0.00", "<0.01", "<0.02", "<0.03", "<0.10")) %>% # remove these characters (33 rows)
  mutate(DOXY..ml.l._num = as.numeric(DOXY..ml.l.),
         month = as.numeric(month),
         year = as.numeric(year),
         quarter = ifelse(month < 4, 1, 2),
         quarter = ifelse(month > 6, 3, quarter),
         quarter = ifelse(month > 9, 4, quarter)) %>% 
  rename("lat" = "Latitude..degrees_north.",
         "lon" = "Longitude..degrees_east.",
         "DOXY" = "DOXY..ml.l._num") %>% 
  filter(lat < 58 & lon > 12.5) %>% 
  mutate(kattegatt = ifelse(lat > 55.5 & lon < 14, "Y", "N")) %>% 
  filter(kattegatt == "N") %>% 
  dplyr::select(-kattegatt) %>% 
  mutate(ID4 = paste(year, quarter, lat, lon, sep = "_")) %>% # Create unique ID (station)
  arrange(ID4) 

# Now we want to filter the rows within each ID with maximum pressure, assuming that 
# is the bottom depth
oxy <- oxy %>%
  group_by(ID4) %>% 
  filter(PRES..db. == max(PRES..db.)) %>% 
  ungroup()

# Check if oxygen varies a lot between quarters
ggplot(oxy, aes(year, DOXY, color = factor(quarter))) +
  geom_point() +
  stat_smooth()

# How many datapoints do I have per unique combination of lat-lon by year and quarter?
oxy %>% 
  group_by(ID4) %>% 
  mutate(n = n()) %>% 
  ggplot(., aes(n)) + geom_histogram()

oxy %>% 
  group_by(ID4) %>% 
  mutate(n = n()) %>%
  filter(n > 1) %>% 
  arrange(ID4) %>% 
  as.data.frame() %>% 
  head(20)

# Since there are multiple samples per year, quarter and lon-lat, I will summarize by ID4
# (i.e. Year, Quarter, Lat and Lon) - i.e. excluding monthly differences
oxy <- oxy %>% 
  group_by(ID4) %>% 
  summarise(mean_DOXY = mean(DOXY)) %>% 
  mutate(ID4_temp = ID4) %>% 
  separate(ID4_temp, c("Year", "Quarter", "lat", "lon"), sep = "([_])") %>% 
  mutate_at(c("Year", "Quarter", "lat", "lon"), as.numeric)

# How many datapoints do I have per unique combination of lat-lon (by year and quarter)?
oxy %>% 
  group_by(ID4) %>% 
  mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

# Now check the spatial resolution of samples
world <- ne_countries(scale = "medium", returnclass = "sf")

oxy %>%
  ggplot(., aes(lon, lat, color = source)) + 
  geom_point(size = 0.5, alpha = 0.8) + 
  facet_wrap(~ Year, ncol = 6) +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(12.5, 21.5), ylim = c(54, 58))


# I will next join in the closet CTD measurement with the haul, and then plot the distances.
# Then I will either remove data where the distance is too long (see the MSC thesis),
# or see if I can use data from another quarter

# Create ID4 in dat
dat <- dat %>% mutate(ID4 = paste(Year, ShootLat, ShootLong, sep = "_"))

# Now join datasets!
# https://stackoverflow.com/questions/55752064/r-finding-closest-coordinates-between-two-large-data-sets
dat_cond <- dat %>%
  rename("lat" = "ShootLat",
         "lon" = "ShootLong") %>% 
  dplyr::select(Year, lat, lon, ID4) %>%
  as.data.frame()

dat_oxy <- oxy %>%
  dplyr::select(Year, lat, lon, mean_DOXY, Quarter) %>%
  as.data.frame() %>% 
  filter(Quarter == 4)

# Now do the loop again and plot arrows for distances for the selected subset
# General area boundaries
xmin <- min(dat$ShootLong)
xmax <- max(dat$ShootLong)
ymin <- min(dat$ShootLat)
ymax <- max(dat$ShootLat)

data_list <- list()

for (i in 1:length(unique(dat_cond$Year))) {
  
  temp_cond <- subset(dat_cond, Year == i + 1990) # First year is 1991
  temp_oxy <- subset(dat_oxy, Year == i + 1990) # First year is 1991
  
  dd <- pointDistance(temp_cond[, 3:2], temp_oxy[, 3:2], lonlat = TRUE, allpairs = FALSE)
  ii <- apply(dd, 1, which.min)
  
  temp_cond$mean_DOXY = temp_oxy$mean_DOXY[ii]
  
  temp_cond$distance = dd[cbind(1:nrow(dd), ii)]
  
  data_list[[i]] <- temp_cond
  
  # Illustrate
  mypath <- file.path("figures/supp", paste(unique(temp_cond$Year), ".jpg", sep = ""))
  
  png(file = mypath)
  
  plot(temp_cond[, 3:2], col = "blue", pch = 20, main = i + 1990,
       xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  points(temp_oxy[, 3:2], col = "red", pch = 20)
  
  for (j in 1:(nrow(temp_cond))) {
    k <- temp_oxy$mean_DOXY == temp_cond$mean_DOXY[j]
    
    x0 <- temp_cond[j, 3]
    y0 <- temp_cond[j, 2]
    
    # Get the index of the vector that is closest by taking the minimum absolute difference
    # Else I get random long errors if the oxygen level is the same (length > 1 in the arrow index)
    
    x1 <- temp_oxy[k, 3][which.min(abs(x0 - temp_oxy[k, 3]))]
    y1 <- temp_oxy[k, 2][which.min(abs(y0 - temp_oxy[k, 2]))]
    
    arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = .1)
  }
  
  dev.off()
  
}
# The warnings are from the arrows being too short and hence not plotted properly

data_oxy_cond <- dplyr::bind_rows(data_list)

# Now join in the oxygen data...
data_oxy_cond <- data_oxy_cond %>%
  dplyr::select(mean_DOXY, ID4, distance) %>%
  distinct(ID4, .keep_all = TRUE)

# No NA here...
unique(is.na(data_oxy_cond$mean_DOXY..ml.l._num))

# Are there any ID4s in data_oxy_cond that are not in dat?
data_oxy_cond$ID4[!data_oxy_cond$ID4 %in% dat$ID4]
# Nope

# Are there any ID4s in dat that are not in data_oxy_cond?
dat$ID4[!dat$ID4 %in% data_oxy_cond$ID4]
# Nope

dat <- left_join(dat, data_oxy_cond)

# Plot distribution of distances
ggplot(dat, aes(distance)) +
  geom_histogram()

# Plot full data to compare which samples are removed if I filter on distance
ggplot(dat, aes(distance)) +
  geom_histogram()

p1 <- dat %>%
  ggplot(., aes(ShootLong, ShootLat, color = distance)) + 
  geom_point(size = 0.6, alpha = 0.8) + 
  facet_wrap(~ Year, ncol = 8) +
  scale_color_viridis() +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(12.5, 21.5), ylim = c(54, 58)) + 
  ggtitle("All data")

# Looks like it's mostly the coastal data are low-intensity data areas that have large
# distances
p2 <- dat %>%
  filter(distance > 10000) %>% 
  ggplot(., aes(ShootLong, ShootLat, color = distance)) + 
  geom_point(size = 0.6, alpha = 0.8) + 
  facet_wrap(~ Year, ncol = 8) +
  scale_color_viridis() +
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(12.5, 21.5), ylim = c(54, 58)) +
  ggtitle("Distance to nearest CTD > 10 km")

p1 / p2 


# OK, stopping here for a minute. Will ask SMHI for advice on oxygen data, and maybe
# acquire model outputs.


# H. PREPARE DATA FOR ANALYSIS =====================================================
d <- dat %>%
  rename("weight_g" = "IndWgt",
         "lat" = "ShootLat",
         "lon" = "ShootLong",
         "year" = "Year",
         "sex" = "Sex") %>% 
  mutate(ln_weight_g = log(weight_g),
         ln_length_cm = log(length_cm),
         Fulton_K = weight_g/(0.01*length_cm^3), # cod-specific, from FishBase
         sex = ifelse(sex == -9, "U", sex),
         sex = as.factor(sex),
         year_f = as.factor(year)) %>% 
  dplyr::select(year, year_f, lat, lon, sex, length_cm, weight_g, ln_length_cm, ln_weight_g,
                Quarter, Fulton_K,
                cpue_cod_above_30cm, cpue_cod_below_30cm, cpue_cod, 
                cpue_fle_above_20cm, cpue_fle_below_20cm, cpue_fle,
                abun_spr, abun_her,
                mean_DOXY, distance) %>% 
  mutate(cpue_cod_above_30cm_st = cpue_cod_above_30cm,
         cpue_cod_below_30cm_st = cpue_cod_below_30cm,
         cpue_cod_st = cpue_cod,
         cpue_fle_above_20cm_st = cpue_fle_above_20cm,
         cpue_fle_below_20cm_st = cpue_fle_below_20cm,
         cpue_fle_st = cpue_fle,
         abun_spr_st = abun_spr,
         abun_her_st = abun_her,
         mean_DOXY_st = mean_DOXY) %>% 
  mutate_at(c("cpue_cod_above_30cm_st", "cpue_cod_below_30cm_st", "cpue_cod_st",
              "cpue_fle_above_20cm_st", "cpue_fle_below_20cm_st", "cpue_fle_st",
              "abun_spr_st", "abun_her_st",
              "mean_DOXY_st"),
            ~(scale(.) %>% as.vector)) %>% 
  filter(Fulton_K < 3 & Fulton_K > 0.15) %>%  # Visual exploration, larger values likely data entry errors
  ungroup()

# filter(d, Fulton_K < 0.5) %>% dplyr::select(Fulton_K, length_cm, weight_g) %>% arrange(Fulton_K) %>% as.data.frame()
# filter(d, Fulton_K > 2.5) %>% dplyr::select(Fulton_K, length_cm, weight_g) %>% arrange(Fulton_K) %>% as.data.frame()

write.csv(d, file = "data/clean_for_analysis/mdat_cond.csv", row.names = FALSE)

str(d)

# I. EXPLORE DATA ==================================================================

# - Do the exploratory plots like this:
#   - https://dpananos.github.io/posts/2018/04/blog-post-8/

# Plot condition over time
ggplot(d, aes(year, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition over time") 

# Plot condition vs oxygen
ggplot(d, aes(mean_DOXY_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition vs oxygen") 

# Plot condition vs oxygen with filter
d %>% 
  filter(distance < 5000) %>% 
  ggplot(., aes(mean_DOXY_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition vs oxygen") 

# Plot condition vs oxygen with filter by year
d %>% 
  filter(distance < 5000) %>% 
  ggplot(., aes(mean_DOXY_st, Fulton_K, fill = factor(year), color = factor(year))) +
  geom_point(alpha = 0.5, size = 0.5) + 
  stat_smooth(method = "lm") + 
  ggtitle("condition vs oxygen") 

# Check if slope changes by year
# https://community.rstudio.com/t/extract-slopes-by-group-broom-dplyr/2751/7
d %>% 
  filter(distance < 5000) %>% 
  split(.$year) %>% 
  purrr::map(~lm(Fulton_K ~ mean_DOXY_st, data = .x)) %>% 
  purrr::map_df(broom::tidy, .id = 'year') %>%
  filter(term == 'mean_DOXY_st') %>% 
  mutate(ci_low = estimate + -1.96*std.error,
         ci_high = estimate + 1.96*std.error) %>% 
  mutate(year_n = as.numeric(year)) %>%
  ggplot(., aes(year_n, estimate)) +
  stat_smooth(method = "lm", color = "black") + 
  geom_point() + 
#  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, x = year_n)) + 
  NULL

# Plot condition vs herring
ggplot(d, aes(abun_her_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition vs herring") 

# Plot condition vs sprat
ggplot(d, aes(abun_spr_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition vs sprat")

# Plot condition vs cod and flounder
p1 <- ggplot(d, aes(cpue_cod_above_30cm_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() + 
  ggtitle("cod_above_30cm") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

p2 <- ggplot(d, aes(cpue_cod_below_30cm_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cod_below_30cm") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

p3 <- ggplot(d, aes(cpue_cod_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_cod_st") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

p4 <- ggplot(d, aes(cpue_fle_above_20cm_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_above_20cm_st") + 
  #coord_cartesian(xlim = c(0, 5)) +
  NULL

p5 <- ggplot(d, aes(cpue_fle_below_20cm_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_below_20cm_st") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

p6 <- ggplot(d, aes(cpue_fle_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_st") + 
  #coord_cartesian(xlim = c(0.001, 5)) + 
  NULL

(p1 + p2 + p3)/(p4 + p5 + p6)

# Do this for all classes... and maybe years?
ggplot(d, aes((cpue_fle_st+cpue_cod_st), Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  xlim(-2, 12) +
  NULL

ggplot(d, aes((cpue_fle_below_20cm_st+cpue_cod_below_30cm_st), Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  NULL
