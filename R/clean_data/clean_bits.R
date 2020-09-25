#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# - Code to clean and merge BITS HH, and CA data directly from DATRAS
#   We want to end up with a dataset of length-at-weight of cod, with haul information 
#   and columns indicating the catch of cod and flounder at the haul.
# 
# - The approach is as follows:
#   1. Read the CA data which has weight-length information. Clean and standardize
#   2. Read in the HH data file, which has information about the haul (coordinates etc)
#   3. Select the IDs that are i both data sets, merge haul and condition data
#   3. Read in the catch covariates. Use only hauls that are in the condition data
#      If the haul is not in the catch data, give a 0 catch
#   4. Add in the remaining covariates (oxygen, pelagics etc). Same here, add 0 if NA
#      after the data join.
# 
#   HH: Record with detailed haul information;
#   HL: Length frequency data; 
#   CA: Sex-maturity-age–length keys (SMALK's) for ICES subdivision.
# 
# A. Load libraries
# 
# B. Get haul information into catch data
# 
# C. Get haul & catch data in to length-weight data 
# 
# D. Prepare data for analysis
# 
# E. Add additional covariates
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
bits_hh$ID <- paste(bits_hh$Year, bits_hh$Quarter, bits_hh$Ship,
                    bits_hh$Gear, bits_hh$HaulNo, bits_hh$Depth, sep = ".")

# Check that per ID, there's only one row
bits_hh %>% 
  group_by(ID) %>% 
  mutate(n = n()) %>% 
  ungroup() %>%
  ggplot(., aes(factor(n))) + geom_bar()

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
# bits_ca$ID <- paste(bits_ca$Year, bits_ca$Quarter, bits_ca$Country, bits_ca$Ship,
#                     bits_ca$Gear, bits_ca$HaulNo, bits_ca$StNo, sep = ".")

bits_ca$ID <- paste(bits_ca$Year, bits_ca$Quarter, bits_ca$Country, bits_ca$Ship,
                    bits_ca$Gear, bits_ca$HaulNo, bits_ca$StNo, sep = ".")

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
  mutate(Length_cm = ifelse(LngtCode == ".", 
                            LngtClass/10,
                            LngtClass)) %>% # Standardize length ((https://vocab.ices.dk/?ref=18))
  as.data.frame()
  
ggplot(bits_ca, aes(Length_cm, fill = LngtCode)) + geom_histogram()


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

# Lastly we can remove hauls from outside the study area and select only quarter 4
dat <- dat %>% 
  filter(ShootLat < 58 & ShootLong > 12.5) %>% 
  mutate(kattegatt = ifelse(ShootLat > 55.5 & ShootLong < 14, "Y", "N")) %>% 
  filter(kattegatt == "N") %>% 
  filter(Quarter == 4) %>% 
  dplyr::select(-kattegatt)


# E. READ THE CPUE COVARIATES ======================================================
cov_dat <- read.csv("data/DATRAS_cpue_length_haul/CPUE per length per haul per hour_2020-09-25 16_15_36.csv")

# Create ID column
cov_dat$ID <- paste(cov_dat$Year, cov_dat$Quarter, cov_dat$Country, cov_dat$Ship,
                    cov_dat$Gear, cov_dat$HaulNo, cov_dat$StNo, sep = ".")

head(cov_dat)

# Filter ID's from the covariate data that are also found in the condition data
condition_IDs <- unique(dat$ID)


# Note that this data has only a single length standard. Convert this to cm, as in the
# condition data
ggplot(cov_dat, aes(LngtClass, fill = Species)) + geom_density(alpha = 0.5)

cov_dat <- cov_dat %>% mutate(Length_cm = LngtClass/10)

ggplot(cov_dat, aes(Length_cm, fill = Species)) + geom_density(alpha = 0.5)

# Remove the individuals with size 0
cov_dat <- cov_dat %>% filter(LngtClass > 0)

# Check the distribution of CPUEs
ggplot(cov_dat, aes(CPUE_number_per_hour)) +
  geom_histogram(alpha = 0.5) + 
  facet_wrap(~ Species) + 
  scale_x_log10()

# Now calculate the mean CPUE per hauls and size group per species. For cod we use 30
# cm and for flounder 20 cm
cod_above_30cm <- cov_dat %>% 
  filter(Species == "Gadus morhua") %>% 
  filter(Length_cm >= 30) %>% 
  group_by(ID) %>% 
  summarise(cpue = sum(CPUE_number_per_hour)) %>% 
  ungroup()

cod_below_30cm <- cov_dat %>% 
  filter(Species == "Gadus morhua") %>% 
  filter(Length_cm < 30) %>% 
  group_by(ID) %>% 
  summarise(cpue = sum(CPUE_number_per_hour)) %>% 
  ungroup()

fle_above_20cm <- cov_dat %>% 
  filter(Species == "Platichthys flesus") %>% 
  filter(Length_cm >= 20) %>% 
  group_by(ID) %>% 
  summarise(cpue = sum(CPUE_number_per_hour)) %>% 
  ungroup()

fle_below_20cm <- cov_dat %>% 
  filter(Species == "Platichthys flesus") %>% 
  filter(Length_cm < 20) %>% 
  group_by(ID) %>% 
  summarise(cpue = sum(CPUE_number_per_hour)) %>% 
  ungroup()

# Test it worked, first by plotting n rows per ID
cod_above_30cm %>% group_by(ID) %>% mutate(n = n()) %>% 
  ggplot(., aes(factor(n))) + geom_bar()

# Next by calculating an example
cod_above_30cm %>% filter(ID == "2002.4..DAN2.TVL.43.")
sum(filter(cov_dat, ID == "2002.4..DAN2.TVL.43." &
             Species == "Gadus morhua" & 
             Length_cm >= 30)$CPUE_number_per_hour)


# F. MERGE CATCH COVARIATES WITH CONDITION DATA ====================================
cod_above_30cm
cod_below_30cm
fle_above_20cm
fle_below_20cm

# Left join dat and cpue data. 

test_id <- head(bits_hh$ID[!bits_hh$ID %in% bits_hl_fle$ID], 1)


test <- left_join(dat, cod_above_30cm) 

head(test)

unique(is.na(test$IndWgt))



# Here I need to save the NAs if they are in the condition data because that means a
# zero catch










# X. PREPARE DATA FOR ANALYSIS =====================================================


# Plot stacked bars of sample size
dat %>% 
  group_by(year, Quarter) %>% 
  summarize(n_ind = n()) %>% 
  ungroup() %>% 
  ggplot(., aes(year, n_ind, fill = factor(Quarter))) +
  geom_bar(stat = "identity")

# Plot sample size over space and color by mean size
# Quarter 1
dat %>%
  filter(Quarter == 1) %>% 
  mutate(pos = paste(lat, lon, sep = "_")) %>% 
  dplyr::group_by(year, pos) %>% 
  mutate(n = n(),
         mean_size = mean(LngtClass)) %>%
  dplyr::ungroup() %>% 
  ggplot(aes(x = lon, y = lat, size = n, color = mean_size)) +
  geom_point(alpha = 0.3) +
  scale_size(range = c(0.01, 3)) +
  facet_wrap(~ year) + 
  scale_color_viridis(option = "magma") +
  ggtitle("Quarter 1") +
  NULL

# Quarter 4
dat %>%
  filter(Quarter == 4) %>% 
  mutate(pos = paste(lat, lon, sep = "_")) %>% 
  dplyr::group_by(year, pos) %>% 
  mutate(n = n(),
         mean_size = mean(LngtClass)) %>%
  dplyr::ungroup() %>% 
  ggplot(aes(x = lon, y = lat, size = n, color = mean_size)) +
  geom_point(alpha = 0.3) +
  scale_size(range = c(0.01, 3)) +
  facet_wrap(~ year) + 
  scale_color_viridis(option = "magma") +
  ggtitle("Quarter 4") +
  NULL

# Filter Q4 because that's what we will have pelagic covariates from

dat <- filter(dat, Quarter == 4)

# Plot length over weight
ggplot(dat, aes(length_cm, weight_g, color = factor(LngtCode))) + geom_point()

# Inspect response variable
ggplot(dat, aes(condition)) + geom_histogram()

# Filter outliers
dat <- dat %>% filter(condition < 2 & condition > 0.25)

ggplot(dat, aes(condition)) + geom_histogram()

# Check the sex-variable
ggplot(dat, aes(Sex)) + geom_bar() # I can remove the U, the NA and the -9's... Lot's of data still!
dat$sex <- ifelse(!dat$Sex %in% c("F", "M"), "U", dat$Sex)
dat$sex <- as.factor(dat$sex)
ggplot(dat, aes(sex)) + geom_bar() 

dat %>% ggplot(., aes(sex)) + geom_bar() + facet_wrap(~ year)

# Plot the response variable
ggplot(dat, aes(lon, lat, color = condition)) + 
  geom_point(size = 0.5) + 
  facet_wrap(~ year) +
  scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 1) +
  NULL

# Read in map for overlaying countries
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

p0 <- ggplot(dat, aes(lon, lat, color = condition, fill = condition)) + 
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(min(dat$lon), max(dat$lon)),
           ylim = c(min(dat$lat), max(dat$lat))) +
  geom_point(size = 0.2) + 
  facet_wrap(~ year, ncol = 5) +
  scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 1) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 1) +
  NULL

pWord0 <- p0 + theme(text = element_text(size = 8),
                     legend.position = "bottom", 
                     legend.title = element_text(size = 6),
                     legend.text = element_text(size = 6),
                     axis.text = element_text(size = 4))

ggsave("figures/data_explore/condition.png", width = 6.5, height = 6.5, dpi = 600)

mdat <- dat

write.csv(mdat, "data/mdat_cond.csv", row.names = FALSE)


