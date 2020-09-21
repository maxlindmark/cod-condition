#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# - Code to clean and merge BITS HH, HL, and CA data directly from DATRAS
#   We want to end up with a dataset of length-at-weight of cod, with haul information 
#   and columns indicating the catch of cod and flounder at the haul.
# 
# The approach is as follows:
#   1. Read and clean the haul information data (HH)
#   2. Read, clean and summarize the catch data (HL) so that 1 row = 1 haul with info on the
#      total catch. Do this by species separately
#   3. Still separately by species, join the HH data and the HL data to merge e.g.
#      coordinates with the catch data.
#   4. Now do a left join by ID (i.e. haul) so that the flounder CPUE becomes a column
#      in the cod data.
#   5. Lastly, we take the CA data which has weight-length information. For the IDs that
#      are in the CA data, we take haul and catch information from the HH and HL data
#   * Note that we could have done steps 1-4 with a subset of the ID's that are in the CA
#     data, but we don't here, maybe we need a complete catch data...
#   The code after this further cleans the data to be used in the model.
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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
# rm(list = ls())

# Load libraries, install if needed
library(tidyverse)
library(readxl)
library(tidylog)
library(RCurl)
library(viridis)
library(RColorBrewer)
library(patchwork)
library(janitor)
library(icesDatras)
library(ggsidekick) # devtools::install_github("seananderson/ggsidekick")
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

# Set a nice theme from Sean Anderson
theme_set(ggsidekick::theme_sleek())


# B. GET HAUL INFORMATION INTO CATCH DATA ==========================================

#** Read HH Data ===================================================================
# Load HH data using the DATRAS package to get catches
# bits_hh <- getDATRAS(record = "HH", survey = "BITS", years = 1991:2020, quarters = 1:4)

# write.csv("data/bits_hh.csv")
bits_hh <- read.csv("data/DATRAS/bits_hh.csv")

# Create ID column
bits_hh$ID <- paste(bits_hh$Year, bits_hh$Quarter, bits_hh$Country, bits_hh$Ship,
                    bits_hh$Gear, bits_hh$HaulNo, bits_hh$StNo, sep = ".")

# Check that per ID, there's only one row
bits_hh %>% 
  group_by(ID) %>% 
  mutate(n = n()) %>% 
  ungroup() %>%
  ggplot(., aes(n)) + geom_bar()

ggplot(bits_hh, aes(BotSal)) + geom_histogram()
ggplot(bits_hh, aes(SurSal)) + geom_histogram()
ggplot(bits_hh, aes(BotTemp)) + geom_histogram()

# Plot haul-duration
ggplot(bits_hh, aes(HaulDur)) + geom_histogram()

# Select only useful columns, this is the dataframe used in the merge later on
bits_hh_filter <- bits_hh %>% dplyr::select(ID, ShootLat, ShootLong, StatRec, Depth, BotTemp,
                                            BotSal, Year, Quarter, HaulDur, DataType)

# Test I only got 1 row per haul
bits_hh_filter %>% 
  group_by(ID) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(n)) + geom_bar()


#** Read cod HL data ====================================================================
# Load HL data using the DATRAS package to get catches
# bits_hl <- getDATRAS(record = "HL", survey = "BITS", years = 1991:2020, quarters = 1:4)

# write.csv("data/bits_hl.csv")
bits_hl <- read.csv("data/DATRAS/bits_hl.csv")

# Filter only cod
bits_hl_cod <- bits_hl %>% filter(SpecCode %in% c("164712", "126436"))
bits_hl_cod$Species <- "Cod"

# Create ID column
bits_hl_cod$ID <- paste(bits_hl_cod$Year, bits_hl_cod$Quarter, bits_hl_cod$Country,
                        bits_hl_cod$Ship, bits_hl_cod$Gear, bits_hl_cod$HaulNo,
                        bits_hl_cod$StNo, sep = ".")

bits_hl_cod$ID <- as.factor(bits_hl_cod$ID)

length(unique(bits_hl_cod$ID))

head(bits_hl_cod)

# Some extremely large observations... This is due to differences in trawl duration. 
# Later, after we merge with haul data, we instead look at CPUE
bits_hl_cod %>% 
  filter(TotalNo < 10000) %>% 
  ggplot(., aes(TotalNo)) + geom_histogram()

max(bits_hl_cod$TotalNo)

# Check that per ID, there's only one unique TotalNo (number of fish in the given haul and species)
bits_hl_cod %>% 
  group_by(ID) %>% 
  mutate(unique_catch = length(unique(TotalNo))) %>% 
  ungroup() %>% 
  ggplot(., aes(unique_catch)) + geom_bar()

# It is not. It seems to be grouped also by sex.
# I need to make a new total column that is the total catch of a species.
# First I'll create a new dataframe that is summarized (1 row per Species, ID and Sex)
# Then I'll add that (the sum across sexes) back into the main data in a new column using left join 
sum_bits_hl_cod <- bits_hl_cod %>% 
  group_by(Species, ID, Sex) %>% 
  summarise(TotalNo = unique(TotalNo)) %>% 
  ungroup() %>% 
  group_by(Species, ID) %>% 
  mutate(TotalNo_all = sum(TotalNo)) %>% 
  ungroup()

# Check it worked
head(dplyr::filter(sum_bits_hl_cod, Sex == "M")$ID, 1)

sum_bits_hl_cod %>% 
  filter(ID == "1991.2.LAT.ZBA.LBT.39.000003") %>% 
  droplevels() %>% 
  dplyr::select(TotalNo, ID, Sex, TotalNo_all) %>% 
  as.data.frame()

# Now do a left join to add it back
# This is what bits_hl_cod looks like before
bits_hl_cod %>% 
  filter(ID == "1991.2.LAT.ZBA.LBT.39.000003") %>% 
  droplevels() %>% 
  dplyr::select(TotalNo, ID, Sex)

bits_hl_cod <- dplyr::left_join(bits_hl_cod, sum_bits_hl_cod)

# Now check it worked:
bits_hl_cod %>% 
  filter(ID == "1991.2.LAT.ZBA.LBT.39.000003") %>% 
  droplevels() %>% 
  dplyr::select(TotalNo, ID, Sex, TotalNo_all)

# Looks correct! Will need to check for an ID with only one sex also
data.frame(bits_hl_cod)

# Now plot # of unique TotalNo per ID
bits_hl_cod %>% 
  group_by(ID) %>% 
  mutate(unique_catch = length(unique(TotalNo_all))) %>% 
  ungroup() %>% 
  ggplot(., aes(unique_catch)) + geom_bar()

# Remove rows with -9 in catch. All the trawl hauls are present in the HH data, so we 
# can safely remove them here since we will join the later and thus get the 0-catch
# hauls as well!
bits_hl_cod <- bits_hl_cod %>% filter(TotalNo_all > -1)

# Remove duplicates. For these data, I only need one row per haul and the amount of
# fish (there's more than one row because it's per length class)
# This means we omit any length-information in the catch (!)
bits_hl_cod %>% 
  group_by(ID) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(n)) + geom_bar()

nrow(bits_hl_cod)

bits_hl_cod <- bits_hl_cod %>% distinct(ID, .keep_all = TRUE)

# Check it worked - plot # rows per ID
bits_hl_cod %>% 
  group_by(ID) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(n)) + geom_bar()


#** Read flounder HL data ===============================================================
# Load HL data using the DATRAS package to get catches
# bits_hl <- getDATRAS(record = "HL", survey = "BITS", years = 1991:2020, quarters = 1:4)

# write.csv("data/bits_hl.csv")
bits_hl <- read.csv("data/DATRAS/bits_hl.csv")

# Filter only flounder
bits_hl_fle <- bits_hl %>% filter(SpecCode %in% c("127141", "172894"))
bits_hl_fle$Species <- "Flounder"

# Create ID column
bits_hl_fle$ID <- paste(bits_hl_fle$Year, bits_hl_fle$Quarter, bits_hl_fle$Country,
                        bits_hl_fle$Ship, bits_hl_fle$Gear, bits_hl_fle$HaulNo,
                        bits_hl_fle$StNo, sep = ".")

bits_hl_fle$ID <- as.factor(bits_hl_fle$ID)

length(unique(bits_hl_fle$ID))

head(bits_hl_fle)

# Some extremely large observations... This is due to differences in trawl duration. 
# Later, after we merge with haul data, we instead look at CPUE
bits_hl_fle %>% 
  filter(TotalNo < 10000) %>% 
  ggplot(., aes(TotalNo)) + geom_histogram()

max(bits_hl_fle$TotalNo)

# Check that per ID, there's only one unique TotalNo (number of fish in the given haul and species)
bits_hl_fle %>% 
  group_by(ID) %>% 
  mutate(unique_catch = length(unique(TotalNo))) %>% 
  ungroup() %>% 
  ggplot(., aes(unique_catch)) + geom_bar()

# It is not. It seems to be grouped also by sex.
# I need to make a new total column that is the total catch of a species.
# First I'll create a new dataframe that is summarized (1 row per Species, ID and Sex)
# Then I'll add that (the sum across sexes) back into the main data in a new column using left join 
sum_bits_hl_fle <- bits_hl_fle %>% 
  group_by(Species, ID, Sex) %>% 
  summarise(TotalNo = unique(TotalNo)) %>% 
  ungroup() %>% 
  group_by(Species, ID) %>% 
  mutate(TotalNo_all = sum(TotalNo)) %>% 
  ungroup()

# Check it worked
head(dplyr::filter(sum_bits_hl_fle, Sex == "M")$ID, 1)

sum_bits_hl_fle %>% 
  filter(ID == "1991.2.LAT.ZBA.LBT.39.000003") %>% 
  droplevels() %>% 
  dplyr::select(TotalNo, ID, Sex, TotalNo_all) %>% 
  as.data.frame()

# Now do a left join to add it back
# This is what bits_hl_fle looks like before
bits_hl_fle %>% 
  filter(ID == "1991.2.LAT.ZBA.LBT.39.000003") %>% 
  droplevels() %>% 
  dplyr::select(TotalNo, ID, Sex)

bits_hl_fle <- dplyr::left_join(bits_hl_fle, sum_bits_hl_fle)

# Now check it worked:
bits_hl_fle %>% 
  filter(ID == "1991.2.LAT.ZBA.LBT.39.000003") %>% 
  droplevels() %>% 
  dplyr::select(TotalNo, ID, Sex, TotalNo_all)

# Looks correct! Will need to check for an ID with only one sex also
data.frame(bits_hl_fle)

# Now plot # of unique TotalNo per ID
bits_hl_fle %>% 
  group_by(ID) %>% 
  mutate(unique_catch = length(unique(TotalNo_all))) %>% 
  ungroup() %>% 
  ggplot(., aes(unique_catch)) + geom_bar()

# Remove rows with -9 in catch. All the trawl hauls are present in the HH data, so we 
# can safely remove them here since we will join the later and thus get the 0-catch
# hauls as well!
bits_hl_fle <- bits_hl_fle %>% filter(TotalNo_all > -1)

# Remove duplicates. For these data, I only need one row per haul and the amount of
# fish (there's more than one row because it's per length class)
# This means we omit any length-information in the catch (!)
bits_hl_fle %>% 
  group_by(ID) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(n)) + geom_bar()

nrow(bits_hl_fle)

bits_hl_fle <- bits_hl_fle %>% distinct(ID, .keep_all = TRUE)

# Check it worked - plot # rows per ID
bits_hl_fle %>% 
  group_by(ID) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(n)) + geom_bar()


#** Join HH data with cod and flounder HL data respectively ========================
#**** Cod ==========================================================================
# Add in columns with haul-information to the HL data (from the HH data).
# This also gives us all the hauls without any fish in them!!

# Join the HH and HL data to get Lat and Long in the HL data
bits_hl_full_cod <- left_join(bits_hh_filter, bits_hl_cod, by = "ID")

# Check an ID that is in the HH but not HL data to see how they are filled in with the join
test_id <- head(bits_hh$ID[!bits_hh$ID %in% bits_hl_cod$ID], 1)

# It's in the HH data...
data.frame(filter(bits_hh_filter, ID == test_id))
# It's NOT in the HL data...
data.frame(filter(bits_hl_cod, ID == test_id))
# It's NA in the joined data...
data.frame(filter(bits_hl_full_cod, ID == test_id))

# Now we need to make a single column for quarter and year (if I don't include Year
# and Quarter in the bits_hh_filter data frame they are returned as NA in the joined 
# data)
bits_hl_full_cod$Quarter <- ifelse(is.na(bits_hl_full_cod$Quarter.x),
                                   bits_hl_full_cod$Quarter.y,
                                   bits_hl_full_cod$Quarter.x)

bits_hl_full_cod$Year <- ifelse(is.na(bits_hl_full_cod$Year.x),
                                bits_hl_full_cod$Year.y,
                                bits_hl_full_cod$Year.x)

# Test if all zero catch IDs (that are in HH but not HL) have NA catch in the bits_hl_full_cod
zero_catch_ids <- bits_hh$ID[!bits_hh$ID %in% bits_hl_cod$ID]

bits_hl_full_cod %>% 
  ungroup %>% 
  filter(ID %in% zero_catch_ids) %>% 
  summarise(unique_catch = unique(TotalNo))

# Change TotalNo_all == NA to 0 and species to cod. That's because NA here means it 
# isn't present - not that's it's a true NA. It's simply a zero catch. 
bits_hl_full_cod$TotalNo_all[is.na(bits_hl_full_cod$TotalNo_all)] <- 0
bits_hl_full_cod$Species[is.na(bits_hl_full_cod$Species)] <- "Cod"

# Do I have the same number of unique hauls in this data as in HH?
length(unique(bits_hl_full_cod$ID))
length(unique(bits_hh$ID))

# Do I have 1 row per haul (ID)?
bits_hl_full_cod %>% 
  group_by(ID) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(n)) + geom_bar()

# Remove any group structure
bits_hl_full_cod <- bits_hl_full_cod %>% ungroup()

# What is the proportion of 0 catches across years and quarters?
bits_hl_full_cod %>% 
  group_by(Year, Quarter) %>% 
  mutate(catch = ifelse(TotalNo_all == 0, "N", "Y")) %>% 
  ungroup() %>% 
  ggplot(., aes(Year, fill = catch)) + 
  geom_bar() + 
  facet_wrap(~ Quarter)

# Add CPUE column. First we need to check if TotalNo is CPUE or catch, see DataType 
unique(bits_hl_full_cod$DataType)
ggplot(bits_hl_full_cod, aes(DataType)) + geom_bar()
bits_hl_full_cod <- bits_hl_full_cod %>% filter(DataType %in% c("C", "R"))

# Calculate CPUE
bits_hl_full_cod$CPUE_cod <- ifelse(bits_hl_full_cod$DataType == "C",
                                    bits_hl_full_cod$TotalNo_all,
                                    (bits_hl_full_cod$TotalNo_all / bits_hl_full_cod$HaulDur)*60)

bits_hl_full_cod %>% 
  filter(CPUE_cod < 7500) %>% 
  ggplot(., aes(CPUE_cod, fill = DataType)) + 
  geom_histogram()


#**** Flounder =====================================================================
# Add in columns with haul-information to the HL data (from the HH data).
# This also gives us all the hauls without any fish in them!!

# Join the HH and HL data to get Lat and Long in the HL data
bits_hl_full_fle <- left_join(bits_hh_filter, bits_hl_fle, by = "ID")

# Check an ID that is in the HH but not HL data to see how they are filled in with the join
test_id <- head(bits_hh$ID[!bits_hh$ID %in% bits_hl_fle$ID], 1)

# It's in the HH data...
data.frame(filter(bits_hh_filter, ID == test_id))
# It's NOT in the HL data...
data.frame(filter(bits_hl_fle, ID == test_id))
# It's NA in the joined data...
data.frame(filter(bits_hl_full_fle, ID == test_id))

# Now we need to make a single column for quarter and year (if I don't include Year
# and Quarter in the bits_hh_filter data frame they are returned as NA in the joined 
# data)
bits_hl_full_fle$Quarter <- ifelse(is.na(bits_hl_full_fle$Quarter.x),
                                   bits_hl_full_fle$Quarter.y,
                                   bits_hl_full_fle$Quarter.x)

bits_hl_full_fle$Year <- ifelse(is.na(bits_hl_full_fle$Year.x),
                                bits_hl_full_fle$Year.y,
                                bits_hl_full_fle$Year.x)

# Test if all zero catch IDs (that are in HH but not HL) have NA catch in the bits_hl_full_fle
zero_catch_ids <- bits_hh$ID[!bits_hh$ID %in% bits_hl_fle$ID]

bits_hl_full_fle %>% 
  ungroup %>% 
  filter(ID %in% zero_catch_ids) %>% 
  summarise(unique_catch = unique(TotalNo))

# Change TotalNo_all == NA to 0 and species to flounder. That's because NA here means it 
# isn't present - not that's it's a true NA. It's simply a zero catch. 
bits_hl_full_fle$TotalNo_all[is.na(bits_hl_full_fle$TotalNo_all)] <- 0
bits_hl_full_fle$Species[is.na(bits_hl_full_fle$Species)] <- "Cod"

# Do I have the same number of unique hauls in this data as in HH?
length(unique(bits_hl_full_fle$ID))
length(unique(bits_hh$ID))

# Do I have 1 row per haul (ID)?
bits_hl_full_fle %>% 
  group_by(ID) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(n)) + geom_bar()

# Remove any group structure
bits_hl_full_fle <- bits_hl_full_fle %>% ungroup()

# What is the proportion of 0 catches across years and quarters?
bits_hl_full_fle %>% 
  group_by(Year, Quarter) %>% 
  mutate(catch = ifelse(TotalNo_all == 0, "N", "Y")) %>% 
  ungroup() %>% 
  ggplot(., aes(Year, fill = catch)) + 
  geom_bar() + 
  facet_wrap(~ Quarter)

# Add CPUE column. First we need to check if TotalNo is CPUE or catch, see DataType 
unique(bits_hl_full_fle$DataType)
ggplot(bits_hl_full_fle, aes(DataType)) + geom_bar()
bits_hl_full_fle <- bits_hl_full_fle %>% filter(DataType %in% c("C", "R"))

# Calculate CPUE
bits_hl_full_fle$CPUE_fle <- ifelse(bits_hl_full_fle$DataType == "C",
                                    bits_hl_full_fle$TotalNo_all,
                                    (bits_hl_full_fle$TotalNo_all / bits_hl_full_fle$HaulDur)*60)

bits_hl_full_fle %>% 
  filter(CPUE_fle < 7500) %>% 
  ggplot(., aes(CPUE_fle, fill = DataType)) + 
  geom_histogram()


#**** Join flounder CPUE as a column to the cod data set ===========================
colnames(bits_hl_full_fle)
colnames(bits_hl_full_cod)

# First trim the haul and catch data of cod
bits_hh_hl <- bits_hl_full_cod %>% dplyr::select(-c(Year.x, Year.y, Quarter.x, Quarter.y))

# Trim them flounder data to include only ID and CPUE of flounder
fle_merge <- bits_hl_full_fle %>% dplyr::select(c(ID, CPUE_fle))

# Join with the trimmed cod data
bits_hh_hl <- left_join(bits_hh_hl, fle_merge, by = "ID")

colnames(bits_hh_hl)

bits_hh_hl %>% 
  filter(CPUE_cod < 500 & CPUE_fle < 500) %>% 
  ggplot(., aes(CPUE_cod, CPUE_fle)) + geom_point()


# C. GET HAUL & CATCH DATA INTO LENGTH-WEIGHT DATA =================================

#*** Read CA data =======================================================================
# Load CA data using the DATRAS package to get catches
# Note we only want cod data here
# bits_ca <- getDATRAS(record = "CA", survey = "BITS", years = 1991:2020, quarters = 1:4)

# write.csv("data/bits_ca.csv")
bits_ca <- read.csv("data/DATRAS/bits_ca.csv")

# Filter only cod and positive length measurements
bits_ca <- bits_ca %>% filter(SpecCode %in% c("164712", "126436") & LngtClass > 0)

# Add new species-column
bits_ca$Species <- "Cod"

# Create ID column
bits_ca$ID <- paste(bits_ca$Year, bits_ca$Quarter, bits_ca$Country, bits_ca$Ship,
                    bits_ca$Gear, bits_ca$HaulNo, bits_ca$StNo, sep = ".")

# Now I need to copy rows with NoAtLngt > 1 so that 1 row = 1 ind
# Firs make a small test
nrow(bits_ca)
head(filter(bits_ca, NoAtLngt == 5))
head(filter(bits_ca, ID == "1992.1.GFR.SOL.H20.33.42" & NoAtLngt == 5), 20)

bits_ca <- bits_ca %>% map_df(., rep, .$NoAtLngt)

head(data.frame(filter(bits_ca, ID == "1992.1.GFR.SOL.H20.33.42" & NoAtLngt == 5)), 20)
nrow(bits_ca)
# Looks ok!

# Now I need to join the bits_hh_hl with the bits_ca data to get haul information and
# catches linked to each length-weight-row
colnames(bits_ca)
colnames(bits_hh_hl)

# Use only columns that are not already in the CA data from the haul and catch data to make it easier...
hh_hl_cols <- colnames(bits_hh_hl)[!colnames(bits_hh_hl) %in% colnames(bits_ca)]

bits_hh_hl_trim <- bits_hh_hl %>% dplyr::select(c(hh_hl_cols, "ID"))
# colnames(bits_hh_hl_trim)
# hh_hl_cols

bits_ca_hh_hl <- left_join(bits_ca, bits_hh_hl_trim, by = "ID") %>% as.data.frame()

colnames(bits_ca_hh_hl)
head(bits_ca_hh_hl)

nrow(bits_ca_hh_hl)
nrow(bits_ca)

length(unique(bits_ca$ID))
length(unique(bits_ca_hh_hl$ID))
length(unique(bits_hh_hl$ID))

# Find an ID that is not in bits_ca but was in bits_hh_hl
test_id <- head(bits_hh_hl$ID[!bits_hh_hl$ID %in% bits_ca_hh_hl$ID], 1)
test_id

# It's in the bits_hh_hl data...
data.frame(filter(bits_hh_hl, ID == test_id))
# It's NOT in the length-weight data...
data.frame(filter(bits_ca_hh_hl, ID == test_id))


# D. PREPARE DATA FOR ANALYSIS =====================================================

# Calculate condition and trim dataset
dat <- bits_ca_hh_hl %>% 
  drop_na(IndWgt) %>% 
  drop_na(LngtClass) %>% 
  filter(IndWgt > 0 & LngtClass > 0) %>%  # Filter positive length and weight
  mutate(length_cm = ifelse(LngtCode == ".", 
                            LngtClass/10,
                            LngtClass)) %>% # Standardize length ((https://vocab.ices.dk/?ref=18))
  mutate(condition = 100*(IndWgt/(length_cm^3))) %>% 
  dplyr::select(condition, Year, ShootLat, ShootLong, ID, Sex, LngtClass, length_cm,
                LngtCode, IndWgt, Quarter, CPUE_cod, CPUE_fle) %>% # Can select more later !
  rename("year" = "Year",
         "lon" = "ShootLong",
         "lat" = "ShootLat",
         "condition" = "condition",
         "weight_g" = "IndWgt")

# Remove hauls outside study area including Kattegatt
dat <- dat %>% 
  filter(lat < 58 & lon > 12.5) %>% 
  mutate(kattegatt = ifelse(lat > 55.5 & lon < 14, "Y", "N")) %>% 
  filter(kattegatt == "N") %>% 
  dplyr::select(-kattegatt)

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


