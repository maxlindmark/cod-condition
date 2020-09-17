#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# Fit and explore different spatial and spatio-temporal glmms with 
# sdmtmb: https://github.com/pbs-assess/sdmTMB
# 
# A. Load libraries
# 
# B. Read data
# 
# C. Explore variables
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
# Load libraries, install if needed
library(tidyverse)
library(tidylog)
library(viridis)


# B. READ DATA =====================================================================
d <- read.csv("data/mdat_cond.csv") 

# Calculate some variables in data
d <- d %>%
  dplyr::select(-X) %>% 
  rename("Y" = "lat", "X" = "lon") %>% 
  mutate(ln_weight_g = log(weight_g),
         ln_length_cm = log(length_cm),
         Fulton_K = weight_g/(0.01*length_cm^3)) %>% # Just an approximation for now
  dplyr::select(year, Y, X, sex, length_cm, weight_g, Quarter, CPUE_cod, CPUE_fle,
                ln_length_cm, ln_weight_g, Fulton_K)

# Plot "Fulton K" in space and time 
d %>%
  ggplot(., aes(X, Y, color = Fulton_K)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  facet_wrap(~ year, ncol = 5) +
  scale_color_gradient2(midpoint = mean(d$Fulton_K)) +
  theme_classic(base_size = 8) + 
  NULL

# Plot "Fulton K" by year
d %>%
  ggplot(., aes(year, Fulton_K)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  theme_classic(base_size = 8) + 
  stat_smooth(method = "lm") +
  NULL

summary(lm(Fulton_K ~ year, data = d))

# Plot "Fulton K" as function of cod CPUE
d %>%
  ggplot(., aes(CPUE_cod, Fulton_K)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  theme_classic(base_size = 8) + 
  stat_smooth(method = "lm") +
  NULL

summary(lm(Fulton_K ~ CPUE_cod, data = d))

# Plot "Fulton K" as function of flounder CPUE
d %>%
  ggplot(., aes(CPUE_fle, Fulton_K)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  theme_classic(base_size = 8) + 
  stat_smooth(method = "lm") +
  NULL

summary(lm(Fulton_K ~ CPUE_fle, data = d))

# Plot "Fulton K" as function of cod CPUE by year
d %>%
  ggplot(., aes(CPUE_cod, Fulton_K)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  theme_classic(base_size = 8) + 
  stat_smooth(method = "lm") +
  facet_wrap(~year, scales = "free") + 
  NULL

# Plot "Fulton K" as function of fle CPUE by year
d %>%
  ggplot(., aes(CPUE_fle, Fulton_K)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  theme_classic(base_size = 8) + 
  stat_smooth(method = "lm") +
  facet_wrap(~year, scales = "free") + 
  NULL


# Plot "Fulton K" as function of cod CPUE by year, check for thresholds
d %>%
  mutate(high = ifelse(CPUE_cod > 2000, "y", "n")) %>% 
  filter(CPUE_cod < 10000) %>%
  filter(Fulton_K < 1.5 & Fulton_K > 0.5) %>% 
  ggplot(., aes(CPUE_cod, Fulton_K, color = high, group = high)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  theme_classic(base_size = 8) + 
  stat_smooth(method = "lm", color = "black") +
  NULL

# Plot "Fulton K" as function of fle CPUE by year, check for thresholds
d %>%
  mutate(high = ifelse(CPUE_fle > 2000, "y", "n")) %>% 
  filter(CPUE_fle < 10000) %>%
  filter(Fulton_K < 1.5 & Fulton_K > 0.5) %>% 
  ggplot(., aes(CPUE_fle, Fulton_K, color = high, group = high)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  theme_classic(base_size = 8) + 
  stat_smooth(method = "lm", color = "black") +
  NULL




