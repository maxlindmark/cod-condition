#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# - Explore data
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# A. LOAD LIBRARIES ================================================================
# rm(list = ls())

# Load libraries, install if needed
library(tidyverse); theme_set(theme_classic())
# library(readxl)
# library(tidylog)
# library(RCurl)
# library(viridis)
# library(RColorBrewer)
# library(patchwork)
# library(janitor)
# library(icesDatras)
# library(mapdata)
# library(patchwork)
# library(rgdal)
# library(raster)
# library(sf)
# library(rgeos)
# library(chron)
# library(lattice)
# library(ncdf4)
# library(sdmTMB) # remotes::install_github("pbs-assess/sdmTMB")
# library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)

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


# B. DATA ==========================================================================
d <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cond.csv")

# These should be factors...
d <- d %>% mutate(year_f = as.factor(year), 
                  sex_f = as.factor(sex))

# Test interaction between cod and depth 
library(sjPlot)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())

fit <- lm(Fulton_K ~ cpue_cod_st*depth, data = d)
plot_model(fit, type = "pred", terms = c("cpue_cod_st", "depth"))

fit2 <- lm(Fulton_K ~ cpue_fle_st*depth, data = d)
plot_model(fit2, type = "pred", terms = c("cpue_fle_st", "depth"))


# Plot slopes for each length-interval
d %>% 
  filter(depth > 0) %>% 
  mutate(depth_intervall = cut(depth,
                               breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                               labels = c("10","20","30", "40", "50", "60", "70", "80", "90", "100"))) %>% 
  as.data.frame() %>% 
  ggplot(., aes(cpue_cod_st, Fulton_K, color = depth_intervall)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  ggtitle("cpue_cod_st") +  
  stat_smooth(method = "lm") + 
  coord_cartesian(xlim = c(0, 8)) +
  NULL


# C. EXPLORE =======================================================================
# - Do the exploratory plots like this:
#   - https://dpananos.github.io/posts/2018/04/blog-post-8/

# Explore depth distribution in data
d %>% 
  filter(depth > 0) %>% 
  mutate(category = cut(length_cm,
                        breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
                        labels = c("10","20","30", "40", "50", "60", "70"))) %>% 
  as.data.frame() %>% 
  ggplot(., aes(depth)) +
  geom_histogram() + 
  facet_wrap(~ category, scales = "free_y") + 
  geom_vline(xintercept = 70, color = "red") + 
  coord_cartesian(expand = 0) +
  NULL

# Plot condition over time
ggplot(d, aes(year, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition over time") 

# Group by deep areas
d %>% 
  filter(depth > 0) %>% 
  mutate(deep = ifelse(depth > 80, "Y", "N")) %>% 
  ggplot(., aes(year, Fulton_K, color = deep)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("condition over time group deep") + 
  NULL


# ** Plot condition vs oxygen ======================================================
ggplot(d, aes(oxy_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition vs oxygen") 

# Group by deep areas
d %>% 
  filter(depth > 0) %>% 
  mutate(deep = ifelse(depth > 80, "Y", "N")) %>% 
  ggplot(., aes(oxy_st, Fulton_K, color = deep)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("condition over oxygen group deep") + 
  NULL

# Plot East vs West Baltic
# Plot condition vs oxygen
d %>% 
  mutate(area = ifelse(lon > 15, "East", "West")) %>% 
  ggplot(., aes(oxy_st, Fulton_K, color = area)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition vs oxygen by area") 

d %>% 
  mutate(area = ifelse(lon > 15, "East", "West")) %>% 
  ggplot(., aes(year, Fulton_K, color = area)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") + 
  ggtitle("condition vs time by area") 

# Check if slope changes by year
# https://community.rstudio.com/t/extract-slopes-by-group-broom-dplyr/2751/7
d %>% 
  split(.$year) %>% 
  purrr::map(~lm(Fulton_K ~ oxy_st, data = .x)) %>% 
  purrr::map_df(broom::tidy, .id = 'year') %>%
  filter(term == 'oxy_st') %>% 
  mutate(ci_low = estimate + -1.96*std.error,
         ci_high = estimate + 1.96*std.error) %>% 
  mutate(year_n = as.numeric(year)) %>%
  ggplot(., aes(year_n, estimate)) +
  stat_smooth(method = "lm", color = "black") + 
  geom_point() + 
  #  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, x = year_n)) + 
  NULL

# A little bit yes


# ** Plot condition vs pelagics ====================================================
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


# ** Plot condition vs cod =========================================================
# Plot the slopes of the condition-cod relationship at depth intervals
d %>% 
  filter(depth > 0) %>% 
  mutate(depth_intervall = cut(depth,
                               breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
                               labels = c("10","20","30", "40", "50", "60", "70", "80", "90"))) %>% 
  split(.$depth_intervall) %>% 
  purrr::map(~lm(Fulton_K ~ cpue_cod_st, data = .x)) %>% 
  purrr::map_df(broom::tidy, .id = 'depth_intervall') %>%
  filter(term == 'cpue_cod_st') %>% 
  mutate(ci_low = estimate + -1.96*std.error,
         ci_high = estimate + 1.96*std.error) %>% 
  ggplot(., aes(depth_intervall, estimate)) +
  stat_smooth(method = "lm", color = "black") + 
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, x = depth_intervall, width = 0.2)) + 
  coord_cartesian(ylim = c(-0.02, 0.02)) +
  NULL

# Plot abundance over time
d %>% 
  group_by(year) %>% 
  summarise(mean_cpue_cod_st = mean(cpue_cod_st)) %>% 
  ggplot(., aes(year, mean_cpue_cod_st)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_cod_st~time") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

# Plot abundance over time
d %>% 
  group_by(year) %>% 
  summarise(mean_cpue_cod_above_30cm_st = mean(cpue_cod_above_30cm_st)) %>% 
  ggplot(., aes(year, mean_cpue_cod_above_30cm_st)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_cod_st~time") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

# Plot abundance over time
d %>% 
  group_by(year) %>% 
  summarise(mean_cpue_cod_below_30cm_st = mean(cpue_cod_below_30cm_st)) %>% 
  ggplot(., aes(year, mean_cpue_cod_below_30cm_st)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_cod_st~time") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

# Condition vs cod
ggplot(d, aes(cpue_cod_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_cod_st") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

# Group by deep areas
d %>% 
  filter(depth > 0) %>% 
  mutate(deep = ifelse(depth > 80, "Y", "N")) %>% 
  ggplot(., aes(cpue_cod_st, Fulton_K, color = deep)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_cod_st group deep") + 
  coord_cartesian(xlim = c(0, 8)) +
  NULL

ggplot(d, aes(cpue_cod_above_30cm_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cod_above_30cm") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

# Group by deep areas
d %>% 
  filter(depth > 0) %>% 
  mutate(deep = ifelse(depth > 80, "Y", "N")) %>% 
  ggplot(., aes(cpue_cod_above_30cm_st, Fulton_K, color = deep)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_cod_above_30cm_st group deep") + 
  coord_cartesian(xlim = c(0, 8)) +
  NULL

ggplot(d, aes(cpue_cod_below_30cm_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cod_below_30cm") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

# Group by deep areas
d %>% 
  filter(depth > 0) %>% 
  mutate(deep = ifelse(depth > 80, "Y", "N")) %>% 
  ggplot(., aes(cpue_cod_below_30cm_st, Fulton_K, color = deep)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_cod_below_30cm_st group deep") + 
  coord_cartesian(xlim = c(0, 8)) +
  NULL


# ** Plot condition vs cod =========================================================
# Condition vs flounder
ggplot(d, aes(cpue_fle_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_st") + 
  #coord_cartesian(xlim = c(0.001, 5)) + 
  NULL

# Group by deep areas
d %>% 
  filter(depth > 0) %>% 
  mutate(deep = ifelse(depth > 70, "Y", "N")) %>% 
  ggplot(., aes(cpue_fle_st, Fulton_K, color = deep)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_st group deep") + 
  coord_cartesian(xlim = c(0, 13)) +
  NULL

ggplot(d, aes(cpue_fle_above_20cm_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_above_20cm_st") + 
  #coord_cartesian(xlim = c(0, 5)) +
  NULL

# Group by deep areas
d %>% 
  filter(depth > 0) %>% 
  mutate(deep = ifelse(depth > 70, "Y", "N")) %>% 
  ggplot(., aes(cpue_fle_above_20cm_st, Fulton_K, color = deep)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_above_20cm_st group deep") + 
  coord_cartesian(xlim = c(0, 13)) +
  NULL

ggplot(d, aes(cpue_fle_below_20cm_st, Fulton_K)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_below_20cm_st") + 
  #coord_cartesian(xlim = c(0.001, 5)) +
  NULL

# Group by deep areas
d %>% 
  filter(depth > 0) %>% 
  mutate(deep = ifelse(depth > 70, "Y", "N")) %>% 
  ggplot(., aes(cpue_fle_below_20cm_st, Fulton_K, color = deep)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth() +
  ggtitle("cpue_fle_below_20cm_st group deep") + 
  coord_cartesian(xlim = c(0, 13)) +
  NULL

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
# Average over rectangle
# Plot condition vs cod and flounder
d %>% 
  group_by(StatRec, year_f) %>% 
  mutate(Fulton_K_mean = mean(Fulton_K),
         cpue_cod_st_mean = mean(cpue_cod_st)) %>% 
  ungroup() %>% 
  distinct(.keep_all = TRUE) %>% 
  ggplot(., aes(cpue_cod_st_mean, Fulton_K_mean)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") +
  facet_wrap(~year_f, scales = "free") + 
  ggtitle("cpue_cod_st_mean rectangle") + 
  NULL

# Plot condition vs cod and flounder
d %>% 
  group_by(StatRec, year_f) %>% 
  mutate(Fulton_K_mean = mean(Fulton_K),
         cpue_fle_st_mean = mean(cpue_fle_st)) %>% 
  ungroup() %>% 
  distinct(.keep_all = TRUE) %>% 
  ggplot(., aes(cpue_fle_st_mean, Fulton_K_mean)) +
  geom_point(shape = 21, fill = "black", color = "white") + 
  stat_smooth(method = "lm") +
  facet_wrap(~year_f, scales = "free") + 
  ggtitle("cpue_fle_st_mean rectangle") + 
  #  coord_cartesian(xlim = c(0, 11)) +
  NULL

# Plot condition vs cod and flounder - average over sub division
# d %>% 
#   group_by(StatRec, year_f) %>% 
#   mutate(Fulton_K_mean = mean(Fulton_K),
#          cpue_cod_st_mean = mean(cpue_cod_st)) %>% 
#   ungroup() %>% 
#   distinct(.keep_all = TRUE) %>% 
#   ggplot(., aes(cpue_cod_st_mean, Fulton_K_mean)) +
#   geom_point(shape = 21, fill = "black", color = "white") + 
#   stat_smooth(method = "lm") +
#   facet_wrap(~year_f, scales = "free") + 
#   ggtitle("cpue_cod_st_mean sub div") + 
#   NULL
# 
# # Plot condition vs cod and flounder
# d %>% 
#   group_by(StatRec, year_f) %>% 
#   mutate(Fulton_K_mean = mean(Fulton_K),
#          cpue_fle_st_mean = mean(cpue_fle_st)) %>% 
#   ungroup() %>% 
#   distinct(.keep_all = TRUE) %>% 
#   ggplot(., aes(cpue_fle_st_mean, Fulton_K_mean)) +
#   geom_point(shape = 21, fill = "black", color = "white") + 
#   stat_smooth(method = "lm") +
#   facet_wrap(~year_f, scales = "free") + 
#   ggtitle("cpue_fle_st_mean  sub div") + 
#   #  coord_cartesian(xlim = c(0, 11)) +
#   NULL