# Short script to make supplementary plots of data 
rm(list = ls())

# Load libraries, install if needed
library(tidyverse); theme_set(theme_light(base_size = 12))
library(tidylog)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(RCurl)
library(RColorBrewer)

# For adding maps to plots
# These are the ranges I'm thinking of using. Convert these to UTM!
ymin = 54
ymax = 58
xmin = 12
xmax = 22

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

# Read data
pred_grid2 <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/pred_grid2.csv") %>% 
  drop_na(oxy, depth, temp)

cpue <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cpue.csv") %>% 
  drop_na(oxy, depth, temp) %>% 
  mutate(Data = "CPUE") %>% 
  filter(lon > 12 & lat < 58) %>% 
  dplyr::select(Data, X, Y, year)

cond <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cond.csv") %>% 
  drop_na(abun_spr, abun_her) %>% 
  mutate(Data = "Condition") %>% 
  filter(lon > 12 & lat < 58) %>% 
  dplyr::select(Data, X, Y, year)

hauls <- bind_rows(cpue, cond)

p1 <- ggplot(swe_coast_proj) +
  geom_raster(data = pred_grid2, aes(x = X*1000, y = Y*1000, fill = factor(SubDiv)), alpha = 0.8) +
  geom_sf() +
  scale_fill_brewer(palette = "Dark2") + 
  labs(x = "Longitude", y = "Latitude", fill = "SubDiv")

# To add text
xmin <- 303379.1; xmax <- 958492.4; xrange <- xmax - xmin
ymin <- 5983578; ymax <- 6450163; yrange <- ymax - ymin

p2 <- ggplot(swe_coast_proj) +
  geom_sf() +
  annotate("text", label = "SWEDEN", x = xmin + 0.25*xrange, y = ymin + 0.7*yrange, color = "black", size = 5) +
  annotate("text", label = "Bornholm", x = xmin + 0.4*xrange, y = ymin + 0.32*yrange, color = "tomato", size = 5) +
  annotate("text", label = "Basin", x = xmin + 0.4*xrange, y = ymin + 0.25*yrange, color = "tomato", size = 5) +
  annotate("text", label = "Öland", x = xmin + 0.5*xrange, y = ymin + 0.7*yrange, angle = 70, color = "tomato", size = 5) +
  annotate("text", label = "Gotland", x = xmin + 0.7*xrange, y = ymin + 0.9*yrange, angle = 70, color = "tomato", size = 5) +
  labs(x = "Longitude", y = "Latitude")

p1 / p2

ggsave("figures/supp/map_subdiv_areas.png", width = 6.5, height = 6.5, dpi = 600)

# ggplot(swe_coast_proj) +
#   geom_point(data = hauls, aes(x = X*1000, y = Y*1000, color = Data), size = 0.2) +
#   geom_sf(size = 0.3) +
#   facet_wrap(~year, ncol = 5) +
#   scale_fill_brewer(palette = "Dark2") + 
#   labs(x = "Longitude", y = "Latitude") + 
#   theme(axis.text.x = element_text(angle = 90),
#         axis.text = element_text(size = 6),
#         strip.text = element_text(size = 8, colour = 'black', margin = margin()),
#         strip.background = element_rect(fill = "grey90"))
# 
# ggsave("figures/supp/data_position_year.png", width = 6.5, height = 6.5, dpi = 600)
