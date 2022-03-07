# Short script to make supplementary plots of data 
rm(list = ls())

# Load libraries, install if needed
library(tidyverse); theme_set(theme_light(base_size = 12))
library(tidylog)
library(sf)
library(sp)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(RCurl)
library(RColorBrewer)
library(fishPifct)

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
pred_grid1 <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/pred_grid_(1_2).csv")
pred_grid2 <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/pred_grid_(2_2).csv")

pred_grid <- bind_rows(pred_grid1, pred_grid2)

cpue <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cpue_q_1_4.csv") %>% 
  drop_na(oxy, depth, temp) %>% 
  mutate(Data = "CPUE") %>% 
  filter(lon > 12 & lat < 58) %>% 
  dplyr::select(Data, X, Y, year)

cond1 <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cond_(1_2).csv") %>% 
  mutate(Data = "Condition") %>% 
  filter(lon > 12 & lat < 58) %>% 
  dplyr::select(Data, X, Y, year)

cond2 <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cond_(2_2).csv") %>% 
    mutate(Data = "Condition") %>% 
    filter(lon > 12 & lat < 58) %>% 
    dplyr::select(Data, X, Y, year)
  
cond <- bind_rows(cond1, cond2)

hauls <- bind_rows(cpue, cond)

p1 <- ggplot(swe_coast_proj) +
  geom_raster(data = pred_grid2, aes(x = X*1000, y = Y*1000, fill = factor(sub_div)), alpha = 0.8) +
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


# Plot ICES rectangles
# https://heima.hafro.is/~einarhj/spatialr/pre_ggplot2.html
scale_longitude_ices <- function(min = -44, max = 68.5, step = 1, ...) {
  breaks <- seq(min + 0.5, max - 0.5, step)
  labels <- geo::d2ir(60, breaks) %>% str_sub(3)
  return(scale_x_continuous(name = NULL,
                            breaks = breaks,
                            labels = labels,
                            ...))
}

scale_latitude_ices <- function(min = 36, max = 84.5, step = 0.5, ...) {
  breaks <- seq(min + 0.25, max - 0.25, step)
  #labels <- geo::d2ir(breaks, 0) %>% str_sub(1, 2)
  return(scale_y_continuous(name = NULL,
                            breaks = breaks,
                            #labels = labels,
                            ...))
}

ir <- 
  sf::read_sf("ftp://ftp.hafro.is/pub/data/shapes/ices_rectangles.gpkg")

ir2 <- ir %>% filter(between(west, 12, 22),
         between(south, 54, 58))

p3 <- ggplot() +
  geom_point(data = pred_grid2, aes(x = lon, y = lat), color = "skyblue") +
  geom_text(data = ir2,
            aes(x = west + 0.5, y = south + 0.25,
                label = icesname),
            angle = 45, 
            colour = "red",
            size = 3) +
  scale_longitude_ices() +
  scale_latitude_ices() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(size = 1),
        axis.ticks = element_blank(),
        panel.background = element_blank()
        , panel.ontop = TRUE
        ) +
  coord_quickmap() + 
  NULL


p1 / p3

ggsave("figures/supp/map_subdiv_areas.png", width = 6.5, height = 6.5, dpi = 600)
