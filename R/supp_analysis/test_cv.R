# Read in the old data
library(tidyverse)
library(sf)
library(sp)
library(ggridges)
library(mapplots)
library(viridis)

d_old <-
  read_csv("/Users/maxlindmark/Desktop/cod_condition-cd328ecbeb93824ace3c4ffc3354fa7ad0f3293a/data/for_analysis/mdat_cond.csv")

d_old <- d_old %>% drop_na(abun_spr_sd)

d_old$fulton <- 100*d_old$weight_g/d_old$length_cm^3

d_old %>% group_by(year) %>% 
  summarise(mean_cond = mean(fulton),
            sd_cond = sd(fulton)) %>% 
  mutate(cv = sd_cond / mean_cond) %>% 
  ggplot(., aes(year, cv)) + geom_point()

# Add UTM coords to old data
LongLatToUTM <- function(x, y, zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

utm_coords <- LongLatToUTM(d_old$lon, d_old$lat, zone = 33)
d_old$X <- utm_coords$X/1000 # for computational reasons
d_old$Y <- utm_coords$Y/1000 # for computational reasons


## new data
d1 <- readr::read_csv("data/for_analysis/mdat_cond_(1_2).csv")
d2 <- readr::read_csv("data/for_analysis/mdat_cond_(2_2).csv")

d_new <- bind_rows(d1, d2)

d_new$fulton <- 100*d_new$weight_g/d_new$length_cm^3

d_new %>% group_by(year) %>% 
  summarise(mean_cond = mean(fulton),
            sd_cond = sd(fulton)) %>% 
  mutate(cv = sd_cond / mean_cond) %>% 
  ggplot(., aes(year, cv)) + geom_point()

summary(d_old)
summary(d_new)

d_new2 <- d_new %>% dplyr::select(year, lat, lon, length_cm, weight_g, fulton) %>% mutate(time = "new")
d_old2 <- d_old %>% dplyr::select(year, lat, lon, length_cm, weight_g, fulton) %>% mutate(time = "old")

d_tot <- bind_rows(d_new2, d_old2)

d_tot %>% group_by(year, time) %>% summarise(mean_fulton = mean(fulton)) %>% 
  ggplot(., aes(year, mean_fulton, color = time)) + geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), se = FALSE)

d_tot %>% group_by(year, time) %>% summarise(sd_fulton = sd(fulton)) %>% 
  ggplot(., aes(year, sd_fulton, color = time)) + geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), se = FALSE)

d_tot %>% group_by(year, time) %>% summarise(mean_fulton = mean(fulton),
                                            sd_fulton = sd(fulton)) %>% 
  ggplot(., aes(year, sd_fulton/mean_fulton, color = time)) + geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3),se = FALSE)

# Ok, so it's mainly the sd that is different, and the mean size. Plot size-distribution
ggplot(d_tot, aes(x = weight_g, y = factor(year), fill = factor(time))) +
  geom_density_ridges2(alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 2500))

ggplot(d_tot, aes(x = length_cm, y = factor(year), fill = factor(time))) +
  geom_density_ridges2(alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 80))

ggplot(d_tot, aes(x = fulton, y = factor(year), fill = factor(time))) +
  geom_density_ridges2(alpha = 0.5) + 
  coord_cartesian(xlim = c(0.6, 1.3))

# Is this a spatial effect?
ymin = 52; ymax = 60; xmin = 10; xmax = 24

map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", continent = "europe")

# Crop the polygon for plotting and efficiency:
# st_bbox(map_data) # find the rough coordinates
swe_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

# Transform our map into UTM 33 coordinates, which is the equal-area projection we fit in:
utm_zone33 <- 32633
swe_coast_proj <- sf::st_transform(swe_coast, crs = utm_zone33)

# Define plotting theme for main plot
theme_plot <- function(base_size = 10, base_family = "") {
  theme_light(base_size = 10, base_family = "") +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.text = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.position = "bottom",
      legend.key.height = unit(0.2, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, -5, -5, -5),
      strip.text = element_text(size = 8, colour = 'gray10', margin = margin()),
      strip.background = element_rect(fill = "grey95")
    )
}

# Define plotting theme for facet_wrap map with years
theme_facet_map <- function(base_size = 10, base_family = "") {
  theme_light(base_size = 10, base_family = "") +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.text = element_text(size = 6),
      strip.text = element_text(size = 8, colour = 'gray10', margin = margin()),
      strip.background = element_rect(fill = "gray95"),
      legend.position = c(0.7, 0.02),
      legend.direction = "horizontal"
    )
}

# Make default base map plot
xmin2 <- 303000; xmax2 <- 916000; xrange <- xmax2 - xmin2
ymin2 <- 5980000; ymax2 <- 6450000; yrange <- ymax2 - ymin2

plot_map_raster <- 
  ggplot(swe_coast_proj) + 
  xlim(xmin2, xmax2) +
  ylim(ymin2, ymax2) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.01) + 
  theme_plot() +
  geom_sf() +
  NULL

plot_map_raster + 
  geom_point(data = filter(d_new, year < 2002), aes(X*1000, Y*1000, fill = fulton),
             shape = 21, color = "black", size = 2, stroke = 0.1) + 
  scale_fill_gradient2(midpoint = 1) + 
  facet_wrap(~year) +
  theme_classic() +
  ggtitle("New")

ggsave("figures/supp/new_fulton_map.png", width = 6.5, height = 6.5, dpi = 600)

plot_map_raster + 
  geom_point(data = filter(d_old, year < 2002), aes(X*1000, Y*1000, fill = fulton),
             shape = 21, color = "black", size = 2, stroke = 0.1) + 
  scale_fill_gradient2(midpoint = 1) + 
  facet_wrap(~year) +
  theme_classic() +
  ggtitle("Old")

ggsave("figures/supp/old_fulton_map.png", width = 6.5, height = 6.5, dpi = 600)

# Now group by ices_rectangle and fill by stdev
d_old$ices_rect <- mapplots::ices.rect2(lon = d_old$lon, lat = d_old$lat)

d_old_rect <- d_old %>%
  group_by(year, ices_rect) %>%
  mutate(sd_fulton = sd(fulton)) %>%
  ungroup() %>% 
  mutate(year_rect = paste(year, ices_rect)) %>% 
  distinct(year_rect, .keep_all = TRUE)

d_new_rect <- d_new %>%
  group_by(year, ices_rect) %>%
  mutate(sd_fulton = sd(fulton)) %>% 
  ungroup() %>% 
  mutate(year_rect = paste(year, ices_rect)) %>% 
  distinct(year_rect, .keep_all = TRUE)
  
plot_map_raster + 
  geom_point(data = filter(d_new_rect, year < 1999), aes(X*1000, Y*1000, color = sd_fulton)) + 
  scale_color_viridis(limits = c(0, 0.3)) + 
  facet_wrap(~year) +
  theme_classic() +
  ggtitle("New")

ggsave("figures/supp/new_fulton_sd_map.png", width = 6.5, height = 6.5, dpi = 600)

plot_map_raster + 
  geom_point(data = filter(d_old_rect, year < 1999), aes(X*1000, Y*1000, color = sd_fulton)) + 
  scale_color_viridis(limits = c(0, 0.3)) + 
  facet_wrap(~year) +
  theme_classic() +
  ggtitle("Old")

ggsave("figures/supp/old_fulton_sd_map.png", width = 6.5, height = 6.5, dpi = 600)

  
  

