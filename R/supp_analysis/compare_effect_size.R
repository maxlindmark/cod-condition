# Compare effect sizes between sdmTMB and simpler lm's
library(tidyverse); theme_set(theme_light(base_size = 12))
library(tidylog)
library(sdmTMB)

d1 <- readr::read_csv("data/for_analysis/mdat_cond_(1_2).csv")
d2 <- readr::read_csv("data/for_analysis/mdat_cond_(2_2).csv")

d <- bind_rows(d1, d2)

# Calculate standardized variables
d <- d %>% 
  mutate(ln_length_cm = log(length_cm),
         ln_weight_g = log(weight_g),
         year_ct = year - mean(year),
         biomass_her_sc = biomass_her,
         biomass_her_sd_sc = biomass_her_sd,
         biomass_spr_sc = biomass_spr,
         biomass_spr_sd_sc = biomass_spr_sd,
         density_cod_sc = density_cod,
         density_cod_rec_sc = density_cod_rec,
         density_fle_sc = density_fle,
         density_fle_rec_sc = density_fle_rec,
         density_saduria_sc = density_saduria,
         density_saduria_rec_sc = density_saduria_rec,
         depth_sc = depth,
         depth_rec_sc = depth_rec,
         oxy_sc = oxy,
         oxy_rec_sc = oxy_rec,
         temp_sc = temp,
         temp_rec_sc = temp_rec)  %>%
  mutate_at(c("biomass_her_sc", "biomass_her_sd_sc", "biomass_spr_sc", "biomass_spr_sd_sc",
              "density_cod_sc", "density_cod_rec_sc", 
              "density_fle_sc", "density_fle_rec_sc", 
              "density_saduria_sc", "density_saduria_rec_sc", 
              "depth_sc", "depth_rec_sc",
              "oxy_sc", "oxy_rec_sc",
              "temp_sc", "temp_rec_sc"
  ),
  ~(scale(.) %>% as.vector)) %>% 
  mutate(year = as.integer(year))

## Fit ind-level model with all predictors
m1 <- lm(ln_weight_g ~ year + ln_length_cm + biomass_her_sc + biomass_her_sd_sc +
           biomass_spr_sc + biomass_spr_sd_sc +
           density_cod_sc + density_cod_rec_sc + 
           density_fle_sc + density_fle_rec_sc + 
           density_saduria_sc + density_saduria_rec_sc +
           depth_sc + depth_rec_sc +
           oxy_sc + oxy_rec_sc + 
           temp_sc + temp_rec_sc, data = d)

## Extract coefficients and join models 
# Read in coefficients from the main model
mfull_est <- read.csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/output/mfull_est.csv") %>%
  dplyr::select(-X) %>% 
  mutate(Model = "sdmTMB", term = as.factor(term))

m1_df <-
  data.frame(estimate = as.data.frame(m1$coefficients)$"m1$coefficients",
             term = rownames(as.data.frame(m1$coefficients)),
             std.error = as.data.frame(summary(m1)$coefficients[, 2])$"summary(m1)$coefficients[, 2]") %>% 
  filter(term %in% mfull_est$term) %>% 
  mutate(Model = "lm",
         conf.low = estimate - 1.96*std.error,
         conf.high = estimate + 1.96*std.error)

plot_dat <- bind_rows(mfull_est, m1_df) %>% 
  mutate(term = as.factor(term))

# Plot effects
ggplot(plot_dat, aes(term, estimate, color = Model)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray40", alpha = 0.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,
                position = position_dodge(width = 0.4)) +
  geom_point(size = 1, position = position_dodge(width = 0.4)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "", y = "Standardized coefficient") +
  coord_flip() +
  theme(plot.margin = unit(c(0.4, 0.4, 0.4, 0), "cm"),
        legend.position = "bottom") +
NULL

ggsave("figures/supp/test_effect_size.png", width = 6, height = 6, dpi = 600)

